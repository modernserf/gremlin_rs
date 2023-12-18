use crate::v2::ea::*;
use crate::v2::register::*;
use crate::v2::vm_68k::Asm;

use super::vm_68k::Branch;
use super::vm_68k::Cond;

// This time we'll really use flags for everything

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ty {
    base_size: usize,
    ref_level: usize,
}

impl Ty {
    pub fn i32() -> Self {
        Self {
            base_size: 4,
            ref_level: 0,
        }
    }
    // (len: i32, str: &[u8])
    pub fn string() -> Self {
        Self {
            base_size: 8,
            ref_level: 0,
        }
    }
    fn size_bytes(&self) -> usize {
        if self.ref_level > 0 {
            4
        } else {
            self.base_size
        }
    }
    fn storage_type(&self) -> StorageType {
        if self.ref_level >= 1 {
            StorageType::Addr
        } else {
            match self.base_size {
                1 => StorageType::Data(Size::Byte),
                2 => StorageType::Data(Size::Short),
                3 | 4 => StorageType::Data(Size::Long),
                _ => StorageType::Stack(self.base_size),
            }
        }
    }
    fn iter_blocks(&self) -> Vec<(Size, usize)> {
        let bytes = self.size_bytes();
        let mut offset = 0;
        let mut out = Vec::new();
        while offset < bytes {
            let take = usize::min(bytes - offset, 4);
            let size = match take {
                4 => Size::Long,
                _ => todo!(),
            };
            out.push((size, offset));
            offset += take;
        }
        out
    }
    pub fn pointer(&self) -> Ty {
        Self {
            base_size: self.base_size,
            ref_level: self.ref_level + 1,
        }
    }
    pub fn deref(&self) -> Ty {
        assert!(self.ref_level > 0);
        Self {
            base_size: self.base_size,
            ref_level: self.ref_level - 1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StorageType {
    Data(Size),
    Addr,
    Stack(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Item {
    Storage(Storage),
    LValue(LValue),
}

impl Item {
    fn ty(&self) -> Ty {
        match self {
            Self::Storage(storage) => storage.ty(),
            Self::LValue(lvalue) => lvalue.ty,
        }
    }
    fn src_ea(&self, memory: &mut Memory) -> EA {
        match self {
            Self::Storage(storage) => storage.ea(memory),
            Self::LValue(lvalue) => lvalue.src_ea(memory),
        }
    }
    fn storage_register(&self, memory: &mut Memory) -> Storage {
        match self {
            Self::Storage(storage) => storage.register(memory),
            Self::LValue(lvalue) => {
                let src = lvalue.src_ea(memory);
                memory.storage_register(lvalue.ty, src)
            }
        }
    }
    fn storage_stack(self, memory: &mut Memory) -> Storage {
        let src = self.src_ea(memory);
        let next = memory.storage_stack(self.ty(), src);
        self.free_transient(memory);
        next
    }
    fn free_transient(self, memory: &mut Memory) {
        match self {
            Self::Storage(storage) => storage.free_transient(memory),
            Self::LValue(_) => {}
        }
    }
    fn deref(&self, memory: &mut Memory) -> Item {
        match self {
            Self::Storage(storage) => {
                Self::Storage(storage.deref(memory, storage.ty().deref(), 0, true))
            }
            Self::LValue(lvalue) => Self::LValue(lvalue.deref()),
        }
    }
    fn field(&self, field: Field) -> Item {
        match self {
            Self::Storage(storage) => Self::Storage(storage.field(field)),
            Self::LValue(lvalue) => Self::LValue(lvalue.field(field)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Storage {
    Constant(Ty, i32),
    Data(Ty, Data),
    Addr(Ty, Addr),
    // negative offset from return addr
    StackArg(Ty, usize),
    // positive offset from end of stored registers
    Stack(Ty, usize),
}

impl Storage {
    fn ty(&self) -> Ty {
        match *self {
            Self::Constant(ty, _) => ty,
            Self::Data(ty, _) => ty,
            Self::Addr(ty, _) => ty,
            Self::StackArg(ty, _) => ty,
            Self::Stack(ty, _) => ty,
        }
    }
    fn field(&self, field: Field) -> Storage {
        match *self {
            Self::StackArg(_, base) => Self::StackArg(field.ty, base + field.offset),
            Self::Stack(_, base) => Self::Stack(field.ty, base - field.offset),
            _ => unimplemented!(),
        }
    }
    fn ea(&self, memory: &mut Memory) -> EA {
        match *self {
            Self::Constant(_, x) => EA::Immediate(x),
            Self::Data(_, d) => {
                memory.data.mark(d);
                EA::Data(d)
            }
            Self::Addr(_, a) => {
                memory.addr.mark(a);
                EA::Addr(a)
            }
            Self::StackArg(_, idx) => EA::Offset(Addr::A6, (8 + idx) as i16),
            Self::Stack(_, idx) => EA::Offset(Addr::A7, (memory.stack_offset - idx) as i16),
        }
    }
    fn register(&self, memory: &mut Memory) -> Storage {
        match *self {
            Self::Data(_, _) => *self,
            Self::Addr(_, _) => *self,
            _ => {
                let src = self.ea(memory);
                memory.storage_register(self.ty(), src)
            }
        }
    }
    fn free_transient(self, memory: &mut Memory) {
        match self {
            Self::Data(_, d) => memory.data.free(d),
            Self::Addr(_, a) => memory.addr.free(a),
            _ => {}
        }
    }
    fn deref(&self, memory: &mut Memory, deref_ty: Ty, offset: usize, consume_addr: bool) -> Self {
        match *self {
            Self::Addr(_, a) => match deref_ty.storage_type() {
                StorageType::Data(size) => {
                    let src = EA::Offset(a, 0);
                    let r = memory.get_data_register();
                    memory.asm.mov(size, src, EA::Data(r));
                    if consume_addr {
                        memory.addr.free(a);
                    } else {
                        memory.addr.mark(a);
                    }
                    Self::Data(deref_ty, r)
                }
                StorageType::Addr => {
                    let src = EA::Offset(a, offset as i16);
                    let r = if consume_addr {
                        a
                    } else {
                        memory.get_addr_register()
                    };
                    memory.asm.mov(Size::Long, src, EA::Addr(r));
                    Self::Addr(deref_ty, r)
                }
                StorageType::Stack(_) => {
                    for (size, o) in deref_ty.iter_blocks().into_iter().rev() {
                        let src = EA::Offset(a, (o + offset) as i16);
                        memory.asm.mov(size, src, EA::PreDec(Addr::A7))
                    }
                    if consume_addr {
                        memory.addr.free(a);
                    } else {
                        memory.addr.mark(a);
                    }
                    memory.stack_offset += deref_ty.size_bytes();
                    Storage::Stack(deref_ty, memory.stack_offset)
                }
            },
            _ => {
                let src = self.ea(memory);
                let r = memory.get_addr_register();
                memory.asm.mov(Size::Long, src, EA::Addr(r));
                Storage::Addr(self.ty(), r).deref(memory, deref_ty, offset, consume_addr)
            }
        }
    }
}

type LocalId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Field {
    ty: Ty,
    offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LValue {
    ty: Ty,
    base: LocalId,
    field: Option<Field>,
    deref_offsets: Vec<usize>,
}

impl LValue {
    fn new(base: LocalId, ty: Ty) -> Self {
        Self {
            ty,
            base,
            field: None,
            deref_offsets: Vec::new(),
        }
    }
    fn src_ea(&self, memory: &mut Memory) -> EA {
        let mut storage = memory.locals[self.base];
        if let Some(field) = self.field {
            storage = storage.field(field)
        }
        for offset in self.deref_offsets.iter() {
            storage = storage.deref(memory, storage.ty().deref(), *offset, false)
        }
        storage.ea(memory)
    }
    fn dest_ea(&self, memory: &mut Memory) -> EA {
        let mut storage = memory.locals[self.base];
        if let Some(field) = self.field {
            storage = storage.field(field)
        }

        let mut dest = storage.ea(memory);

        if self.deref_offsets.len() > 0 {
            let addr = memory.get_addr_register();
            for offset in self.deref_offsets.iter() {
                let src = dest.offset(*offset);
                memory.asm.mov(Size::Long, src, EA::Addr(addr));
                dest = EA::Offset(addr, 0)
            }
        }

        dest
    }
    fn deref(&self) -> LValue {
        let mut next = self.clone();
        next.deref_offsets.push(0);
        next.ty = next.ty.deref();
        next
    }
    fn field(&self, field: Field) -> Self {
        let mut next = self.clone();
        next.ty = field.ty;
        match (next.field, next.deref_offsets.len()) {
            (None, 0) => {
                next.field = Some(field);
            }
            (Some(prev), 0) => {
                next.field = Some(Field {
                    ty: field.ty,
                    offset: prev.offset + field.offset,
                })
            }
            _ => {
                let last_deref = next.deref_offsets.pop().unwrap();
                next.deref_offsets.push(last_deref + field.offset);
            }
        };
        next
    }
}

type IfIdx = usize;
type LoopIdx = usize;

#[derive(Debug)]
pub struct MatchBuilder {
    jmp_base_idx: usize,
    checked_cases: Vec<bool>,
    case_ends: Vec<usize>,
}

impl MatchBuilder {
    pub fn init(memory: &mut Memory, d: Data, case_count: usize) -> Self {
        // ensure index is in range (TODO: disable with flag)
        memory
            .asm
            .chk(Size::Long, EA::Immediate(case_count as i32), EA::Data(d));
        // get displacement from jump table
        memory.asm.asl(Size::Long, EA::Immediate(1), EA::Data(d));
        memory.asm.mov(
            Size::Short,
            EA::PCIdxData(d, PC::Displacement(6)),
            EA::Data(d),
        );
        // jump to dest
        memory.asm.jmp(EA::PCIdxData(d, PC::Displacement(0)));
        memory.data.free(d);
        // make space for jump table
        let jmp_idx = memory.asm.data_16(&vec![0; case_count]);
        Self {
            jmp_base_idx: jmp_idx,
            case_ends: Vec::new(),
            checked_cases: vec![false; case_count],
        }
    }
    pub fn case(&mut self, memory: &mut Memory, case: usize) {
        let idx = self.take_case_idx(case);
        let offset = self.current_offset(memory);
        memory.asm.fixup(idx, |asm| {
            asm.data_16(&[offset]);
        });
    }
    pub fn default_case(&mut self, memory: &mut Memory) {
        let mut cases_to_patch = Vec::new();
        for i in 0..self.checked_cases.len() {
            if !self.checked_cases[i] {
                cases_to_patch.push(self.take_case_idx(i));
            }
        }
        let offset = self.current_offset(memory);
        for idx in cases_to_patch {
            memory.asm.fixup(idx, |asm| {
                asm.data_16(&[offset]);
            });
        }
    }
    pub fn case_end(&mut self, memory: &mut Memory) {
        let end_idx = memory
            .asm
            .branch(Cond::True, Branch::Placeholder(Size::Short));
        self.case_ends.push(end_idx);
    }
    pub fn end(self, memory: &mut Memory) {
        for checked in self.checked_cases.iter() {
            if !checked {
                panic!("not all cases checked")
            }
        }
        for case_end in self.case_ends {
            memory.asm.fixup_branch_to_here(case_end);
        }
    }

    fn take_case_idx(&mut self, case: usize) -> usize {
        if self.checked_cases[case] {
            panic!("duplicate case")
        }
        self.checked_cases[case] = true;
        self.jmp_base_idx + case * 2
    }
    fn current_offset(&mut self, memory: &mut Memory) -> i16 {
        (memory.asm.here() - self.jmp_base_idx + 2) as i16
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Flags {}

impl Flags {}

#[derive(Debug, Default)]
pub struct Memory {
    flags: Flags,
    locals: Vec<Storage>,
    stack: Vec<Item>,
    data: RegisterAllocator<Data>,
    addr: RegisterAllocator<Addr>,
    asm: Asm,
    stack_offset: usize,
    strings: Vec<(usize, String)>,
}

impl Memory {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn end(mut self) -> Asm {
        self.asm.halt();
        self.resolve_strings();
        self.asm
    }

    pub fn push_i32(&mut self, value: i32) {
        self.stack
            .push(Item::Storage(Storage::Constant(Ty::i32(), value)))
    }

    pub fn push_ident(&mut self, id: LocalId) {
        let original = self.locals[id];
        self.stack
            .push(Item::LValue(LValue::new(id, original.ty())));
    }

    pub fn push_struct(&mut self, size: usize) {
        let ty = Ty {
            base_size: size,
            ref_level: 0,
        };
        self.stack_offset += size;
        self.asm
            .sub(Size::Long, EA::Immediate(size as i32), EA::Addr(Addr::A7));
        self.stack
            .push(Item::Storage(Storage::Stack(ty, self.stack_offset)));
    }

    pub fn push_string(&mut self, str: String) {
        self.asm.mov(
            Size::Long,
            EA::Immediate(str.len() as i32),
            EA::PreDec(Addr::A7),
        );
        let to_fixup = self.asm.here();
        self.asm
            .load_ea(EA::PCOffset(PC::Displacement(0)), EA::PreDec(Addr::A7));

        self.strings.push((to_fixup, str));

        self.stack_offset += 8;
        self.stack.push(Item::Storage(Storage::Stack(
            Ty::string(),
            self.stack_offset,
        )));
    }
    pub fn println(&mut self) {
        let item = self.pop_item();
        assert_eq!(item.ty(), Ty::string());
        item.storage_stack(self);
        self.asm.println();
        self.stack_offset -= 8;
    }
    pub fn log(&mut self, str: &str) {
        self.asm.mov(
            Size::Long,
            EA::Immediate(str.len() as i32),
            EA::PreDec(Addr::A7),
        );
        let to_fixup = self.asm.here();
        self.asm
            .load_ea(EA::PCOffset(PC::Displacement(0)), EA::PreDec(Addr::A7));
        self.asm.println();

        self.strings.push((to_fixup, str.to_string()));
    }
    fn resolve_strings(&mut self) {
        let strs = std::mem::take(&mut self.strings);
        for (at, str) in strs {
            let line = self.asm.string(&str);
            self.asm.fixup(at, |asm| {
                asm.load_ea(EA::PCOffset(PC::Line(line)), EA::PreDec(Addr::A7));
            });
        }
    }

    pub fn local(&mut self) -> LocalId {
        let item = self.pop_item();
        let ty = item.ty();
        let id = self.locals.len();
        let storage = match (item, ty.storage_type()) {
            (Item::Storage(storage @ Storage::Data(_, r)), StorageType::Data(_)) => {
                self.data.mark(r);
                storage
            }
            (Item::Storage(storage @ Storage::Addr(_, r)), StorageType::Addr) => {
                self.addr.mark(r);
                storage
            }
            (Item::Storage(storage @ Storage::Stack(_, _)), StorageType::Stack(_)) => storage,
            (item, StorageType::Data(ea_size)) => {
                let d = self.get_data_register();
                let src = item.src_ea(self);
                self.asm.mov(ea_size, src, EA::Data(d));
                Storage::Data(ty, d)
            }
            (item, StorageType::Addr) => {
                let d = self.get_addr_register();
                let src = item.src_ea(self);
                self.asm.mov(Size::Long, src, EA::Addr(d));
                Storage::Addr(ty, d)
            }
            (item, StorageType::Stack(_)) => item.storage_stack(self),
        };
        self.locals.push(storage);
        return id;
    }

    pub fn local_stack(&mut self) -> LocalId {
        let item = self.pop_item();
        let id = self.locals.len();
        let storage = match item {
            Item::Storage(storage @ Storage::Stack(_, _)) => storage,
            _ => item.storage_stack(self),
        };
        self.locals.push(storage);
        return id;
    }

    pub fn add(&mut self) {
        // TODO: flags for constant shortcuts (folding, 0s, operand juggling)
        let r = self.pop_item();
        let l = self.pop_item();
        let src = r.src_ea(self);
        let dest_storage = l.storage_register(self);
        let dest = dest_storage.ea(self);
        self.asm.add(Size::Long, src, dest);
        self.stack.push(Item::Storage(dest_storage));
    }

    pub fn if_begin(&mut self, cond: Cond) -> IfIdx {
        self.cmp2();
        let if_idx = self
            .asm
            .branch(cond.inverse(), Branch::Placeholder(Size::Short));
        self.scope_begin();
        if_idx
    }

    pub fn if_else(&mut self, if_idx: IfIdx) -> IfIdx {
        self.scope_end();
        let end_if_idx = self
            .asm
            .branch(Cond::True, Branch::Placeholder(Size::Short));

        self.scope_begin();
        self.asm.fixup_branch_to_here(if_idx);
        end_if_idx
    }

    pub fn if_end(&mut self, if_idx: IfIdx) {
        self.scope_end();
        self.asm.fixup_branch_to_here(if_idx);
    }

    pub fn loop_begin(&mut self) -> LoopIdx {
        let idx = self
            .asm
            .branch(Cond::True, Branch::Placeholder(Size::Short));
        debug_assert!(idx + 4 == self.asm.here());
        self.scope_begin();
        idx
    }

    pub fn loop_check(&mut self, loop_idx: LoopIdx) {
        self.scope_end();
        self.asm.fixup_branch_to_here(loop_idx);
    }

    pub fn loop_end(&mut self, cond: Cond, loop_idx: LoopIdx) {
        self.cmp2();
        self.asm.branch(cond, Branch::Line(loop_idx + 4));
    }

    pub fn match_begin(&mut self, case_count: usize) -> MatchBuilder {
        // TODO: matches with few cases should be if-else chain instead of jump table

        let item = self.pop_item();
        // load index into data register
        let d = match item.storage_register(self) {
            Storage::Data(_, d) => d,
            _ => unimplemented!(),
        };
        MatchBuilder::init(self, d, case_count)
    }
    pub fn match_case(&mut self, builder: &mut MatchBuilder, case: usize) {
        builder.case(self, case);
        self.scope_begin();
    }
    pub fn match_default_case(&mut self, builder: &mut MatchBuilder) {
        builder.default_case(self);
        self.scope_begin();
    }
    pub fn match_case_end(&mut self, builder: &mut MatchBuilder) {
        self.scope_end();
        builder.case_end(self);
    }
    pub fn match_end(&mut self, builder: MatchBuilder) {
        builder.end(self);
    }
    pub fn match_default_end(&mut self, mut builder: MatchBuilder) {
        builder.default_case(self);
        builder.end(self);
    }

    pub fn pointer(&mut self) {
        let item = self.pop_item();
        let src = item.src_ea(self);
        let ptr = self.storage_pointer(item.ty(), src);
        item.free_transient(self);
        self.stack.push(Item::Storage(ptr));
    }

    pub fn deref(&mut self) {
        let item = self.pop_item();
        let next = item.deref(self);
        self.stack.push(next);
    }

    pub fn assign(&mut self) {
        let rvalue = self.pop_item();
        let lvalue = match self.pop_item() {
            Item::LValue(l) => l,
            _ => unimplemented!(),
        };
        let dest = lvalue.dest_ea(self);
        for (size, offset) in lvalue.ty.iter_blocks() {
            let src = rvalue.src_ea(self).offset(offset);
            let dest = dest.offset(offset);
            self.asm.mov(size, src, dest)
        }
        rvalue.free_transient(self);
    }

    pub fn assign_field(&mut self, field_offset: usize) {
        let value = self.pop_item();
        let target = match self.pop_item() {
            Item::Storage(storage) => storage,
            _ => unimplemented!(),
        };

        for (size, offset) in value.ty().iter_blocks() {
            let src = value.src_ea(self).offset(offset);
            let dest = target.ea(self).offset(offset + field_offset);
            self.asm.mov(size, src, dest)
        }

        value.free_transient(self);
        self.stack.push(Item::Storage(target));
    }

    pub fn add_assign(&mut self) {
        // TODO: flags for constant shortcuts (folding, 0s, operand juggling)
        let rvalue = self.pop_item();
        let lvalue = match self.pop_item() {
            Item::LValue(l) => l,
            _ => unimplemented!(),
        };
        let src = rvalue.src_ea(self);
        let dest = lvalue.dest_ea(self);
        self.asm.add(Size::Long, src, dest);
        rvalue.free_transient(self);
    }

    pub fn field(&mut self, offset: usize, ty: &super::ast::Ty) {
        let item = self.pop_item();
        let ty = Ty {
            base_size: ty.base_size(),
            ref_level: ty.ref_level,
        };
        let res = item.field(Field { ty, offset });
        self.stack.push(res)
    }

    pub fn assert_eq(&mut self) {
        let r = self.pop_item();
        r.storage_stack(self);
        let l = self.pop_item();
        l.storage_stack(self);
        self.asm.assert_eq();
        self.stack_offset -= 8;
    }

    // conditionals
    fn cmp2(&mut self) {
        // TODO: take / release condition code register
        let r = self.pop_item();
        let l = self.pop_item();

        // TODO: Constant folding, EA optimization
        let src = r.src_ea(self);
        let dest = l.storage_register(self).ea(self);
        self.asm.cmp(Size::Long, src, dest);
    }

    // scope
    fn scope_begin(&mut self) {}
    fn scope_end(&mut self) {
        // TODO: handle spills, deallocate stack
    }

    // utils
    fn pop_item(&mut self) -> Item {
        self.stack.pop().unwrap()
    }
    fn get_data_register(&mut self) -> Data {
        // TODO: make range an explicit config param
        let res = self.data.take_in_range(2..=7);
        if res.spilled {
            todo!("spill register")
        }
        res.register
    }
    fn get_addr_register(&mut self) -> Addr {
        // TODO: make range an explicit config param
        let res = self.addr.take_in_range(2..=4);
        if res.spilled {
            todo!("spill register")
        }
        res.register
    }
    fn storage_register(&mut self, ty: Ty, src: EA) -> Storage {
        match ty.storage_type() {
            StorageType::Data(size) => {
                let r = self.get_data_register();
                self.asm.mov(size, src, EA::Data(r));
                Storage::Data(ty, r)
            }
            StorageType::Addr => {
                let r = self.get_addr_register();
                self.asm.mov(Size::Long, src, EA::Addr(r));
                Storage::Addr(ty, r)
            }
            _ => unimplemented!(),
        }
    }
    fn storage_pointer(&mut self, ty: Ty, src: EA) -> Storage {
        match src {
            EA::Offset(_, _) => {
                let r = self.get_addr_register();
                self.asm.load_ea(src, EA::Addr(r));
                Storage::Addr(ty.pointer(), r)
            }
            _ => unimplemented!(),
        }
    }
    fn storage_stack(&mut self, ty: Ty, src: EA) -> Storage {
        for (size, offset) in ty.iter_blocks().into_iter().rev() {
            let src = match src {
                // when pushing from one part of the stack to another, the offset remains fixed
                EA::Offset(Addr::A7, base_offset) => {
                    let fixed_offset = base_offset + ty.size_bytes() as i16 - size.bytes() as i16;
                    EA::Offset(Addr::A7, fixed_offset)
                }
                EA::Offset(a, base_offset) => EA::Offset(a, base_offset + offset as i16),
                // TODO: PC offsets?
                _ => src,
            };
            self.asm.mov(size, src, EA::PreDec(Addr::A7))
        }
        self.stack_offset += ty.size_bytes();
        Storage::Stack(ty, self.stack_offset)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::vm_68k::VM;

    fn run_vm(asm: Asm) -> VM {
        let mut vm = VM::new(256);
        let init_sp = 120;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        vm.load_memory(init_pc as usize, &asm.out);
        vm.run();
        vm
    }

    #[test]
    fn smoke_test() {
        let mut m = Memory::new();

        m.push_i32(123);
        let a = m.local();

        m.push_ident(a);
        m.push_i32(100);
        m.add();
        let res = m.local();

        m.push_ident(res);
        m.push_i32(223);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn assign() {
        let mut m = Memory::new();

        m.push_i32(123);
        let a = m.local();

        m.push_ident(a);
        m.push_i32(456);
        m.assign();

        m.push_ident(a);
        m.push_i32(456);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    #[should_panic]
    fn assign_rvalue() {
        let mut m = Memory::new();
        m.push_i32(10);
        m.push_i32(20);
        m.assign();
    }

    #[test]
    fn pointers() {
        let mut m = Memory::new();

        m.push_i32(123);
        let a = m.local_stack();

        m.push_ident(a);
        m.pointer();
        let a_ptr = m.local();

        m.push_ident(a);
        m.pointer();
        let a_stack_ptr = m.local_stack();

        m.push_ident(a);
        m.push_i32(456);
        m.assign();

        m.push_ident(a_ptr);
        m.deref();
        m.push_i32(456);
        m.assert_eq();

        m.push_ident(a_stack_ptr);
        m.deref();
        m.push_i32(456);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn mut_pointer() {
        let mut m = Memory::new();

        m.push_i32(123);
        let a = m.local_stack();

        m.push_ident(a);
        m.pointer();
        let a_ptr = m.local();

        m.push_ident(a_ptr);
        m.deref();
        m.push_i32(456);
        m.assign();

        m.push_ident(a);
        m.push_i32(456);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    #[should_panic]
    fn pointer_register() {
        let mut m = Memory::new();

        m.push_i32(123);
        let a = m.local();

        m.push_ident(a);
        m.pointer();
    }

    #[test]
    fn records() {
        let point = Ty {
            base_size: 8,
            ref_level: 0,
        };
        let int = super::super::ast::Ty::int();

        let mut m = Memory::new();

        m.push_struct(point.size_bytes());
        // x
        m.push_i32(123);
        m.assign_field(0);
        // y
        m.push_i32(456);
        m.assign_field(4);
        let p = m.local();

        m.push_ident(p);
        m.field(0, &int);
        m.push_i32(123);
        m.assert_eq();

        m.push_ident(p);
        m.field(4, &int);
        m.push_i32(456);
        m.assert_eq();

        m.push_struct(point.size_bytes());
        let p2 = m.local();

        m.push_ident(p2);
        m.push_ident(p);
        m.assign();

        m.push_ident(p);
        m.field(0, &int);
        m.push_i32(789);
        m.assign();

        m.push_ident(p2);
        m.field(0, &int);
        m.push_i32(123);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn record_pointers() {
        let point = Ty {
            base_size: 8,
            ref_level: 0,
        };
        let int = super::super::ast::Ty::int();

        let mut m = Memory::new();
        m.push_struct(point.size_bytes());
        let p = m.local();

        m.push_ident(p);
        m.pointer();
        let ptr_p = m.local();

        m.push_ident(p);
        m.field(0, &int);
        m.pointer();
        let ptr_p_x = m.local();

        m.push_ident(p);
        m.field(0, &int);
        m.push_i32(123);
        m.assign();

        m.push_ident(ptr_p);
        m.deref();
        m.field(0, &int);
        m.push_i32(123);
        m.assert_eq();

        m.push_ident(ptr_p_x);
        m.deref();
        m.push_i32(123);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn if_else() {
        let mut m = Memory::new();

        m.push_i32(2);
        let cmp = m.local();

        m.push_i32(0);
        let res = m.local();

        m.push_ident(cmp);
        m.push_i32(2);
        let if_ = m.if_begin(Cond::Equal);
        m.push_ident(res);
        m.push_i32(10);
        m.assign();
        let else_ = m.if_else(if_);
        m.push_ident(res);
        m.push_i32(20);
        m.assign();
        m.if_end(else_);

        m.push_ident(res);
        m.push_i32(10);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn loops() {
        let mut m = Memory::new();

        m.push_i32(0);
        let i = m.local();

        m.push_i32(0);
        let sum = m.local();

        let loop_idx = m.loop_begin();

        m.push_ident(sum);
        m.push_ident(i);
        m.add_assign();

        m.push_ident(i);
        m.push_i32(1);
        m.add_assign();

        m.loop_check(loop_idx);

        m.push_ident(i);
        m.push_i32(5);
        m.loop_end(Cond::Less, loop_idx);

        m.push_ident(sum);
        m.push_i32(10);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn match_case() {
        let mut m = Memory::new();

        m.push_i32(2);
        let case = m.local();
        m.push_i32(0);
        let result = m.local();

        m.push_ident(case);
        let mut b = m.match_begin(4);

        m.match_case(&mut b, 0);
        m.push_ident(result);
        m.push_i32(10);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_default_case(&mut b);
        m.push_ident(result);
        m.push_i32(30);
        m.assign();
        m.match_case_end(&mut b);

        m.match_end(b);

        m.push_ident(result);
        m.push_i32(20);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn match_default_case() {
        let mut m = Memory::new();

        m.push_i32(1);
        let case = m.local();
        m.push_i32(0);
        let result = m.local();

        m.push_ident(case);
        let mut b = m.match_begin(4);

        m.match_case(&mut b, 0);
        m.push_ident(result);
        m.push_i32(10);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_default_case(&mut b);
        m.push_ident(result);
        m.push_i32(30);
        m.assign();
        m.match_case_end(&mut b);

        m.match_end(b);

        m.push_ident(result);
        m.push_i32(30);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn match_default_case_empty() {
        let mut m = Memory::new();

        m.push_i32(1);
        let case = m.local();
        m.push_i32(1);
        let result = m.local();

        m.push_ident(case);
        let mut b = m.match_begin(4);

        m.match_case(&mut b, 0);
        m.push_ident(result);
        m.push_i32(10);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_default_end(b);

        m.push_ident(result);
        m.push_i32(1);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    #[should_panic]
    fn missing_case() {
        let mut m = Memory::new();

        m.push_i32(1);
        let case = m.local();
        m.push_i32(0);
        let result = m.local();

        m.push_ident(case);
        let mut b = m.match_begin(4);

        m.match_case(&mut b, 0);
        m.push_ident(result);
        m.push_i32(10);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_end(b);
    }

    #[test]
    #[should_panic]
    fn duplicate_case() {
        let mut m = Memory::new();

        m.push_i32(1);
        let case = m.local();
        m.push_i32(0);
        let result = m.local();

        m.push_ident(case);
        let mut b = m.match_begin(4);

        m.match_case(&mut b, 0);
        m.push_ident(result);
        m.push_i32(10);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_default_case(&mut b);
        m.push_ident(result);
        m.push_i32(30);
        m.assign();
        m.match_case_end(&mut b);

        m.match_end(b);
    }

    #[test]
    #[should_panic]
    fn case_index_out_of_range() {
        let mut m = Memory::new();

        m.push_i32(10);
        let case = m.local();
        m.push_i32(0);
        let result = m.local();

        m.push_ident(case);
        let mut b = m.match_begin(4);

        m.match_case(&mut b, 0);
        m.push_ident(result);
        m.push_i32(10);
        m.assign();
        m.match_case_end(&mut b);

        m.match_case(&mut b, 2);
        m.push_ident(result);
        m.push_i32(20);
        m.assign();
        m.match_case_end(&mut b);

        m.match_default_case(&mut b);
        m.push_ident(result);
        m.push_i32(30);
        m.assign();
        m.match_case_end(&mut b);

        m.match_end(b);

        m.push_ident(result);
        m.push_i32(20);
        m.assert_eq();

        run_vm(m.end());
    }

    #[test]
    fn strings() {
        let mut m = Memory::new();

        m.push_string("Hello, world!".to_string());
        let hello = m.local();

        m.push_ident(hello);
        m.println();

        m.log("Another string!");

        run_vm(m.end());
    }
}
