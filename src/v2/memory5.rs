use crate::v2::ea::*;
use crate::v2::register::*;
use crate::v2::vm_68k::{Asm, Cond};

// This time we'll really use flags for everything

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Ty {
    base_size: usize,
    ref_level: usize,
}

impl Ty {
    fn i32() -> Self {
        Self {
            base_size: 4,
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
            let take = usize::max(bytes - offset, 4);
            let size = match take {
                4 => Size::Long,
                _ => todo!(),
            };
            out.push((size, offset));
            offset += take;
        }
        out
    }
    fn pointer(&self) -> Ty {
        Self {
            base_size: self.base_size,
            ref_level: self.ref_level + 1,
        }
    }
    fn deref(&self) -> Ty {
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
    fn storage_original(&self, memory: &mut Memory) -> Storage {
        match self {
            Self::Storage(storage) => *storage,
            Self::LValue(lvalue) => lvalue.storage_src(memory),
        }
    }
    fn storage_register(&self, memory: &mut Memory) -> Storage {
        match self {
            Self::Storage(storage) => storage.register(memory),
            Self::LValue(lvalue) => lvalue.storage_src(memory).register_copy(memory),
        }
    }
    fn storage_stack(&self, memory: &mut Memory) -> Storage {
        let ty = self.ty();
        let storage = self.storage_original(memory);
        for (size, offset) in ty.iter_blocks().into_iter().rev() {
            let src = storage.ea(memory, offset);
            memory.asm.mov(size, src, EA::PreDec(Addr::A7))
        }
        storage.free_transient(memory);
        memory.stack_offset += ty.size_bytes();
        Storage::Stack(ty, memory.stack_offset)
    }
    fn free_transient(self, memory: &mut Memory) {
        match self {
            Self::Storage(storage) => storage.free_transient(memory),
            Self::LValue(_) => {}
        }
    }
    fn pointer(&self, memory: &mut Memory) -> Item {
        match self {
            Self::Storage(storage) => Self::Storage(storage.pointer(memory)),
            Self::LValue(lvalue) => Self::Storage(lvalue.storage_src(memory).pointer(memory)),
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
            Self::Constant(_, _) => unimplemented!(),
            Self::Data(_, _) => unimplemented!(),
            Self::Addr(_, _) => unimplemented!(),
            Self::StackArg(_, base) => Self::StackArg(field.ty, base + field.offset),
            Self::Stack(_, base) => Self::Stack(field.ty, base - field.offset),
        }
    }
    fn ea(&self, memory: &mut Memory, offset: usize) -> EA {
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
            Self::StackArg(_, idx) => EA::Offset(Addr::A6, (8 + idx + offset) as i16),
            Self::Stack(_, idx) => {
                EA::Offset(Addr::A7, (memory.stack_offset - idx + offset) as i16)
            }
        }
    }
    fn register(&self, memory: &mut Memory) -> Storage {
        match *self {
            Self::Data(_, _) => *self,
            Self::Addr(_, _) => *self,
            _ => self.register_copy(memory),
        }
    }
    fn register_copy(&self, memory: &mut Memory) -> Storage {
        match self.ty().storage_type() {
            StorageType::Data(size) => {
                let r = memory.get_data_register();
                let src = self.ea(memory, 0);
                memory.asm.mov(size, src, EA::Data(r));
                Self::Data(self.ty(), r)
            }
            StorageType::Addr => {
                let r = memory.get_addr_register();
                let src = self.ea(memory, 0);
                memory.asm.mov(Size::Long, src, EA::Addr(r));
                Self::Addr(self.ty(), r)
            }
            _ => unimplemented!(),
        }
    }
    fn free_transient(self, memory: &mut Memory) {
        match self {
            Self::Data(_, d) => memory.data.free(d),
            Self::Addr(_, a) => memory.addr.free(a),
            _ => {}
        }
    }
    fn pointer(&self, memory: &mut Memory) -> Self {
        match self {
            Self::StackArg(_, _) | Self::Stack(_, _) => {
                let r = memory.get_addr_register();
                let src = self.ea(memory, 0);
                memory.asm.load_ea(src, EA::Addr(r));
                Self::Addr(self.ty().pointer(), r)
            }
            _ => unimplemented!(),
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
                    }
                    memory.stack_offset += deref_ty.size_bytes();
                    Storage::Stack(deref_ty, memory.stack_offset)
                }
            },
            _ => {
                let src = self.ea(memory, 0);
                let r = memory.get_addr_register();
                memory.asm.mov(Size::Long, src, EA::Addr(r));
                Storage::Addr(self.ty(), r).deref(memory, deref_ty, offset, consume_addr)
            }
        }
    }
}

type LocalId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Field {
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
    fn storage_src(&self, memory: &mut Memory) -> Storage {
        let mut storage = memory.locals[self.base];
        if let Some(field) = self.field {
            storage = storage.field(field)
        }
        match (storage, self.deref_offsets.len()) {
            (_, 0) => {}
            (Storage::Addr(_, r), 1) => {
                memory.addr.mark(r);
                storage = storage.deref(memory, storage.ty().deref(), self.deref_offsets[0], false)
            }
            _ => {
                let r = memory.get_addr_register();
                let src = storage.ea(memory, 0);
                memory.asm.mov(Size::Long, src, EA::Addr(r));
                for offset in self.deref_offsets.iter() {
                    storage = Storage::Addr(storage.ty(), r).deref(
                        memory,
                        storage.ty().deref(),
                        *offset,
                        true,
                    );
                }
            }
        }
        storage
    }
    fn deref(&self) -> LValue {
        let mut next = self.clone();
        next.deref_offsets.push(0);
        next.ty = next.ty.deref();
        next
    }
    fn assign(&self, memory: &mut Memory, src_storage: Storage) {
        let dest_storage = self.storage_src(memory);
        for (size, offset) in self.ty.iter_blocks() {
            let src = src_storage.ea(memory, offset);
            let dest = dest_storage.ea(memory, offset);
            memory.asm.mov(size, src, dest)
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Flags {}

impl Flags {}

#[derive(Debug, Default)]
struct Memory {
    flags: Flags,
    locals: Vec<Storage>,
    stack: Vec<Item>,
    data: RegisterAllocator<Data>,
    addr: RegisterAllocator<Addr>,
    asm: Asm,
    stack_offset: usize,
}

impl Memory {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn end(mut self) -> Asm {
        self.asm.halt();
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
                let src = item.storage_original(self).ea(self, 0);
                self.asm.mov(ea_size, src, EA::Data(d));
                Storage::Data(ty, d)
            }
            (item, StorageType::Addr) => {
                let d = self.get_addr_register();
                let src = item.storage_original(self).ea(self, 0);
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
        // Need clearer names for these
        let src = r.storage_original(self).ea(self, 0);
        let dest_storage = l.storage_register(self);
        let dest = dest_storage.ea(self, 0);
        self.asm.add(Size::Long, src, dest);
        r.free_transient(self);
        l.free_transient(self);
        self.stack.push(Item::Storage(dest_storage));
    }

    pub fn pointer(&mut self) {
        let item = self.pop_item();
        let next = item.pointer(self);
        self.stack.push(next);
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
        let src = rvalue.storage_original(self);
        lvalue.assign(self, src);
        src.free_transient(self);
    }

    pub fn assert_eq(&mut self) {
        let r = self.pop_item();
        let r = r.storage_stack(self);
        let l = self.pop_item();
        let l = l.storage_stack(self);
        self.asm.assert_eq();
        self.stack_offset -= 8;
        r.free_transient(self);
        l.free_transient(self);
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

        m.push_ident(a_stack_ptr);
        m.deref();
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
}
