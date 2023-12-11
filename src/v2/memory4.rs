use super::vm::*;

mod register {
    use crate::v2::vm::*;
    use std::marker::PhantomData;
    use std::ops::RangeInclusive;

    pub trait Register: Copy + PartialEq + Eq {
        fn idx(&self) -> usize;
        fn from_idx(idx: usize) -> Self;
        fn ea(&self) -> EA;
    }

    impl Register for Data {
        fn idx(&self) -> usize {
            *self as usize
        }
        fn from_idx(idx: usize) -> Self {
            DATA_IDX[idx]
        }
        fn ea(&self) -> EA {
            EA::Data(*self)
        }
    }

    impl Register for Address {
        fn idx(&self) -> usize {
            *self as usize
        }
        fn from_idx(idx: usize) -> Self {
            ADDR_IDX[idx]
        }
        fn ea(&self) -> EA {
            EA::Address(*self)
        }
    }

    pub struct RegisterMap<T: Register> {
        registers_lru: [RegisterRecord; 8],
        next_id: LastUsed,
        phantom: PhantomData<T>,
    }

    type LastUsed = usize;

    #[derive(Debug, Clone, Copy)]
    struct RegisterRecord {
        last_used: LastUsed,
    }

    impl<T: Register> RegisterMap<T> {
        pub fn new() -> Self {
            Self {
                registers_lru: [RegisterRecord { last_used: 0 }; 8],
                next_id: 1,
                phantom: PhantomData,
            }
        }
        pub fn mark(&mut self, register: T) {
            self.registers_lru[register.idx()].last_used = self.next_id;
            self.next_id += 1;
        }
        pub fn find_next_in_range(&mut self, range: RangeInclusive<usize>) -> T {
            let mut min = (*range.start(), self.registers_lru[*range.start()]);
            for i in range {
                let record = self.registers_lru[i];
                if record.last_used < min.1.last_used {
                    min = (i, record)
                }
            }
            let r = Register::from_idx(min.0);
            self.mark(r);
            r
        }
        pub fn used_in_range(&self, range: RangeInclusive<usize>) -> Vec<T> {
            let mut out = Vec::new();
            for i in range {
                let record = self.registers_lru[i];
                if record.last_used > 0 {
                    out.push(Register::from_idx(i))
                }
            }
            out
        }
    }

    #[cfg(test)]
    #[test]
    fn test() {
        let mut m = RegisterMap::new();

        m.mark(Data::D0);
        m.mark(Data::D3);
        assert_eq!(m.find_next_in_range(2..=7), Data::D2);
        assert_eq!(m.find_next_in_range(2..=7), Data::D4);

        assert_eq!(
            m.used_in_range(0..=7),
            vec![Data::D0, Data::D2, Data::D3, Data::D4]
        );
    }
}
use register::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Ty {
    base_size: Word,
    ref_level: Word,
}

impl Ty {
    fn word() -> Self {
        Self {
            base_size: WORD_BYTES,
            ref_level: 0,
        }
    }
    fn size(&self) -> Word {
        if self.ref_level > 0 {
            WORD_BYTES
        } else {
            self.base_size
        }
    }
    fn use_address_register(&self) -> bool {
        self.ref_level > 0
    }
    fn pointer(&self) -> Self {
        Self {
            base_size: self.base_size,
            ref_level: self.ref_level + 1,
        }
    }
    fn deref(&self) -> Option<Self> {
        if self.ref_level == 0 {
            None
        } else {
            Some(Self {
                base_size: self.base_size,
                ref_level: self.ref_level - 1,
            })
        }
    }
}

type ItemId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Storage {
    Constant(Word),
    Data(Data),
    Address(Ty, Address),
    ConditionCode(Ty),
    Frame(Ty, Word),
    Stack(Ty, Word),
}

impl Storage {
    fn ty(&self) -> Ty {
        match *self {
            Self::Constant(_) => Ty::word(),
            Self::Data(_) => Ty::word(),
            Self::Address(ty, _) => ty,
            Self::ConditionCode(ty) => ty,
            Self::Frame(ty, _) => ty,
            Self::Stack(ty, _) => ty,
        }
    }
    fn ea(&self, memory: &mut Memory) -> EA {
        match self {
            Self::Constant(value) => EA::Immediate(*value),
            Self::Data(r) => {
                memory.data_registers.mark(*r);
                EA::Data(*r)
            }
            Self::Address(_, r) => {
                memory.address_registers.mark(*r);
                EA::Address(*r)
            }
            Self::ConditionCode(_) => {
                panic!("cannot use condition code as src")
            }
            Self::Frame(_, a6_offset) => {
                memory.address_registers.mark(Address::A6);
                EA::Offset(Address::A6, *a6_offset)
            }
            Self::Stack(_, stack_offset) => {
                memory.address_registers.mark(Address::A7);
                EA::Offset(Address::A7, memory.stack_size - *stack_offset)
            }
        }
    }
    fn add_ref(&mut self, memory: &mut Memory) -> Storage {
        let ea = self.ea(memory);
        match self {
            Self::Frame(ty, _) => {
                let a = memory.load_address(ea);
                Storage::Address(ty.pointer(), a)
            }
            Self::Stack(ty, _) => {
                let a = memory.load_address(ea);
                Storage::Address(ty.pointer(), a)
            }
            _ => {
                memory.spill_storage(self);
                self.add_ref(memory)
            }
        }
    }
    fn get_field(self, ty: Ty, offset: Word) -> Storage {
        match self {
            Self::Frame(_, base) => Self::Frame(ty, base + offset),
            Self::Stack(_, base) => Self::Stack(ty, base - offset),
            _ => panic!("get field on non-memory item"),
        }
    }
    fn to_dest(self, memory: &mut Memory) -> Storage {
        match self {
            Self::Constant(value) => memory.ea_to_storage(EA::Immediate(value), Ty::word()),
            _ => self,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LValue {
    ty: Ty,
    item_id: ItemId,
    base_offset: Word,
    deref_offsets: Vec<Word>,
}

impl LValue {
    fn storage(&self, memory: &mut Memory) -> Storage {
        if self.base_offset != 0 || self.deref_offsets.len() > 0 {
            return self.to_dest(memory);
        }
        memory.get_storage(self.item_id)
    }
    fn ea(&self, memory: &mut Memory) -> EA {
        let storage = memory.get_storage(self.item_id);
        let mut ea = storage.ea(memory).offset(self.base_offset).unwrap();
        for d in self.deref_offsets.iter() {
            ea = match ea {
                EA::Address(a) => EA::Offset(a, *d),
                EA::Offset(a, o) => {
                    let a = memory.deref_memory(a, o);
                    EA::Offset(a, *d)
                }
                _ => panic!("invalid deref"),
            }
        }
        ea
    }
    fn get_field(mut self, ty: Ty, offset: Word) -> LValue {
        self.ty = ty;
        if self.deref_offsets.len() > 0 {
            let idx = self.deref_offsets.len() - 1;
            self.deref_offsets[idx] += offset;
        } else {
            self.base_offset += offset;
        }
        self
    }
    fn deref(mut self, offset: Word) -> LValue {
        self.ty = self.ty.deref().unwrap();
        self.deref_offsets.push(offset);
        self
    }
    fn to_dest(&self, memory: &mut Memory) -> Storage {
        let src = self.ea(memory);
        memory.ea_to_storage(src, self.ty)
    }
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
    fn ea(&self, memory: &mut Memory) -> EA {
        match self {
            Self::Storage(storage) => storage.ea(memory),
            Self::LValue(lvalue) => lvalue.ea(memory),
        }
    }
    fn get_field(self, ty: Ty, offset: Word) -> Item {
        match self {
            Self::Storage(storage) => Self::Storage(storage.get_field(ty, offset)),
            Self::LValue(lvalue) => Self::LValue(lvalue.get_field(ty, offset)),
        }
    }
    fn storage(&self, memory: &mut Memory) -> Storage {
        match self {
            Self::Storage(storage) => *storage,
            Self::LValue(lvalue) => lvalue.storage(memory),
        }
    }
    fn to_dest(self, memory: &mut Memory) -> Storage {
        match self {
            Self::Storage(storage) => storage.to_dest(memory),
            Self::LValue(lvalue) => lvalue.to_dest(memory),
        }
    }
    fn deref(self, memory: &mut Memory, offset: Word) -> Item {
        match self {
            Self::Storage(_) => {
                let ty = self.ty().deref().unwrap();
                let addr = memory.deref_register(&self);
                Self::Storage(memory.ea_to_storage(EA::Offset(addr, offset), ty))
            }
            Self::LValue(lvalue) => Self::LValue(lvalue.deref(offset)),
        }
    }
    fn index_src(&self, memory: &mut Memory, stride: Word, index: Storage) -> EA {
        match index {
            Storage::Constant(value) => {
                let addr = memory.deref_register(&self);
                EA::Offset(addr, value * stride)
            }
            Storage::Data(data) => {
                if stride != 1 {
                    memory.out.mul(EA::Data(data), EA::Immediate(stride));
                }
                let addr = memory.deref_register(&self);
                EA::OffsetIndexed(addr, data, 0)
            }
            Storage::Frame(_, _) | Storage::Stack(_, _) => {
                let data = memory.get_data_register();
                let src = self.ea(memory);
                memory.out.mov(EA::Data(data), src);
                if stride != 1 {
                    memory.out.mul(EA::Data(data), EA::Immediate(stride));
                }
                let addr = memory.deref_register(&self);
                EA::OffsetIndexed(addr, data, 0)
            }
            _ => panic!("invalid index"),
        }
    }
    fn index(self, memory: &mut Memory, item_ty: Ty, index: Storage) -> Storage {
        let src = self.index_src(memory, item_ty.size(), index);
        memory.ea_to_storage(src, item_ty)
    }
    fn fast_index(self, memory: &mut Memory, item_ty: Ty, offset: Storage) -> Storage {
        let src = self.index_src(memory, 1, offset);
        memory.ea_to_storage(src, item_ty)
    }
    fn try_constant(&self) -> Option<Word> {
        match self {
            Self::Storage(Storage::Constant(value)) => Some(*value),
            _ => None,
        }
    }
    fn add_ref(&self, memory: &mut Memory) -> Storage {
        self.storage(memory).add_ref(memory)
    }
}

#[derive(Debug, Clone)]
struct Local {
    storage: Storage,
}

struct Scope {
    initial_stack_size: Word,
    initial_locals_size: usize,
    spilled_registers: Vec<(ItemId, EA)>,
}

type SubIndex = Word;
type IfIdx = Word;
type LoopIdx = Word;

struct SubBuilder {
    idx: SubIndex,
    used_registers: [bool; 4], // D0,D1,A0,A1
    frame_offset: Word,
}

impl SubBuilder {
    fn new(idx: SubIndex) -> Self {
        Self {
            idx,
            used_registers: [false; 4],
            frame_offset: WORD_BYTES * 2, // frame pointer + return addr
        }
    }
    fn get_return_storage(&mut self, ty: Ty) -> Storage {
        self.used_registers = [false; 4];
        self.get_param_storage(ty)
    }
    fn get_param_storage(&mut self, ty: Ty) -> Storage {
        if ty.use_address_register() {
            self.get_address_storage(ty)
        } else if ty.size() == WORD_BYTES {
            self.get_data_storage(ty)
        } else {
            self.get_frame_storage(ty)
        }
    }
    fn get_data_storage(&mut self, ty: Ty) -> Storage {
        if !self.used_registers[0] {
            self.used_registers[0] = true;
            Storage::Data(Data::D0)
        } else if !self.used_registers[1] {
            self.used_registers[1] = true;
            Storage::Data(Data::D1)
        } else {
            self.get_frame_storage(ty)
        }
    }
    fn get_address_storage(&mut self, ty: Ty) -> Storage {
        if !self.used_registers[2] {
            self.used_registers[2] = true;
            Storage::Address(ty, Address::A0)
        } else if !self.used_registers[3] {
            self.used_registers[3] = true;
            Storage::Address(ty, Address::A1)
        } else {
            self.get_frame_storage(ty)
        }
    }
    fn get_frame_storage(&mut self, ty: Ty) -> Storage {
        let size = ty.size();
        let storage = Storage::Frame(ty, self.frame_offset);
        self.frame_offset += size;
        storage
    }
}

struct IterBuilder {
    item_ty: Ty,
    begin: LoopIdx,
    to_end: LoopIdx,
}

struct MatchBuilder {
    jmp_idx: Word,
    checked_cases: Vec<bool>,
    case_ends: Vec<Word>,
}

impl MatchBuilder {
    fn new(jmp_idx: Word, case_count: usize) -> Self {
        Self {
            jmp_idx,
            case_ends: Vec::new(),
            checked_cases: vec![false; case_count],
        }
    }
    fn case(&mut self, index: usize) -> Word {
        if self.checked_cases[index] {
            panic!("duplicate case")
        }
        self.checked_cases[index] = true;
        self.jmp_idx + ((index as Word) * WORD_BYTES)
    }
    fn case_end(&mut self, end_idx: Word) {
        self.case_ends.push(end_idx)
    }
    fn default_case(&mut self) -> Vec<Word> {
        let mut cases_to_patch = Vec::new();
        for i in 0..self.checked_cases.len() {
            if !self.checked_cases[i] {
                cases_to_patch.push(self.case(i));
            }
        }
        cases_to_patch
    }
    fn done(&self) {
        for checked in self.checked_cases.iter() {
            if !checked {
                panic!("not all cases checked")
            }
        }
    }
}

struct Memory {
    locals: Vec<Local>,
    stack: Vec<Item>,
    stack_size: Word,
    block_scopes: Vec<Scope>,
    data_registers: RegisterMap<Data>,
    address_registers: RegisterMap<Address>,
    out: Writer,
    // subroutine stuff
    return_storage: Option<Storage>,
    params_size: Word,
    movem_push_idx: Word,
    movem_pops: Vec<Word>,
}

impl Memory {
    pub fn new() -> Self {
        Memory {
            locals: Vec::new(),
            stack: Vec::new(),
            stack_size: 0,
            block_scopes: Vec::new(),
            data_registers: RegisterMap::new(),
            address_registers: RegisterMap::new(),
            out: Writer::new(),
            return_storage: None,
            params_size: 0,
            movem_push_idx: -1,
            movem_pops: Vec::new(),
        }
    }
    fn sub_init(&mut self) {
        let mut next = Self::new();
        next.out = std::mem::take(&mut self.out);
        *self = next;
    }

    // base exprs
    fn item_constant(&mut self, value: Word) {
        self.stack.push(Item::Storage(Storage::Constant(value)))
    }
    fn item_undefined(&mut self, ty: Ty) {
        let storage = self.reserve_storage(ty);
        self.stack.push(Item::Storage(storage))
    }
    fn item_identifier(&mut self, id: ItemId) {
        let ty = self.get_storage(id).ty();

        self.stack.push(Item::LValue(LValue {
            ty,
            item_id: id,
            base_offset: 0,
            deref_offsets: Vec::new(),
        }));
    }
    fn item_structure(&mut self, ty: Ty) {
        let size = ty.size();
        self.out.sub(EA::Address(Address::A7), EA::Immediate(size));
        self.stack_size += size;
        let storage = Storage::Stack(ty, self.stack_size);
        self.stack.push(Item::Storage(storage));
    }
    fn item_structure_offset(&mut self, offset: Word) {
        let item = self.pop_item();
        let record = self.pop_item();
        let src = item.ea(self);
        let dest = record.ea(self).offset(offset).unwrap();
        self.mov_multiple(item.ty(), dest, src);
        self.stack.push(record);
    }
    // complex exprs
    fn get_field(&mut self, ty: Ty, offset: Word) {
        let item = self.pop_item();
        let next = item.get_field(ty, offset);
        self.stack.push(next)
    }
    fn add_ref(&mut self) {
        let item = self.pop_item();
        let next = item.add_ref(self);
        self.stack.push(Item::Storage(next));
    }
    fn deref(&mut self, offset: Word) {
        let item = self.pop_item();
        let deref = item.deref(self, offset);
        self.stack.push(deref)
    }
    fn index(&mut self, item_ty: Ty) {
        let index = self.pop_item().storage(self);
        let array = self.pop_item();
        let next = array.index(self, item_ty, index);
        self.stack.push(Item::Storage(next))
    }
    fn assign(&mut self) {
        let value = self.pop_item();
        let target = self.pop_lvalue();
        let src = value.ea(self);
        let dest = target.ea(self);
        self.mov_multiple(value.ty(), dest, src);
    }
    fn add(&mut self) {
        self.op(Writer::add, |l, r| l + r);
    }
    fn add_assign(&mut self) {
        self.op_assign(Writer::add)
    }

    fn call(&mut self, idx: SubIndex, params: &[Ty], return_ty: Ty) {
        let mut builder = SubBuilder::new(idx);

        let mut param_storage = Vec::new();
        for param in params {
            let storage = builder.get_param_storage(*param);
            match storage {
                Storage::Data(d) => {
                    self.spill(d);
                }
                Storage::Address(_, a) => {
                    self.spill(a);
                }
                _ => {}
            };
            param_storage.push(storage);
        }
        let ret_storage = builder.get_return_storage(return_ty);
        match ret_storage {
            Storage::Data(d) => {
                self.spill(d);
            }
            Storage::Address(_, a) => {
                self.spill(a);
            }
            _ => {}
        };

        let frame_stack_difference = WORD_BYTES * 2;
        let to_alloc = builder.frame_offset - frame_stack_difference;
        self.stack_size += to_alloc;
        if to_alloc > 0 {
            self.out
                .sub(EA::Address(Address::A7), EA::Immediate(to_alloc));
        }

        let next_len = self.stack.len() - params.len();
        let args = self.stack.drain(next_len..).collect::<Vec<_>>();
        for (i, arg) in args.iter().enumerate() {
            let src = arg.ea(self);
            let dest = match param_storage[i] {
                Storage::Data(d) => EA::Data(d),
                Storage::Address(_, a) => EA::Address(a),
                Storage::Frame(_, offset) => {
                    EA::Offset(Address::A7, offset - frame_stack_difference)
                }
                _ => unreachable!(),
            };
            self.mov_multiple(arg.ty(), dest, src);
        }

        self.out.branch_subroutine(idx);
        self.stack.push(Item::Storage(ret_storage));
    }

    // statements
    // let: in register or on stack
    fn define_let(&mut self) -> ItemId {
        let storage = self.pop_item().to_dest(self);
        let id = self.locals.len();
        self.locals.push(Local { storage });
        id
    }
    // var: always on stack
    fn define_var(&mut self) -> ItemId {
        let item = self.pop_item();
        let src = item.ea(self);
        let storage = self.ea_to_stack(src, item.ty());
        let id = self.locals.len();
        self.locals.push(Local { storage });
        id
    }
    fn if_begin(&mut self, cond: Cond) -> IfIdx {
        let right = self.pop_item();
        let left = self.pop_item();
        let base = left.ea(self);
        let comp = right.ea(self);
        self.out.cmp(base, comp);
        let if_idx = self.out.branch(cond, 0);
        self.scope_begin();
        if_idx
    }
    fn if_else(&mut self, if_idx: IfIdx) -> IfIdx {
        self.scope_end();
        let else_idx = self.out.branch(Cond::Always, 0);
        self.out.fixup_branch(if_idx, else_idx);
        self.scope_begin();
        else_idx
    }
    fn if_end(&mut self, if_idx: IfIdx) {
        self.scope_end();
        let end_idx = self.out.branch_dest();
        self.out.fixup_branch(if_idx, end_idx);
    }

    fn loop_begin(&mut self) -> LoopIdx {
        let loop_header = self.out.branch(Cond::Always, 0);
        self.scope_begin();
        loop_header
    }
    fn loop_check_begin(&mut self, loop_header: LoopIdx) {
        self.scope_end();
        let dest = self.out.branch_dest();
        self.out.fixup_branch(loop_header, dest);
    }
    fn loop_check(&mut self, cond: Cond, loop_header: LoopIdx) {
        let right = self.pop_item();
        let left = self.pop_item();
        let base = left.ea(self);
        let comp = right.ea(self);
        self.out.cmp(base, comp);
        let loop_begin_idx = loop_header + 3;
        self.out.branch(cond, loop_begin_idx);
    }

    fn iter_begin(&mut self, item_ty: Ty) -> (ItemId, IterBuilder) {
        let offset = self.pop_item().to_dest(self);
        self.locals.push(Local { storage: offset });

        let length = self.pop_item();
        let array = self.pop_item();
        self.scope_begin();

        let begin = self.out.branch_dest();
        let offset_ea = offset.ea(self);
        let length_ea = length.ea(self);
        self.out.cmp(offset_ea, length_ea);
        let to_end = self.out.branch(Cond::Zero, 0);

        let item = array.fast_index(self, item_ty, offset);
        let item_id = self.locals.len();
        self.locals.push(Local { storage: item });
        (
            item_id,
            IterBuilder {
                item_ty,
                begin,
                to_end,
            },
        )
    }
    fn iter_end(&mut self, builder: IterBuilder) {
        self.scope_end();
        let offset = self.locals.pop().unwrap().storage;
        let offset_ea = offset.ea(self);
        self.out
            .add(offset_ea, EA::Immediate(builder.item_ty.size()));

        self.out.branch(Cond::Always, builder.begin);
        let end = self.out.branch_dest();
        self.out.fixup_branch(builder.to_end, end);
    }

    fn match_begin(&mut self, table_size: usize) -> MatchBuilder {
        let idx = self.pop_item();
        // load index into data register
        let d = match idx {
            Item::Storage(Storage::Data(d)) => d,
            _ => {
                let d = self.get_data_register();
                self.spill(d);
                let src = idx.ea(self);
                self.out.mov(EA::Data(d), src);
                d
            }
        };
        // index -> offset (todo use shift)
        self.out.mul(EA::Data(d), EA::Immediate(4));

        // load current PC into address register
        let a = self.get_address_register();
        self.spill(a);
        self.out.load_address_pc(a);
        // add displacement to address
        let base_displacement = 2;
        self.out
            .add(EA::Address(a), EA::PCIndexed(d, base_displacement));
        // jump to address
        self.out.jmp_addr(a);
        let jmp_idx = self.out.branch_dest();
        self.out.data_32(&vec![0; table_size]);
        MatchBuilder::new(jmp_idx, table_size)
    }
    fn match_case(&mut self, builder: &mut MatchBuilder, case_id: usize) {
        let case_fixup = builder.case(case_id);
        self.out.fixup_jump_table(case_fixup);
        self.scope_begin();
    }
    fn match_default_case(&mut self, builder: &mut MatchBuilder) {
        let remaining_cases = builder.default_case();
        for case in remaining_cases {
            self.out.fixup_jump_table(case);
        }
        self.scope_begin();
    }
    fn match_case_end(&mut self, builder: &mut MatchBuilder) {
        self.scope_end();
        let idx = self.out.branch(Cond::Always, 0);
        builder.case_end(idx);
    }
    fn match_end(&mut self, builder: MatchBuilder) {
        let end = self.out.branch_dest();
        for case_end in builder.case_ends {
            self.out.fixup_branch(case_end, end);
        }
    }

    fn subroutine_begin(&mut self) -> SubBuilder {
        self.sub_init();
        let idx = self.out.branch_dest();
        SubBuilder::new(idx)
    }
    fn subroutine_param(&mut self, builder: &mut SubBuilder, ty: Ty) -> ItemId {
        let storage = builder.get_param_storage(ty);
        // called to mark register as used
        storage.ea(self);
        let id = self.locals.len();
        self.locals.push(Local { storage });
        id
    }
    fn subroutine_return_param(&mut self, builder: &mut SubBuilder, ty: Ty) {
        self.return_storage = Some(builder.get_return_storage(ty));
    }
    fn subroutine_end_params(&mut self, builder: &mut SubBuilder) {
        self.out.link(Address::A6, 0);
        let idx = self.out.movem_push(EA::Offset(Address::A6, 0), &[], &[]);
        self.movem_push_idx = idx;
        self.params_size = builder.frame_offset - (WORD_BYTES * 2);
    }
    fn subroutine_return_void(&mut self) {
        assert!(self.return_storage.is_none());
        self.subroutine_return_base();
    }
    fn subroutine_return_value(&mut self) {
        let value = self.pop_item();
        let return_storage = self.return_storage.unwrap();
        let src = value.ea(self);
        let dest = return_storage.ea(self);
        self.mov_multiple(return_storage.ty(), dest, src);
        self.subroutine_return_base();
    }
    fn subroutine_return_base(&mut self) {
        let idx = self.out.movem_pop(&[], &[], EA::Offset(Address::A6, 0));
        self.movem_pops.push(idx);
        self.out.unlink(Address::A6);
        self.out.ret(self.params_size);
    }
    fn subroutine_end(&mut self) {
        let used_data = self.data_registers.used_in_range(2..=7);
        let used_addr = self.address_registers.used_in_range(2..=4);
        let size = (used_data.len() + used_addr.len()) as Word * WORD_BYTES;
        self.out
            .fixup_link(self.movem_push_idx - 3, Address::A6, -size);
        self.out.fixup_movem_push(
            self.movem_push_idx,
            EA::Offset(Address::A6, -size),
            &used_data,
            &used_addr,
        );
        for idx in self.movem_pops.iter() {
            self.out
                .fixup_movem_pop(*idx, &used_data, &used_addr, EA::Offset(Address::A6, -size));
        }
        self.out.halt()
    }
    // top level
    fn begin_program(&mut self) {
        // writes a jump to a to-be-defined main function, sets up "globals"
        unimplemented!()
    }
    fn begin_module(&mut self) {
        // writes an "error-not a program" op, sets up "globals"
        unimplemented!()
    }
    fn begin_main_subroutine(&mut self) -> SubBuilder {
        // fixup begin_program jump, init sub builder
        unimplemented!()
    }

    // utils
    fn pop_lvalue(&mut self) -> LValue {
        match self.pop_item() {
            Item::Storage(_) => panic!("expected lvalue"),
            Item::LValue(lvalue) => lvalue,
        }
    }
    fn pop_item(&mut self) -> Item {
        self.stack.pop().unwrap()
    }
    fn reserve_storage(&mut self, ty: Ty) -> Storage {
        if ty.use_address_register() {
            let a = self.get_address_register();
            Storage::Address(ty, a)
        } else if ty.size() > WORD_BYTES {
            self.stack_size += ty.size();
            Storage::Stack(ty, self.stack_size)
        } else {
            let d = self.get_data_register();
            Storage::Data(d)
        }
    }
    fn ea_to_storage(&mut self, src: EA, ty: Ty) -> Storage {
        if ty.use_address_register() {
            let a = self.get_address_register();
            self.out.mov(EA::Address(a), src);
            Storage::Address(ty, a)
        } else {
            if ty.size() > WORD_BYTES {
                self.push_multiple(ty, src);
                self.stack_size += ty.size();
                Storage::Stack(ty, self.stack_size)
            } else {
                let d = self.get_data_register();
                self.out.mov(EA::Data(d), src);
                Storage::Data(d)
            }
        }
    }
    fn ea_to_stack(&mut self, src: EA, ty: Ty) -> Storage {
        self.push_multiple(ty, src);
        self.stack_size += ty.size();
        Storage::Stack(ty, self.stack_size)
    }
    fn push_multiple(&mut self, ty: Ty, src: EA) {
        let words = ty.size() / WORD_BYTES;

        match src {
            EA::Offset(Address::A7, _) => {
                // stack offset is static relative to moving stack pointer
                let offset = (words - 1) * WORD_BYTES;
                for _ in 0..words {
                    self.out
                        .mov(EA::PreDec(Address::A7), src.offset(offset).unwrap())
                }
            }
            _ => {
                // pushing to stack runs back-to-front
                for i in (words - 1)..=0 {
                    let offset = i * WORD_BYTES;
                    self.out
                        .mov(EA::PreDec(Address::A7), src.offset(offset).unwrap())
                }
            }
        }
    }
    fn mov_multiple(&mut self, ty: Ty, dest: EA, src: EA) {
        let words = ty.size() / WORD_BYTES;
        for i in 0..words {
            let offset = i * WORD_BYTES;
            self.out
                .mov(dest.offset(offset).unwrap(), src.offset(offset).unwrap());
        }
    }
    fn op_assign(&mut self, op: fn(&mut Writer, EA, EA)) {
        let right = self.pop_item();
        let left = self.pop_lvalue();
        let dest = left.ea(self);
        let src = right.ea(self);
        op(&mut self.out, dest, src);
    }

    fn op(&mut self, op: fn(&mut Writer, EA, EA), const_f: fn(Word, Word) -> Word) {
        let right = self.pop_item();
        let left = self.pop_item();
        match (left.try_constant(), right.try_constant()) {
            (Some(l), Some(r)) => {
                self.stack
                    .push(Item::Storage(Storage::Constant(const_f(l, r))));
            }
            _ => {
                let src = right.ea(self);
                let left = left.to_dest(self);
                let dest = left.ea(self);
                op(&mut self.out, dest, src);
                self.stack.push(Item::Storage(left));
            }
        }
    }
    fn get_storage(&self, id: ItemId) -> Storage {
        self.locals[id].storage
    }
    fn get_storage_mut(&mut self, id: ItemId) -> &mut Storage {
        &mut self.locals[id].storage
    }
    fn load_address(&mut self, ea: EA) -> Address {
        let (src_a, o) = match ea {
            EA::Offset(a, o) => (a, o),
            _ => panic!("invalid load address"),
        };
        let dest_a = self.get_address_register();
        self.out.load_address(dest_a, src_a, o);
        dest_a
    }
    fn deref_register(&mut self, item: &Item) -> Address {
        let base_ea = item.ea(self);
        match base_ea {
            EA::Address(r) => r,
            EA::Offset(r, o) => self.deref_memory(r, o),
            _ => panic!("invalid deref"),
        }
    }
    fn deref_memory(&mut self, addr: Address, offset: Word) -> Address {
        let register = self.get_address_register();
        self.out
            .mov(EA::Address(register), EA::Offset(addr, offset));
        register
    }
    fn get_address_register(&mut self) -> Address {
        let a = self.address_registers.find_next_in_range(0..=4);
        self.spill(a);
        a
    }
    fn get_data_register(&mut self) -> Data {
        let d = self.data_registers.find_next_in_range(0..=7);
        self.spill(d);
        d
    }

    fn spill_storage(&mut self, storage: &mut Storage) {
        let ty = storage.ty();
        self.stack_size += WORD_BYTES;
        let dest = EA::PreDec(Address::A7);
        let src = storage.ea(self);
        self.out.mov(dest, src);
        *storage = Storage::Stack(ty, self.stack_size);
    }

    fn try_spill_storage(&mut self, storage: &mut Storage, register: impl Register) -> bool {
        let src = register.ea();
        match storage {
            Storage::Address(_, addr) if EA::Address(*addr) == src => {}
            Storage::Data(data) if EA::Data(*data) == src => {}
            _ => {
                return false;
            }
        };
        self.spill_storage(storage);
        return true;
    }
    fn spill(&mut self, register: impl Register) {
        let mut locals = std::mem::take(&mut self.locals);
        for (idx, local) in locals.iter_mut().enumerate() {
            let did_change = self.try_spill_storage(&mut local.storage, register);
            if did_change {
                if let Some(current_block) = self.current_block_mut() {
                    if current_block.initial_locals_size < idx {
                        current_block.spilled_registers.push((idx, register.ea()))
                    }
                }

                self.locals = locals;
                return;
            }
        }
        self.locals = locals;

        let mut stack = std::mem::take(&mut self.stack);
        for mut item in stack.iter_mut() {
            match &mut item {
                Item::LValue(_) => {}
                Item::Storage(storage) => {
                    let did_change = self.try_spill_storage(storage, register);
                    if did_change {
                        break;
                    }
                }
            }
        }

        self.stack = stack;
    }

    fn current_block_mut(&mut self) -> Option<&mut Scope> {
        self.block_scopes.iter_mut().last()
    }
    fn scope_begin(&mut self) {
        assert_eq!(self.stack.len(), 0);
        self.block_scopes.push(Scope {
            initial_stack_size: self.stack_size,
            initial_locals_size: self.locals.len(),
            spilled_registers: Vec::new(),
        })
    }
    fn scope_end(&mut self) {
        assert_eq!(self.stack.len(), 0);
        let scope = self.block_scopes.pop().unwrap();
        // unspill registers
        for (id, dest) in scope.spilled_registers {
            let storage = self.get_storage(id);
            let ty = storage.ty();
            let src = storage.ea(self);
            match dest {
                EA::Address(a) => {
                    self.out.mov(EA::Address(a), src);
                    *self.get_storage_mut(id) = Storage::Address(ty, a);
                }
                EA::Data(d) => {
                    self.out.mov(EA::Data(d), src);
                    *self.get_storage_mut(id) = Storage::Data(d);
                }
                _ => unreachable!(),
            };
        }
        // deallocate stack
        let to_drop = self.stack_size - scope.initial_stack_size;
        self.stack_size = scope.initial_stack_size;
        if to_drop > 0 {
            self.out
                .add(EA::Address(Address::A7), EA::Immediate(to_drop));
        }

        // forget locals
        self.locals.truncate(scope.initial_locals_size);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::vm::{Address::*, Data::*, EA::*};

    const INT: Ty = Ty {
        ref_level: 0,
        base_size: WORD_BYTES,
    };

    impl Memory {
        fn expect_stack(&self, stack: Vec<Item>) {
            assert_eq!(self.stack, stack)
        }
        fn expect_locals(&self, locals: Vec<Storage>) {
            assert_eq!(
                self.locals.iter().map(|l| l.storage).collect::<Vec<_>>(),
                locals
            )
        }
        fn expect_output(&self, f: impl Fn(&mut Writer)) {
            let mut out = Writer::new();
            f(&mut out);
            self.out.expect_output(out);
        }
    }

    #[test]
    fn smoke_test() {
        let m = Memory::new();
        m.expect_output(|_| {});
        m.expect_stack(vec![]);
    }

    #[test]
    fn constants() {
        let mut m = Memory::new();
        m.item_constant(10);
        m.item_constant(20);
        m.add();
        m.expect_stack(vec![
            //
            Item::Storage(Storage::Constant(30)),
        ]);
    }

    #[test]
    fn identifiers() {
        let mut m = Memory::new();
        m.item_constant(10);
        let x = m.define_let();

        m.item_identifier(x);
        m.item_constant(20);
        m.assign();

        m.expect_output(|w| {
            w.mov(Data(D0), Immediate(10));
            w.mov(Data(D0), Immediate(20));
        });
    }

    #[test]
    #[should_panic]
    fn unknown_identifier() {
        let mut m = Memory::new();
        m.item_identifier(10);
    }

    #[test]
    fn records() {
        let point = Ty {
            base_size: WORD_BYTES * 2,
            ref_level: 0,
        };
        let mut m = Memory::new();
        m.item_structure(point);
        m.item_constant(10);
        m.item_structure_offset(0);
        m.item_constant(20);
        m.item_structure_offset(WORD_BYTES);

        m.expect_stack(vec![
            //
            Item::Storage(Storage::Stack(point, WORD_BYTES * 2)),
        ]);

        m.get_field(INT, 0);
        m.expect_stack(vec![
            //
            Item::Storage(Storage::Stack(INT, WORD_BYTES * 2)),
        ]);
    }

    #[test]
    fn array() {
        let mut m = Memory::new();
        let array_ty = Ty {
            base_size: WORD_BYTES * 3,
            ref_level: 0,
        };

        m.item_structure(array_ty);
        m.item_constant(10);
        m.item_structure_offset(0);
        m.item_constant(20);
        m.item_structure_offset(WORD_BYTES);
        m.item_constant(30);
        m.item_structure_offset(WORD_BYTES * 2);
        m.add_ref();
        let array = m.define_let();

        m.item_identifier(array);
        m.item_constant(1);
        m.index(INT);
        m.define_let();

        m.expect_locals(vec![
            //
            Storage::Address(array_ty.pointer(), A0),
            Storage::Data(D0),
        ]);
    }

    #[test]
    fn ref_deref() {
        let mut m = Memory::new();
        m.item_constant(123);
        m.add_ref();

        m.expect_output(|w| {
            w.mov(PreDec(A7), Immediate(123));
            w.load_address(A0, A7, 0);
        });

        m.expect_stack(vec![
            //
            Item::Storage(Storage::Address(INT.pointer(), A0)),
        ]);

        m.deref(0);
        m.expect_output(|w| {
            w.mov(PreDec(A7), Immediate(123));
            w.load_address(A0, A7, 0);
            w.mov(Data(D0), Offset(A0, 0));
        });
        m.expect_stack(vec![
            //
            Item::Storage(Storage::Data(D0)),
        ]);
    }

    #[test]
    fn subroutine() {
        let mut m = Memory::new();
        let mut add2 = m.subroutine_begin();
        let l = m.subroutine_param(&mut add2, INT);
        let r = m.subroutine_param(&mut add2, INT);
        m.subroutine_return_param(&mut add2, INT);
        m.subroutine_end_params(&mut add2);

        m.item_identifier(l);
        m.item_identifier(r);
        m.add();
        m.subroutine_return_value();

        m.subroutine_end();

        let mut main = m.subroutine_begin();
        m.subroutine_end_params(&mut main);

        m.item_constant(1);
        let a = m.define_let();

        m.item_constant(2);
        m.item_identifier(a);
        m.call(add2.idx, &[INT, INT], INT);

        m.subroutine_return_void();
        m.subroutine_end();

        m.expect_output(|w| {
            let add2 = w.branch_dest();
            // add2
            let displacememt = WORD_BYTES * 1;
            w.link(A6, -displacememt);
            w.movem_push(Offset(A6, -displacememt), &[D2], &[]);

            w.mov(Data(D2), Data(D0));
            w.add(Data(D2), Data(D1));

            w.mov(Data(D0), Data(D2));

            w.movem_pop(&[D2], &[], Offset(A6, -displacememt));
            w.unlink(A6);
            w.ret(0);

            w.halt();

            // main
            w.link(A6, 0);
            w.movem_push(Offset(A6, 0), &[], &[]);

            // let a = 1
            w.mov(Data(D0), Immediate(1));

            // add2(2, a);
            w.mov(PreDec(A7), Data(D0)); // spill
            w.mov(Data(D0), Immediate(2)); // assign args
            w.mov(Data(D1), Offset(A7, 0));
            w.branch_subroutine(add2);

            w.movem_pop(&[], &[], Offset(A6, 0));
            w.unlink(A6);
            w.ret(0);

            w.halt();
        });
    }

    #[test]
    fn conditional() {
        let mut m = Memory::new();
        let mut sub = m.subroutine_begin();
        let x = m.subroutine_param(&mut sub, INT);
        m.subroutine_return_param(&mut sub, INT);
        m.subroutine_end_params(&mut sub);

        m.item_identifier(x);
        m.item_constant(10);
        let if_idx = m.if_begin(Cond::Zero);

        m.item_constant(1);
        m.subroutine_return_value();
        m.if_end(if_idx);

        m.item_constant(0);
        m.subroutine_return_value();
        m.subroutine_end();

        m.expect_output(|w| {
            w.link(A6, 0);
            w.movem_push(Offset(A6, 0), &[], &[]);

            w.cmp(Data(D0), Immediate(10));
            let to_else = w.branch(Cond::Zero, 0);

            w.mov(Data(D0), Immediate(1));
            w.movem_pop(&[], &[], Offset(A6, 0));
            w.unlink(A6);
            w.ret(0);

            let else_idx = w.branch_dest();
            w.fixup_branch(to_else, else_idx);
            w.mov(Data(D0), Immediate(0));
            w.movem_pop(&[], &[], Offset(A6, 0));
            w.unlink(A6);
            w.ret(0);

            w.halt();
        });
    }

    #[test]
    fn while_() {
        let mut m = Memory::new();
        m.item_constant(0);
        let sum = m.define_let();
        m.item_constant(0);
        let idx = m.define_let();

        let loop_ = m.loop_begin();
        // sum += i;
        m.item_identifier(sum);
        m.item_identifier(idx);
        m.add_assign();

        // i += 1;
        m.item_identifier(idx);
        m.item_constant(1);
        m.add_assign();

        // while i < 5
        m.loop_check_begin(loop_);
        m.item_identifier(idx);
        m.item_constant(5);
        m.loop_check(Cond::Negative, loop_);

        m.expect_output(|w| {
            w.mov(Data(D0), Immediate(0));
            w.mov(Data(D1), Immediate(0));
            let l = w.branch(Cond::Always, 0);
            let begin = w.branch_dest();

            w.add(Data(D0), Data(D1));
            w.add(Data(D1), Immediate(1));

            let check = w.branch_dest();
            w.fixup_branch(l, check);
            w.cmp(Data(D1), Immediate(5));
            w.branch(Cond::Negative, begin);
        });
    }

    #[test]
    fn foreach() {
        let mut m = Memory::new();
        let array_ty = Ty {
            base_size: WORD_BYTES * 3,
            ref_level: 0,
        };

        m.item_structure(array_ty);
        m.item_constant(10);
        m.item_structure_offset(0);
        m.item_constant(20);
        m.item_structure_offset(WORD_BYTES);
        m.item_constant(30);
        m.item_structure_offset(WORD_BYTES * 2);
        m.add_ref();
        let array = m.define_let();
        m.item_constant(WORD_BYTES * 3);
        let len = m.define_let();

        m.item_constant(0);
        let sum = m.define_let();

        m.item_identifier(array);
        m.item_identifier(len);
        m.item_constant(0);
        let (item, iter_builder) = m.iter_begin(INT);

        m.item_identifier(sum);
        m.item_identifier(item);
        m.add_assign();

        m.iter_end(iter_builder);

        m.expect_output(|w| {
            w.sub(Address(A7), Immediate(WORD_BYTES * 3));
            w.mov(Offset(A7, 0), Immediate(10));
            w.mov(Offset(A7, WORD_BYTES), Immediate(20));
            w.mov(Offset(A7, WORD_BYTES * 2), Immediate(30));
            w.load_address(A0, A7, 0);

            w.mov(Data(D0), Immediate(WORD_BYTES * 3)); // len
            w.mov(Data(D1), Immediate(0)); // item
            w.mov(Data(D2), Immediate(0)); // offset

            let begin_idx = w.branch_dest();
            w.cmp(Data(D2), Data(D0));
            let to_end = w.branch(Cond::Zero, 0);

            // item
            w.mov(Data(D3), OffsetIndexed(A0, D2, 0));
            // sum += item
            w.add(Data(D1), Data(D3));

            w.add(Data(D2), Immediate(WORD_BYTES));
            w.branch(Cond::Always, begin_idx);
            let end_idx = w.branch_dest();
            w.fixup_branch(to_end, end_idx);
        });
    }

    #[test]
    fn jump_table() {
        let mut m = Memory::new();

        m.item_constant(10);
        let e = m.define_let();

        m.item_undefined(INT);
        let res = m.define_let();

        m.item_identifier(e);
        let mut mb = m.match_begin(5);

        m.match_case(&mut mb, 0);
        m.item_identifier(res);
        m.item_constant(10);
        m.assign();
        m.match_case_end(&mut mb);

        m.match_case(&mut mb, 1);
        m.item_identifier(res);
        m.item_constant(20);
        m.assign();
        m.match_case_end(&mut mb);

        m.match_default_case(&mut mb);
        m.item_identifier(res);
        m.item_constant(0);
        m.assign();
        m.match_case_end(&mut mb);

        m.match_end(mb);

        m.expect_output(|w| {
            w.mov(Data(D0), Immediate(10));
            // Data(D1) := undefined

            w.mov(Data(D2), Data(D0));
            w.mul(Data(D2), Immediate(4));
            w.load_address_pc(A0);
            w.add(Address(A0), PCIndexed(D2, 2));
            w.jmp_addr(A0);
            let jmp_table = w.branch_dest();
            w.data_32(&[0, 0, 0, 0, 0]);
            let mut end_fixups = Vec::new();

            w.fixup_jump_table(jmp_table);
            w.mov(Data(D1), Immediate(10));
            let idx = w.branch(Cond::Always, 0);
            end_fixups.push(idx);

            w.fixup_jump_table(jmp_table + WORD_BYTES);
            w.mov(Data(D1), Immediate(20));
            let idx = w.branch(Cond::Always, 0);
            end_fixups.push(idx);

            w.fixup_jump_table(jmp_table + (WORD_BYTES * 2));
            w.fixup_jump_table(jmp_table + (WORD_BYTES * 3));
            w.fixup_jump_table(jmp_table + (WORD_BYTES * 4));
            w.mov(Data(D1), Immediate(0));
            let idx = w.branch(Cond::Always, 0);
            end_fixups.push(idx);

            let end = w.branch_dest();
            for fixup in end_fixups.into_iter() {
                w.fixup_branch(fixup, end);
            }
        });
    }
}
