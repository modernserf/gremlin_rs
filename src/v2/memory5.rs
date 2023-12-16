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
        if self.ref_level > 1 {
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
        let storage = memory.locals[self.base];
        let storage = if let Some(field) = self.field {
            storage.field(field)
        } else {
            storage
        };
        if self.deref_offsets.len() > 0 {
            todo!("derefs")
        }
        storage
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Flags {}

impl Flags {
    pub fn new() -> Self {
        Self {}
    }
}

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

    pub fn assert_eq(&mut self) {
        let r = self.pop_item();
        let r = r.storage_stack(self);
        let l = self.pop_item();
        let l = l.storage_stack(self);
        self.asm.assert_eq();
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
}
