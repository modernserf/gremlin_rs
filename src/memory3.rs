#![allow(dead_code)]

use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
enum CompileError {
    UnknownIdentifier(String),
    ExpectedType { expected: Ty, received: Ty },
}
use CompileError::*;

type Compile<T> = Result<T, CompileError>;

#[derive(Debug, PartialEq, Eq)]
enum EA {
    Immediate(Word),
    Data(Data),
    Address(Address),
    Offset(Address, Word),
    PreDec(Address),
}

const PUSH: EA = EA::PreDec(Address::A7);

type SrcEA = EA;
type DestEA = EA;

#[derive(Debug, PartialEq, Eq)]
enum Op {
    Mov(DestEA, SrcEA),
    Add(DestEA, SrcEA),
    LoadAddress(DestEA, SrcEA),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Int,
    Bool,
    Pointer(Box<Ty>),
}

impl Ty {
    fn check(&self, other: &Ty) -> Compile<()> {
        if self == other {
            Ok(())
        } else {
            Err(ExpectedType {
                expected: self.clone(),
                received: other.clone(),
            })
        }
    }
    fn pointer(self) -> Self {
        Self::Pointer(Box::new(self))
    }
    fn deref(self) -> Compile<Self> {
        match self {
            Self::Pointer(ty) => Ok(*ty),
            ty => Err(ExpectedType {
                expected: ty.clone().pointer(),
                received: ty,
            }),
        }
    }
}

type Word = i32;
type StackID = usize;
type FrameOffset = Word;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Data {
    D0 = 0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}

const DATA_IDX: [Data; 8] = [
    Data::D0,
    Data::D1,
    Data::D2,
    Data::D3,
    Data::D4,
    Data::D5,
    Data::D6,
    Data::D7,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Address {
    A0 = 0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
}

const ADDR_IDX: [Address; 8] = [
    Address::A0,
    Address::A1,
    Address::A2,
    Address::A3,
    Address::A4,
    Address::A5,
    Address::A6,
    Address::A7,
];

struct Memory {
    output: Vec<Op>,
    stack: Vec<StackItem>,
    locals: HashMap<String, StackID>,
    frame_offset: FrameOffset,
    used_data_registers: [usize; 8],
    used_address_registers: [usize; 8],
}

// a register machine pretending to be a stack machine
impl Memory {
    fn new() -> Self {
        Self {
            output: Vec::new(),
            stack: Vec::new(),
            locals: HashMap::new(),
            frame_offset: 0,
            used_data_registers: [0; 8],
            used_address_registers: [0; 8],
        }
    }
    fn get_data_register(&mut self) -> Option<Data> {
        for i in 2..8 {
            if self.used_data_registers[i] == 0 {
                self.used_data_registers[i] = 1;
                return Some(DATA_IDX[i]);
            }
        }
        return None;
    }
    fn get_address_register(&mut self) -> Option<Address> {
        for i in 2..7 {
            if self.used_address_registers[i] == 0 {
                self.used_address_registers[i] = 1;
                return Some(ADDR_IDX[i]);
            }
        }
        return None;
    }
    fn dec_data_register(&mut self, r: Data) {
        self.used_data_registers[r as usize] -= 1;
    }
    fn dec_address_register(&mut self, r: Address) {
        self.used_address_registers[r as usize] -= 1;
    }
    // push a const integer onto stack
    pub fn push_int(&mut self, value: Word) {
        self.stack.push(StackItem {
            ty: Ty::Int,
            loc: Loc::Const(value),
            parent: None,
        });
    }
    pub fn push_bool(&mut self, value: bool) {
        self.stack.push(StackItem {
            ty: Ty::Bool,
            loc: Loc::Const(if value { 1 } else { 0 }),
            parent: None,
        });
    }
    // assign name to the top stack item
    pub fn assign_local(&mut self, name: String) {
        let id = self.stack.len() - 1;
        self.locals.insert(name, id);
    }
    // push a copy of the named item onto the stack
    pub fn get_local(&mut self, name: &str) -> Compile<()> {
        let id = self
            .locals
            .get(name)
            .ok_or_else(|| UnknownIdentifier(name.to_string()))?;
        let mut item = self.stack[*id].clone();
        item.parent = item.parent.or(Some(*id));
        self.stack.push(item);

        Ok(())
    }
    // if the top item of the stack is a constant or borrowed, turn it into a value
    pub fn materialize(&mut self) {
        let mut item = self.stack.pop().unwrap();
        self.materialize_item(&mut item);
        let ty = item.ty;
        self.stack.push(StackItem {
            ty,
            loc: item.loc,
            parent: None,
        });
    }
    fn materialize_item(&mut self, item: &mut StackItem) {
        if item.is_materialized() {
            return;
        }

        item.parent = None;
        if let Some(r) = self.get_data_register() {
            let dest = EA::Data(r);
            let src = self.item_src(item.clone());
            self.output.push(Op::Mov(dest, src));
            item.loc = Loc::Data(r);
        } else {
            self.item_to_stack(item);
        }
    }
    fn item_to_stack(&mut self, item: &mut StackItem) {
        let src = self.item_src(item.clone());
        self.frame_offset += 1;
        self.output.push(Op::Mov(PUSH, src));
        item.loc = Loc::Stack(self.frame_offset)
    }
    fn item_src(&mut self, item: StackItem) -> EA {
        match item.loc {
            Loc::Const(value) => EA::Immediate(value),
            Loc::Data(data) => {
                if item.parent.is_none() {
                    self.dec_data_register(data);
                }
                EA::Data(data)
            }
            Loc::Address(addr) => {
                if item.parent.is_none() {
                    self.dec_address_register(addr);
                }
                EA::Address(addr)
            }
            Loc::AddressIndirect(addr, offset) => EA::Offset(addr, offset),
            Loc::Stack(offset) => {
                let stack_offset = self.frame_offset - offset;
                EA::Offset(Address::A7, stack_offset)
            }
            Loc::StackIndirect(frame_offset, indirect_offset) => {
                let stack_offset = self.frame_offset - frame_offset;
                let src = EA::Offset(Address::A7, stack_offset);
                self.output.push(Op::Mov(EA::Address(Address::A0), src));
                EA::Offset(Address::A0, indirect_offset)
            }
        }
    }
    fn item_dest(&mut self, item: &StackItem) -> EA {
        match &item.loc {
            Loc::Data(data) => EA::Data(*data),
            Loc::Stack(offset) => {
                let stack_offset = self.frame_offset - *offset;
                EA::Offset(Address::A7, stack_offset)
            }
            _ => panic!("invalid dest"),
        }
    }
    // pop two items from the stack, and push their sum
    pub fn add(&mut self) -> Compile<()> {
        let mut right = self.stack.pop().unwrap();
        let mut left = self.stack.pop().unwrap();
        Ty::Int.check(&left.ty)?;
        Ty::Int.check(&right.ty)?;

        match (&left.loc, &right.loc) {
            (Loc::Const(l), Loc::Const(r)) => {
                let sum = l + r;
                self.push_int(sum);
            }
            (Loc::Const(l), _) => {
                self.materialize_item(&mut right);
                let dest = self.item_dest(&right);
                let src = EA::Immediate(*l);
                self.output.push(Op::Add(dest, src));
                self.stack.push(right);
            }
            _ => {
                self.materialize_item(&mut left);
                let dest = self.item_dest(&left);
                let src = self.item_src(right);
                self.output.push(Op::Add(dest, src));
                self.stack.push(left);
            }
        };

        Ok(())
    }
    fn load_address(&mut self, item: StackItem) {
        let ty = item.ty.clone().pointer();
        let src = self.item_src(item);
        if let Some(addr) = self.get_address_register() {
            self.output.push(Op::LoadAddress(EA::Address(addr), src));
            self.stack.push(StackItem {
                ty,
                loc: Loc::Address(addr),
                parent: None,
            });
        } else {
            self.frame_offset += 1;
            self.output.push(Op::LoadAddress(PUSH, src));
            self.stack.push(StackItem {
                ty,
                loc: Loc::Stack(self.frame_offset),
                parent: None,
            });
        }
    }
    pub fn make_pointer(&mut self) {
        let mut item = self.stack.pop().unwrap();
        dbg!(&item);
        if let Some(parent_id) = item.parent {
            let mut parent_item = self.stack[parent_id].clone();
            if let Loc::Stack(_) = parent_item.loc {
                // load the existing stack value directly
                self.load_address(parent_item);
            } else {
                // update the item to be stack-based, then load the address
                self.item_to_stack(&mut parent_item);
                self.stack[parent_id] = parent_item.clone();
                self.load_address(parent_item);
            }
        } else {
            // create a temporary item, then load its address
            self.item_to_stack(&mut item);
            self.load_address(item);
        }
    }
    pub fn deref_pointer(&mut self) -> Compile<()> {
        let item = self.stack.pop().unwrap();
        let ty = item.ty.deref()?;
        let loc = match item.loc {
            Loc::Address(a) => Loc::AddressIndirect(a, 0),
            Loc::Stack(offset) => Loc::StackIndirect(offset, 0),
            _ => unreachable!(),
        };
        self.stack.push(StackItem {
            ty,
            loc,
            parent: None,
        });
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackItem {
    ty: Ty,
    loc: Loc,
    parent: Option<StackID>,
}

impl StackItem {
    // TODO: come up with some reason for this
    fn is_materialized(&self) -> bool {
        match &self.loc {
            Loc::Const(_) => false,
            Loc::AddressIndirect(_, _) => false,
            Loc::StackIndirect(_, _) => false,
            _ => self.parent.is_none(),
        }
    }
}

type IndirectOffset = Word;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Loc {
    Const(Word),
    Data(Data),
    Address(Address),
    AddressIndirect(Address, IndirectOffset),
    Stack(FrameOffset),
    StackIndirect(FrameOffset, IndirectOffset),
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{Address::*, Data::*, Op::*, EA::*};

    fn expect_err(res: Compile<()>, err: CompileError) {
        assert_eq!(res, Err(err));
    }

    impl Memory {
        fn expect_stack(&self, stack: Vec<StackItem>) {
            assert_eq!(self.stack, stack);
        }
        fn expect_output(&self, out: Vec<Op>) {
            assert_eq!(self.output, out);
        }
    }

    #[test]
    fn const_integer() {
        let mut m = Memory::new();
        m.push_int(123);
        m.expect_stack(vec![StackItem {
            ty: Ty::Int,
            loc: Loc::Const(123),
            parent: None,
        }]);
        m.expect_output(vec![]);
    }

    #[test]
    fn materialize_int() {
        let mut m = Memory::new();
        for i in 0..10 {
            m.push_int(i);
            m.materialize();
        }
        m.expect_output(vec![
            Mov(Data(D2), Immediate(0)),
            Mov(Data(D3), Immediate(1)),
            Mov(Data(D4), Immediate(2)),
            Mov(Data(D5), Immediate(3)),
            Mov(Data(D6), Immediate(4)),
            Mov(Data(D7), Immediate(5)),
            Mov(PUSH, Immediate(6)),
            Mov(PUSH, Immediate(7)),
            Mov(PUSH, Immediate(8)),
            Mov(PUSH, Immediate(9)),
        ]);
    }

    #[test]
    fn identifiers() {
        let mut m = Memory::new();
        m.push_int(123);
        m.assign_local("foo".to_string());
        m.push_int(456);
        m.materialize();
        m.assign_local("bar".to_string());
        m.get_local("foo").unwrap();
        m.get_local("bar").unwrap();

        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(123),
                parent: None,
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
                parent: None,
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(123),
                parent: Some(0),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
                parent: Some(1),
            },
        ]);
    }

    #[test]
    fn unknown_identifier() {
        let mut m = Memory::new();
        expect_err(m.get_local("foo"), UnknownIdentifier("foo".to_string()));
    }

    #[test]
    fn add_const() {
        let mut m = Memory::new();
        m.push_int(100);
        m.assign_local("foo".to_string());
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.add().unwrap();

        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(100),
                parent: None,
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(300),
                parent: None,
            },
        ]);
    }

    #[test]
    fn add_left_const() {
        let mut m = Memory::new();
        m.push_int(100);
        m.assign_local("foo".to_string());
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.materialize();
        m.add().unwrap();

        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(100),
                parent: None,
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
                parent: None,
            },
        ]);
        m.expect_output(vec![
            Mov(Data(D2), Immediate(200)),
            Add(Data(D2), Immediate(100)),
        ]);
    }

    #[test]
    fn add_register_register() {
        let mut m = Memory::new();
        m.push_int(100);
        m.materialize();
        m.assign_local("foo".to_string());
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.materialize();
        m.add().unwrap();
        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
                parent: None,
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D4),
                parent: None,
            },
        ]);

        m.expect_output(vec![
            Mov(Data(D2), Immediate(100)),
            Mov(Data(D3), Immediate(200)),
            Mov(Data(D4), Data(D2)),
            Add(Data(D4), Data(D3)),
        ])
    }

    #[test]
    fn add_type_error() {
        let mut m = Memory::new();
        m.push_int(100);
        m.push_bool(true);
        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(100),
                parent: None,
            },
            StackItem {
                ty: Ty::Bool,
                loc: Loc::Const(1),
                parent: None,
            },
        ]);

        expect_err(
            m.add(),
            ExpectedType {
                expected: Ty::Int,
                received: Ty::Bool,
            },
        )
    }

    #[test]
    fn pointers() {
        let mut m = Memory::new();
        m.push_int(100);
        m.assign_local("foo".to_string());
        m.get_local("foo").unwrap();
        m.make_pointer();
        m.assign_local("ptr".to_string());
        m.get_local("ptr").unwrap();
        m.deref_pointer().unwrap();
        m.materialize();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(Address(A2), Offset(A7, 0)),
            Mov(Data(D2), Offset(A2, 0)),
        ]);
    }

    #[test]
    fn pointer_on_stack() {
        let mut m = Memory::new();
        m.push_int(100);
        m.assign_local("foo".to_string());
        for _ in 0..6 {
            m.get_local("foo").unwrap();
            m.make_pointer();
        }
        m.assign_local("ptr".to_string());
        m.get_local("ptr").unwrap();
        m.deref_pointer().unwrap();
        m.materialize();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(Address(A2), Offset(A7, 0)),
            LoadAddress(Address(A3), Offset(A7, 0)),
            LoadAddress(Address(A4), Offset(A7, 0)),
            LoadAddress(Address(A5), Offset(A7, 0)),
            LoadAddress(Address(A6), Offset(A7, 0)),
            LoadAddress(PUSH, Offset(A7, 0)),
            Mov(Address(A0), Offset(A7, 0)),
            Mov(Data(D2), Offset(A0, 0)),
        ]);
    }
}
