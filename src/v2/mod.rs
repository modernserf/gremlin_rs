#![allow(dead_code)]
mod memory4;
mod ty;
mod vm;

use crate::v2::ty::*;

use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum CompileError {
    UnknownIdentifier(String),
    ExpectedType { expected: Ty, received: Ty },
    InvalidAssignment,
    InvalidConst,
    InvalidVarPointer,
}
use CompileError::*;

type Compile<T> = Result<T, CompileError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Mov(DestEA, SrcEA),
    LoadAddress(DestEA, SrcEA),
    Add(DestEA, SrcEA),
    Link(Address, Word),
    Unlink(Address),

    Save,
    Restore,
    // Save(DestEA, RegisterList),
    // Restore(RegisterList, SrcEA),
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RegisterList {
    data: [bool; 8],
    address: [bool; 8],
}

impl RegisterList {
    fn new() -> Self {
        Self {
            data: [false; 8],
            address: [false; 8],
        }
    }
    fn add_data(&mut self, data: Data) {
        self.data[data as usize] = true;
    }
    fn add_address(&mut self, address: Address) {
        self.address[address as usize] = true;
    }
    fn size(&self) -> Word {
        let mut bytes = 0;
        for used in self.data.iter() {
            if *used {
                bytes += 4;
            }
        }
        for used in self.address.iter() {
            if *used {
                bytes += 4;
            }
        }
        bytes
    }
}

// Types

type Word = i32;
type StackID = usize;
type FrameOffset = Word;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Data {
    D0 = 0, // 1st data arg or return value
    D1,     // 2nd data arg or array indexing
    D2,     // locals
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
    A0 = 0, // 1st ptr arg or return value
    A1,     // 2nd ptr arg or dereferencing ptr on stack
    A2,     // locals
    A3,
    A4,
    A5, // context pointer
    A6, // frame pointer
    A7, // stack pointer
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackItem {
    ty: Ty,
    loc: Loc,
}

impl StackItem {
    // TODO: come up with some reason for this
    fn is_materialized(&self) -> bool {
        match &self.loc {
            Loc::Const(_) => false,
            Loc::Id(_) => false,
            Loc::AddressIndirect(_, _, _) => false,
            Loc::StackIndirect(_, _, _) => false,
            _ => true,
        }
    }
}

type IndirectOffset = Word;
type IsVar = bool;

// TODO:
// - "ref" loc, which become AddressIndirect / StackIndirect when materialized
#[derive(Debug, Clone, PartialEq, Eq)]
enum Loc {
    // compile-time only values
    Const(Word),
    Id(StackID),
    // runtime values
    Data(Data),
    Address(Address, IsVar),
    AddressIndirect(Address, IndirectOffset, IsVar),
    Stack(FrameOffset, IsVar),
    StackIndirect(FrameOffset, IndirectOffset, IsVar),
}

struct ScopeFrame {
    base_id: StackID,
    locals: HashMap<String, StackID>,
}
impl ScopeFrame {
    fn root() -> Self {
        Self {
            base_id: 0,
            locals: HashMap::new(),
        }
    }
}

struct Memory {
    output: Vec<Op>,
    stack: Vec<StackItem>,
    scope: Vec<ScopeFrame>,
    frame_offset: FrameOffset,
    used_data_registers: [usize; 8],
    used_address_registers: [usize; 8],
    return_frame_offset: FrameOffset,
    return_ty: Ty,
}

// a register machine pretending to be a stack machine
impl Memory {
    fn new() -> Self {
        Self {
            output: Vec::new(),
            stack: Vec::new(),
            scope: vec![ScopeFrame::root()],
            frame_offset: 0,
            used_data_registers: [0; 8],
            used_address_registers: [0; 8],
            return_frame_offset: 0,
            return_ty: Ty::Void,
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
        for i in 2..5 {
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
        });
    }
    pub fn push_bool(&mut self, value: bool) {
        self.stack.push(StackItem {
            ty: Ty::Bool,
            loc: Loc::Const(if value { 1 } else { 0 }),
        });
    }
    pub fn push_data_arg(&mut self, ty: Ty, register: Data) {
        assert_eq!(self.used_data_registers[register as usize], 0);
        self.used_data_registers[register as usize] = 1;
        self.stack.push(StackItem {
            ty,
            loc: Loc::Data(register),
        });
    }
    pub fn push_stack_arg(&mut self, ty: Ty) {
        self.frame_offset += ty.size().unwrap();
        self.stack.push(StackItem {
            ty,
            loc: Loc::Stack(self.frame_offset, false),
        });
    }
    pub fn define_const(&mut self, name: String) -> Compile<()> {
        let id = self.stack.len() - 1;
        match &self.stack[id].loc {
            Loc::Const(_) => {}
            _ => return Err(InvalidConst),
        }
        let top_frame = self.scope.len() - 1;
        self.scope[top_frame].locals.insert(name, id);
        Ok(())
    }
    pub fn define_let(&mut self, name: String) {
        self.materialize();
        let id = self.stack.len() - 1;
        let top_frame = self.scope.len() - 1;
        self.scope[top_frame].locals.insert(name, id);
    }
    pub fn define_var(&mut self, name: String) {
        let mut item = self.stack.pop().unwrap();
        self.item_to_stack(&mut item, true);
        self.stack.push(item);
        let id = self.stack.len() - 1;
        let top_frame = self.scope.len() - 1;
        self.scope[top_frame].locals.insert(name, id);
    }
    // push a copy of the named item onto the stack
    pub fn get_local(&mut self, name: &str) -> Compile<()> {
        for frame in self.scope.iter().rev() {
            if let Some(id) = frame.locals.get(name) {
                let parent = &self.stack[*id];
                let ty = parent.ty.clone();
                let id = match &parent.loc {
                    Loc::Id(id) => *id,
                    _ => *id,
                };
                self.stack.push(StackItem {
                    ty,
                    loc: Loc::Id(id),
                });
                return Ok(());
            }
        }
        Err(UnknownIdentifier(name.to_string()))
    }
    pub fn begin_scope(&mut self) {
        self.scope.push(ScopeFrame {
            base_id: self.stack.len(),
            locals: HashMap::new(),
        });
    }
    pub fn end_scope(&mut self) {
        let frame = self.scope.pop().unwrap();
        let mut to_drop = 0;
        for item in self.stack.drain(frame.base_id..).collect::<Vec<_>>() {
            match item.loc {
                Loc::Address(a, _) => {
                    self.dec_address_register(a);
                }
                Loc::Data(d) => {
                    self.dec_data_register(d);
                }
                Loc::Stack(_, _) => {
                    to_drop += item.ty.size().unwrap();
                }
                _ => {}
            }
        }
        if to_drop > 0 {
            self.frame_offset -= to_drop;
            self.output
                .push(Op::Add(EA::Address(Address::A7), EA::Immediate(to_drop)));
        }
    }

    // if the top item of the stack is a constant or borrowed, turn it into a value
    fn materialize(&mut self) {
        let mut item = self.stack.pop().unwrap();
        self.materialize_item(&mut item);
        let ty = item.ty;
        self.stack.push(StackItem { ty, loc: item.loc });
    }
    fn copy_item(&mut self, item: &mut StackItem, is_var: bool) {
        if let Some(r) = self.get_data_register() {
            let dest = EA::Data(r);
            let src = self.item_src(item.clone());
            self.output.push(Op::Mov(dest, src));
            item.loc = Loc::Data(r);
        } else {
            self.item_to_stack(item, is_var);
        }
    }
    fn materialize_item(&mut self, item: &mut StackItem) {
        if item.is_materialized() {
            return;
        }
        self.copy_item(item, false)
    }
    fn item_to_stack(&mut self, item: &mut StackItem, is_var: bool) {
        let src = self.item_src(item.clone());
        self.frame_offset += item.ty.size().unwrap();
        self.output.push(Op::Mov(PUSH, src));
        item.loc = Loc::Stack(self.frame_offset, is_var)
    }
    fn item_src(&mut self, item: StackItem) -> EA {
        self.item_src__(item, false)
    }
    fn item_src__(&mut self, item: StackItem, preserve_register: bool) -> EA {
        match item.loc {
            Loc::Const(value) => EA::Immediate(value),
            Loc::Id(id) => {
                let parent_item = self.stack[id].clone();
                self.item_src__(parent_item, true)
            }
            Loc::Data(data) => {
                if !preserve_register {
                    self.dec_data_register(data);
                }
                EA::Data(data)
            }
            Loc::Address(addr, _) => {
                if !preserve_register {
                    self.dec_address_register(addr);
                }
                EA::Address(addr)
            }
            Loc::AddressIndirect(addr, offset, _) => EA::Offset(addr, offset),
            Loc::Stack(offset, _) => {
                let stack_offset = self.frame_offset - offset;
                EA::Offset(Address::A7, stack_offset)
            }
            Loc::StackIndirect(frame_offset, indirect_offset, _) => {
                let stack_offset = self.frame_offset - frame_offset;
                let src = EA::Offset(Address::A7, stack_offset);
                self.output.push(Op::Mov(EA::Address(Address::A1), src));
                EA::Offset(Address::A1, indirect_offset)
            }
        }
    }
    fn item_dest(&mut self, item: &StackItem) -> EA {
        match &item.loc {
            Loc::Data(data) => EA::Data(*data),
            Loc::Stack(offset, _) => {
                let stack_offset = self.frame_offset - *offset;
                EA::Offset(Address::A7, stack_offset)
            }
            _ => panic!("invalid dest"),
        }
    }
    fn item_const(&self, item: &StackItem) -> Option<Word> {
        match &item.loc {
            Loc::Const(value) => Some(*value),
            Loc::Id(id) => self.item_const(&self.stack[*id]),
            _ => None,
        }
    }
    // pop two items from the stack, and push their sum
    pub fn add(&mut self) -> Compile<()> {
        let mut right = self.stack.pop().unwrap();
        let mut left = self.stack.pop().unwrap();
        Ty::Int.unify(&left.ty)?;
        Ty::Int.unify(&right.ty)?;

        let left_const = self.item_const(&left);
        let right_const = self.item_const(&right);

        match (&left_const, &right_const) {
            (Some(l), Some(r)) => {
                let sum = l + r;
                self.push_int(sum);
            }
            (Some(l), _) => {
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
    fn load_address(&mut self, item: StackItem, is_var: bool) {
        let ty = if is_var {
            item.ty.clone().var_pointer()
        } else {
            item.ty.clone().pointer()
        };
        let src = self.item_src(item);
        if let Some(addr) = self.get_address_register() {
            self.output.push(Op::LoadAddress(EA::Address(addr), src));
            self.stack.push(StackItem {
                ty,
                loc: Loc::Address(addr, is_var),
            });
        } else {
            self.frame_offset += ty.size().unwrap();
            self.output.push(Op::LoadAddress(PUSH, src));
            self.stack.push(StackItem {
                ty,
                loc: Loc::Stack(self.frame_offset, is_var),
            });
        }
    }
    pub fn make_pointer(&mut self) {
        let mut item = self.stack.pop().unwrap();
        if let Loc::Id(parent_id) = item.loc {
            let parent_item = self.stack[parent_id].clone();
            if let Loc::Stack(_, _) = parent_item.loc {
                // load the existing stack value directly
                self.load_address(parent_item, false);
                return;
            }
        }
        // create a temporary item, then load its address
        self.item_to_stack(&mut item, false);
        self.load_address(item, false);
    }
    pub fn make_var_pointer(&mut self) -> Compile<()> {
        let item = self.stack.pop().unwrap();
        if let Loc::Id(parent_id) = item.loc {
            let parent_item = self.stack[parent_id].clone();
            if let Loc::Stack(_, is_var) = &self.stack[parent_id].loc {
                if *is_var {
                    self.load_address(parent_item, true);
                    return Ok(());
                }
            }
        }
        Err(InvalidVarPointer)
    }
    fn deref_item(&mut self, item: StackItem) -> Compile<StackItem> {
        let ty = item.ty.deref()?;
        let loc = match item.loc {
            Loc::Address(a, v) => Loc::AddressIndirect(a, 0, v),
            Loc::Stack(offset, v) => Loc::StackIndirect(offset, 0, v),
            Loc::Id(id) => {
                let parent_item = self.stack[id].clone();
                return self.deref_item(parent_item);
            }
            _ => unreachable!(),
        };
        Ok(StackItem { ty, loc })
    }
    pub fn deref_pointer(&mut self) -> Compile<()> {
        let item = self.stack.pop().unwrap();
        let deref_item = self.deref_item(item)?;
        self.stack.push(deref_item);
        Ok(())
    }
    pub fn assign(&mut self) -> Compile<()> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        left.ty.unify(&right.ty)?;

        let dest = match left.loc {
            Loc::AddressIndirect(addr, offset, is_var) => {
                if !is_var {
                    return Err(InvalidAssignment);
                }
                EA::Offset(addr, offset)
            }
            Loc::StackIndirect(frame_offset, indirect_offset, is_var) => {
                if !is_var {
                    return Err(InvalidAssignment);
                }
                let stack_offset = self.frame_offset - frame_offset;
                let src = EA::Offset(Address::A7, stack_offset);
                // TODO: stash subroutine arg using A1, if applicable
                self.output.push(Op::Mov(EA::Address(Address::A1), src));
                EA::Offset(Address::A1, indirect_offset)
            }
            Loc::Id(id) => {
                let parent_item = self.stack[id].clone();
                match parent_item.loc {
                    Loc::Const(_) => {
                        return Err(InvalidAssignment);
                    }
                    _ => self.item_dest(&parent_item),
                }
            }
            _ => return Err(InvalidAssignment),
        };

        let src = self.item_src(right);
        self.output.push(Op::Mov(dest, src));
        Ok(())
    }
    pub fn sub_begin(&mut self, return_ty: Ty) -> Compile<()> {
        self.return_ty = return_ty;
        // allocate return addr
        self.frame_offset += 4;
        // save registers
        self.frame_offset += 40;
        self.output.push(Op::Save);
        self.return_frame_offset = self.frame_offset;
        Ok(())
    }
    pub fn sub_return(&mut self) -> Compile<()> {
        let item = self.stack.pop().unwrap();
        self.return_ty.unify(&item.ty)?;
        // TODO: handle returning on stack or with addr
        let src = self.item_src(item);
        self.output.push(Op::Mov(EA::Data(Data::D0), src));
        self.sub_return__();
        Ok(())
    }
    pub fn sub_return_void(&mut self) -> Compile<()> {
        self.return_ty.unify(&Ty::Void)?;
        self.sub_return__();
        Ok(())
    }
    fn sub_return__(&mut self) {
        let to_drop = self.frame_offset - self.return_frame_offset;
        if to_drop > 0 {
            self.output
                .push(Op::Add(EA::Address(Address::A7), EA::Immediate(to_drop)));
        }
        self.output.push(Op::Restore);
        self.output.push(Op::Return);
    }
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
        m.define_const("foo".to_string()).unwrap();
        m.push_int(456);
        m.define_let("bar".to_string());
        m.get_local("foo").unwrap();
        m.get_local("bar").unwrap();

        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(123),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Id(0),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Id(1),
            },
        ]);
    }

    #[test]
    fn invalid_const() {
        let mut m = Memory::new();
        m.push_int(123);
        m.define_let("foo".to_string());
        m.get_local("foo").unwrap();
        expect_err(m.define_const("bar".to_string()), InvalidConst);
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
        m.define_const("foo".to_string()).unwrap();
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.add().unwrap();

        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(100),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(300),
            },
        ]);
    }

    #[test]
    fn add_left_const() {
        let mut m = Memory::new();
        m.push_int(100);
        m.define_const("foo".to_string()).unwrap();
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.materialize();
        m.add().unwrap();

        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Const(100),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
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
        m.define_let("foo".to_string());
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.materialize();
        m.add().unwrap();
        m.expect_stack(vec![
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D2),
            },
            StackItem {
                ty: Ty::Int,
                loc: Loc::Data(D4),
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
            },
            StackItem {
                ty: Ty::Bool,
                loc: Loc::Const(1),
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
    fn assign() {
        let mut m = Memory::new();
        m.push_int(100);
        m.define_let("foo".to_string());
        m.get_local("foo").unwrap();
        m.push_int(200);
        m.materialize();
        m.assign().unwrap();

        m.expect_output(vec![
            //
            Mov(Data(D2), Immediate(100)),
            Mov(Data(D3), Immediate(200)),
            Mov(Data(D2), Data(D3)),
        ]);
    }

    #[test]
    fn invalid_assignment() {
        let mut m = Memory::new();
        m.push_int(100);
        m.push_int(200);
        expect_err(m.assign(), InvalidAssignment);
    }

    #[test]
    fn invalid_const_assignment() {
        let mut m = Memory::new();
        m.push_int(100);
        m.define_const("foo".to_string()).unwrap();
        m.push_int(200);
        expect_err(m.assign(), InvalidAssignment);
    }

    #[test]
    fn pointers() {
        let mut m = Memory::new();
        m.push_int(100);
        m.define_const("foo".to_string()).unwrap();
        m.get_local("foo").unwrap();
        m.make_pointer();
        m.define_let("ptr".to_string());
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
        m.define_var("foo".to_string());
        for _ in 0..4 {
            m.get_local("foo").unwrap();
            m.make_pointer();
        }
        m.define_let("ptr".to_string());
        m.get_local("ptr").unwrap();
        m.deref_pointer().unwrap();
        m.materialize();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(Address(A2), Offset(A7, 0)),
            LoadAddress(Address(A3), Offset(A7, 0)),
            LoadAddress(Address(A4), Offset(A7, 0)),
            LoadAddress(PUSH, Offset(A7, 0)),
            Mov(Address(A1), Offset(A7, 0)),
            Mov(Data(D2), Offset(A1, 0)),
        ]);
    }

    #[test]
    fn invalid_deref() {
        let mut m = Memory::new();
        m.push_int(100);

        expect_err(
            m.deref_pointer(),
            ExpectedType {
                expected: Ty::Int.pointer(),
                received: Ty::Int,
            },
        )
    }

    #[test]
    fn invalid_var_pointer() {
        let mut m = Memory::new();

        m.push_int(100);
        m.define_let("foo".to_string());
        m.get_local("foo").unwrap();

        expect_err(m.make_var_pointer(), InvalidVarPointer);
    }

    #[test]
    fn pointer_assign() {
        let mut m = Memory::new();

        m.push_int(100);
        m.define_var("foo".to_string());
        m.get_local("foo").unwrap();
        m.make_var_pointer().unwrap();
        m.define_let("ptr".to_string());
        m.get_local("ptr").unwrap();
        m.deref_pointer().unwrap();
        m.push_int(200);
        m.assign().unwrap();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(Address(A2), Offset(A7, 0)),
            Mov(Offset(A2, 0), Immediate(200)),
        ]);
    }

    #[test]
    fn invalid_pointer_assign() {
        let mut m = Memory::new();

        m.push_int(100);
        m.define_let("foo".to_string());
        m.get_local("foo").unwrap();
        m.make_pointer();
        m.define_let("ptr".to_string());
        m.get_local("ptr").unwrap();
        m.deref_pointer().unwrap();
        m.push_int(200);

        expect_err(m.assign(), InvalidAssignment);
    }

    #[test]
    fn scope() {
        let mut m = Memory::new();
        m.begin_scope();
        for i in 0..8 {
            m.push_int(i);
            m.materialize();
        }
        m.end_scope();
        m.push_int(8);
        m.materialize();

        m.expect_output(vec![
            Mov(Data(D2), Immediate(0)),
            Mov(Data(D3), Immediate(1)),
            Mov(Data(D4), Immediate(2)),
            Mov(Data(D5), Immediate(3)),
            Mov(Data(D6), Immediate(4)),
            Mov(Data(D7), Immediate(5)),
            Mov(PUSH, Immediate(6)),
            Mov(PUSH, Immediate(7)),
            Add(Address(A7), Immediate(8)),
            Mov(Data(D2), Immediate(8)),
        ]);
    }

    #[test]
    fn scope_bindings() {
        let mut m = Memory::new();
        m.begin_scope();
        m.push_int(100);
        m.define_let("foo".to_string());
        m.end_scope();

        expect_err(m.get_local("foo"), UnknownIdentifier("foo".to_string()))
    }

    #[test]
    fn subroutine() {
        let mut m = Memory::new();
        m.push_data_arg(Ty::Int, D0);
        m.define_let("x".to_string());
        m.push_data_arg(Ty::Int, D1);
        m.define_let("y".to_string());
        m.sub_begin(Ty::Int).unwrap();

        m.get_local("x").unwrap();
        m.get_local("y").unwrap();
        m.add().unwrap();
        m.sub_return().unwrap();

        m.expect_output(vec![
            Save,
            Mov(Data(D2), Data(D0)),
            Add(Data(D2), Data(D1)),
            Mov(Data(D0), Data(D2)),
            Restore,
            Return,
        ]);
    }

    #[test]
    fn subroutine_stack_cleanup() {
        let mut m: Memory = Memory::new();
        m.sub_begin(Ty::Void).unwrap();
        for i in 0..10 {
            m.push_int(i);
            m.materialize();
        }
        m.sub_return_void().unwrap();

        m.expect_output(vec![
            Save,
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
            Add(Address(A7), Immediate(16)),
            Restore,
            Return,
        ]);
    }

    #[test]
    fn subroutine_return_err() {
        let mut m = Memory::new();
        m.sub_begin(Ty::Int).unwrap();
        m.push_bool(true);

        expect_err(
            m.sub_return(),
            ExpectedType {
                expected: Ty::Int,
                received: Ty::Bool,
            },
        );
    }
}
