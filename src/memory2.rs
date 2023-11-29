#![allow(dead_code)]

use crate::runtime::{IRCond, IROp, Register, Word, EA, IR};
use crate::sub::TySub;
use crate::ty::Ty;
use crate::{Compile, CompileError::*};
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Memory2 {
    stack: Vec<StackItem>,
    scope: Vec<ScopeFrame>,
    module_scope: ModuleScope,
    current_sub: Option<SubContext>,
    output: Vec<IR>,
    data_registers: HashMap<Data, Ty>,
    address_registers: HashMap<Address, Ty>,
    cc_zero_locked: bool,
}

impl Memory2 {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            scope: Vec::new(),
            module_scope: ModuleScope::new(),
            current_sub: None,
            output: Vec::new(),
            data_registers: HashMap::new(),
            address_registers: HashMap::new(),
            cc_zero_locked: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackItem {
    kind: StackItemKind,
    last_updated: usize, // the index of the instruction that last wrote to this address
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum StackItemKind {
    // low block in a local variable
    Local(String, Ty),
    // low block in a subroutine argument
    Arg(String, Ty),
    // low block in a subroutine return slot
    Return(Ty),
    // low block in an unassigned expression
    Expr(Ty),
    // a subsequent block of the value below it
    Owned,
    // the return address on the stack between args & locals
    ReturnAddr,
}
use StackItemKind::*;

impl StackItemKind {
    fn ty(&self) -> &Ty {
        match &self {
            Local(_, ty) => ty,
            Arg(_, ty) => ty,
            Return(ty) => ty,
            Expr(ty) => ty,
            _ => panic!("expected value with type"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ScopeFrame {
    // stack index of _high_ block of first item, i.e. 0 at root scope
    start_index: usize,
    // map of identifier to index of low block of value, i.e. "frame offset"
    locals: HashMap<String, usize>,
    // whether there was a return in this specific frame
    did_return: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModuleScope {
    types: HashMap<String, Ty>,
    subs: HashMap<String, SubRecord>,
}

impl ModuleScope {
    fn new() -> Self {
        Self {
            types: HashMap::from_iter(vec![
                ("Void".to_string(), Ty::void()),
                ("Int".to_string(), Ty::int()),
                ("Bool".to_string(), Ty::bool()),
            ]),
            subs: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SubRecord {
    ty: Ty,
    // where the sub begins in the output
    index: usize,
}

impl SubRecord {
    fn return_ty(&self) -> Ty {
        let ty = self.ty.get_sub().unwrap();
        ty.ret.clone()
    }
    fn params_size(&self) -> Word {
        let ty = self.ty.get_sub().unwrap();
        ty.params
            .iter()
            .map(|p| p.size())
            .fold(0, |acc, size| acc + size)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SubContext {
    name: String,
    return_ty: Ty,
    return_idx: usize,
    return_addr_idx: usize,
    did_return: bool,
    ty: Ty,
    index: usize,
}

// Calling convention
// [return value, args..., return addr, locals...] top
// caller:
// Sub(SP, sizeof return)
// Mov(Push, arg) ...
// JSR(sub)
// ...
// Add(SP, sizeof args)
// callee:
// ...
// Mov(SP+return, result)
// Add(SP, sizeof locals)
// RTS

impl Memory2 {
    fn get_sub(&self, name: &str) -> Compile<SubRecord> {
        self.module_scope
            .subs
            .get(name)
            .cloned()
            .ok_or_else(|| UnknownIdentifier(name.to_string()))
    }

    fn begin_call(&mut self, sub: &SubRecord) {
        let size = sub.return_ty().size();
        if size > 0 {
            for _ in 1..size {
                self.push_stack_item(Owned);
            }
            self.push_stack_item(Expr(sub.return_ty().clone()));
            self.output
                .push(IR::Sub(EA::Register(Register::SP), EA::Immediate(size)));
        }
    }

    fn end_call(&mut self, sub: &SubRecord) {
        self.invalidate_volatile_registers();
        self.clobber_cc_zero();

        self.output.push(IR::Call(sub.index as Word));
        let to_drop = sub.params_size();
        if to_drop > 0 {
            self.output
                .push(IR::Add(EA::Register(Register::SP), EA::Immediate(to_drop)));
            let new_len = self.stack.len() - to_drop as usize;
            self.stack.truncate(new_len);
        }
    }

    fn begin_sub(&mut self, name: String, params: Vec<(String, Ty)>, ret: Ty) {
        self.scope = Vec::new();
        self.stack = Vec::new();
        self.init_registers();
        self.clobber_cc_zero();

        // begin sub scope
        self.init_scope();
        let ty = Ty::sub(TySub::new(
            params.iter().map(|(_, ty)| ty.clone()).collect(),
            ret.clone(),
        ));
        let return_idx = if ret.size() > 0 {
            // return slot
            for _ in 1..ret.size() {
                self.push_stack_item(Owned);
            }
            self.push_stack_item(Return(ret.clone()))
        } else {
            0
        };

        // params
        for (name, ty) in params.into_iter() {
            for _ in 1..ty.size() {
                self.push_stack_item(Owned);
            }
            let idx = self.push_stack_item(Arg(name.clone(), ty));
            self.insert_scope(name, idx);
        }
        // return addr
        let return_addr_idx = self.push_stack_item(ReturnAddr);

        // begin locals scope
        self.enter_block();

        self.current_sub = Some(SubContext {
            name,
            did_return: false,
            return_ty: ret,
            return_idx,
            return_addr_idx,
            ty,
            index: self.output.len(),
        });
    }

    fn return_sub(&mut self, src: Option<Src>) -> Compile<()> {
        let ctx = self.current_sub.as_mut().unwrap();
        ctx.did_return = true;
        let scope_idx = self.scope.len() - 1;
        self.scope[scope_idx].did_return = true;

        let target = Target {
            ty: ctx.return_ty.clone(),
            idx: ctx.return_idx,
            offset: 0,
        };
        let to_drop = self.stack.len() - ctx.return_addr_idx - 1;

        if let Some(src) = src {
            // TODO: check return ty
            self.set_dest(Dest::Target(target), src);
        } else {
            ctx.return_ty.check(&Ty::void())?;
        }

        if to_drop > 0 {
            self.output.push(IR::Add(
                EA::Register(Register::SP),
                EA::Immediate(to_drop as Word),
            ));
        }
        self.output.push(IR::Return);

        Ok(())
    }

    fn end_sub(&mut self) -> Compile<()> {
        if !self.current_sub.as_ref().unwrap().did_return {
            self.return_sub(None)?;
        }
        let ctx = self.current_sub.take().unwrap();
        self.module_scope.subs.insert(
            ctx.name,
            SubRecord {
                ty: ctx.ty,
                index: ctx.index,
            },
        );
        Ok(())
    }
}

const PUSH: EA = EA::PreDec(Register::SP);
const POP: EA = EA::PostInc(Register::SP);

trait ConsumableRegister {
    fn try_get(&self, mem: &mut Memory2) -> Option<Ty>;
    fn try_insert(&self, mem: &mut Memory2, ty: Ty) -> Option<Ty>;
    fn try_remove(&self, mem: &mut Memory2) -> Option<Ty>;
}
trait CR: ConsumableRegister + Debug {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Data {
    D0,
}

impl Data {
    fn ea(&self) -> EA {
        match self {
            Data::D0 => EA::Register(Register::R0),
        }
    }
}

impl ConsumableRegister for Data {
    fn try_get(&self, mem: &mut Memory2) -> Option<Ty> {
        mem.data_registers.get(self).cloned()
    }
    fn try_insert(&self, mem: &mut Memory2, ty: Ty) -> Option<Ty> {
        mem.data_registers.insert(*self, ty)
    }
    fn try_remove(&self, mem: &mut Memory2) -> Option<Ty> {
        mem.data_registers.remove(self)
    }
}
impl CR for Data {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Address {
    A0,
}

impl Address {
    fn ea(&self) -> EA {
        match self {
            Address::A0 => EA::Register(Register::A0),
        }
    }
    fn ea_offset(&self, offset: Word) -> EA {
        match self {
            Address::A0 => EA::Offset(Register::A0, offset),
        }
    }
}

impl ConsumableRegister for Address {
    fn try_get(&self, mem: &mut Memory2) -> Option<Ty> {
        mem.address_registers.get(self).cloned()
    }
    fn try_insert(&self, mem: &mut Memory2, ty: Ty) -> Option<Ty> {
        mem.address_registers.insert(*self, ty)
    }
    fn try_remove(&self, mem: &mut Memory2) -> Option<Ty> {
        mem.address_registers.remove(self)
    }
}
impl CR for Address {}

impl Memory2 {
    fn init_registers(&mut self) {
        self.data_registers = HashMap::new();
        self.address_registers = HashMap::new();
    }
    fn invalidate_volatile_registers(&mut self) {
        self.invalidate_register(Address::A0);
        self.invalidate_register(Data::D0);
    }

    // TODO: iterate through more items
    fn get_data_register(&mut self) -> Option<Data> {
        if self.data_registers.contains_key(&Data::D0) {
            None
        } else {
            Some(Data::D0)
        }
    }
    fn get_address_register(&mut self) -> Option<Address> {
        if self.address_registers.contains_key(&Address::A0) {
            None
        } else {
            Some(Address::A0)
        }
    }
    fn invalidate_register(&mut self, r: impl CR) {
        if r.try_remove(self).is_some() {
            panic!("{:?} in use", r);
        }
    }
    fn take_register(&mut self, r: impl CR, ty: Ty) {
        if r.try_insert(self, ty).is_some() {
            panic!("{:?} in use", r);
        }
    }
    fn replace_register(&mut self, r: impl CR, ty: Ty) {
        if r.try_insert(self, ty).is_none() {
            panic!("{:?} is vacant", r);
        }
    }
    fn borrow_register(&mut self, r: impl CR) -> Ty {
        if let Some(ty) = r.try_get(self) {
            ty
        } else {
            panic!("{:?} is vacant", r);
        }
    }
    fn release_register(&mut self, r: impl CR) -> Ty {
        if let Some(ty) = r.try_remove(self) {
            ty
        } else {
            panic!("{:?} is vacant", r);
        }
    }
}

// the result of evaluating an expression
type FrameOffset = usize;
type AddressOffset = Word;
type SubIndex = usize;
type RefLevel = usize;

#[derive(Debug, Clone)]
enum Item {
    // not yet runtime values, can delay evaluation but need to be materialized for use
    Constant(Ty, Word),
    Local(Ty, FrameOffset, RefLevel),
    // Record(Ty, Vec<(FieldOffset, Item))>
    // Sub(Ty, SubIndex), TODO
    // volatile runtime values, need to be preserved in some contexts
    // Cmp, TODO
    Data(Ty, Data),                              // small data
    Address(Ty, Address),                        // pointers
    AddressIndirect(Ty, Address, AddressOffset), // data accessed thru pointer
    // durable runtime values
    Stack(Ty, FrameOffset), // large data, or ran out of registers
}

impl Memory2 {
    fn stack_offset__(&self, idx: usize) -> EA {
        EA::Offset(Register::SP, (self.stack.len() - idx - 1) as Word)
    }
    fn release_register__<T: CR>(&mut self, r: T) -> T {
        if let Some(_) = r.try_remove(self) {
            r
        } else {
            panic!("{:?} is vacant", r);
        }
    }
    fn item_src(&mut self, item: Item) -> (Ty, EA) {
        match item {
            Item::Constant(ty, value) => (ty, EA::Immediate(value)),
            Item::Local(ty, idx, ref_level) => {
                assert_eq!(ref_level, 0);
                (ty, self.stack_offset__(idx))
            }
            Item::Data(ty, r) => (ty, self.release_register__(r).ea()),
            Item::Address(ty, r) => (ty, self.release_register__(r).ea()),
            Item::AddressIndirect(ty, r, offset) => {
                (ty, self.release_register__(r).ea_offset(offset))
            }
            // TODO: ensure this is at top of stack
            Item::Stack(ty, _) => (ty, POP),
        }
    }

    fn to_stack(&mut self, ty: Ty, src_ea: EA) -> usize {
        let src_ea = match src_ea {
            // POP into PUSH -- item already on stack where it should be
            EA::PostInc(Register::SP) => return self.stack.len() - 1,
            // account for predec offset in PUSH -- stack offset stays constant when copying
            EA::Offset(Register::SP, offset) => EA::Offset(Register::SP, offset + ty.size()),
            it => it,
        };

        // write high to low
        for _ in 1..ty.size() {
            self.push_stack_item(Owned);
            self.output.push(IR::Mov(PUSH, src_ea));
        }
        let idx = self.push_stack_item(Expr(ty));
        self.output.push(IR::Mov(PUSH, src_ea));
        idx
    }

    fn push_item(&mut self, item: Item) -> usize {
        let (ty, src_ea) = match item {
            Item::Local(ty, offset, 1) => {
                let idx = self.push_stack_item(Expr(ty));
                self.output
                    .push(IR::LoadAddress(PUSH, self.stack_offset__(offset)));
                return idx;
            }
            item => self.item_src(item),
        };
        let idx = self.to_stack(ty.clone(), src_ea);
        idx
    }

    // put into register or push onto stack
    fn materialize_item(&mut self, item: Item) -> Item {
        // most items are already materialized
        match &item {
            Item::Constant(_, _) => {}
            Item::Local(_, _, _) => {}
            _ => return item,
        };
        let (ty, src_ea) = self.item_src(item);

        self.clobber_cc_zero();
        let is_ptr = ty.deref().is_ok();
        let size = ty.size();
        if is_ptr && size == 1 {
            if let Some(a) = self.get_address_register() {
                a.try_insert(self, ty.clone());
                self.output.push(IR::Mov(a.ea(), src_ea));
                return Item::Address(ty, a);
            }
        } else if size == 1 {
            if let Some(d) = self.get_data_register() {
                d.try_insert(self, ty.clone());
                self.output.push(IR::Mov(d.ea(), src_ea));
                return Item::Data(ty, d);
            }
        }

        let idx = self.to_stack(ty.clone(), src_ea);
        Item::Stack(ty, idx)
    }

    // apply op from src to dest, updating the value of dest
    fn apply_item(&mut self, op: IROp, dest: &Item, src: Item) {
        // TODO: check types, item kinds in caller
        self.clobber_cc_zero();
        let (src_ty, src_ea) = self.item_src(src);

        let (dest_ty, dest_ea) = match dest {
            Item::Constant(_, _) => panic!("cannot assign to constant"),
            Item::Local(ty, idx, ref_level) => {
                assert_eq!(*ref_level, 0);
                (ty, self.stack_offset__(*idx))
            }
            Item::Data(ty, d) => (ty, d.ea()),
            Item::Address(ty, a) => (ty, a.ea()),
            Item::AddressIndirect(ty, a, o) => (ty, a.ea_offset(*o)),
            Item::Stack(ty, idx) => (ty, self.stack_offset__(*idx)),
        };
        assert_eq!(src_ty.size(), dest_ty.size());

        // write low to high
        for i in 0..src_ty.size() {
            match dest {
                Item::Local(_, idx, ref_level) => {
                    assert_eq!(*ref_level, 0);
                    self.update_stack_item(*idx, i as usize)
                }
                Item::Stack(_, idx) => self.update_stack_item(*idx, i as usize),
                _ => {}
            };

            let src_ea_offset = if src_ea == POP {
                src_ea
            } else {
                src_ea.add_offset(i)
            };
            self.output.push(op(dest_ea.add_offset(i), src_ea_offset));
        }
    }

    fn ref_item(&mut self, item: Item) -> Compile<Item> {
        // let (ty, src_ea) =
        match item {
            Item::Local(ty, idx, ref_level) => {
                return Ok(Item::Local(ty.add_ref(), idx, ref_level + 1));
            }
            _ => return Err(InvalidRef),
        };
    }

    fn deref_item(&mut self, item: Item) -> Compile<Item> {
        let item = self.materialize_item(item);
        match item {
            Item::Address(ty, a) => return Ok(Item::AddressIndirect(ty.deref()?, a, 0)),
            Item::AddressIndirect(ty, a, offset) => {
                self.output.push(IR::Mov(a.ea(), a.ea_offset(offset)));
                return Ok(Item::AddressIndirect(ty.deref()?, a, offset));
            }
            Item::Stack(_, _) => {
                todo!("how do I deref without a free register?");
            }
            _ => {
                return Err(InvalidDeref);
            }
        };
    }

    fn drop_item(&mut self, item: Item) {
        match item {
            Item::Constant(_, _) => {}
            Item::Local(_, _, _) => {}
            Item::Data(_, r) => {
                self.release_register__(r);
            }
            Item::Address(_, a) => {
                self.release_register__(a);
            }
            Item::AddressIndirect(_, a, _) => {
                self.release_register__(a);
            }
            Item::Stack(_, idx) => {
                let to_drop = self.stack.len() - idx - 1;
                if to_drop > 0 {
                    self.output.push(IR::Add(
                        EA::Register(Register::SP),
                        EA::Immediate(to_drop as Word),
                    ));
                    let new_len = self.stack.len() - to_drop;
                    self.stack.truncate(new_len);
                }
            }
        };
    }
}

#[cfg(test)]
mod item_test {
    use super::{Address::*, Data::*, *};
    use crate::runtime::{Register::SP, EA::*, IR, IR::*};

    // mini AST / parser
    enum Stmt {
        Let(&'static str, Expr),
        Expr(Expr),
    }

    enum Expr {
        Int(Word),
        Ident(&'static str),
        Add(Box<Expr>, Box<Expr>),
        Assign(Box<Expr>, Box<Expr>),
        Ref(Box<Expr>),
        Deref(Box<Expr>),
    }
    impl Expr {
        fn add(l: Expr, r: Expr) -> Expr {
            Expr::Add(Box::new(l), Box::new(r))
        }
        fn assign(l: Expr, r: Expr) -> Expr {
            Expr::Assign(Box::new(l), Box::new(r))
        }
        fn ref_(expr: Expr) -> Expr {
            Expr::Ref(Box::new(expr))
        }
        fn deref(expr: Expr) -> Expr {
            Expr::Deref(Box::new(expr))
        }
    }

    impl Memory2 {
        fn p_expr(&mut self, expr: Expr) -> Compile<Item> {
            match expr {
                Expr::Int(value) => Ok(Item::Constant(Ty::int(), value)),
                Expr::Ident(name) => {
                    let idx = self
                        .get_scope(name)
                        .ok_or_else(|| UnknownIdentifier(name.to_string()))?;
                    let ty = self.stack[idx].kind.ty().clone();
                    Ok(Item::Local(ty, idx, 0))
                }
                Expr::Ref(expr) => {
                    let item = self.p_expr(*expr)?;
                    self.ref_item(item)
                }
                Expr::Deref(expr) => {
                    let item = self.p_expr(*expr)?;
                    self.deref_item(item)
                }
                Expr::Assign(left, right) => {
                    let left = self.p_expr(*left)?;

                    match &left {
                        Item::Local(_, _, _) => {}
                        Item::AddressIndirect(_, _, _) => {}
                        _ => return Err(InvalidAssignment),
                    };

                    let right = self.p_expr(*right)?;
                    self.apply_item(IR::Mov, &left, right);
                    Ok(left)
                }
                Expr::Add(left, right) => {
                    let left = self.p_expr(*left)?;
                    if let Item::Constant(ty, l) = left {
                        let right = self.p_expr(*right)?;
                        // constant folding
                        if let Item::Constant(_, r) = right {
                            return Ok(Item::Constant(ty, l + r));
                        }
                        let right = self.materialize_item(right);
                        // reverse commutative operands
                        self.apply_item(IR::Add, &right, Item::Constant(ty, l));
                        return Ok(right);
                    }

                    // need to materialize left _before_ parsing right, in case left is volatile
                    let left = self.materialize_item(left);

                    let right = self.p_expr(*right)?;
                    self.apply_item(IR::Add, &left, right);
                    Ok(left)
                }
            }
        }

        fn p_stmt(&mut self, stmt: Stmt) -> Compile<()> {
            match stmt {
                Stmt::Let(name, expr) => {
                    let item = self.p_expr(expr)?;
                    let idx = self.push_item(item);
                    self.insert_scope(name.to_string(), idx);
                    self.stack[idx].kind = match &self.stack[idx].kind {
                        Expr(t) => Local(name.to_string(), t.clone()),
                        _ => unreachable!(),
                    }
                }
                Stmt::Expr(expr) => {
                    let item = self.p_expr(expr)?;
                    self.drop_item(item);
                }
            }
            Ok(())
        }
        fn p_block(&mut self, block: Vec<Stmt>) -> Compile<()> {
            for stmt in block {
                self.p_stmt(stmt)?;
            }
            Ok(())
        }
    }

    #[test]
    fn assignment() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            Stmt::Let("foo", Expr::Int(123)),
            Stmt::Expr(Expr::assign(Expr::Ident("foo"), Expr::Int(456))),
        ])
        .unwrap();
        m.expect_output(vec![
            //
            Mov(PUSH, Immediate(123)),
            Mov(Offset(SP, 0), Immediate(456)),
        ]);
    }

    #[test]
    fn add_constant() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::add(Expr::Int(100), Expr::Int(200))),
        ])
        .unwrap();
        m.expect_output(vec![
            //
            Mov(PUSH, Immediate(300)),
        ]);
    }

    #[test]
    fn add_const_left() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("bar", Expr::add(Expr::Int(200), Expr::Ident("foo"))),
        ])
        .unwrap();
        m.expect_output(vec![
            //
            Mov(PUSH, Immediate(100)),
            Mov(D0.ea(), Offset(SP, 0)),
            Add(D0.ea(), Immediate(200)),
            Mov(PUSH, D0.ea()),
        ]);
    }

    #[test]
    fn add_2() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("bar", Expr::Int(200)),
            Stmt::Let("baz", Expr::add(Expr::Ident("foo"), Expr::Ident("bar"))),
        ])
        .unwrap();
        m.expect_output(vec![
            //
            Mov(PUSH, Immediate(100)),
            Mov(PUSH, Immediate(200)),
            Mov(D0.ea(), Offset(SP, 1)),
            Add(D0.ea(), Offset(SP, 0)),
            Mov(PUSH, D0.ea()),
        ]);
    }

    #[test]
    fn add_3_left() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("bar", Expr::Int(200)),
            Stmt::Let(
                "baz",
                Expr::add(
                    Expr::add(Expr::Ident("foo"), Expr::Int(300)),
                    Expr::Ident("bar"),
                ),
            ),
        ])
        .unwrap();
        m.expect_output(vec![
            //
            Mov(PUSH, Immediate(100)),
            Mov(PUSH, Immediate(200)),
            Mov(D0.ea(), Offset(SP, 1)),
            Add(D0.ea(), Immediate(300)),
            Add(D0.ea(), Offset(SP, 0)),
            Mov(PUSH, D0.ea()),
        ]);
    }

    #[test]
    fn add_3_right() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("bar", Expr::Int(200)),
            Stmt::Let(
                "baz",
                Expr::add(
                    Expr::Ident("foo"),
                    Expr::add(Expr::Ident("bar"), Expr::Int(300)),
                ),
            ),
        ])
        .unwrap();
        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            Mov(PUSH, Immediate(200)),
            Mov(D0.ea(), Offset(SP, 1)),
            Mov(PUSH, Offset(SP, 1)),
            Add(Offset(SP, 0), Immediate(300)),
            Add(D0.ea(), POP),
            Mov(PUSH, D0.ea()),
        ]);
    }

    #[test]
    fn ref_deref() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("ptr", Expr::ref_(Expr::Ident("foo"))),
            Stmt::Let("bar", Expr::deref(Expr::Ident("ptr"))),
        ])
        .unwrap();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(PUSH, Offset(SP, 1)),
            Mov(A0.ea(), Offset(SP, 0)),
            Mov(PUSH, A0.ea_offset(0)),
        ]);
    }

    #[test]
    fn double_ref_deref() {
        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("ptr", Expr::ref_(Expr::Ident("foo"))),
            Stmt::Let("ptr_ptr", Expr::ref_(Expr::Ident("ptr"))),
            Stmt::Let("bar", Expr::deref(Expr::deref(Expr::Ident("ptr_ptr")))),
        ])
        .unwrap();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(PUSH, Offset(SP, 1)),
            LoadAddress(PUSH, Offset(SP, 1)),
            Mov(A0.ea(), Offset(SP, 0)),
            Mov(A0.ea(), A0.ea_offset(0)),
            Mov(PUSH, A0.ea_offset(0)),
        ]);
    }

    #[test]
    fn ref_assignment() {
        // let mut foo = 100;
        // let ptr = &mut foo;
        // *ptr = 200;

        let mut m = Memory2::new();
        m.init_scope();
        m.p_block(vec![
            //
            Stmt::Let("foo", Expr::Int(100)),
            Stmt::Let("ptr", Expr::ref_(Expr::Ident("foo"))),
            Stmt::Expr(Expr::assign(
                Expr::deref(Expr::Ident("ptr")),
                Expr::Int(200),
            )),
        ])
        .unwrap();

        m.expect_output(vec![
            Mov(PUSH, Immediate(100)),
            LoadAddress(PUSH, Offset(SP, 1)),
            Mov(A0.ea(), Offset(SP, 0)),
            Mov(A0.ea_offset(0), Immediate(200)),
        ])
    }
}

// a location on the stack which can be either a src or dest
#[derive(Debug, Clone)]
struct Target {
    ty: Ty,
    idx: usize,
    offset: usize,
}

#[derive(Debug)]
enum Src {
    Immediate(Ty, Word),
    Target(Target),
    Pop,
    // TODO: when would I want to keep a data register src?
    PopData(Data),
    PopAddress(Address),
    AddressIndirect(Address, Ty, usize),
}

// dest for "accumulator" operations, _not_ push
#[derive(Debug, Clone)]
enum Dest {
    Data(Data),
    Address(Address),
    AddressIndirect(Address, Ty, usize),
    Target(Target),
}

#[derive(Debug)]
enum CmpSrc {
    Immediate(Ty, Word),
    Data(Data),
}

impl Memory2 {
    // src constructors
    fn immediate(&self, ty: Ty, value: Word) -> Src {
        assert_eq!(ty.size(), 1);
        Src::Immediate(ty, value)
    }
    fn identifier(&self, name: &str) -> Compile<Src> {
        let target = self.identifier_target(name)?;
        Ok(Src::Target(target))
    }
    fn pop(&self) -> Src {
        Src::Pop
    }
    // target constructors
    fn identifier_target(&self, name: &str) -> Compile<Target> {
        let idx = self
            .get_scope(name)
            .ok_or_else(|| UnknownIdentifier(name.to_string()))?;
        let ty = self.stack[idx].kind.ty().clone();
        Ok(Target { ty, idx, offset: 0 })
    }
    fn target_field(&self, parent: Target, field_name: &str) -> Compile<Target> {
        let rec = parent.ty.get_record()?;
        let field = rec.get(field_name, None)?;
        Ok(Target {
            ty: field.ty.clone(),
            idx: parent.idx,
            offset: parent.offset + field.offset as usize,
        })
    }

    pub fn push(&mut self, src: Src) -> Target {
        let (ty, ea) = match src {
            Src::Immediate(ty, value) => (ty, EA::Immediate(value)),
            Src::Target(target) => {
                let ea = self.stack_offset(&target).add_offset(target.ty.size()); // account for pre-decrement
                (target.ty, ea)
            }
            Src::PopData(r) => (self.release_register(r), r.ea()),
            Src::PopAddress(r) => (self.release_register(r), r.ea()),
            Src::AddressIndirect(r, ty, offset) => (ty, self.deref_register(r, offset)),
            Src::Pop => {
                panic!("cannot combine push & pop");
            }
        };
        // write high to low
        for _ in 1..ty.size() {
            self.push_stack_item(Owned);
            self.output.push(IR::Mov(PUSH, ea));
        }
        let idx = self.push_stack_item(Expr(ty.clone()));
        self.output.push(IR::Mov(PUSH, ea));
        self.clobber_cc_zero();
        Target { ty, idx, offset: 0 }
    }
    pub fn push_structure(&mut self, ty: Ty) -> Target {
        let size = ty.size();
        for _ in 1..size {
            self.push_stack_item(Owned);
        }
        let idx = self.push_stack_item(Expr(ty.clone()));
        self.output
            .push(IR::Sub(EA::Register(Register::SP), EA::Immediate(size)));
        self.clobber_cc_zero();
        Target { ty, idx, offset: 0 }
    }
    pub fn set_dest(&mut self, dest: Dest, src: Src) {
        let (src_ty, src_ea) = match src {
            Src::Immediate(ty, value) => (ty, EA::Immediate(value)),
            Src::PopData(r) => (self.release_register(r), r.ea()),
            Src::PopAddress(r) => (self.release_register(r), r.ea()),
            Src::AddressIndirect(r, ty, offset) => (ty, self.deref_register(r, offset)),
            Src::Target(target) => {
                let ea = self.stack_offset(&target);
                (target.ty, ea)
            }
            Src::Pop => {
                let ty = self.expect_pop_expr().clone();
                (ty, POP)
            }
        };
        match dest {
            Dest::Data(r) => {
                self.take_register(r, src_ty);
                self.output.push(IR::Mov(r.ea(), src_ea));
            }
            Dest::Address(r) => {
                match (src_ea, r.ea()) {
                    // allow Mov(A0, (A0))
                    (EA::Offset(a, _), EA::Register(b)) if a == b => {
                        self.replace_register(r, src_ty);
                    }
                    _ => {
                        self.take_register(r, src_ty);
                    }
                };
                self.output.push(IR::Mov(r.ea(), src_ea))
            }
            Dest::AddressIndirect(r, _, offset) => {
                let dest_ea = self.deref_register(r, offset);
                for i in 0..src_ty.size() {
                    let src_ea_offset = if src_ea == POP {
                        src_ea
                    } else {
                        src_ea.add_offset(i)
                    };
                    self.output
                        .push(IR::Mov(dest_ea.add_offset(i), src_ea_offset));
                }
            }
            Dest::Target(target) => {
                target.ty.check(&src_ty).expect("type");
                // write low to high
                for i in 0..src_ty.size() {
                    self.update_stack_item(target.idx, target.offset + i as usize);
                    let src_ea_offset = if src_ea == POP {
                        src_ea
                    } else {
                        src_ea.add_offset(i)
                    };
                    self.output.push(IR::Mov(
                        self.stack_offset(&target).add_offset(i),
                        src_ea_offset,
                    ));
                }
            }
        }
        if src_ea == POP {
            self.stack.pop();
        }
        self.clobber_cc_zero();
    }

    // pointers
    pub fn push_address(&mut self, target: Target) -> Target {
        let idx = self.push_stack_item(Expr(target.ty.add_ref()));

        let ea = self.stack_offset(&target);
        self.output.push(IR::LoadAddress(PUSH, ea));
        Target {
            ty: target.ty.add_ref(),
            idx,
            offset: 0,
        }
    }
    pub fn load_address(&mut self, r: Address, target: Target) {
        self.take_register(r, target.ty.add_ref());
        let ea = self.stack_offset(&target);
        self.output.push(IR::LoadAddress(r.ea(), ea));
    }

    // arithmetic
    pub fn accumulate(&mut self, op: IROp, dest: Dest, src: Src) {
        let (src_ty, src_ea) = match src {
            Src::Immediate(ty, value) => (ty, EA::Immediate(value)),
            Src::PopData(r) => (self.release_register(r), r.ea()),
            Src::PopAddress(r) => (self.release_register(r), r.ea()),
            Src::AddressIndirect(r, ty, offset) => (ty, self.deref_register(r, offset)),
            Src::Target(target) => {
                let ea = self.stack_offset(&target);
                (target.ty, ea)
            }
            Src::Pop => {
                let ty = self.expect_pop_expr().clone();
                (ty, POP)
            }
        };
        assert_eq!(src_ty.size(), 1);
        let (dest_ty, dest_ea) = match dest {
            Dest::Data(r) => (self.borrow_register(r), r.ea()),
            Dest::Address(r) => (self.borrow_register(r), r.ea()),
            Dest::AddressIndirect(r, ty, offset) => {
                self.borrow_register(r);
                (ty, self.deref_register(r, offset))
            }
            Dest::Target(target) => {
                let ea = self.stack_offset(&target);
                (target.ty, ea)
            }
        };
        dest_ty.check(&src_ty).expect("dest != src");
        self.output.push(op(dest_ea, src_ea));
        if src_ea == POP {
            self.stack.pop();
        }
        self.clobber_cc_zero();
    }

    // compare & branch
    pub fn compare(&mut self, left: Src, right: CmpSrc) {
        if self.cc_zero_locked {
            panic!("cc in use");
        }
        let (left_ty, left_ea) = match left {
            Src::Immediate(ty, value) => (ty, EA::Immediate(value)),
            Src::PopData(r) => (self.release_register(r), r.ea()),
            Src::PopAddress(r) => (self.release_register(r), r.ea()),
            Src::AddressIndirect(r, ty, offset) => (ty, self.deref_register(r, offset)),
            Src::Target(target) => (target.ty.clone(), self.stack_offset(&target)),
            Src::Pop => (self.expect_pop_expr().clone(), POP),
        };
        assert_eq!(left_ty.size(), 1);
        let right_ea = match right {
            CmpSrc::Data(r) => {
                self.release_register(r);
                r.ea()
            }
            CmpSrc::Immediate(_, value) => EA::Immediate(value),
        };
        self.output.push(IR::Cmp(left_ea, right_ea));
        if left_ea == POP {
            self.stack.pop();
        }

        self.cc_zero_locked = true;
    }
    pub fn forward_branch_if(&mut self, cond: IRCond) -> usize {
        if !self.cc_zero_locked {
            panic!("cc zero vacant");
        }
        let idx = self.output.len();
        self.output.push(IR::BranchIf(EA::Immediate(-1), cond));

        self.cc_zero_locked = false;
        idx
    }
    pub fn resolve_forward_branch(&mut self, idx: usize) {
        let here = self.output.len();
        let displacement = here - idx - 1;
        self.output[idx] = match &self.output[idx] {
            IR::BranchIf(EA::Immediate(-1), cond) => {
                IR::BranchIf(EA::Immediate(displacement as Word), *cond)
            }
            _ => unreachable!(),
        };
    }
    pub fn loop_begin(&mut self) -> usize {
        self.output.len()
    }
    pub fn loop_end(&mut self, idx: usize) {
        let here = self.output.len();
        let displacement = (here - idx + 1) as Word;
        self.output
            .push(IR::BranchIf(EA::Immediate(-displacement), IRCond::Always));
    }

    // scope
    pub fn assign_local(&mut self, name: String, maybe_ty: Option<Ty>) -> Compile<()> {
        // TODO: this assumes its the top ofthe stack and there's no junk around it
        let idx = self.stack.len() - 1;
        let top = &mut self.stack[idx];
        top.kind = match &top.kind {
            Expr(ty) => {
                if let Some(let_ty) = maybe_ty {
                    let_ty.check(&ty)?;
                }
                Local(name.clone(), ty.clone())
            }
            _ => unreachable!(),
        };
        self.insert_scope(name, idx);

        Ok(())
    }

    fn init_scope(&mut self) {
        assert!(self.scope.len() == 0);
        self.scope = vec![ScopeFrame {
            start_index: 0,
            locals: HashMap::new(),
            did_return: false,
        }];
    }
    fn enter_block(&mut self) {
        self.scope.push(ScopeFrame {
            start_index: self.stack.len(),
            locals: HashMap::new(),
            did_return: false,
        })
    }
    fn exit_block(&mut self) {
        let frame = self.scope.pop().expect("scope frame");
        let to_remove = self.stack.len() - frame.start_index;
        self.stack.truncate(frame.start_index);

        if !frame.did_return && to_remove > 0 {
            self.output.push(IR::Add(
                EA::Register(Register::SP),
                EA::Immediate(to_remove as Word),
            ));
        }
    }

    fn get_scope(&self, name: &str) -> Option<usize> {
        // get from scope
        for frame in self.scope.iter().rev() {
            if let Some(index) = frame.locals.get(name) {
                return Some(*index);
            }
        }
        return None;
    }
    fn insert_scope(&mut self, name: String, idx: usize) {
        let len = self.scope.len();
        self.scope[len - 1].locals.insert(name, idx);
    }
    // utils
    fn stack_offset(&self, target: &Target) -> EA {
        EA::Offset(Register::SP, (self.stack.len() - target.idx - 1) as Word)
            .add_offset(target.offset as Word)
    }
    fn deref_register(&mut self, r: Address, offset: usize) -> EA {
        let ty = self.release_register(r).deref().expect("pointer");
        // TODO: check that offset field makes sense for type
        assert!(ty.size() >= offset as Word);
        r.ea_offset(offset as Word)
    }
    fn push_stack_item(&mut self, kind: StackItemKind) -> usize {
        self.stack.push(StackItem {
            kind,
            last_updated: self.output.len(),
        });
        self.stack.len() - 1
    }
    fn update_stack_item(&mut self, idx: usize, offset: usize) {
        self.stack[idx - offset].last_updated = self.output.len();
    }
    fn expect_pop_expr(&mut self) -> &Ty {
        let idx = self.stack.len() - 1;
        match &self.stack[idx].kind {
            Expr(ty) => {
                assert_eq!(ty.size(), 1);
                ty
            }
            _ => panic!("expected expr"),
        }
    }
    fn clobber_cc_zero(&mut self) {
        if self.cc_zero_locked {
            panic!("clobbered cc zero");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        runtime::{IRCond::*, Register::SP, EA::*, IR, IR::*},
        ty::TyRecord,
    };

    impl Memory2 {
        pub fn expect_output(&mut self, output: Vec<IR>) {
            assert_eq!(self.output, output);
        }
        pub fn expect_stack(&mut self, stack: Vec<StackItemKind>) {
            assert_eq!(
                self.stack
                    .iter()
                    .map(|s| s.kind.clone())
                    .collect::<Vec<_>>(),
                stack
            );
        }
    }

    #[test]
    fn identifiers() {
        let mut m = Memory2::new();
        m.init_scope();
        m.push(m.immediate(Ty::int(), 123));
        m.assign_local("foo".to_string(), None).unwrap();
        let foo = m.identifier("foo").unwrap();
        m.push(foo);
        m.expect_output(vec![
            //
            Mov(PUSH, Immediate(123)),
            Mov(PUSH, Offset(SP, 1)),
        ]);
        m.expect_stack(vec![
            //
            Local("foo".to_string(), Ty::int()),
            Expr(Ty::int()),
        ]);
    }

    #[test]
    fn add_register() {
        let mut m = Memory2::new();
        m.init_scope();
        m.push(m.immediate(Ty::int(), 123));
        m.assign_local("foo".to_string(), None).unwrap();
        let data = m.get_data_register().unwrap();
        m.set_dest(Dest::Data(data), m.immediate(Ty::int(), 456));
        let foo = m.identifier("foo").unwrap();
        m.push(foo);
        m.accumulate(IR::Add, Dest::Data(data), Src::Pop);
        m.push(Src::PopData(data));

        m.expect_output(vec![
            Mov(PUSH, Immediate(123)),
            Mov(data.ea(), Immediate(456)),
            Mov(PUSH, Offset(SP, 1)),
            Add(data.ea(), POP),
            Mov(PUSH, data.ea()),
        ]);
        m.expect_stack(vec![
            //
            Local("foo".to_string(), Ty::int()),
            Expr(Ty::int()),
        ]);
    }

    #[test]
    fn assign_register() {
        let mut m = Memory2::new();
        m.init_scope();
        m.push(m.immediate(Ty::int(), 123));
        m.assign_local("foo".to_string(), None).unwrap();
        let d = m.get_data_register().unwrap();
        m.set_dest(Dest::Data(d), m.immediate(Ty::int(), 456));
        let foo = m.identifier_target("foo").unwrap();
        m.set_dest(Dest::Target(foo), Src::PopData(d));

        m.expect_output(vec![
            Mov(PUSH, Immediate(123)),
            Mov(d.ea(), Immediate(456)),
            Mov(Offset(SP, 0), d.ea()),
        ]);
        m.expect_stack(vec![
            //
            Local("foo".to_string(), Ty::int()),
        ]);
    }

    #[test]
    fn pair() {
        let mut m = Memory2::new();
        let pair_ty = {
            let mut pair_ty = TyRecord::new(1);
            pair_ty.insert("x".to_string(), Ty::int(), None).unwrap();
            pair_ty.insert("y".to_string(), Ty::int(), None).unwrap();
            Ty::record(pair_ty)
        };

        m.init_scope();
        // let foo := Pair { x: 123, y: 456 };
        let base = m.push_structure(pair_ty.clone());
        m.set_dest(
            Dest::Target(m.target_field(base.clone(), "x").unwrap()),
            m.immediate(Ty::int(), 123),
        );
        m.set_dest(
            Dest::Target(m.target_field(base.clone(), "y").unwrap()),
            m.immediate(Ty::int(), 456),
        );
        m.assign_local("foo".to_string(), None).unwrap();
        // foo;
        let foo = m.identifier("foo").unwrap();
        m.push(foo);

        m.expect_stack(vec![
            Owned,
            Local("foo".to_string(), pair_ty.clone()),
            Owned,
            Expr(pair_ty.clone()),
        ]);
        m.expect_output(vec![
            Sub(Register(SP), Immediate(2)),
            Mov(Offset(SP, 0), Immediate(123)),
            Mov(Offset(SP, 1), Immediate(456)),
            Mov(PUSH, Offset(SP, 2)),
            Mov(PUSH, Offset(SP, 2)),
        ]);
    }

    #[test]
    fn if_stmt() {
        let mut m = Memory2::new();
        m.init_scope();
        // let foo := 123
        m.push(m.immediate(Ty::int(), 123));
        m.assign_local("foo".to_string(), None).unwrap();
        // if foo = 456 then
        let foo = m.identifier("foo").unwrap();
        m.push(foo);
        m.compare(Src::Pop, CmpSrc::Immediate(Ty::int(), 456));
        let b = m.forward_branch_if(IRCond::NotZero);
        m.enter_block();
        // let bar := 789
        m.push(m.immediate(Ty::int(), 789));
        m.assign_local("bar".to_string(), None).unwrap();
        // foo := 42
        let foo = m.identifier_target("foo").unwrap();
        m.set_dest(Dest::Target(foo), m.immediate(Ty::int(), 42));
        // end
        m.exit_block();
        m.resolve_forward_branch(b);

        m.expect_stack(vec![
            //
            Local("foo".to_string(), Ty::int()),
        ]);
        m.expect_output(vec![
            Mov(PUSH, Immediate(123)),
            Mov(PUSH, Offset(SP, 1)),
            Cmp(POP, Immediate(456)),
            BranchIf(Immediate(3), NotZero),
            Mov(PUSH, Immediate(789)),
            Mov(Offset(SP, 1), Immediate(42)),
            Add(Register(SP), Immediate(1)),
        ])
    }

    #[test]
    fn while_stmt() {
        let mut m = Memory2::new();
        m.init_scope();
        // let counter := 0;
        m.push(m.immediate(Ty::int(), 0));
        m.assign_local("counter".to_string(), None).unwrap();
        // while i != 10
        let loop_idx = m.loop_begin();
        let counter = m.identifier("counter").unwrap();
        m.push(counter);
        m.compare(Src::Pop, CmpSrc::Immediate(Ty::int(), 10));
        let out_idx = m.forward_branch_if(Zero);
        m.enter_block();
        // counter := counter + 1;
        let counter = m.identifier_target("counter").unwrap();
        m.accumulate(IR::Add, Dest::Target(counter), m.immediate(Ty::int(), 1));
        // end
        m.exit_block();
        m.loop_end(loop_idx);
        m.resolve_forward_branch(out_idx);

        m.expect_stack(vec![
            //
            Local("counter".to_string(), Ty::int()),
        ]);
        m.expect_output(vec![
            // let counter := 0
            Mov(PUSH, Immediate(0)),
            // while i != 10
            Mov(PUSH, Offset(SP, 1)),
            Cmp(POP, Immediate(10)),
            BranchIf(Immediate(2), Zero),
            // counter := counter + 1;
            Add(Offset(SP, 0), Immediate(1)),
            BranchIf(Immediate(-5), Always),
        ])
    }

    #[test]
    fn pointers() {
        let mut m = Memory2::new();
        let pair_ty = {
            let mut pair_ty = TyRecord::new(1);
            pair_ty.insert("x".to_string(), Ty::int(), None).unwrap();
            pair_ty.insert("y".to_string(), Ty::int(), None).unwrap();
            Ty::record(pair_ty)
        };

        m.init_scope();
        // let foo := Pair { x: 123, y: 456 };
        let base = m.push_structure(pair_ty.clone());
        m.set_dest(
            Dest::Target(m.target_field(base.clone(), "x").unwrap()),
            m.immediate(Ty::int(), 123),
        );
        m.set_dest(
            Dest::Target(m.target_field(base.clone(), "y").unwrap()),
            m.immediate(Ty::int(), 456),
        );
        m.assign_local("foo".to_string(), None).unwrap();
        // let ptr := &foo;
        let ptr = m.push_address(base);
        m.assign_local("ptr".to_string(), None).unwrap();
        // ptr[].y
        let a = m.get_address_register().unwrap();
        m.set_dest(Dest::Address(a), Src::Target(ptr));
        let y = {
            let rec = pair_ty.get_record().unwrap();
            let field = rec.get("y", None).unwrap().clone();
            Src::AddressIndirect(a, field.ty.clone(), field.offset as usize)
        };
        m.push(y);

        m.expect_stack(vec![
            Owned,
            Local("foo".to_string(), pair_ty.clone()),
            Local("ptr".to_string(), pair_ty.add_ref()),
            Expr(Ty::int()),
        ]);
        m.expect_output(vec![
            // let foo := Pair { x: 123, y: 456 };
            Sub(Register(SP), Immediate(2)),
            Mov(Offset(SP, 0), Immediate(123)),
            Mov(Offset(SP, 1), Immediate(456)),
            // let ptr := &foo;
            LoadAddress(PUSH, Offset(SP, 1)),
            // ptr[].y
            Mov(a.ea(), Offset(SP, 0)),
            Mov(PUSH, a.ea_offset(1)),
        ]);
    }

    #[test]
    fn pointer_assign() {
        let mut m = Memory2::new();
        let pair_ty = {
            let mut pair_ty = TyRecord::new(1);
            pair_ty.insert("x".to_string(), Ty::int(), None).unwrap();
            pair_ty.insert("y".to_string(), Ty::int(), None).unwrap();
            Ty::record(pair_ty)
        };

        m.init_scope();
        // let foo := Pair { x: 123, y: 456 };
        let base = m.push_structure(pair_ty.clone());
        m.set_dest(
            Dest::Target(m.target_field(base.clone(), "x").unwrap()),
            m.immediate(Ty::int(), 123),
        );
        m.set_dest(
            Dest::Target(m.target_field(base.clone(), "y").unwrap()),
            m.immediate(Ty::int(), 456),
        );
        m.assign_local("foo".to_string(), None).unwrap();
        // let ptr := &foo;
        let ptr = m.push_address(base);
        m.assign_local("ptr".to_string(), None).unwrap();
        // ptr[].y := 789
        let a = m.get_address_register().unwrap();
        m.set_dest(Dest::Address(a), Src::Target(ptr));
        let y_offset = {
            let rec = pair_ty.get_record().unwrap();
            let field = rec.get("y", None).unwrap().clone();
            field.offset as usize
        };
        m.set_dest(
            Dest::AddressIndirect(a, Ty::int(), y_offset),
            m.immediate(Ty::int(), 789),
        );

        m.expect_stack(vec![
            Owned,
            Local("foo".to_string(), pair_ty.clone()),
            Local("ptr".to_string(), pair_ty.add_ref()),
        ]);
        m.expect_output(vec![
            // let foo := Pair { x: 123, y: 456 };
            Sub(Register(SP), Immediate(2)),
            Mov(Offset(SP, 0), Immediate(123)),
            Mov(Offset(SP, 1), Immediate(456)),
            // let ptr := &foo;
            LoadAddress(PUSH, Offset(SP, 1)),
            // ptr[].y := 789
            Mov(a.ea(), Offset(SP, 0)),
            Mov(a.ea_offset(1), Immediate(789)),
        ]);
    }

    #[test]
    fn sub_calls() {
        let mut m = Memory2::new();

        // sub add(a: Int, b: Int) -> Int
        m.begin_sub(
            "add".to_string(),
            vec![("a".to_string(), Ty::int()), ("b".to_string(), Ty::int())],
            Ty::int(),
        );
        let r = m.get_data_register().unwrap();
        m.set_dest(Dest::Data(r), m.identifier("a").unwrap());
        m.accumulate(IR::Add, Dest::Data(r), m.identifier("b").unwrap());

        m.expect_stack(vec![
            StackItemKind::Return(Ty::int()),
            Arg("a".to_string(), Ty::int()),
            Arg("b".to_string(), Ty::int()),
            ReturnAddr,
        ]);

        m.return_sub(Some(Src::PopData(r))).unwrap();
        m.end_sub().unwrap();

        // sub main()
        m.begin_sub("main".to_string(), vec![], Ty::void());
        let add = m.get_sub("add").unwrap();
        m.begin_call(&add);
        m.push(Src::Immediate(Ty::int(), 123));
        m.push(Src::Immediate(Ty::int(), 456));
        m.end_call(&add);

        m.expect_stack(vec![
            //
            ReturnAddr,
            Expr(Ty::int()),
        ]);

        m.end_sub().unwrap();

        m.expect_output(vec![
            // sub add(a: Int, b: Int) -> Int
            // return a + b
            Mov(r.ea(), Offset(SP, 2)),
            Add(r.ea(), Offset(SP, 1)),
            Mov(Offset(SP, 3), r.ea()),
            IR::Return,
            // sub main()
            // add(123, 456)
            Sub(Register(SP), Immediate(1)),
            Mov(PUSH, Immediate(123)),
            Mov(PUSH, Immediate(456)),
            Call(add.index as Word),
            Add(Register(SP), Immediate(2)),
            // cleanup
            Add(Register(SP), Immediate(1)),
            IR::Return,
        ])
    }
}
