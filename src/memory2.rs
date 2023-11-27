#![allow(dead_code)]

use crate::runtime::{IRCond, IROp, Register, Word, EA, IR};
use crate::ty::Ty;
use crate::{Compile, CompileError::*};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Memory2 {
    stack: Vec<StackItem>,
    scope: Vec<ScopeFrame>,
    module_scope: ModuleScope,
    output: Vec<IR>,
    // TODO: more registers, status codes
    r0_locked: Option<Ty>,
    cc_zero_locked: bool,
}

impl Memory2 {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            scope: Vec::new(),
            module_scope: ModuleScope::new(),
            output: Vec::new(),
            r0_locked: None,
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

const PUSH: EA = EA::PreDec(Register::SP);
const POP: EA = EA::PostInc(Register::SP);
const R0: EA = EA::Register(Register::R0);

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
    // TODO: should using a register as a src always release it?
    Register,
    RegisterIndirect(Ty, usize),
    Pop,
}

// dest for "accumulator" operations, _not_ push
#[derive(Debug)]
enum Dest {
    Register,
    Target(Target),
}

#[derive(Debug)]
enum CmpSrc {
    Immediate(Ty, Word),
    Register,
}

impl Memory2 {
    // src constructors
    fn immediate(&self, ty: Ty, value: Word) -> Src {
        assert_eq!(ty.size(), 1);
        Src::Immediate(ty, value)
    }
    fn register(&self) -> Src {
        Src::Register
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

    // mov & allocate
    pub fn push(&mut self, src: Src) -> Target {
        let (ty, ea) = match src {
            Src::Immediate(ty, value) => (ty, EA::Immediate(value)),
            Src::Target(target) => {
                let ea = self.stack_offset(&target).add_offset(target.ty.size()); // account for pre-decrement
                (target.ty, ea)
            }
            Src::Register => (self.release_r0(), EA::Register(Register::R0)),
            Src::RegisterIndirect(ty, offset) => {
                self.borrow_r0();
                (ty, self.deref_register(offset))
            }
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
            Src::Register => (self.release_r0(), EA::Register(Register::R0)),
            Src::RegisterIndirect(ty, offset) => (ty, self.deref_register(offset)),
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
            Dest::Register => {
                // allow Mov(R0, (R0))
                match src_ea {
                    EA::Offset(Register::R0, _) => {
                        self.replace_r0(src_ty);
                    }
                    _ => {
                        self.take_r0(src_ty);
                    }
                };
                self.output.push(IR::Mov(R0, src_ea));
                if src_ea == POP {
                    self.stack.pop();
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
                    if src_ea == POP {
                        self.stack.pop();
                    }
                }
            }
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
    pub fn load_address(&mut self, target: Target) {
        self.take_r0(target.ty.add_ref());
        let ea = self.stack_offset(&target);
        self.output.push(IR::LoadAddress(R0, ea));
    }

    // arithmetic
    pub fn accumulate(&mut self, op: IROp, dest: Dest, src: Src) {
        let (src_ty, src_ea) = match src {
            Src::Immediate(ty, value) => (ty, EA::Immediate(value)),
            Src::Register => (self.release_r0(), R0),
            Src::RegisterIndirect(ty, offset) => (ty, self.deref_register(offset)),
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
            Dest::Register => (self.release_r0(), R0),
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
        if dest_ea == R0 {
            self.r0_locked = Some(dest_ty);
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
            Src::Register => (self.release_r0(), R0),
            Src::RegisterIndirect(ty, offset) => (ty, self.deref_register(offset)),
            Src::Target(target) => (target.ty.clone(), self.stack_offset(&target)),
            Src::Pop => (self.expect_pop_expr().clone(), POP),
        };
        assert_eq!(left_ty.size(), 1);
        let right_ea = match right {
            CmpSrc::Register => {
                self.release_r0();
                R0
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
        }];
    }
    fn enter_scope(&mut self) {
        self.scope.push(ScopeFrame {
            start_index: self.stack.len(),
            locals: HashMap::new(),
        })
    }
    fn exit_scope(&mut self) {
        let frame = self.scope.pop().expect("scope frame");
        let to_remove = self.stack.len() - frame.start_index;
        self.stack.truncate(frame.start_index);
        if to_remove > 0 {
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
    fn deref_register(&mut self, offset: usize) -> EA {
        let ty = self.borrow_r0().deref().expect("pointer");
        // TODO: check that offset field makes sense for type
        assert!(ty.size() >= offset as Word);
        EA::Offset(Register::R0, offset as Word)
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
    fn take_r0(&mut self, ty: Ty) {
        if self.r0_locked.is_some() {
            panic!("r0 in use");
        }
        self.r0_locked = Some(ty);
    }
    fn borrow_r0(&mut self) -> Ty {
        match &self.r0_locked {
            Some(ty) => ty.clone(),
            None => panic!("r0 is vacant"),
        }
    }
    fn replace_r0(&mut self, ty: Ty) {
        if self.r0_locked.is_none() {
            panic!("r0 is vacant");
        }
        self.r0_locked = Some(ty);
    }
    fn release_r0(&mut self) -> Ty {
        let ty = match &self.r0_locked {
            Some(ty) => ty.clone(),
            None => panic!("r0 is vacant"),
        };
        self.r0_locked = None;
        ty
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        runtime::{IRCond::*, Register::R0, Register::SP, EA::*, IR::*},
        ty::TyRecord,
    };

    impl Memory2 {
        fn expect_output(&mut self, output: Vec<IR>) {
            assert_eq!(self.output, output);
        }
        fn expect_stack(&mut self, stack: Vec<StackItemKind>) {
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
        m.set_dest(Dest::Register, m.immediate(Ty::int(), 456));
        let foo = m.identifier("foo").unwrap();
        m.push(foo);
        m.accumulate(IR::Add, Dest::Register, Src::Pop);
        m.push(Src::Register);

        m.expect_output(vec![
            Mov(PUSH, Immediate(123)),
            Mov(Register(R0), Immediate(456)),
            Mov(PUSH, Offset(SP, 1)),
            Add(Register(R0), POP),
            Mov(PUSH, Register(R0)),
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
        m.set_dest(Dest::Register, m.immediate(Ty::int(), 456));
        let foo = m.identifier_target("foo").unwrap();
        m.set_dest(Dest::Target(foo), m.register());

        m.expect_output(vec![
            Mov(PUSH, Immediate(123)),
            Mov(Register(R0), Immediate(456)),
            Mov(Offset(SP, 0), Register(R0)),
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
        m.enter_scope();
        // let bar := 789
        m.push(m.immediate(Ty::int(), 789));
        m.assign_local("bar".to_string(), None).unwrap();
        // foo := 42
        let foo = m.identifier_target("foo").unwrap();
        m.set_dest(Dest::Target(foo), m.immediate(Ty::int(), 42));
        // end
        m.exit_scope();
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
        m.enter_scope();
        // counter := counter + 1;
        let counter = m.identifier_target("counter").unwrap();
        m.accumulate(IR::Add, Dest::Target(counter), m.immediate(Ty::int(), 1));
        // end
        m.exit_scope();
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
        m.set_dest(Dest::Register, Src::Target(ptr));
        let y = {
            let rec = pair_ty.get_record().unwrap();
            let field = rec.get("y", None).unwrap().clone();
            Src::RegisterIndirect(field.ty.clone(), field.offset as usize)
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
            Mov(Register(R0), Offset(SP, 0)),
            Mov(PUSH, Offset(R0, 1)),
        ]);
    }
}
