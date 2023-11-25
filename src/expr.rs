use crate::block::*;
use crate::memory::*;
use crate::op::Op;
use crate::runtime::*;
use crate::ty::*;
use crate::{Compile, CompileError::*};

#[derive(Debug)]
pub struct Expr {
    pub ty: Ty,
    kind: ExprKind,
}

#[derive(Debug)]
pub struct Reference {
    // 0 = an identifier, 1 = a pointer
    pub deref_level: usize,
    // ref_level 0 = the location of the referent, 1 = the location of the pointer to the referent, etc
    pub next: Block,
    // segment of referent that we want (e.g. a particular struct field within a struct)
    pub focus: Slice,
}

impl Reference {
    // TODO: can a src/dest "own" a register
    fn to_dest(self, memory: &mut Memory) -> (Dest, Option<Register>) {
        match self.deref_level {
            0 => (Dest::Block(self.next.focus(self.focus)), None),
            1 => {
                // TODO: how should multiple iterations of deref work?
                let (register, dest) = memory.deref_to_dest(self.next, self.focus);
                (dest, Some(register))
            }
            _ => unimplemented!(),
        }
    }
    fn to_src(self, memory: &mut Memory) -> (Src, Option<Register>) {
        match self.deref_level {
            0 => (Src::Block(self.next.focus(self.focus)), None),
            1 => {
                let (register, src) = memory.deref_to_src(self.next, self.focus);
                (src, Some(register))
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
enum ExprKind {
    // a value in memory, not assigned to a variable
    Block(Block),
    // a value known at compile time that has not yet been written to memory
    Constant(Word),
    // the result of a comparison that set the status register
    Cond(IRCond),
    // a value accessible through a variable (ie an "lvalue")
    Reference(Reference),
    // a subroutine
    Sub(SubIndex),
}

impl Expr {
    pub fn resolved(ty: Ty, block: Block) -> Self {
        Self {
            ty,
            kind: ExprKind::Block(block),
        }
    }
    pub fn constant(ty: Ty, value: Word) -> Self {
        Self {
            ty,
            kind: ExprKind::Constant(value),
        }
    }
    pub fn cond(cond: IRCond) -> Self {
        Self {
            ty: Ty::bool(),
            kind: ExprKind::Cond(cond),
        }
    }
    pub fn lvalue(ty: Ty, block: Block) -> Self {
        Self {
            ty,
            kind: ExprKind::Reference(Reference {
                deref_level: 0,
                focus: Slice::with_size(block.size()),
                next: block,
            }),
        }
    }
    pub fn sub(ty: Ty, sub_index: SubIndex) -> Self {
        Self {
            ty,
            kind: ExprKind::Sub(sub_index),
        }
    }
    pub fn assign_ctx(self) -> Compile<ExprTarget> {
        match self.kind {
            ExprKind::Reference(r) => Ok(ExprTarget::Reference(r)),
            _ => Err(Expected("lvalue")),
        }
    }
    pub fn cast_ty(self, ty: Ty) -> Compile<Self> {
        Ok(Self {
            ty: self.ty.cast(ty)?,
            kind: self.kind,
        })
    }
    pub fn sub_index(&self) -> Compile<SubIndex> {
        match &self.kind {
            ExprKind::Sub(sub_index) => Ok(*sub_index),
            _ => Err(Expected("subroutine")),
        }
    }
    pub fn record_field(self, field_name: &str) -> Compile<Self> {
        let record = self.ty.get_record()?;
        let field = record.get(field_name, None)?;
        match self.kind {
            ExprKind::Constant(_) => unimplemented!(),
            ExprKind::Block(block) => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference(Reference {
                    deref_level: 0,
                    next: block,
                    focus: field.to_slice(),
                }),
            }),
            ExprKind::Reference(r) => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference(Reference {
                    deref_level: r.deref_level,
                    next: r.next,
                    focus: field.to_slice(),
                }),
            }),
            _ => unimplemented!(),
        }
    }
    //
    pub fn add_ref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        match self.kind {
            ExprKind::Reference(r) => {
                let out = target.load_address(memory, r.next);
                Ok(Self::resolved(self.ty.add_ref(), out))
            }
            _ => Err(InvalidRef),
        }
    }
    pub fn deref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        let deref_ty = self.ty.deref()?;
        match self.kind {
            ExprKind::Block(block) => {
                let (register, src) = memory.deref_to_src(block, Slice::with_size(deref_ty.size()));
                let out = target.mov(memory, src);
                memory.free_register(register);
                Ok(Self::resolved(deref_ty, out))
            }
            ExprKind::Reference(Reference {
                deref_level,
                next,
                focus,
            }) => Ok(Self {
                ty: deref_ty,
                kind: ExprKind::Reference(Reference {
                    deref_level: deref_level + 1,
                    next,
                    focus,
                }),
            }),
            _ => unreachable!(),
        }
    }
    pub fn op_rhs(self, memory: &mut Memory, ty: Ty, op: Op, left: Expr) -> Expr {
        // TODO: in (a < b) | (c < d), how do we ensure that we've saved a < b to stack
        // by the time we execute c < d?

        match (&left.kind, self.kind) {
            (ExprKind::Constant(l), ExprKind::Constant(r)) => {
                let value = op.inline(*l, r);
                Expr::constant(ty, value)
            }
            (_, ExprKind::Constant(r)) => {
                let left = Src::Block(left.to_stack(memory).block);
                op.apply(memory, ty, left, Src::Immediate(r))
            }
            (_, ExprKind::Block(r)) => {
                // TODO: try popping left
                let left = Src::Block(left.to_stack(memory).block);
                op.apply(memory, ty, left, Src::Block(r))
            }
            (_, ExprKind::Cond(r)) => {
                let left = Src::Block(left.to_stack(memory).block);
                let right = Src::Block(memory.set_if(Dest::Stack, r));
                op.apply(memory, ty, left, right)
            }
            (_, ExprKind::Reference(r)) => {
                let left = Src::Block(left.to_stack(memory).block);
                // TODO
                let (src, register) = r.to_src(memory);
                let out = op.apply(memory, ty, left, src);
                if let Some(register) = register {
                    memory.free_register(register);
                }
                out
            }
            _ => unreachable!(),
        }
    }

    pub fn resolve_branch_cond(self, memory: &mut Memory) -> IRCond {
        Ty::bool().check(&self.ty).expect("bool");
        match self.kind {
            ExprKind::Constant(value) => {
                if value == 1 {
                    IRCond::Never // ie `if true then ... end` never branches at the `then`
                } else {
                    IRCond::Always
                }
            }
            ExprKind::Cond(cond) => cond,
            _ => {
                let res = self.to_stack(memory);
                memory.cmp_bool(res.block)
            }
        }
    }

    // expr resolved to stack has an addressable value, can be accumulated upon
    pub fn to_stack(self, memory: &mut Memory) -> ResolvedExpr {
        self.resolve_inner(memory, ExprTarget::Stack)
    }

    pub fn resolve(self, memory: &mut Memory, target: ExprTarget) {
        self.resolve_inner(memory, target);
    }
    fn resolve_inner(self, memory: &mut Memory, target: ExprTarget) -> ResolvedExpr {
        match self.kind {
            ExprKind::Block(block) => {
                let block = target.maybe_move_block(memory, block);
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Constant(value) => {
                let block = target.mov(memory, Src::Immediate(value));
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Cond(cond) => {
                let block = target.set_if(memory, cond);
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Reference(r) => {
                let (src, register) = r.to_src(memory);
                let block = target.mov(memory, src);
                if let Some(register) = register {
                    memory.free_register(register);
                }
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Sub(_) => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedExpr {
    pub ty: Ty,
    pub block: Block,
}

impl ResolvedExpr {
    pub fn void() -> Self {
        ResolvedExpr {
            ty: Ty::void(),
            block: Block::Frame(Slice::with_size(0)),
        }
    }
    pub fn to_expr(self) -> Expr {
        Expr::resolved(self.ty, self.block)
    }
}

#[derive(Debug)]
pub enum ExprTarget {
    Stack,
    Reference(Reference),
}

impl ExprTarget {
    fn to_dest(self, memory: &mut Memory) -> (Dest, Option<Register>) {
        match self {
            Self::Stack => (Dest::Stack, None),
            Self::Reference(r) => r.to_dest(memory),
        }
    }
    fn mov(self, memory: &mut Memory, src: Src) -> Block {
        let (dest, register) = self.to_dest(memory);
        let block = memory.mov(dest, src);
        if let Some(r) = register {
            memory.free_register(r);
        }
        block
    }
    fn set_if(self, memory: &mut Memory, cond: IRCond) -> Block {
        let (dest, register) = self.to_dest(memory);
        let block = memory.set_if(dest, cond);
        if let Some(r) = register {
            memory.free_register(r);
        }
        block
    }
    fn load_address(self, memory: &mut Memory, block: Block) -> Block {
        let src = Src::Block(block);
        let (dest, register) = self.to_dest(memory);
        let block = memory.load_address(dest, src);
        if let Some(r) = register {
            memory.free_register(r);
        }
        block
    }
    fn maybe_move_block(self, memory: &mut Memory, block: Block) -> Block {
        match self {
            Self::Stack => block,
            _ => self.mov(memory, Src::Block(block)),
        }
    }
}
