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
enum ExprKind {
    // a value in memory
    Resolved(Block),
    // a value known at compile time that has not yet been written to memory
    Constant(Word),
    // the result of a comparison that set the status register
    Cond(IRCond),

    // a reference to a value in memory
    Reference {
        // 0 = an identifier, 1 = a pointer
        deref_level: usize,
        // ref_level 0 = the location of the referent, 1 = the location of the pointer to the referent, etc
        next: Block,
        // segment of referent that we want (e.g. a particular struct field within a struct)
        focus: Slice,
    },
    // a subroutine
    Sub(SubIndex),
}

impl Expr {
    pub fn resolved(ty: Ty, block: Block) -> Self {
        Self {
            ty,
            kind: ExprKind::Resolved(block),
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
            kind: ExprKind::Reference {
                deref_level: 0,
                focus: Slice::with_size(block.size()),
                next: block,
            },
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
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                if deref_level > 0 {
                    Ok(ExprTarget::RefBlock(next))
                } else {
                    Ok(ExprTarget::Block(next.focus(focus)))
                }
            }
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
            ExprKind::Resolved(block) => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference {
                    deref_level: 0,
                    next: block,
                    focus: Slice::from_record_field(field),
                },
            }),
            ExprKind::Reference {
                deref_level, next, ..
            } => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference {
                    deref_level,
                    next,
                    focus: Slice::from_record_field(field),
                },
            }),
            _ => unimplemented!(),
        }
    }
    //
    pub fn add_ref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        match self.kind {
            ExprKind::Reference { next, .. } => {
                let out = target.load_address(memory, next);
                Ok(Self::resolved(self.ty.add_ref(), out))
            }
            _ => Err(InvalidRef),
        }
    }
    pub fn deref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        let deref_ty = self.ty.deref()?;
        match self.kind {
            ExprKind::Resolved(block) => {
                let out = target.mov_deref(memory, block, Slice::with_size(deref_ty.size()));
                Ok(Self::resolved(deref_ty, out))
            }
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => Ok(Self {
                ty: deref_ty,
                kind: ExprKind::Reference {
                    deref_level: deref_level + 1,
                    next,
                    focus,
                },
            }),
            _ => unreachable!(),
        }
    }
    pub fn op_rhs(self, memory: &mut Memory, ty: Ty, op: Op, left: Expr) -> Expr {
        match (&left.kind, &self.kind) {
            (ExprKind::Constant(l), ExprKind::Constant(r)) => {
                let value = op.inline(*l, *r);
                return Expr::constant(ty, value);
            }
            _ => {}
        };

        // TODO: left doesnt need to be on stack, just needs to be addressable
        let left = left.resolve(memory, ExprTarget::Stack);
        // fixme
        let dest = Src::Block(left.block);
        match self.kind {
            ExprKind::Resolved(block) => op.apply(memory, ty, dest, Src::Block(block)),
            ExprKind::Constant(value) => op.apply(memory, ty, dest, Src::Immediate(value)),
            ExprKind::Cond(cond) => {
                let block = memory.set_if(Dest::Stack, cond);
                op.apply(memory, ty, dest, Src::Block(block))
            }
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                // TODO
                assert!(deref_level == 0);
                op.apply(memory, ty, dest, Src::Block(next.focus(focus)))
            }
            ExprKind::Sub(_) => unreachable!(),
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
                let res = self.resolve(memory, ExprTarget::Stack);
                memory.cmp_bool(res.block)
            }
        }
    }

    pub fn resolve(self, memory: &mut Memory, target: ExprTarget) -> ResolvedExpr {
        match self.kind {
            ExprKind::Resolved(block) => {
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
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                if deref_level == 0 {
                    let block = target.mov(memory, Src::Block(next.focus(focus)));
                    ResolvedExpr { ty: self.ty, block }
                } else if deref_level == 1 {
                    let block = target.mov_deref(memory, next, focus);
                    ResolvedExpr { ty: self.ty, block }
                } else {
                    unimplemented!()
                }
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
    Block(Block),
    RefBlock(Block),
}

impl ExprTarget {
    fn mov(self, memory: &mut Memory, src: Src) -> Block {
        match self {
            Self::Stack => memory.mov(Dest::Stack, src),
            Self::Block(block) => memory.mov(Dest::Block(block), src),
            Self::RefBlock(ptr_block) => {
                let (register, dest) = memory.deref_to_dest(ptr_block, src.size());
                let block = memory.mov(dest, src);
                memory.free_register(register);
                block
            }
        }
    }
    fn set_if(self, memory: &mut Memory, cond: IRCond) -> Block {
        match self {
            Self::Stack => memory.set_if(Dest::Stack, cond),
            Self::Block(block) => memory.set_if(Dest::Block(block), cond),
            Self::RefBlock(ptr_block) => {
                let (register, dest) = memory.deref_to_dest(ptr_block, 1);
                let block = memory.set_if(dest, cond);
                memory.free_register(register);
                block
            }
        }
    }
    fn load_address(self, memory: &mut Memory, block: Block) -> Block {
        let src = Src::Block(block);
        match self {
            Self::Stack => memory.load_address(Dest::Stack, src),
            Self::Block(block) => memory.load_address(Dest::Block(block), src),
            Self::RefBlock(ptr_block) => {
                let (register, dest) = memory.deref_to_dest(ptr_block, 1);
                let block = memory.load_address(dest, src);
                memory.free_register(register);
                block
            }
        }
    }
    fn mov_deref(self, memory: &mut Memory, ptr_block: Block, focus: Slice) -> Block {
        let (register, src) = memory.deref_to_src(ptr_block, focus);
        let block = self.mov(memory, src);
        memory.free_register(register);
        block
    }
    fn maybe_move_block(self, memory: &mut Memory, block: Block) -> Block {
        match self {
            Self::Stack => block,
            _ => self.mov(memory, Src::Block(block)),
        }
    }
}
