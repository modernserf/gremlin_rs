use crate::memory::*;
use crate::op::Op;
use crate::runtime::*;
use crate::ty::*;

use crate::{Compile, CompileError::*};

#[derive(Debug)]
pub struct Expr {
    pub ty: Ty,
    kind: ExprKind,
    // ctx: ExprTarget,
}

#[derive(Debug)]
enum ExprKind {
    // a value in memory
    Resolved(Block),
    // a value known at compile time that has not yet been written to memory
    Constant(Word),
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
    pub fn get_constant(&self) -> Option<Word> {
        match &self.kind {
            ExprKind::Constant(value) => Some(*value),
            _ => None,
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
                // ctx: self.ctx.shrink_to(field.ty.size()),
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
                // ctx: self.ctx.shrink_to(field.ty.size()),
            }),
            ExprKind::Sub(_) => unreachable!(),
        }
    }
    //
    pub fn add_ref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        match self.kind {
            ExprKind::Reference { next, .. } => {
                let out = target.write_ref(memory, next);
                Ok(Self::resolved(self.ty.add_ref(), out))
            }
            _ => Err(InvalidRef),
        }
    }
    pub fn deref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        let deref_ty = self.ty.deref()?;
        match self.kind {
            ExprKind::Resolved(block) => {
                let out =
                    target.write_deref(memory, block, &deref_ty, Slice::with_size(deref_ty.size()));
                Ok(Self::resolved(deref_ty, out))
            }
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => Ok(Self {
                ty: deref_ty,
                // ctx: self.ctx,
                kind: ExprKind::Reference {
                    deref_level: deref_level + 1,
                    next,
                    focus,
                },
            }),
            _ => unreachable!(),
        }
    }
    pub fn op_rhs(self, memory: &mut Memory, op: Op, dest_block: Block) {
        let dest = Dest::Block(dest_block);
        match self.kind {
            ExprKind::Resolved(block) => memory.write(op.ir(), dest, Src::Block(block)),
            ExprKind::Constant(value) => memory.write(op.ir(), dest, Src::Immediate(value)),
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                // TODO
                assert!(deref_level == 0);
                memory.write(op.ir(), dest, Src::Block(next.focus(focus)))
            }
            ExprKind::Sub(_) => unreachable!(),
        };
    }
    pub fn bitset_field(
        self,
        field_name: &str,
        memory: &mut Memory,
        target: ExprTarget,
    ) -> Compile<Expr> {
        let field = self.ty.oneof_member(field_name)?.clone();
        let block = self.resolve(memory, target).block;
        memory.bit_test(block, Src::Immediate(field.index));
        memory.set_if(Dest::Block(block), IRCond::NotZero);
        Ok(Expr::resolved(Ty::bool(), block))
    }
    pub fn resolve(self, memory: &mut Memory, target: ExprTarget) -> ResolvedExpr {
        match self.kind {
            ExprKind::Resolved(block) => {
                let block = target.maybe_move_block(memory, block);
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Constant(value) => {
                let block = target.write_const(memory, value);
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                if deref_level == 0 {
                    let block = target.write_block(memory, next.focus(focus));
                    ResolvedExpr { ty: self.ty, block }
                } else if deref_level == 1 {
                    let block = target.write_deref(memory, next, &self.ty, focus);
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
    fn write_op(self, memory: &mut Memory, op: IROp, src: Src, size: Word) -> Block {
        match self {
            Self::Stack => memory.write(op, Dest::Stack(size), src),
            Self::Block(block) => memory.write(op, Dest::Block(block), src),
            Self::RefBlock(ptr_block) => {
                let (register, dest) = memory.deref_to_dest(ptr_block, size);
                let block = memory.write(op, dest, src);
                memory.free_register(register);
                block
            }
        }
    }
    fn write_const(self, memory: &mut Memory, value: Word) -> Block {
        self.write_op(memory, IR::Mov, Src::Immediate(value), 1)
    }
    fn write_block(self, memory: &mut Memory, block: Block) -> Block {
        self.write_op(memory, IR::Mov, Src::Block(block), block.size())
    }
    fn write_ref(self, memory: &mut Memory, block: Block) -> Block {
        let src = Src::Block(block);
        self.write_op(memory, IR::LoadAddress, src, 1)
    }
    fn write_deref(
        self,
        memory: &mut Memory,
        ptr_block: Block,
        deref_ty: &Ty,
        focus: Slice,
    ) -> Block {
        let size = deref_ty.size();
        let (register, src) = memory.deref_to_src(ptr_block, focus);
        let block = self.write_op(memory, IR::Mov, src, size);
        memory.free_register(register);
        block
    }
    fn maybe_move_block(self, memory: &mut Memory, block: Block) -> Block {
        match self {
            Self::Stack => block,
            _ => self.write_block(memory, block),
        }
    }
}
