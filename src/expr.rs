use std::collections::HashMap;

use crate::memory::*;
use crate::op::Op;
use crate::record::*;
use crate::runtime::*;
use crate::ty::*;

use crate::{Compile, CompileError::*};

#[derive(Debug)]
pub struct Expr {
    pub ty: Ty,
    kind: ExprKind,
    ctx: ExprTarget,
}

#[derive(Debug)]
#[allow(dead_code)]
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
}

impl Expr {
    pub fn resolved(ty: Ty, block: Block) -> Self {
        Self {
            ty,
            kind: ExprKind::Resolved(block),
            ctx: ExprTarget::Block(block),
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
    pub fn add_ref(self, memory: &mut Memory) -> Compile<Expr> {
        match self.kind {
            ExprKind::Reference { next, .. } => {
                let out = memory.write(IR::LoadAddress, self.ctx.to_dest(1), Src::Block(next));
                Ok(Self::resolved(self.ty.add_ref(), out))
            }
            _ => Err(InvalidRef),
        }
    }
    pub fn deref(self, memory: &mut Memory) -> Compile<Expr> {
        let deref_ty = self.ty.deref()?;
        match self.kind {
            ExprKind::Resolved(block) => {
                let size = deref_ty.size();
                let out = memory.deref(block, self.ctx.to_dest(size), Slice::with_size(size));
                Ok(Self::resolved(deref_ty, out))
            }
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => Ok(Self {
                ty: deref_ty,
                ctx: self.ctx,
                kind: ExprKind::Reference {
                    deref_level: deref_level + 1,
                    next,
                    focus,
                },
            }),
            _ => unreachable!(),
        }
    }
    pub fn cast_ty(self, ty: Ty) -> Compile<Self> {
        Ok(Self {
            ty: self.ty.cast(ty)?,
            kind: self.kind,
            ctx: self.ctx,
        })
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
                ctx: self.ctx.shrink_to(field.ty.size()),
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
                ctx: self.ctx.shrink_to(field.ty.size()),
            }),
        }
    }
    pub fn op(self, memory: &mut Memory, op: Op, other: Expr, out_ty: Ty) -> Expr {
        match (&self.get_constant(), &other.get_constant()) {
            (Some(l), Some(r)) => {
                let value = op.inline(*l, *r);
                self.ctx.constant(out_ty, value)
            }
            _ => {
                let left = self.resolve(memory);
                let dest = Dest::Block(left.block);
                let block = match other.kind {
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
                };
                Self {
                    ty: out_ty,
                    ctx: ExprTarget::Block(block),
                    kind: ExprKind::Resolved(block),
                }
            }
        }
    }
    pub fn bitset_field(self, field_name: &str, memory: &mut Memory) -> Compile<Expr> {
        let field = self.ty.oneof_member(field_name)?.clone();
        let dest = Dest::Block(self.resolve(memory).block);
        let out = memory.write(IR::BitTest, dest, Src::Immediate(field.index));
        let ty = Ty::bool();
        memory.write(IR::Mov, Dest::Block(out), Src::R0);
        Ok(Expr::resolved(ty, out))
    }
    pub fn begin_match(self, memory: &mut Memory) -> Compile<MatchBuilder> {
        let res = self.resolve(memory);
        let record = res.ty.get_record()?;
        let case_field = record.case_field.as_ref().ok_or(Expected("case field"))?;
        let case_value = res.block.record_field(&case_field);
        let jump_table = memory.begin_match(case_value, record.cases.len());
        Ok(MatchBuilder::new(record, res.block, jump_table))
    }

    pub fn resolve(self, memory: &mut Memory) -> ResolvedExpr {
        match self.kind {
            ExprKind::Resolved(block) => ResolvedExpr { ty: self.ty, block },
            ExprKind::Constant(value) => {
                let block = memory.write(IR::Mov, self.ctx.to_dest(1), Src::Immediate(value));
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                if deref_level == 0 {
                    let src = next.focus(focus);
                    let block =
                        memory.write(IR::Mov, self.ctx.to_dest(src.size()), Src::Block(src));
                    ResolvedExpr { ty: self.ty, block }
                } else if deref_level == 1 {
                    let block = memory.deref(next, self.ctx.to_dest(self.ty.size()), focus);
                    ResolvedExpr { ty: self.ty, block }
                } else {
                    unimplemented!()
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct ResolvedExpr {
    pub ty: Ty,
    pub block: Block,
}

impl ResolvedExpr {
    pub fn to_expr(self) -> Expr {
        Expr::resolved(self.ty, self.block)
    }
}

pub struct Scope {
    locals: HashMap<String, ScopeRecord>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }
    pub fn identifier(&self, key: &str, ctx: ExprTarget) -> Compile<Expr> {
        let record = self
            .locals
            .get(key)
            .ok_or_else(|| UnknownIdentifier(key.to_string()))?;
        let block = Block::local(record.frame_offset, record.ty.size());
        Ok(ctx.lvalue(record.ty.clone(), block))
    }
    pub fn store_local(&mut self, key: String, ty: Ty, frame_offset: Word) {
        self.locals.insert(key, ScopeRecord { frame_offset, ty });
    }
    pub fn add_case_binding(&mut self, name: String, ty: Ty, block: Block) {
        self.locals.insert(
            name,
            ScopeRecord {
                frame_offset: block.frame_offset().expect("frame offset"),
                ty,
            },
        );
    }
}

#[derive(Debug)]
struct ScopeRecord {
    frame_offset: Word,
    ty: Ty,
}

#[derive(Debug)]
pub enum ExprTarget {
    Stack,
    Block(Block),
    RefBlock(Block),
}

impl ExprTarget {
    pub fn constant(self, ty: Ty, value: Word) -> Expr {
        Expr {
            ty,
            ctx: self,
            kind: ExprKind::Constant(value),
        }
    }
    pub fn lvalue(self, ty: Ty, block: Block) -> Expr {
        Expr {
            ty,
            ctx: self,
            kind: ExprKind::Reference {
                deref_level: 0,
                focus: Slice::with_size(block.size()),
                next: block,
            },
        }
    }
    pub fn to_dest(self, size: Word) -> Dest {
        match self {
            Self::Stack => Dest::Stack(size),
            Self::Block(block) => Dest::Block(block),
            Self::RefBlock(block) => Dest::RefBlock(block, size),
        }
    }
    pub fn allocate(self, size: Word, mem: &mut Memory) -> Block {
        match self {
            Self::Stack => mem.allocate(size),
            Self::Block(block) => {
                if block.size() == size {
                    block
                } else {
                    unimplemented!("allocate in wrong size block")
                }
            }
            Self::RefBlock(_) => unimplemented!(),
        }
    }
    fn shrink_to(self, size: Word) -> Self {
        match self {
            Self::Block(block) => Self::Block(block.shrink_to(size)),
            Self::Stack => Self::Stack,
            _ => unimplemented!(),
        }
    }
}
