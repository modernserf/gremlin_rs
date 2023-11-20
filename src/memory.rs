use std::collections::HashMap;

use crate::op::Op;
use crate::runtime::*;
use crate::ty::*;
use crate::Compile;
use crate::CompileError::*;

#[derive(Debug)]
pub struct Memory {
    locals: HashMap<String, ScopeRecord>,
    locals_offset: Word,
    current_frame_offset: Word,
    output: Vec<IR>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            locals_offset: 0,
            current_frame_offset: 0,
            output: Vec::new(),
        }
    }
    pub fn done(self) -> Vec<IR> {
        self.output
    }
    fn allocate(&mut self, size: Word) -> Block {
        self.current_frame_offset += size;
        if size > 0 {
            self.output.push(IR::Sub(EA::SP, EA::Immediate(size)));
        }
        Block::stack(self.current_frame_offset, size)
    }
    fn write(&mut self, op: IROp, dest: Dest, src: Src) -> Block {
        match dest {
            Dest::Stack(size) => {
                if size != 1 {
                    let block = self.allocate(size);
                    self.write_to_block(op, block, src)
                } else {
                    let ea_src = src.to_ea(self.current_frame_offset, 0);
                    self.output.push(op(EA::PushStack, ea_src));
                    self.current_frame_offset += 1;
                    Block::stack(self.current_frame_offset, 1)
                }
            }
            Dest::Block(block) => self.write_to_block(op, block, src),
            Dest::R0 => {
                self.output
                    .push(op(EA::R0, src.to_ea(self.current_frame_offset, 0)));
                Block::R0
            }
            Dest::RefBlock(ptr_block, size) => {
                self.write(IR::Mov, Dest::R0, Src::Block(ptr_block));
                self.write_to_block(op, Block::r0_offset(0, size), src)
            }
        }
    }
    fn write_to_block(&mut self, op: IROp, dest: Block, src: Src) -> Block {
        for i in 0..dest.size() {
            let ea_src = src.to_ea(self.current_frame_offset, i);
            let ea_dest = dest.to_ea(self.current_frame_offset, i);

            if ea_src == ea_dest && op == IR::Mov {
                continue;
            }

            self.output.push(op(ea_dest, ea_src));
        }
        dest
    }
    pub fn identifier(&self, key: &str, ctx: ExprContext) -> Compile<Expr> {
        let record = self
            .locals
            .get(key)
            .ok_or_else(|| UnknownIdentifier(key.to_string()))?;
        let block = Block::local(record.frame_offset, record.ty.size());
        Ok(ctx.lvalue(record.ty.clone(), block))
    }
    pub fn store_local(&mut self, key: String, expr: ResolvedExpr) {
        self.compact(expr.block);
        self.locals_offset = self.current_frame_offset;
        self.locals.insert(
            key,
            ScopeRecord {
                frame_offset: self.locals_offset,
                ty: expr.ty,
            },
        );
    }
    pub fn compact(&mut self, block: Block) {
        let stack_space = block.stack_space();
        if stack_space.size == 0 {
            return;
        }
        let to_shift = stack_space.offset - stack_space.size - self.locals_offset;
        if to_shift > 0 {
            for i in 0..stack_space.size {
                // copy high to low
                let base_stack_offset = self.current_frame_offset - stack_space.offset - i;
                let ea_src = EA::StackOffset(base_stack_offset);
                let ea_dest = EA::StackOffset(base_stack_offset + to_shift);

                self.output.push(IR::Mov(ea_dest, ea_src));
            }
        }

        let to_remove = self.current_frame_offset - stack_space.offset + to_shift;
        if to_remove > 0 {
            self.output.push(IR::Add(EA::SP, EA::Immediate(to_remove)));
        }
        self.current_frame_offset = stack_space.offset;
    }
    fn deref(
        &mut self,
        ptr_block: Block,
        dest_size: Word,
        ctx: ExprContext,
        focus: Slice,
    ) -> Block {
        assert_eq!(ptr_block.size(), 1);
        self.write(IR::Mov, Dest::R0, Src::Block(ptr_block));
        self.write(
            IR::Mov,
            ctx.to_dest(dest_size),
            Src::R0Offset {
                offset: focus.offset,
                size: focus.size,
            },
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Slice {
    offset: Word,
    size: Word,
}

impl Slice {
    fn from_struct_field(field: &StructField) -> Self {
        Self {
            offset: field.offset,
            size: field.ty.size(),
        }
    }
    fn shrink_top(self, new_size: Word) -> Self {
        assert!(new_size <= self.size);
        Self {
            offset: self.offset - self.size + new_size,
            size: new_size,
        }
    }
    fn focus(&self, other: Slice) -> Self {
        assert!(other.size <= self.size);
        Self {
            offset: self.offset - other.offset,
            size: other.size,
        }
    }
}

// a location in memory
#[derive(Debug, Copy, Clone)]
pub enum Block {
    Stack(Slice),
    Local(Slice),
    R0,
    R0Offset(Slice),
}

impl Block {
    fn stack(offset: Word, size: Word) -> Self {
        Self::Stack(Slice { offset, size })
    }
    fn local(offset: Word, size: Word) -> Self {
        Self::Local(Slice { offset, size })
    }
    fn r0_offset(offset: Word, size: Word) -> Self {
        Self::R0Offset(Slice { offset, size })
    }

    fn size(&self) -> Word {
        match &self {
            Self::Stack(slice) => slice.size,
            Self::Local(slice) => slice.size,
            Self::R0 => 1,
            Self::R0Offset(slice) => slice.size,
        }
    }
    fn shrink_to(self, new_size: Word) -> Self {
        match self {
            Self::Stack(slice) => Self::Stack(slice.shrink_top(new_size)),
            Self::Local(slice) => Self::Local(slice.shrink_top(new_size)),
            _ => unimplemented!(),
        }
    }
    fn to_ea(self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Local(slice) => EA::StackOffset(current_frame_offset - slice.offset + index),
            Self::Stack(slice) => EA::StackOffset(current_frame_offset - slice.offset + index),
            Self::R0 => EA::R0,
            Self::R0Offset(slice) => EA::R0Offset(slice.offset),
        }
    }
    fn stack_space(self) -> Slice {
        match self {
            Self::Stack(slice) => slice,
            _ => Slice { offset: 0, size: 0 },
        }
    }
    fn focus(&self, focus: Slice) -> Block {
        match &self {
            Self::Local(slice) => Self::Local(slice.focus(focus)),
            Self::Stack(slice) => Self::Local(slice.focus(focus)),
            _ => unimplemented!(),
        }
    }
    pub fn struct_field(&self, field: &StructField) -> Block {
        self.focus(Slice::from_struct_field(field))
    }
    pub fn array_index(&self, item_ty: &Ty, index: Word) -> Block {
        self.focus(Slice {
            size: item_ty.size(),
            offset: item_ty.size() * index,
        })
    }
}

#[derive(Debug)]
struct ScopeRecord {
    frame_offset: Word,
    ty: Ty,
}
#[derive(Debug)]
enum Src {
    Block(Block),
    Immediate(Word),
    R0,
    R0Offset { offset: Word, size: Word },
}

impl Src {
    fn size(&self) -> Word {
        match &self {
            Self::Block(block) => block.size(),
            Self::Immediate(_) => 1,
            Self::R0 => 1,
            Self::R0Offset { size, .. } => *size,
        }
    }
    fn to_ea(&self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Immediate(value) => EA::Immediate(*value),
            Self::Block(block) => block.to_ea(current_frame_offset, index),
            Self::R0Offset { offset, .. } => EA::R0Offset(*offset),
            Self::R0 => EA::R0,
        }
    }
}

enum Dest {
    Block(Block),
    RefBlock(Block, Word),
    Stack(Word),
    R0,
}

#[derive(Debug)]
pub struct Expr {
    pub ty: Ty,
    kind: ExprKind,
    ctx: ExprContext,
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
            ctx: ExprContext::Block(block),
        }
    }
    pub fn get_constant(&self) -> Option<Word> {
        match &self.kind {
            ExprKind::Constant(value) => Some(*value),
            _ => None,
        }
    }
    pub fn assign_ctx(self) -> Compile<ExprContext> {
        match self.kind {
            ExprKind::Reference {
                deref_level,
                next,
                focus,
            } => {
                if deref_level > 0 {
                    Ok(ExprContext::RefBlock(next))
                } else {
                    Ok(ExprContext::Block(next.focus(focus)))
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
                let out = memory.deref(block, size, self.ctx, Slice { offset: 0, size });
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
    pub fn struct_field(self, field_name: &str) -> Compile<Self> {
        let field = self.ty.struct_field(field_name, None)?;
        match self.kind {
            ExprKind::Constant(_) => unimplemented!(),
            ExprKind::Resolved(block) => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference {
                    deref_level: 0,
                    next: block,
                    focus: Slice::from_struct_field(field),
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
                    focus: Slice::from_struct_field(field),
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
                    ctx: ExprContext::Block(block),
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
                    let block = memory.deref(next, self.ty.size(), self.ctx, focus);
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

#[derive(Debug)]
pub enum ExprContext {
    Stack,
    Block(Block),
    RefBlock(Block),
}

impl ExprContext {
    pub fn constant(self, ty: Ty, value: Word) -> Expr {
        Expr {
            ty,
            ctx: self,
            kind: ExprKind::Constant(value),
        }
    }
    fn lvalue(self, ty: Ty, block: Block) -> Expr {
        Expr {
            ty,
            ctx: self,
            kind: ExprKind::Reference {
                deref_level: 0,
                focus: Slice {
                    offset: 0,
                    size: block.size(),
                },
                next: block,
            },
        }
    }
    fn to_dest(self, size: Word) -> Dest {
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
