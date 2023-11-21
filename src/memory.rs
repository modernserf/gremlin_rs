use std::collections::HashMap;

use crate::expr::*;
use crate::record::*;
use crate::runtime::*;
use crate::ty::*;
use crate::Compile;
use crate::CompileError::*;

pub struct Scope {
    locals: HashMap<String, ScopeRecord>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
        }
    }
    pub fn identifier(&self, key: &str, ctx: ExprContext) -> Compile<Expr> {
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
        let frame_offset = match block {
            Block::Stack(slice) => slice.offset,
            Block::Local(slice) => slice.offset,
            _ => panic!("invalid block"),
        };
        self.locals.insert(name, ScopeRecord { frame_offset, ty });
    }
}

#[derive(Debug)]
pub struct Memory {
    locals_offset: Word,
    current_frame_offset: Word,
    output: Vec<IR>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            locals_offset: 0,
            current_frame_offset: 0,
            output: Vec::new(),
        }
    }
    pub fn done(self) -> Vec<IR> {
        self.output
    }
    pub fn allocate(&mut self, size: Word) -> Block {
        self.current_frame_offset += size;
        if size > 0 {
            self.output.push(IR::Sub(EA::SP, EA::Immediate(size)));
        }
        Block::stack(self.current_frame_offset, size)
    }
    pub fn store_local(&mut self, key: String, expr: ResolvedExpr, scope: &mut Scope) {
        self.compact(expr.block);
        self.locals_offset = self.current_frame_offset;
        scope.store_local(key, expr.ty, self.locals_offset);
    }
    pub fn write(&mut self, op: IROp, dest: Dest, src: Src) -> Block {
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
    pub fn deref(
        &mut self,
        ptr_block: Block,
        dest_size: Word,
        ctx: ExprContext,
        focus: Option<Slice>,
    ) -> Block {
        let focus = focus.unwrap_or(Slice::with_size(dest_size));
        assert_eq!(ptr_block.size(), 1);
        self.write(IR::Mov, Dest::R0, Src::Block(ptr_block));
        self.write(IR::Mov, ctx.to_dest(dest_size), Src::R0Offset(focus))
    }

    pub fn begin_cond(&mut self, expr: Expr) -> CondRecord {
        let res = expr.resolve(self);
        let ea = match res.block {
            Block::Stack(slice) => {
                assert!(
                    slice.offset == self.current_frame_offset,
                    "must be top of stack"
                );
                self.current_frame_offset -= 1;
                EA::PopStack
            }
            block => block.to_ea(self.current_frame_offset, 0),
        };
        let index = self.output.len();
        self.output.push(IR::BranchZero(EA::Immediate(-1), ea));
        CondRecord { index }
    }

    pub fn begin_else(&mut self, if_rec: CondRecord) -> CondRecord {
        let else_index = self.output.len();
        self.output
            .push(IR::BranchZero(EA::Immediate(-1), EA::Immediate(0)));
        self.end_if(if_rec);
        CondRecord { index: else_index }
    }

    pub fn end_if(&mut self, rec: CondRecord) {
        let original = &self.output[rec.index];
        let displacement = (self.output.len() - rec.index - 1) as Word;
        self.output[rec.index] = match original {
            IR::BranchZero(_, ea) => IR::BranchZero(EA::Immediate(displacement), *ea),
            _ => unreachable!(),
        };
    }

    pub fn begin_while(&mut self) -> WhileRecord {
        WhileRecord {
            index: self.output.len(),
        }
    }

    pub fn end_while(&mut self, while_rec: WhileRecord, cond: CondRecord) {
        let jump_back = (self.output.len() - while_rec.index + 1) as Word;
        self.output
            .push(IR::BranchZero(EA::Immediate(-jump_back), EA::Immediate(0)));
        self.end_if(cond);
    }

    pub fn begin_match(&mut self, block: Block, size: usize) -> usize {
        self.output.push(IR::BranchZero(
            block.to_ea(self.current_frame_offset, 0),
            EA::Immediate(0),
        ));
        let jump_table_addr = self.output.len();
        for i in 0..size {
            self.output
                .push(IR::BranchZero(EA::Immediate(-1), EA::Immediate(0)));
        }
        jump_table_addr
    }
    pub fn set_jump_target(&mut self, table_index: usize, offset: usize) {
        let displacement = (self.output.len() - table_index - 1 - offset) as Word;
        self.output[table_index + offset] =
            IR::BranchZero(EA::Immediate(displacement), EA::Immediate(0));
    }
    pub fn end_case(&mut self) -> usize {
        let index = self.output.len();
        self.output
            .push(IR::BranchZero(EA::Immediate(-1), EA::Immediate(0)));
        index
    }
    pub fn set_jump_end_targets(&mut self, table_index: usize, len: usize, case_ends: &[usize]) {
        let displacement = (self.output.len() - table_index - 1) as Word;
        for i in 0..len {
            match &self.output[table_index + i] {
                IR::BranchZero(EA::Immediate(-1), _) => {}
                IR::BranchZero(_, _) => continue,
                _ => panic!("expected jump table instruction"),
            }
            self.output[table_index + i] =
                IR::BranchZero(EA::Immediate(displacement), EA::Immediate(0));
        }
        for index in case_ends {
            let displacement = (self.output.len() - index - 1) as Word;
            self.output[*index] = IR::BranchZero(EA::Immediate(displacement), EA::Immediate(0));
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Slice {
    offset: Word,
    size: Word,
}

impl Slice {
    pub fn with_size(size: Word) -> Self {
        Self { offset: 0, size }
    }

    pub fn from_struct_field(field: &RecordField) -> Self {
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

    pub fn size(&self) -> Word {
        match &self {
            Self::Stack(slice) => slice.size,
            Self::Local(slice) => slice.size,
            Self::R0 => 1,
            Self::R0Offset(slice) => slice.size,
        }
    }
    pub fn shrink_to(self, new_size: Word) -> Self {
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
    pub fn focus(&self, focus: Slice) -> Block {
        match &self {
            Self::Local(slice) => Self::Local(slice.focus(focus)),
            Self::Stack(slice) => Self::Stack(slice.focus(focus)),
            _ => unimplemented!(),
        }
    }
    pub fn struct_field(&self, field: &RecordField) -> Block {
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
pub enum Src {
    Block(Block),
    Immediate(Word),
    R0,
    R0Offset(Slice),
}

impl Src {
    fn to_ea(&self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Immediate(value) => EA::Immediate(*value),
            Self::Block(block) => block.to_ea(current_frame_offset, index),
            Self::R0Offset(slice) => EA::R0Offset(slice.offset + index),
            Self::R0 => EA::R0,
        }
    }
}

#[derive(Debug)]
pub struct CondRecord {
    index: usize,
}

#[derive(Debug)]
pub struct WhileRecord {
    index: usize,
}

pub enum Dest {
    Block(Block),
    RefBlock(Block, Word),
    Stack(Word),
    R0,
}
