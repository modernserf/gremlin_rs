use crate::expr::*;
use crate::runtime::*;
use crate::ty::*;

#[derive(Debug)]
pub struct Memory {
    output: Vec<IR>,
    locals_offset: Word,
    current_frame_offset: Word,
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
    pub fn done_program(self, main_sub: SubIndex) -> CompileResult {
        CompileResult {
            code: self.output,
            entry_point: main_sub.index,
        }
    }
    pub fn debug_stack(&mut self) {
        self.output.push(IR::DebugStack)
    }
    pub fn panic(&mut self) {
        self.output.push(IR::Panic);
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
    pub fn from_record_field(field: &RecordField) -> Self {
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
    pub fn stack(offset: Word, size: Word) -> Self {
        Self::Stack(Slice { offset, size })
    }
    pub fn local(offset: Word, size: Word) -> Self {
        Self::Local(Slice { offset, size })
    }
    pub fn r0_offset(offset: Word, size: Word) -> Self {
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
    pub fn frame_offset(self) -> Option<Word> {
        match self {
            Self::Stack(slice) => Some(slice.offset),
            Self::Local(slice) => Some(slice.offset),
            _ => None,
        }
    }
    fn to_ea(self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Local(slice) => {
                EA::Offset(Register::Stack, current_frame_offset - slice.offset + index)
            }
            Self::Stack(slice) => {
                EA::Offset(Register::Stack, current_frame_offset - slice.offset + index)
            }
            Self::R0 => EA::Register(Register::Data),
            Self::R0Offset(slice) => EA::Offset(Register::Data, slice.offset),
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
    pub fn record_field(&self, field: &RecordField) -> Block {
        self.focus(Slice::from_record_field(field))
    }
    pub fn array_index(&self, item_ty: &Ty, index: Word) -> Block {
        self.focus(Slice {
            size: item_ty.size(),
            offset: item_ty.size() * index,
        })
    }
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
            Self::R0Offset(slice) => EA::Offset(Register::Data, slice.offset + index),
            Self::R0 => EA::Register(Register::Data),
        }
    }
}

pub enum Dest {
    Block(Block),
    RefBlock(Block, Word),
    Stack(Word),
    R0,
}

impl Memory {
    pub fn write(&mut self, op: IROp, dest: Dest, src: Src) -> Block {
        match dest {
            Dest::Stack(size) => {
                if size != 1 {
                    let block = self.allocate(size);
                    self.write_to_block(op, block, src)
                } else {
                    let ea_src = src.to_ea(self.current_frame_offset, 0);
                    self.output.push(op(EA::PreDec(Register::Stack), ea_src));
                    self.current_frame_offset += 1;
                    Block::stack(self.current_frame_offset, 1)
                }
            }
            Dest::Block(block) => self.write_to_block(op, block, src),
            Dest::R0 => {
                self.output.push(op(
                    EA::Register(Register::Data),
                    src.to_ea(self.current_frame_offset, 0),
                ));
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
    fn shift(&mut self, block: Block, to_shift: Word) {
        if to_shift == 0 {
            return;
        }
        let stack_space = block.stack_space();
        for i in 0..stack_space.size {
            // copy high to low
            let base_stack_offset = self.current_frame_offset - stack_space.offset - i;
            let ea_src = EA::Offset(Register::Stack, base_stack_offset);
            let ea_dest = EA::Offset(Register::Stack, base_stack_offset + to_shift);

            self.output.push(IR::Mov(ea_dest, ea_src));
        }
    }
    fn drop(&mut self, to_remove: Word) {
        if to_remove > 0 {
            self.output.push(IR::Add(
                EA::Register(Register::Stack),
                EA::Immediate(to_remove),
            ));
        }
    }
    pub fn compact(&mut self, block: Block) {
        let stack_space = block.stack_space();
        if stack_space.size == 0 {
            return;
        }
        let to_shift = stack_space.offset - stack_space.size - self.locals_offset;
        self.shift(block, to_shift);
        let to_remove = self.current_frame_offset - stack_space.offset + to_shift;
        self.drop(to_remove);
        self.current_frame_offset = stack_space.offset;
    }
    pub fn assign(&mut self, block: Block) -> Word {
        self.compact(block);
        self.locals_offset = self.current_frame_offset;
        self.locals_offset
    }
    pub fn allocate(&mut self, size: Word) -> Block {
        self.current_frame_offset += size;
        if size > 0 {
            self.output
                .push(IR::Sub(EA::Register(Register::Stack), EA::Immediate(size)));
        }
        Block::stack(self.current_frame_offset, size)
    }
    pub fn deref(&mut self, ptr_block: Block, dest: Dest, focus: Slice) -> Block {
        assert_eq!(ptr_block.size(), 1);
        self.write(IR::Mov, Dest::R0, Src::Block(ptr_block));
        self.write(IR::Mov, dest, Src::R0Offset(focus))
    }
}

#[derive(Debug)]
pub struct CondIndex {
    index: usize,
}

#[derive(Debug)]
pub struct WhileIndex {
    index: usize,
}

impl Memory {
    pub fn begin_cond(&mut self, expr: Expr, target: ExprTarget) -> CondIndex {
        let res = expr.resolve(self, target);
        let ea = match res.block {
            Block::Stack(slice) => {
                assert!(
                    slice.offset == self.current_frame_offset,
                    "must be top of stack"
                );
                self.current_frame_offset -= 1;
                EA::PostInc(Register::Stack)
            }
            block => block.to_ea(self.current_frame_offset, 0),
        };
        let index = self.output.len();
        self.output.push(IR::BranchZero(EA::Immediate(-1), ea));
        CondIndex { index }
    }
    pub fn begin_else(&mut self, if_rec: CondIndex) -> CondIndex {
        let else_index = self.output.len();
        self.output
            .push(IR::BranchZero(EA::Immediate(-1), EA::Immediate(0)));
        self.end_if(if_rec);
        CondIndex { index: else_index }
    }
    pub fn end_if(&mut self, rec: CondIndex) {
        let original = &self.output[rec.index];
        let displacement = (self.output.len() - rec.index - 1) as Word;
        self.output[rec.index] = match original {
            IR::BranchZero(_, ea) => IR::BranchZero(EA::Immediate(displacement), *ea),
            _ => unreachable!(),
        };
    }
    pub fn begin_while(&mut self) -> WhileIndex {
        WhileIndex {
            index: self.output.len(),
        }
    }
    pub fn end_while(&mut self, while_rec: WhileIndex, cond: CondIndex) {
        let jump_back = (self.output.len() - while_rec.index + 1) as Word;
        self.output
            .push(IR::BranchZero(EA::Immediate(-jump_back), EA::Immediate(0)));
        self.end_if(cond);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SubIndex {
    index: Word,
}

impl Memory {
    pub fn sub(&self) -> SubIndex {
        SubIndex {
            index: self.output.len() as Word,
        }
    }
    pub fn call_sub(&mut self, sub_index: SubIndex, args_size: Word) {
        self.output.push(IR::Call(sub_index.index));
        self.current_frame_offset -= args_size;
        self.drop(args_size);
    }
    pub fn return_sub(&mut self) {
        self.drop(self.current_frame_offset);
        self.current_frame_offset = 0;
        self.locals_offset = 0;
        self.output.push(IR::Return);
    }
}

#[derive(Debug)]
pub struct ScopeIndex {
    frame_offset: Word,
    locals_offset: Word,
}

impl ScopeIndex {
    pub fn root() -> Self {
        ScopeIndex {
            frame_offset: 0,
            locals_offset: 0,
        }
    }
}

impl Memory {
    pub fn begin_scope(&self) -> ScopeIndex {
        ScopeIndex {
            frame_offset: self.current_frame_offset,
            locals_offset: self.locals_offset,
        }
    }
    pub fn end_scope(&mut self, scope_index: ScopeIndex) {
        let to_drop = self.current_frame_offset - scope_index.frame_offset;
        self.current_frame_offset = scope_index.frame_offset;
        self.locals_offset = scope_index.locals_offset;
        self.drop(to_drop);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct CaseIndex {
    index: usize,
}

impl Memory {
    pub fn begin_match(&mut self, block: Block, size: usize) -> CaseIndex {
        self.output.push(IR::BranchZero(
            block.to_ea(self.current_frame_offset, 0),
            EA::Immediate(0),
        ));
        let index = self.output.len();
        for _ in 0..size {
            self.output
                .push(IR::BranchZero(EA::Immediate(-1), EA::Immediate(0)));
        }
        CaseIndex { index }
    }
    pub fn set_jump_target(&mut self, rec: CaseIndex, offset: usize) {
        let displacement = (self.output.len() - rec.index - 1 - offset) as Word;
        self.output[rec.index + offset] =
            IR::BranchZero(EA::Immediate(displacement), EA::Immediate(0));
    }
    pub fn end_case(&mut self) -> usize {
        let index = self.output.len();
        self.output
            .push(IR::BranchZero(EA::Immediate(-1), EA::Immediate(0)));
        index
    }
    pub fn set_jump_end_targets(&mut self, rec: CaseIndex, len: usize, case_ends: &[usize]) {
        let displacement = (self.output.len() - rec.index - 1) as Word;
        for i in 0..len {
            match &self.output[rec.index + i] {
                IR::BranchZero(EA::Immediate(-1), _) => {}
                IR::BranchZero(_, _) => continue,
                _ => panic!("expected jump table instruction"),
            }
            self.output[rec.index + i] =
                IR::BranchZero(EA::Immediate(displacement), EA::Immediate(0));
        }
        for index in case_ends {
            let displacement = (self.output.len() - index - 1) as Word;
            self.output[*index] = IR::BranchZero(EA::Immediate(displacement), EA::Immediate(0));
        }
    }
}
