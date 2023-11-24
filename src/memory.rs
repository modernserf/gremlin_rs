use crate::expr::*;
use crate::runtime::*;
use crate::ty::*;

#[derive(Debug)]
pub struct Memory {
    output: Vec<IR>,
    pub current_frame_offset: Word,
    free_registers: Vec<Register>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            current_frame_offset: 0,
            output: Vec::new(),
            free_registers: vec![Register::R0],
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
    pub fn take_register(&mut self) -> Option<Register> {
        self.free_registers.pop()
    }
    pub fn free_register(&mut self, register: Register) {
        if self.free_registers.contains(&register) {
            panic!("register is already free")
        }
        self.free_registers.push(register)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Slice {
    pub offset: Word,
    pub size: Word,
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
    // relative to (imaginary) frame pointer, converted to stack-relative
    Frame(Slice),
    Register(Register),
    Offset(Register, Slice),
}

impl Block {
    pub fn frame(offset: Word, size: Word) -> Self {
        Self::Frame(Slice { offset, size })
    }
    pub fn size(&self) -> Word {
        match &self {
            Self::Frame(slice) => slice.size,
            Self::Register(_) => 1,
            Self::Offset(_, slice) => slice.size,
        }
    }
    pub fn frame_slice(self) -> Option<Slice> {
        match self {
            Self::Frame(slice) => Some(slice),
            _ => None,
        }
    }
    fn to_ea(self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Frame(slice) => {
                EA::Offset(Register::SP, current_frame_offset - slice.offset + index)
            }
            Self::Register(register) => {
                assert_eq!(index, 0);
                EA::Register(*register)
            }
            Self::Offset(register, slice) => EA::Offset(*register, slice.offset + index),
        }
    }
    pub fn focus(&self, focus: Slice) -> Block {
        match &self {
            Self::Frame(slice) => Self::Frame(slice.focus(focus)),
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
}

impl Src {
    fn to_ea(&self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Block(block) => block.to_ea(current_frame_offset, index),
            Self::Immediate(value) => {
                assert_eq!(index, 0);
                EA::Immediate(*value)
            }
        }
    }
}

pub enum Dest {
    Block(Block),
    Stack(Word),
}

impl Memory {
    // TODO: this really only makes sense for mov, everything else should operate on a per-word basis
    pub fn write(&mut self, op: IROp, dest: Dest, src: Src) -> Block {
        match dest {
            Dest::Stack(size) => {
                if size != 1 {
                    let block = self.allocate(size);
                    self.write_to_block(op, block, src)
                } else {
                    let ea_src = src.to_ea(self.current_frame_offset, 0);
                    self.output.push(op(EA::PreDec(Register::SP), ea_src));
                    self.current_frame_offset += 1;
                    Block::frame(self.current_frame_offset, 1)
                }
            }
            Dest::Block(block) => self.write_to_block(op, block, src),
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
    // move a slice "up" the stack in-place
    fn shift(&mut self, stack_slice: Slice, to_shift: Word) {
        if to_shift == 0 {
            return;
        }
        for i in 0..stack_slice.size {
            // copy high to low
            let base_stack_offset = self.current_frame_offset - stack_slice.offset - i;
            let ea_src = EA::Offset(Register::SP, base_stack_offset);
            let ea_dest = EA::Offset(Register::SP, base_stack_offset + to_shift);

            self.output.push(IR::Mov(ea_dest, ea_src));
        }
    }
    pub fn allocate(&mut self, size: Word) -> Block {
        self.current_frame_offset += size;
        if size > 0 {
            self.output
                .push(IR::Sub(EA::Register(Register::SP), EA::Immediate(size)));
        }
        Block::frame(self.current_frame_offset, size)
    }
    fn drop(&mut self, to_remove: Word) {
        if to_remove > 0 {
            self.current_frame_offset -= to_remove;
            self.output.push(IR::Add(
                EA::Register(Register::SP),
                EA::Immediate(to_remove),
            ));
        }
    }
    pub fn compact(&mut self, block: Block, prev_frame_offset: Word) {
        let stack_slice = block.frame_slice().unwrap_or(Slice::with_size(0));
        if stack_slice.size == 0 || stack_slice.offset <= prev_frame_offset {
            return;
        }
        let to_shift = stack_slice.offset - stack_slice.size - prev_frame_offset;
        self.shift(stack_slice, to_shift);
        let to_remove = self.current_frame_offset - stack_slice.offset + to_shift;
        self.drop(to_remove);
    }
    pub fn assign(&mut self, block: Block, prev_frame_offset: Word) -> Word {
        self.compact(block, prev_frame_offset);
        self.current_frame_offset
    }
    pub fn deref_to_dest(&mut self, ptr_block: Block, size: Word) -> (Register, Dest) {
        let register = self.load_ptr(ptr_block);
        let dest = Dest::Block(Block::Offset(register, Slice::with_size(size)));
        (register, dest)
    }
    pub fn deref_to_src(&mut self, ptr_block: Block, focus: Slice) -> (Register, Src) {
        let register = self.load_ptr(ptr_block);
        let src = Src::Block(Block::Offset(register, focus));
        (register, src)
    }
    fn load_ptr(&mut self, ptr_block: Block) -> Register {
        assert_eq!(ptr_block.size(), 1);
        let register = self.take_register().expect("free register");
        self.output.push(IR::Mov(
            EA::Register(register),
            ptr_block.to_ea(self.current_frame_offset, 0),
        ));
        register
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
            Block::Frame(slice) => {
                assert!(
                    slice.offset == self.current_frame_offset,
                    "must be top of stack"
                );
                self.current_frame_offset -= 1;
                EA::PostInc(Register::SP)
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
        self.drop(args_size);
    }
    pub fn return_sub(&mut self) {
        self.drop(self.current_frame_offset);
        self.output.push(IR::Return);
    }
}

#[derive(Debug)]
pub struct ScopeIndex {
    frame_offset: Word,
}

impl ScopeIndex {
    pub fn root() -> Self {
        ScopeIndex { frame_offset: 0 }
    }
}

impl Memory {
    pub fn begin_scope(&self) -> ScopeIndex {
        ScopeIndex {
            frame_offset: self.current_frame_offset,
        }
    }
    pub fn end_scope(&mut self, scope_index: ScopeIndex) {
        let to_drop = self.current_frame_offset - scope_index.frame_offset;
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

// miscellaneous cond
// TODO: take / free respective status registers
impl Memory {
    pub fn bit_test(&mut self, target: Block, src: Src) {
        self.output.push(IR::BitTest(
            target.to_ea(self.current_frame_offset, 0),
            src.to_ea(self.current_frame_offset, 0),
        ))
    }
    pub fn set_if(&mut self, dest: Dest, cond: IRCond) {
        let ea = match dest {
            Dest::Stack(_) => EA::PreDec(Register::SP),
            Dest::Block(block) => block.to_ea(self.current_frame_offset, 0),
        };

        self.output.push(IR::SetIf(ea, cond))
    }
}
