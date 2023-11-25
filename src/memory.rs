use crate::block::*;
use crate::expr::*;
use crate::runtime::*;

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

#[derive(Debug)]
pub enum Src {
    Block(Block),
    Offset(Register, Slice),
    Immediate(Word),
}

impl Src {
    pub fn size(&self) -> Word {
        match &self {
            Self::Block(block) => block.size(),
            Self::Offset(_, slice) => slice.size,
            Self::Immediate(_) => 1,
        }
    }
    fn to_ea(self, current_frame_offset: Word) -> EA {
        match &self {
            Self::Block(block) => block.to_ea(current_frame_offset),
            Self::Offset(r, slice) => EA::Offset(*r, slice.offset),
            Self::Immediate(value) => EA::Immediate(*value),
        }
    }
    // TODO: only pop new stack values, not lvalues
    // this should probably be handled by expr
    fn to_pop_ea(self, memory: &mut Memory) -> EA {
        match self {
            Self::Block(block) => match block.to_ea(memory.current_frame_offset) {
                EA::Offset(Register::SP, 0) => {
                    memory.current_frame_offset -= 1;
                    EA::PostInc(Register::SP)
                }
                ea => ea,
            },
            _ => self.to_ea(memory.current_frame_offset),
        }
    }
    // TODO: better interface for op.apply()
    pub fn as_dest(self) -> Dest {
        match self {
            Self::Block(block) => Dest::Block(block),
            Self::Offset(r, slice) => Dest::Offset(r, slice),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub enum Dest {
    Block(Block),
    Offset(Register, Slice),
    Stack,
}

impl Memory {
    pub fn accumulate(&mut self, op: IROp, dest: Dest, src: Src) -> Block {
        let ea_src = src.to_ea(self.current_frame_offset);
        match dest {
            Dest::Stack => {
                self.output.push(op(EA::PreDec(Register::SP), ea_src));
                self.current_frame_offset += 1;
                Block::new(self.current_frame_offset, 1)
            }
            Dest::Block(block) => {
                let ea_dest = block.to_ea(self.current_frame_offset);
                self.output.push(op(ea_dest, ea_src));
                block
            }
            _ => unimplemented!(),
        }
    }
    pub fn mov(&mut self, dest: Dest, src: Src) -> Block {
        let size = src.size();
        match dest {
            Dest::Stack => {
                if size != 1 {
                    let block = self.allocate(size);
                    self.mov_inner(
                        block.to_ea(self.current_frame_offset),
                        src.to_ea(self.current_frame_offset),
                        size,
                    );
                    block
                } else {
                    let ea_src = src.to_ea(self.current_frame_offset);
                    self.output.push(IR::Mov(EA::PreDec(Register::SP), ea_src));
                    self.current_frame_offset += 1;
                    Block::new(self.current_frame_offset, 1)
                }
            }
            Dest::Block(block) => {
                assert_eq!(block.size(), size);
                self.mov_inner(
                    block.to_ea(self.current_frame_offset),
                    src.to_ea(self.current_frame_offset),
                    size,
                );
                block
            }
            Dest::Offset(r, slice) => {
                assert_eq!(slice.size, size);
                self.mov_inner(
                    EA::Offset(r, slice.offset),
                    src.to_ea(self.current_frame_offset),
                    size,
                );
                // TODO
                Block::new(0, 0)
            }
        }
    }
    fn mov_inner(&mut self, ea_dest: EA, ea_src: EA, size: Word) {
        if ea_src == ea_dest {
            return;
        }
        for i in 0..size {
            self.output
                .push(IR::Mov(ea_dest.add_offset(i), ea_src.add_offset(i)));
        }
    }
    pub fn load_address(&mut self, dest: Dest, src: Src) -> Block {
        let ea_src = src.to_ea(self.current_frame_offset);
        match dest {
            Dest::Stack => {
                self.output
                    .push(IR::LoadAddress(EA::PreDec(Register::SP), ea_src));
                self.current_frame_offset += 1;
                Block::new(self.current_frame_offset, 1)
            }
            Dest::Block(block) => {
                let ea_dest = block.to_ea(self.current_frame_offset);
                self.output.push(IR::LoadAddress(ea_dest, ea_src));
                block
            }
            _ => unimplemented!(),
        }
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
        Block::new(self.current_frame_offset, size)
    }
    pub fn drop(&mut self, to_remove: Word) {
        if to_remove > 0 {
            self.current_frame_offset -= to_remove;
            self.output.push(IR::Add(
                EA::Register(Register::SP),
                EA::Immediate(to_remove),
            ));
        }
    }
    pub fn compact(&mut self, block: Block, prev_frame_offset: Word) {
        let stack_slice = Slice {
            offset: block.frame_offset(),
            size: block.size(),
        };
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
    pub fn load_ptr_iter(&mut self, ptr_block: Block, deref_count: usize) -> Register {
        assert_eq!(ptr_block.size(), 1);
        let register = self.take_register().expect("free register");
        let mut src_ea = ptr_block.to_ea(self.current_frame_offset);

        // TODO: Can you actually do MOV A0 <- (A0)?
        for _ in 0..deref_count {
            self.output.push(IR::Mov(EA::Register(register), src_ea));
            src_ea = EA::Register(register);
        }

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
    pub fn begin_cond(&mut self, expr: Expr) -> CondIndex {
        let cond = expr.resolve_branch_cond(self);
        let index = self.output.len();
        self.output.push(IR::BranchIf(EA::Immediate(-1), cond));
        CondIndex { index }
    }
    pub fn begin_else(&mut self, if_rec: CondIndex) -> CondIndex {
        let else_index = self.output.len();
        self.output
            .push(IR::BranchIf(EA::Immediate(-1), IRCond::Always));
        self.end_if(if_rec);
        CondIndex { index: else_index }
    }
    pub fn end_if(&mut self, rec: CondIndex) {
        let original = &self.output[rec.index];
        let displacement = (self.output.len() - rec.index - 1) as Word;
        self.output[rec.index] = match original {
            IR::BranchIf(_, cond) => IR::BranchIf(EA::Immediate(displacement), *cond),
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
            .push(IR::BranchIf(EA::Immediate(-jump_back), IRCond::Always));
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
        self.output.push(IR::BranchIf(
            block.to_ea(self.current_frame_offset),
            IRCond::Always,
        ));
        let index = self.output.len();
        for _ in 0..size {
            self.output
                .push(IR::BranchIf(EA::Immediate(-1), IRCond::Always));
        }
        CaseIndex { index }
    }
    pub fn set_jump_target(&mut self, rec: CaseIndex, offset: usize) {
        let displacement = (self.output.len() - rec.index - 1 - offset) as Word;
        self.output[rec.index + offset] = IR::BranchIf(EA::Immediate(displacement), IRCond::Always);
    }
    pub fn end_case(&mut self) -> usize {
        let index = self.output.len();
        self.output
            .push(IR::BranchIf(EA::Immediate(-1), IRCond::Always));
        index
    }
    pub fn set_jump_end_targets(&mut self, rec: CaseIndex, len: usize, case_ends: &[usize]) {
        let displacement = (self.output.len() - rec.index - 1) as Word;
        for i in 0..len {
            match &self.output[rec.index + i] {
                IR::BranchIf(EA::Immediate(-1), IRCond::Always) => {
                    self.output[rec.index + i] =
                        IR::BranchIf(EA::Immediate(displacement), IRCond::Always);
                }
                IR::BranchIf(_, _) => continue,
                _ => panic!("expected jump table instruction"),
            }
        }
        for index in case_ends {
            let displacement = (self.output.len() - index - 1) as Word;
            self.output[*index] = IR::BranchIf(EA::Immediate(displacement), IRCond::Always);
        }
    }
}

// miscellaneous cond
// TODO: take / free respective status registers
impl Memory {
    pub fn bit_test(&mut self, target: Block, src: Src) {
        self.output.push(IR::BitTest(
            target.to_ea(self.current_frame_offset),
            src.to_ea(self.current_frame_offset),
        ))
    }
    pub fn set_if(&mut self, dest: Dest, cond: IRCond) -> Block {
        let (ea, out) = match dest {
            Dest::Stack => (
                EA::PreDec(Register::SP),
                Block::new(self.current_frame_offset + 1, 1),
            ),
            Dest::Block(block) => (block.to_ea(self.current_frame_offset), block),
            _ => unimplemented!(),
        };

        self.output.push(IR::SetIf(ea, cond));
        out
    }
    pub fn cmp_bool(&mut self, block: Block) -> IRCond {
        let ea = Src::Block(block).to_pop_ea(self);
        self.output.push(IR::Cmp(ea, EA::Immediate(1)));
        IRCond::Zero
    }
    pub fn cmp(&mut self, left: Src, right: Src) {
        let right = right.to_ea(self.current_frame_offset);
        let left = left.to_pop_ea(self);
        self.output.push(IR::Cmp(left, right))
    }
}
