pub type Word = i32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EA {
    Immediate(Word),
    // registers
    R0,
    SP,
    // registers with offset
    StackOffset(Word),
    R0Offset(Word),
    // (SP + R0 + offset)
    Indexed(Word),
    // stack ops
    PushStack,
    PopStack,
}

type IRDest = EA;
type IRSrc = EA;
pub type IROp = fn(IRDest, IRSrc) -> IR;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IR {
    Mov(IRDest, IRSrc),
    LoadAddress(IRDest, IRSrc),
    Add(IRDest, IRSrc),
    Sub(IRDest, IRSrc),
    Mult(IRDest, IRSrc),
    And(IRDest, IRSrc),
    Or(IRDest, IRSrc),
    Xor(IRDest, IRSrc),
    BitTest(IRDest, IRSrc),
}

pub struct Runtime {
    r0: Word,
    sp: Word,
    ip: usize,
    memory: Vec<Word>,
}

impl Runtime {
    pub fn eval(program: &[IR]) -> Word {
        let mut runtime = Self::new(128);
        runtime.run_program(program);
        runtime.memory[runtime.sp as usize]
    }
    fn new(memory_size: Word) -> Self {
        Self {
            r0: 0,
            sp: memory_size,
            ip: 0,
            memory: vec![0; memory_size as usize],
        }
    }
    fn run_program(&mut self, program: &[IR]) {
        while self.ip < program.len() {
            let instruction = &program[self.ip];
            self.ip += 1;
            self.run_instr(instruction);
        }
    }
    fn run_instr(&mut self, instr: &IR) {
        match &instr {
            IR::Mov(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = value;
            }
            IR::LoadAddress(dest, src) => {
                let effective_address = self.get_effective_address(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = effective_address;
            }
            IR::Add(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr += value;
            }
            IR::Sub(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr -= value;
            }
            IR::Mult(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr *= value;
            }
            IR::Xor(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr ^= value;
            }
            IR::And(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr &= value;
            }
            IR::Or(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr |= value;
            }
            // TODO: use a status register instead of r0
            IR::BitTest(dest, src) => {
                let bit = self.get_src(*src);
                let dest = *self.get_dest(*dest);
                self.r0 = if (dest & (1 << bit)) > 0 { 1 } else { 0 }
            }
        }
    }
    fn get_src(&mut self, src: EA) -> Word {
        match src {
            EA::Immediate(value) => value,
            EA::R0 => self.r0,
            EA::R0Offset(offset) => self.memory[(self.r0 + offset) as usize],
            EA::StackOffset(offset) => self.memory[(self.sp + offset) as usize],
            EA::PopStack => {
                let value = self.memory[self.sp as usize];
                self.sp += 1;
                value
            }
            EA::Indexed(offset) => self.memory[(self.r0 + self.sp + offset) as usize],
            _ => unimplemented!(),
        }
    }
    fn get_effective_address(&self, src: EA) -> Word {
        match src {
            EA::StackOffset(offset) => self.sp + offset,
            _ => unimplemented!(),
        }
    }
    fn get_dest(&mut self, dest: EA) -> &mut Word {
        match dest {
            EA::R0 => &mut self.r0,
            EA::R0Offset(offset) => &mut self.memory[(self.r0 + offset) as usize],
            EA::SP => &mut self.sp,
            EA::StackOffset(offset) => &mut self.memory[(self.sp + offset) as usize],
            EA::PushStack => {
                self.sp -= 1;
                &mut self.memory[self.sp as usize]
            }
            _ => unimplemented!(),
        }
    }
}