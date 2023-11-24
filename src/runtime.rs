pub type Word = i32;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Register {
    Stack,
    Data,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum EA {
    // #123
    Immediate(Word),
    // D0
    Register(Register),
    // $ff00
    Absolute(Word),
    // n(SP)
    Offset(Register, Word),
    // n(SP, D0)
    Index(Register, Register, Word),
    // (SP)+
    PostInc(Register),
    // -(SP)
    PreDec(Register),
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
    Equal(IRDest, IRSrc),
    NotEqual(IRDest, IRSrc),
    BitTest(IRDest, IRSrc),
    BranchZero(IRDest, IRSrc),
    Call(Word),
    Return,
    DebugStack,
    Panic,
    Halt,
}

#[derive(Debug, Clone)]
pub struct CompileResult {
    pub code: Vec<IR>,
    pub entry_point: Word,
}

pub struct Runtime {
    r0: Word,
    sp: Word,
    ip: usize,
    memory: Vec<Word>,
}

impl Runtime {
    #[allow(dead_code)]
    pub fn eval(program: &[IR]) -> Word {
        let mut runtime = Self::new(128);
        runtime.run_program(program);
        runtime.memory[runtime.sp as usize]
    }
    pub fn eval_result(mut result: CompileResult) {
        let mut runtime = Self::new(128);
        runtime.ip = result.code.len();
        result.code.push(IR::Call(result.entry_point as Word));
        result.code.push(IR::Halt);
        runtime.run_program(&result.code);
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
            if instruction == &IR::Halt {
                break;
            }
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
            // TODO: Cmp IR that puts result in status register
            IR::Equal(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = if value == *dest_ptr { 1 } else { 0 }
            }
            IR::NotEqual(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = if value == *dest_ptr { 0 } else { 1 }
            }

            // TODO: use a status register instead of r0
            IR::BitTest(dest, src) => {
                let bit = self.get_src(*src);
                let dest = *self.get_dest(*dest);
                self.r0 = if (dest & (1 << bit)) > 0 { 1 } else { 0 }
            }

            IR::BranchZero(dest, src) => {
                let value = self.get_src(*src);
                let displacement = self.get_branch_dest(*dest);
                if value == 0 {
                    self.ip = (self.ip as Word + displacement) as usize
                }
            }
            IR::Call(addr) => {
                // push return address
                self.sp -= 1;
                self.memory[self.sp as usize] = self.ip as Word;
                self.ip = *addr as usize;
            }
            IR::Return => {
                // pop return address
                let addr = self.memory[self.sp as usize];
                self.sp += 1;
                self.ip = addr as usize;
            }

            IR::DebugStack => {
                println!("->{:?}", &self.memory[(self.sp as usize)..]);
            }
            IR::Panic => {
                println!("->{:?}", &self.memory[(self.sp as usize)..]);
                panic!("runtime panic");
            }
            IR::Halt => {}
        }
    }
    fn get_src(&mut self, src: EA) -> Word {
        match src {
            EA::Immediate(value) => value,
            EA::Register(register) => *self.get_register(register),

            EA::Absolute(addr) => self.memory[addr as usize],
            EA::Offset(register, offset) => {
                let addr = *self.get_register(register) + offset;
                self.memory[addr as usize]
            }

            EA::Index(l, r, offset) => {
                let addr = *self.get_register(l) + *self.get_register(r) + offset;
                self.memory[addr as usize]
            }

            EA::PostInc(register) => {
                let addr = *self.get_register(register);
                let result = self.memory[addr as usize];
                *self.get_register(register) += 1;
                result
            }
            EA::PreDec(register) => {
                *self.get_register(register) -= 1;
                let addr = *self.get_register(register);
                self.memory[addr as usize]
            }
            _ => unimplemented!(),
        }
    }
    fn get_register(&mut self, register: Register) -> &mut Word {
        match register {
            Register::Data => &mut self.r0,
            Register::Stack => &mut self.sp,
        }
    }
    fn get_effective_address(&mut self, src: EA) -> Word {
        match src {
            EA::Offset(register, offset) => *self.get_register(register) + offset,
            _ => unimplemented!(),
        }
    }
    fn get_dest(&mut self, dest: EA) -> &mut Word {
        match dest {
            EA::Register(register) => self.get_register(register),
            EA::Offset(register, offset) => {
                let addr = *self.get_register(register) + offset;
                &mut self.memory[addr as usize]
            }
            EA::PreDec(register) => {
                *self.get_register(register) -= 1;
                let addr = *self.get_register(register);
                &mut self.memory[addr as usize]
            }
            _ => unimplemented!(),
        }
    }
    fn get_branch_dest(&mut self, dest: EA) -> Word {
        match dest {
            EA::Immediate(value) => value,
            EA::Offset(register, offset) => {
                let addr = *self.get_register(register) + offset;
                self.memory[addr as usize]
            }
            _ => unimplemented!(),
        }
    }
}
