use crate::ir::{IRDest, IRKind, IRSrc, Word, IR};

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
        match &instr.kind {
            IRKind::Move(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = value;
            }
            IRKind::LoadAddress(dest, src) => {
                let effective_address = self.get_effective_address(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = effective_address;
            }
            IRKind::Add(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr += value;
            }
            IRKind::Mult(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr *= value;
            }
            IRKind::Not(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = value ^ 1;
            }
            IRKind::And(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr &= value;
            }
            IRKind::Or(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr |= value;
            }
        }
    }
    fn get_src(&mut self, src: IRSrc) -> Word {
        match src {
            IRSrc::Immediate(value) => value,
            IRSrc::AtR0 => self.memory[self.r0 as usize],
            IRSrc::StackOffset(offset) => self.memory[(self.sp + offset) as usize],
            IRSrc::PopStack => {
                let value = self.memory[self.sp as usize];
                self.sp += 1;
                value
            }
        }
    }
    fn get_effective_address(&self, src: IRSrc) -> Word {
        match src {
            IRSrc::StackOffset(offset) => self.sp + offset,
            _ => unimplemented!(),
        }
    }
    fn get_dest<'a>(&'a mut self, dest: IRDest) -> &'a mut Word {
        match dest {
            IRDest::R0 => &mut self.r0,
            IRDest::StackOffset(offset) => &mut self.memory[(self.sp + offset) as usize],
            IRDest::PushStack => {
                self.sp -= 1;
                &mut self.memory[self.sp as usize]
            }
        }
    }
}
