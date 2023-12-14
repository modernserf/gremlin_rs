use super::ea::*;

const ADDRESS_MASK: i32 = 0x7FFF_FFFF;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EAView {
    Data(usize),
    Addr(usize),
    Memory(i32),
    Immediate(i32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RunState {
    Ready,
    Halt,
}

struct VM {
    memory: Vec<u8>,
    data: [i32; 8],
    addr: [i32; 8],
    pc: i32,
    run_state: RunState,
}

impl VM {
    fn new(size: usize) -> Self {
        Self {
            memory: vec![0; size],
            data: [0; 8],
            addr: [0; 8],
            pc: 0,
            run_state: RunState::Halt,
        }
    }
    fn load_memory(&mut self, offset: usize, memory: &[u8]) {
        for (i, byte) in memory.iter().enumerate() {
            self.memory[offset + i] = *byte
        }
    }
    fn reset(&mut self) {
        self.addr[7] = self.mem_i32(0);
        self.pc = self.mem_i32(4);
        self.run_state = RunState::Halt;
    }
    fn run(&mut self) {
        self.run_state = RunState::Ready;
        while self.run_state == RunState::Ready {
            self.run_1();
        }
    }
    fn run_1(&mut self) {
        let instr = self.mem_u16(self.pc);
        let tag = instr >> 12;
        let reg = (instr >> 9 & 0b111) as usize;
        let mode = (instr >> 6) & 0b111;

        match (tag, reg, mode) {
            // MOVE
            (0b0001, _, _) => {
                let src = self.ea_read(Size::Byte);
                let dest = self.ea_read_inner(Size::Byte, mode, reg);
                self.ea_apply(dest, src, Size::Byte, |_, r| r);
            }
            (0b0011, _, _) => {
                let src = self.ea_read(Size::Short);
                let dest = self.ea_read_inner(Size::Short, mode, reg);
                self.ea_apply(dest, src, Size::Short, |_, r| r);
            }
            (0b0010, _, _) => {
                let src = self.ea_read(Size::Word);
                let dest = self.ea_read_inner(Size::Word, mode, reg);
                self.ea_apply(dest, src, Size::Word, |_, r| r);
            }
            // TRAP
            (0b0100, 0b111, 0b001) => {
                self.pc += 2;
                // TODO: "in-universe" halt
                self.run_state = RunState::Halt;
            }
            // ADDI
            (0b0000, 0b011, _) => {
                let (size, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte)),
                    1 => (Size::Short, self.ea_read(Size::Short)),
                    2 => (Size::Word, self.ea_read(Size::Word)),
                    _ => unreachable!(),
                };
                let src = self.immediate_read(size);
                self.ea_apply(dest, src, size, |l, r| l + r);
            }

            // ADDQ
            (0b0101, _, _) => {
                let quick = (instr >> 9) & 0b111;
                let quick = if quick == 0 { 8 } else { quick as i32 };
                let mode = (instr >> 6) & 0b11;
                let (size, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte)),
                    1 => (Size::Short, self.ea_read(Size::Short)),
                    2 => (Size::Word, self.ea_read(Size::Word)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, EAView::Immediate(quick), size, |l, r| l + r);
            }
            // ADD / ADDA
            (0b1101, _, _) => {
                let (size, src, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    1 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    2 => (Size::Word, self.ea_read(Size::Word), EAView::Data(reg)),
                    3 => (Size::Byte, EAView::Data(reg), self.ea_read(Size::Byte)),
                    4 => (Size::Short, EAView::Data(reg), self.ea_read(Size::Short)),
                    5 => (Size::Word, EAView::Data(reg), self.ea_read(Size::Word)),
                    6 => (Size::Short, self.ea_read(Size::Short), EAView::Addr(reg)),
                    7 => (Size::Word, self.ea_read(Size::Word), EAView::Addr(reg)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, src, size, |l, r| l + r);
            }
            _ => unimplemented!(),
        }
    }
    // big endian
    fn mem_i32(&self, idx: i32) -> i32 {
        let i = (idx & ADDRESS_MASK) as usize;
        i32::from_be_bytes([
            self.memory[i],
            self.memory[i + 1],
            self.memory[i + 2],
            self.memory[i + 3],
        ])
    }
    // ops, absolute addresses
    fn mem_u16(&self, idx: i32) -> u16 {
        let i = (idx & ADDRESS_MASK) as usize;
        u16::from_be_bytes([self.memory[i], self.memory[i + 1]])
    }
    // displacements
    fn mem_i16(&self, idx: i32) -> i16 {
        let i = (idx & ADDRESS_MASK) as usize;
        i16::from_be_bytes([self.memory[i], self.memory[i + 1]])
    }
    fn mem_u8(&self, idx: i32) -> u8 {
        self.memory[(idx & ADDRESS_MASK) as usize]
    }
    fn ea_src(&self, src: EAView, size: Size) -> i32 {
        match src {
            EAView::Data(d) => self.data[d],
            EAView::Addr(a) => self.addr[a],
            EAView::Memory(i) => match size {
                Size::Byte => self.mem_u8(i) as i32,
                Size::Short => self.mem_i16(i) as i32,
                Size::Word => self.mem_i32(i),
            },
            EAView::Immediate(x) => x,
        }
    }
    fn ea_apply(&mut self, dest: EAView, src: EAView, size: Size, apply: fn(i32, i32) -> i32) {
        let value = self.ea_src(src, size);
        let prev = self.ea_src(dest, size);
        match dest {
            EAView::Data(d) => self.data[d] = apply(prev, value),
            EAView::Addr(a) => self.addr[a] = apply(prev, value),
            EAView::Memory(idx) => {
                let i = (idx & ADDRESS_MASK) as usize;
                let bytes = apply(prev, value).to_be_bytes();
                match size {
                    Size::Byte => {
                        self.memory[i] = bytes[3];
                    }
                    Size::Short => {
                        self.memory[i] = bytes[2];
                        self.memory[i + 1] = bytes[3];
                    }
                    Size::Word => {
                        self.memory[i] = bytes[0];
                        self.memory[i + 1] = bytes[1];
                        self.memory[i + 2] = bytes[2];
                        self.memory[i + 3] = bytes[3];
                    }
                }
            }
            _ => todo!("illegal instruction"),
        }
    }
    fn ea_read(&mut self, size: Size) -> EAView {
        let instr = self.mem_u16(self.pc);
        self.pc += 2;

        let reg_id = (instr & 0b111) as usize;
        let mode = (instr >> 3) & 0b111;
        self.ea_read_inner(size, mode, reg_id)
    }
    fn ea_read_inner(&mut self, size: Size, mode: u16, reg_id: usize) -> EAView {
        match (mode, reg_id) {
            (0, d) => EAView::Data(d),
            (1, a) => EAView::Addr(a),
            (2, a) => EAView::Memory(self.addr[a]),
            (3, a) => {
                let view = EAView::Memory(self.addr[a]);
                self.addr[a] += size.bytes();
                view
            }
            (4, a) => {
                self.addr[a] -= size.bytes();
                EAView::Memory(self.addr[a])
            }
            (5, a) => {
                let offset = self.mem_i16(self.pc) as i32;
                self.pc += 2;
                EAView::Memory(self.addr[a] + offset)
            }
            (6, a) => {
                let register = self.mem_u8(self.pc) as usize;
                let offset = self.mem_u8(self.pc + 1) as i32;
                self.pc += 2;
                let id = register & !0b1000;
                if register == id {
                    EAView::Memory(self.addr[a] + self.data[id] + offset)
                } else {
                    EAView::Memory(self.addr[a] + self.addr[id] + offset)
                }
            }
            (7, 0) => {
                let offset = self.mem_u16(self.pc) as i32;
                self.pc += 2;
                EAView::Memory(offset)
            }
            (7, 1) => {
                let offset = self.mem_i32(self.pc);
                self.pc += 4;
                EAView::Memory(offset)
            }
            (7, 2) => {
                let pc_base = self.pc;
                let disp = self.mem_i16(self.pc) as i32;
                self.pc += 2;
                EAView::Memory(pc_base + disp)
            }
            (7, 3) => {
                let pc_base = self.pc;
                let register = self.mem_u8(self.pc) as usize;
                let offset = self.mem_u8(self.pc + 1) as i32;
                self.pc += 2;
                let id = register & !0b1000;
                if register == id {
                    EAView::Memory(pc_base + self.data[id] + offset)
                } else {
                    EAView::Memory(pc_base + self.addr[id] + offset)
                }
            }
            (7, 4) => {
                let (bytes, value) = match size {
                    Size::Byte => (2, self.mem_u8(self.pc + 1) as i32),
                    Size::Short => (2, self.mem_i16(self.pc) as i32),
                    Size::Word => (4, self.mem_i32(self.pc)),
                };
                self.pc += bytes;
                EAView::Immediate(value)
            }
            _ => unreachable!(),
        }
    }
    fn immediate_read(&mut self, size: Size) -> EAView {
        match size {
            Size::Byte => {
                let value = self.mem_u8(self.pc + 1) as i32;
                self.pc += 2;
                EAView::Immediate(value)
            }
            Size::Short => {
                let value = self.mem_i16(self.pc) as i32;
                self.pc += 2;
                EAView::Immediate(value)
            }
            Size::Word => {
                let value = self.mem_i32(self.pc) as i32;
                self.pc += 4;
                EAView::Immediate(value)
            }
        }
    }
}

type Quick = u16; // 3
fn quick_positive(word: i32) -> Option<Quick> {
    match word {
        1..=7 => Some(word as Quick),
        8 => Some(0 as Quick),
        _ => None,
    }
}
fn quick_zero(word: i32) -> Option<Quick> {
    match word {
        0..=7 => Some(word as Quick),
        _ => None,
    }
}
struct Asm {
    out: Vec<u8>,
}

impl Asm {
    pub fn new() -> Self {
        Self { out: Vec::new() }
    }
    pub fn add(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (src, EA::Data(d)) => {
                let op_mode = match size {
                    Size::Byte => 0,
                    Size::Short => 1,
                    Size::Word => 2,
                };
                self.tag4_reg3_mode3_ea(0b1101, d as u16, op_mode);
                self.push_ea(size, src);
            }
            (src, EA::Addr(a)) => {
                let op_mode = match size {
                    Size::Byte => unimplemented!(),
                    Size::Short => 6,
                    Size::Word => 7,
                };
                self.tag4_reg3_mode3_ea(0b1101, a as u16, op_mode);
                self.push_ea(size, src);
            }
            (EA::Data(d), dest) => {
                let op_mode = match size {
                    Size::Byte => 3,
                    Size::Short => 4,
                    Size::Word => 5,
                };
                self.tag4_reg3_mode3_ea(0b1101, d as u16, op_mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                if let Some(q) = quick_positive(x) {
                    let mode = match size {
                        Size::Byte => 0,
                        Size::Short => 1,
                        Size::Word => 2,
                    };
                    self.tag4_reg3_mode3_ea(0b0101, q, mode);
                    self.push_ea(size, dest);
                } else {
                    let mode = match size {
                        Size::Byte => 0,
                        Size::Short => 1,
                        Size::Word => 2,
                    };
                    self.tag4_reg3_mode3_ea(0, 0b011, mode);
                    self.push_ea(size, dest);
                    self.push_immediate(size, x);
                }
            }
            _ => unimplemented!(),
        }
    }
    pub fn mov(&mut self, size: Size, src: EA, dest: EA) {
        let tag = match size {
            Size::Byte => 0b0001,
            Size::Short => 0b0011,
            Size::Word => 0b0010,
        };
        let mut dest_dummy = vec![0];
        dest.write(size, &mut dest_dummy);
        let reg = dest_dummy[0] & 0b111;
        let mode = (dest_dummy[0] >> 3) & 0b111;
        self.tag4_reg3_mode3_ea(tag, reg as u16, mode as u16);
        self.push_ea(size, src);
        self.out.extend(dest_dummy[1..].iter());
    }
    pub fn halt(&mut self) {
        // TRAP #0
        self.push_u16(0b0100_1110_0100_0000);
    }
    pub fn data(&mut self, data: &[u8]) {
        for byte in data {
            self.out.push(*byte);
        }
        if (self.out.len() & 1) == 1 {
            self.out.push(0);
        }
    }
    pub fn here(&self) -> usize {
        self.out.len()
    }
    pub fn fixup(&mut self, at: usize, f: impl Fn(&mut Asm)) {
        let mut a = Asm::new();
        f(&mut a);
        for (i, byte) in a.out.into_iter().enumerate() {
            self.out[at + i] = byte;
        }
    }

    // instruction formats
    fn tag4_reg3_mode3_ea(&mut self, tag: u16, register: u16, mode: u16) {
        let base = (tag << 12) + (register << 9) + (mode << 6);
        self.push_u16(base);
    }
    fn push_u16(&mut self, value: u16) {
        let bytes = value.to_be_bytes();
        self.out.push(bytes[0]);
        self.out.push(bytes[1]);
    }
    fn push_ea(&mut self, size: Size, ea: EA) {
        ea.write(size, &mut self.out);
    }
    fn push_immediate(&mut self, size: Size, value: i32) {
        let bytes = value.to_be_bytes();
        match size {
            Size::Byte => {
                self.out.push(0);
                self.out.push(bytes[3]);
            }
            Size::Short => {
                self.out.push(bytes[2]);
                self.out.push(bytes[3]);
            }
            Size::Word => {
                self.out.push(bytes[0]);
                self.out.push(bytes[1]);
                self.out.push(bytes[2]);
                self.out.push(bytes[3]);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::ea::{Size::*, EA::*};
    use crate::v2::register::{Addr::*, Data::*};

    #[test]
    fn smoke_test() {
        let mut vm = VM::new(256);
        let init_sp = 120;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        let mut asm = Asm::new();

        asm.mov(Word, Immediate(10), Data(D0));
        asm.mov(Word, Immediate(20), Data(D1));
        asm.add(Word, Data(D0), Data(D1));
        asm.halt();

        asm.mov(Word, Immediate(30), Data(D0));
        asm.mov(Word, Immediate(10), Addr(A0));
        asm.add(Word, Data(D0), Addr(A0));
        asm.halt();

        asm.mov(Word, Immediate(10), PreDec(A7));
        asm.add(Word, Immediate(5), Offset(A7, 0));
        asm.halt();

        asm.mov(Word, Immediate(10), Offset(A7, 0));
        asm.mov(Word, Immediate(20), Data(D2));
        asm.add(Word, PostInc(A7), Data(D2));
        asm.halt();

        asm.mov(Word, Immediate(10), PreDec(A7));
        asm.mov(Word, Immediate(0), PreDec(A7));
        asm.add(Word, Immediate(20), Offset(A7, 4));
        asm.halt();

        vm.load_memory(init_pc as usize, &asm.out);

        vm.run();
        assert_eq!(vm.data[1], 30);
        vm.run();
        assert_eq!(vm.addr[0], 40);
        vm.run();
        assert_eq!(vm.mem_i32(vm.addr[7]), 15);
        vm.run();
        assert_eq!(vm.data[2], 30);
        vm.run();
        assert_eq!(vm.mem_i32(vm.addr[7] + 4), 30);
    }

    #[test]
    fn bytes() {
        let mut vm = VM::new(256);
        let init_sp = 120;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        let mut asm = Asm::new();

        asm.mov(Byte, Immediate(0), Data(D0));

        let instr = asm.here();
        asm.add(Byte, PCOffset(0), Data(D0));
        asm.add(Byte, PCOffset(0), Data(D0));
        asm.add(Byte, PCOffset(0), Data(D0));
        asm.halt();

        let data = asm.here();
        asm.data(&[10, 20, 30]);
        asm.fixup(instr, |asm| {
            let base_offset = (data - instr - 2) as i32;
            asm.add(Byte, PCOffset(base_offset), Data(D0));
            asm.add(Byte, PCOffset(base_offset - 4 + 1), Data(D0));
            asm.add(Byte, PCOffset(base_offset - 8 + 2), Data(D0));
        });

        vm.load_memory(init_pc as usize, &asm.out);
        vm.run();
        assert_eq!(vm.data[0], 60);
    }
}
