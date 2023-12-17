use super::{ea::*, register::Addr};

const ADDRESS_MASK: i32 = 0x7FFF_FFFF;

// bit operations

fn bit_set(it: isize, idx: usize, value: bool) -> isize {
    if value {
        it | 1 << idx
    } else {
        it & !(1 << idx)
    }
}
fn bit_test<T: Into<isize>>(it: T, idx: usize) -> bool {
    it.into() & (1 << idx) != 0
}

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

enum StatusFlag {
    Carry = 0,
    Overflow = 1,
    Zero = 2,
    Negative = 3,
    Extend = 4,
}

pub struct VM {
    memory: Vec<u8>,
    data: [i32; 8],
    addr: [i32; 8],
    pc: i32,
    status: u16,
    run_state: RunState,
}

impl VM {
    pub fn new(size: usize) -> Self {
        Self {
            memory: vec![0; size],
            data: [0; 8],
            addr: [0; 8],
            pc: 0,
            status: 0,
            run_state: RunState::Halt,
        }
    }
    pub fn load_memory(&mut self, offset: usize, memory: &[u8]) {
        for (i, byte) in memory.iter().enumerate() {
            self.memory[offset + i] = *byte
        }
    }
    pub fn reset(&mut self) {
        self.addr[7] = self.mem_i32(0);
        self.pc = self.mem_i32(4);
        self.run_state = RunState::Halt;
    }
    pub fn run(&mut self) {
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
            // immediate ops
            (0b000, _, _) => {
                let (size, dest) = self.mode_read_0(mode);
                let src = self.immediate_read(size);
                let op = match reg {
                    0b000 => |l, r| l | r,
                    0b001 => |l, r| l & r,
                    0b010 => |l, r| l - r,
                    0b011 => |l, r| l + r,
                    0b101 => |l, r| l ^ r,
                    0b110 => {
                        let left = self.ea_src(dest, size);
                        let right = self.ea_src(src, size);
                        self.set_cond(left - right);
                        return;
                    }
                    _ => unimplemented!(),
                };
                self.ea_apply(dest, src, size, op);
            }

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
                let src = self.ea_read(Size::Long);
                let dest = self.ea_read_inner(Size::Long, mode, reg);
                self.ea_apply(dest, src, Size::Long, |_, r| r);
            }
            // RTS/RTD/TRAP
            (0b0100, 0b111, 0b001) => {
                self.pc += 2;
                let lo6 = instr & 0b111111;
                match lo6 {
                    // TODO: "in-universe" traps
                    // TRAP #0 : halt
                    0 => {
                        self.run_state = RunState::Halt;
                    }
                    // TRAP #1 : assert_eq
                    1 => {
                        let left = self.pop_stack();
                        let right = self.pop_stack();
                        assert_eq!(left, right);
                    }
                    0b110101 => {
                        let ret = self.pop_stack();
                        self.pc = ret;
                    }
                    0b110100 => {
                        let disp = self.mem_u16(self.pc) as i32;
                        self.pc += 2;
                        let ret = self.pop_stack();
                        self.addr[7] += disp;
                        self.pc = ret;
                    }
                    _ => {}
                }
            }
            // JSR
            (0b0100, 0b111, 0b010) => {
                let addr = match self.ea_read(Size::Long) {
                    EAView::Memory(addr) => addr,
                    _ => unimplemented!(),
                };
                self.push_stack(self.pc);
                self.pc = addr;
            }
            // JMP
            (0b0100, 0b111, 0b011) => {
                let addr = match self.ea_read(Size::Long) {
                    EAView::Memory(addr) => addr,
                    _ => unimplemented!(),
                };
                self.pc = addr;
            }
            // PEA
            (0b0100, 0b100, 0b001) => {
                let addr = match self.ea_read(Size::Long) {
                    EAView::Memory(addr) => addr,
                    _ => unimplemented!(),
                };
                self.push_stack(addr);
            }
            // LEA
            (0b0100, _, 0b111) => {
                let addr = match self.ea_read(Size::Long) {
                    EAView::Memory(addr) => addr,
                    _ => unimplemented!(),
                };
                let dest = EAView::Addr(reg);
                self.ea_apply(dest, EAView::Immediate(addr), Size::Long, |_, x| x);
            }
            // NEG
            (0b0100, 0b010, _) => {
                let (size, dest) = self.mode_read_0(mode);
                self.ea_apply_1(dest, size, |x| -x);
            }
            // NOT
            (0b0100, 0b011, _) => {
                let (size, dest) = self.mode_read_0(mode);
                self.ea_apply_1(dest, size, |x| !x);
            }
            // CHK
            (0b0100, _, _) => {
                let (size, src) = match mode {
                    0b110 => (Size::Short, self.ea_read(Size::Short)),
                    0b100 => (Size::Long, self.ea_read(Size::Long)),
                    _ => unreachable!(),
                };
                let max = self.ea_src(src, size);
                let value = self.ea_src(EAView::Data(reg), size);
                if value < 0 || value > max {
                    panic!("CHK exception")
                }
            }
            // Control flow goes here
            // ADDQ / SUBQ
            (0b0101, _, _) => {
                let quick = (instr >> 9) & 0b111;
                let quick = if quick == 0 { 8 } else { quick as i32 };
                let mode = (instr >> 6) & 0b111;

                let (size, dest, is_add) = match mode {
                    0b000 => (Size::Byte, self.ea_read(Size::Byte), true),
                    0b001 => (Size::Short, self.ea_read(Size::Short), true),
                    0b010 => (Size::Long, self.ea_read(Size::Long), true),
                    0b100 => (Size::Byte, self.ea_read(Size::Byte), false),
                    0b101 => (Size::Short, self.ea_read(Size::Short), false),
                    0b110 => (Size::Long, self.ea_read(Size::Long), false),
                    // Scc goes here
                    _ => unreachable!(),
                };
                if is_add {
                    self.ea_apply(dest, EAView::Immediate(quick), size, |l, r| l + r);
                } else {
                    self.ea_apply(dest, EAView::Immediate(quick), size, |l, r| l - r);
                }
            }
            // BRA, BSR, Bcc
            (0b0110, _, _) => {
                let cond = (instr & 0x0F00) >> 8;
                let disp_8 = i8::from_be_bytes([instr.to_be_bytes()[1]]);
                self.pc += 2;

                let (if_true, if_false) = if disp_8 == 0 {
                    (self.pc + (self.mem_i16(self.pc) as i32), self.pc + 2)
                } else if disp_8 == -1 {
                    (self.pc + self.mem_i32(self.pc), self.pc + 4)
                } else {
                    (self.pc + disp_8 as i32, self.pc)
                };
                if cond == 1 {
                    self.push_stack(self.pc);
                    self.pc = if_true;
                    return;
                }
                if self.check_cond(Cond::from(cond)) {
                    self.pc = if_true
                } else {
                    self.pc = if_false
                }
            }
            // MOVEQ here
            // OR / DIV
            (0b1000, _, _) => {
                let (size, src, dest) = match mode {
                    0b000 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    0b001 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    0b010 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),
                    0b100 => (Size::Byte, EAView::Data(reg), self.ea_read(Size::Byte)),
                    0b101 => (Size::Short, EAView::Data(reg), self.ea_read(Size::Short)),
                    0b110 => (Size::Long, EAView::Data(reg), self.ea_read(Size::Long)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, src, size, |l, r| l | r);
            }
            // SUB / SUBA
            (0b1001, _, _) => {
                let (size, src, dest) = self.mode_read_data_dir(mode, reg);
                self.ea_apply(dest, src, size, |l, r| l - r);
            }
            // A-traps go here
            // CMP / CMPA / CMPM / EOR
            (0b1011, _, _) => {
                let reg_ay = (instr & 0b111) as usize;
                let is_cmpm = (instr & 0b111000) == 0b00100;

                let (size, src, dest) = match mode {
                    0b000 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    0b001 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    0b010 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),
                    0b011 => (Size::Short, self.ea_read(Size::Short), EAView::Addr(reg)),
                    0b111 => (Size::Long, self.ea_read(Size::Long), EAView::Addr(reg)),
                    0b100 => {
                        if !is_cmpm {
                            let dest = self.ea_read(Size::Byte);
                            self.ea_apply(dest, EAView::Data(reg), Size::Byte, |l, r| l ^ r);
                            return;
                        }
                        self.pc += 2;
                        let src = EAView::Memory(self.addr[reg]);
                        self.addr[reg] += 1;
                        let dest = EAView::Memory(self.addr[reg_ay]);
                        self.addr[reg] += 1;
                        (Size::Byte, src, dest)
                    }
                    0b101 => {
                        if !is_cmpm {
                            let dest = self.ea_read(Size::Short);
                            self.ea_apply(dest, EAView::Data(reg), Size::Short, |l, r| l ^ r);
                            return;
                        }
                        self.pc += 2;
                        let src = EAView::Memory(self.addr[reg]);
                        self.addr[reg] += 2;
                        let dest = EAView::Memory(self.addr[reg_ay]);
                        self.addr[reg] += 2;
                        (Size::Short, src, dest)
                    }
                    0b110 => {
                        if !is_cmpm {
                            let dest = self.ea_read(Size::Long);
                            self.ea_apply(dest, EAView::Data(reg), Size::Long, |l, r| l ^ r);
                            return;
                        }
                        self.pc += 2;
                        let src = EAView::Memory(self.addr[reg]);
                        self.addr[reg] += 4;
                        let dest = EAView::Memory(self.addr[reg_ay]);
                        self.addr[reg] += 4;
                        (Size::Long, src, dest)
                    }
                    _ => unreachable!(),
                };
                let left = self.ea_src(dest, size);
                let right = self.ea_src(src, size);
                self.set_cond(left - right);
            }
            // AND / MUL
            (0b1100, _, _) => {
                let exg_mode = instr >> 3 & 0b11111;
                let exg_reg = (instr & 0b111) as usize;

                let (size, src, dest) = match mode {
                    0b000 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    0b001 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    0b010 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),
                    0b011 => todo!("MULU"),
                    0b100 => (Size::Byte, EAView::Data(reg), self.ea_read(Size::Byte)),
                    0b101 => match exg_mode {
                        0b01000 => {
                            self.pc += 2;
                            self.ea_exg(EAView::Data(reg), EAView::Data(exg_reg));
                            return;
                        }
                        0b01001 => {
                            self.pc += 2;
                            self.ea_exg(EAView::Addr(reg), EAView::Addr(exg_reg));
                            return;
                        }
                        _ => (Size::Short, EAView::Data(reg), self.ea_read(Size::Short)),
                    },
                    0b110 => match exg_mode {
                        0b10001 => {
                            self.pc += 2;
                            self.ea_exg(EAView::Data(reg), EAView::Addr(exg_reg));
                            return;
                        }
                        _ => (Size::Long, EAView::Data(reg), self.ea_read(Size::Long)),
                    },
                    0b111 => todo!("MULS"),

                    _ => unreachable!(),
                };
                self.ea_apply(dest, src, size, |l, r| l & r);
            }
            // ADD / ADDA
            (0b1101, _, _) => {
                let (size, src, dest) = self.mode_read_data_dir(mode, reg);
                self.ea_apply(dest, src, size, |l, r| l + r);
            }
            // shifts
            (0b1110, _, _) => {
                let dest = EAView::Data(instr as usize & 0b111);
                let src_type = instr & 0b100000;
                let (to_left, size) = match mode {
                    0b000 => (false, Size::Byte),
                    0b001 => (false, Size::Short),
                    0b010 => (false, Size::Long),
                    0b100 => (true, Size::Byte),
                    0b101 => (true, Size::Short),
                    0b110 => (true, Size::Long),
                    _ => unimplemented!(),
                };
                let src = match (src_type, reg) {
                    (0, 0) => EAView::Immediate(8),
                    (0, n) => EAView::Immediate(n as i32),
                    (_, r) => EAView::Data(r),
                };
                self.pc += 2;
                if to_left {
                    self.ea_apply(dest, src, size, |l, r| l << r);
                } else {
                    self.ea_apply(dest, src, size, |l, r| l >> r);
                }
            }
            _ => unimplemented!(),
        }
    }
    fn set_cond(&mut self, value: i32) {
        // zero
        self.status = bit_set(self.status as isize, 2, value == 0) as u16;
        // negative
        self.status = bit_set(self.status as isize, 3, value < 0) as u16;
        // TODO: carry, overflow flags
    }
    fn check_cond(&self, cond: Cond) -> bool {
        use Cond::*;
        let c = self.status & 1 != 0;
        let v = self.status & 2 != 0;
        let z = self.status & 4 != 0;
        let n = self.status & 8 != 0;
        match cond {
            True => true,
            False => false,
            High => !c && !z,
            LowSame => c || z,

            CarryClear => !c,
            CarrySet => c,
            NotEqual => !z,
            Equal => z,

            OverflowClear => !v,
            OverflowSet => v,
            Plus => !n,
            Minus => n,

            GreaterEqual => z || (n && v) || (!n && !v),
            Less => (n && !v && !z) || (!n && v && !z),
            Greater => (n && v && !z) || (!n && !v && !z),
            LessEqual => z || (n && !v) || (!n && v),
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

    fn mode_read_0(&mut self, mode: u16) -> (Size, EAView) {
        match mode {
            0 => (Size::Byte, self.ea_read(Size::Byte)),
            1 => (Size::Short, self.ea_read(Size::Short)),
            2 => (Size::Long, self.ea_read(Size::Long)),
            _ => unreachable!(),
        }
    }
    fn mode_read_data_dir(&mut self, mode: u16, reg: usize) -> (Size, EAView, EAView) {
        match mode {
            0 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
            1 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
            2 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),

            4 => (Size::Byte, EAView::Data(reg), self.ea_read(Size::Byte)),
            5 => (Size::Short, EAView::Data(reg), self.ea_read(Size::Short)),
            6 => (Size::Long, EAView::Data(reg), self.ea_read(Size::Long)),

            3 => (Size::Short, self.ea_read(Size::Short), EAView::Addr(reg)),
            7 => (Size::Long, self.ea_read(Size::Long), EAView::Addr(reg)),
            _ => unreachable!(),
        }
    }
    fn stack(&self, offset: i32) -> i32 {
        self.mem_i32(self.addr[7] + offset)
    }
    fn pop_stack(&mut self) -> i32 {
        let val = self.stack(0);
        self.addr[7] += 4;
        val
    }
    fn push_stack(&mut self, value: i32) {
        self.addr[7] -= 4;
        self.ea_apply(
            EAView::Memory(self.addr[7]),
            EAView::Immediate(value),
            Size::Long,
            |_, x| x,
        );
    }
    fn ea_exg(&mut self, left: EAView, right: EAView) {
        let swap = self.ea_src(left, Size::Long);
        self.ea_apply(left, right, Size::Long, |_, x| x);
        self.ea_apply(right, EAView::Immediate(swap), Size::Long, |_, x| x);
    }
    fn ea_src(&self, src: EAView, size: Size) -> i32 {
        match src {
            EAView::Data(d) => self.data[d],
            EAView::Addr(a) => self.addr[a],
            EAView::Memory(i) => match size {
                Size::Byte => self.mem_u8(i) as i32,
                Size::Short => self.mem_i16(i) as i32,
                Size::Long => self.mem_i32(i),
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
                    Size::Long => {
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
    fn ea_apply_1(&mut self, dest: EAView, size: Size, apply: fn(i32) -> i32) {
        let prev = self.ea_src(dest, size);
        match dest {
            EAView::Data(d) => self.data[d] = apply(prev),
            EAView::Addr(a) => self.addr[a] = apply(prev),
            EAView::Memory(idx) => {
                let i = (idx & ADDRESS_MASK) as usize;
                let bytes = apply(prev).to_be_bytes();
                match size {
                    Size::Byte => {
                        self.memory[i] = bytes[3];
                    }
                    Size::Short => {
                        self.memory[i] = bytes[2];
                        self.memory[i + 1] = bytes[3];
                    }
                    Size::Long => {
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
                    Size::Long => (4, self.mem_i32(self.pc)),
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
            Size::Long => {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cond {
    True = 0x0,
    False,
    High,
    LowSame,

    CarryClear,
    CarrySet,
    NotEqual,
    Equal,

    OverflowClear,
    OverflowSet,
    Plus,
    Minus,

    GreaterEqual,
    Less,
    Greater,
    LessEqual = 0xF,
}

impl Cond {
    pub fn inverse(&self) -> Self {
        use Cond::*;
        match self {
            True => False,
            False => True,
            High => LowSame,
            LowSame => High,
            CarryClear => CarrySet,
            CarrySet => CarryClear,
            NotEqual => Equal,
            Equal => NotEqual,
            OverflowClear => OverflowSet,
            OverflowSet => OverflowClear,
            Plus => Minus,
            Minus => Plus,
            GreaterEqual => Less,
            Less => GreaterEqual,
            Greater => LessEqual,
            LessEqual => Greater,
        }
    }
}

impl From<u16> for Cond {
    fn from(value: u16) -> Self {
        use Cond::*;
        match value {
            0 => True,
            1 => False,
            2 => High,
            3 => LowSame,
            4 => CarryClear,
            5 => CarrySet,
            6 => NotEqual,
            7 => Equal,
            8 => OverflowClear,
            9 => OverflowSet,
            10 => Plus,
            11 => Minus,
            12 => GreaterEqual,
            13 => Less,
            14 => Greater,
            15 => LessEqual,
            _ => unreachable!(),
        }
    }
}

pub enum Branch {
    Displacement(Size, i32),
    Line(usize),
    Placeholder(Size),
}

#[derive(Debug, Default)]
pub struct Asm {
    pub out: Vec<u8>,
    pub base_offset: usize,
}

impl Asm {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn org(base_offset: usize) -> Self {
        Self {
            out: Vec::new(),
            base_offset,
        }
    }
    pub fn add(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(d), dest) => {
                let mode = self.mode_456(size);
                self.tag4_reg3_mode3_ea(0b1101, d as u16, mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                if let Some(q) = quick_positive(x) {
                    let mode = self.mode_012(size);
                    self.tag4_reg3_mode3_ea(0b0101, q, mode);
                    self.push_ea(size, dest);
                } else {
                    let mode = self.mode_012(size);
                    self.tag4_reg3_mode3_ea(0, 0b011, mode);
                    self.push_ea(size, dest);
                    self.push_immediate(size, x);
                }
            }
            (src, EA::Data(d)) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0b1101, d as u16, mode);
                self.push_ea(size, src);
            }
            (src, EA::Addr(a)) => {
                let mode = self.mode_37(size);
                self.tag4_reg3_mode3_ea(0b1101, a as u16, mode);
                self.push_ea(size, src);
            }
            _ => unimplemented!(),
        }
    }
    pub fn sub(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(d), dest) => {
                let mode = self.mode_456(size);
                self.tag4_reg3_mode3_ea(0b1001, d as u16, mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                if let Some(q) = quick_positive(x) {
                    let mode = self.mode_456(size);
                    self.tag4_reg3_mode3_ea(0b0101, q, mode);
                    self.push_ea(size, dest);
                } else {
                    let mode = self.mode_012(size);
                    self.tag4_reg3_mode3_ea(0, 0b010, mode);
                    self.push_ea(size, dest);
                    self.push_immediate(size, x);
                }
            }
            (src, EA::Data(d)) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0b1001, d as u16, mode);
                self.push_ea(size, src);
            }
            (src, EA::Addr(a)) => {
                let mode = self.mode_37(size);
                self.tag4_reg3_mode3_ea(0b1001, a as u16, mode);
                self.push_ea(size, src);
            }
            _ => unimplemented!(),
        }
    }
    pub fn mov(&mut self, size: Size, src: EA, dest: EA) {
        let tag = match size {
            Size::Byte => 0b0001,
            Size::Short => 0b0011,
            Size::Long => 0b0010,
        };
        let mut dest_dummy = vec![0];
        dest.write(size, &mut dest_dummy, 0);
        let reg = dest_dummy[0] & 0b111;
        let mode = (dest_dummy[0] >> 3) & 0b111;
        self.tag4_reg3_mode3_ea(tag, reg as u16, mode as u16);
        self.push_ea(size, src);
        self.out.extend(dest_dummy[1..].iter());
    }
    pub fn and(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(d), dest) => {
                let mode = self.mode_456(size);
                self.tag4_reg3_mode3_ea(0b1100, d as u16, mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0, 1, mode);
                self.push_ea(size, dest);
                self.push_immediate(size, x);
            }
            (src, EA::Data(d)) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0b1100, d as u16, mode);
                self.push_ea(size, src);
            }
            _ => unimplemented!(),
        }
    }
    pub fn asl(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(src), EA::Data(dest)) => {
                let mode = self.mode_456(size);
                self.tag4_reg3_mode3_ea(0b1110, src as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + 0b100000 + dest as u8);
            }
            (EA::Immediate(x), EA::Data(d)) if x > 0 && x <= 8 => {
                let mode = self.mode_456(size);
                let count = x & 0b111;
                self.tag4_reg3_mode3_ea(0b1110, count as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + d as u8);
            }
            // ASL <ea> not implemented
            _ => unimplemented!(),
        }
    }
    pub fn asr(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(src), EA::Data(dest)) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0b1110, src as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + 0b100000 + dest as u8);
            }
            (EA::Immediate(x), EA::Data(d)) if x > 0 && x <= 8 => {
                let mode = self.mode_012(size);
                let count = x & 0b111;
                self.tag4_reg3_mode3_ea(0b1110, count as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + d as u8);
            }
            // ASR <ea> not implemented
            _ => unimplemented!(),
        }
    }
    pub fn branch(&mut self, cond: Cond, branch: Branch) -> usize {
        let here = self.here();
        assert_ne!(cond, Cond::False);
        self.branch_internal(cond, branch);
        here
    }
    pub fn branch_sub(&mut self, branch: Branch) {
        self.branch_internal(Cond::False, branch)
    }
    fn branch_internal(&mut self, cond: Cond, branch: Branch) {
        let here = self.here() + 2;
        let offset = match branch {
            Branch::Displacement(_, x) => x,
            Branch::Line(line) => line as i32 - here as i32,
            Branch::Placeholder(Size::Byte) => 1,
            Branch::Placeholder(Size::Short) => 128,
            Branch::Placeholder(Size::Long) => 32768,
        };

        let disp_byte = match offset {
            -128..=127 => offset as i8,
            -32768..=32767 => 0,
            _ => -1,
        };
        self.tag4_cond_4_disp8(0b0110, cond as u8, disp_byte);
        if disp_byte == 0 {
            let bytes = (offset as i16).to_be_bytes();
            self.out.push(bytes[0]);
            self.out.push(bytes[1]);
        } else if disp_byte == -1 {
            let bytes = offset.to_be_bytes();
            self.out.push(bytes[0]);
            self.out.push(bytes[1]);
            self.out.push(bytes[2]);
            self.out.push(bytes[3]);
        }
    }
    pub fn chk(&mut self, size: Size, src: EA, target: EA) {
        match target {
            EA::Data(d) => {
                let mode = match size {
                    Size::Byte => unimplemented!(),
                    Size::Short => 0b110,
                    Size::Long => 0b100,
                };
                self.tag4_reg3_mode3_ea(0b0100, d as u16, mode);
                self.push_ea(size, src);
            }
            _ => unimplemented!(),
        }
    }
    pub fn cmp(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (src, EA::Data(d)) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0b1011, d as u16, mode);
                self.push_ea(size, src);
            }
            (src, EA::Addr(a)) => {
                let mode = match size {
                    Size::Byte => unimplemented!(),
                    Size::Short => 0b011,
                    Size::Long => 0b111,
                };
                self.tag4_reg3_mode3_ea(0b1011, a as u16, mode);
                self.push_ea(size, src);
            }
            (EA::Immediate(value), dest) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0, 0b110, mode);
                self.push_ea(size, dest);
                self.push_immediate(size, value);
            }
            (EA::PostInc(src), EA::PostInc(dest)) => {
                let mode = match size {
                    Size::Byte => 0b100,
                    Size::Short => 0b101,
                    Size::Long => 0b110,
                };
                self.tag4_reg3_mode3_ea(0b1011, dest as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + 0b1000 + src as u8);
            }
            _ => unimplemented!(),
        }
    }
    pub fn or(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(d), dest) => {
                let mode = match size {
                    Size::Byte => 0b100,
                    Size::Short => 0b101,
                    Size::Long => 0b110,
                };
                self.tag4_reg3_mode3_ea(0b1000, d as u16, mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0, 0, mode);
                self.push_ea(size, dest);
                self.push_immediate(size, x);
            }
            (src, EA::Data(d)) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0b1000, d as u16, mode);
                self.push_ea(size, src);
            }
            _ => unimplemented!(),
        }
    }
    pub fn xor(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(d), dest) => {
                let mode = match size {
                    Size::Byte => 0b100,
                    Size::Short => 0b101,
                    Size::Long => 0b110,
                };
                self.tag4_reg3_mode3_ea(0b1011, d as u16, mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                let mode = self.mode_012(size);
                self.tag4_reg3_mode3_ea(0, 0b101, mode);
                self.push_ea(size, dest);
                self.push_immediate(size, x);
            }
            _ => unimplemented!(),
        }
    }
    pub fn neg(&mut self, size: Size, dest: EA) {
        let mode = self.mode_012(size);
        self.tag4_reg3_mode3_ea(0b0100, 0b010, mode);
        self.push_ea(size, dest);
    }
    pub fn not(&mut self, size: Size, dest: EA) {
        let mode = self.mode_012(size);
        self.tag4_reg3_mode3_ea(0b0100, 0b011, mode);
        self.push_ea(size, dest);
    }
    pub fn load_ea(&mut self, src: EA, dest: EA) {
        match (src, dest) {
            (src, EA::Addr(a)) if src.is_control_mode() => {
                self.tag4_reg3_mode3_ea(0b0100, a as u16, 0b111);
                self.push_ea(Size::Long, src)
            }
            (src, EA::PreDec(Addr::A7)) if src.is_control_mode() => {
                self.tag4_reg3_mode3_ea(0b0100, 0b100, 0b001);
                self.push_ea(Size::Long, src)
            }
            _ => unimplemented!(),
        }
    }
    pub fn exg(&mut self, src: EA, dest: EA) {
        let (mode, l, r) = match (src, dest) {
            (EA::Data(l), EA::Data(r)) => (0b101000, l as u16, r as u16),
            (EA::Addr(l), EA::Addr(r)) => (0b101001, l as u16, r as u16),
            (EA::Data(d), EA::Addr(a)) => (0b110001, d as u16, a as u16),
            (EA::Addr(a), EA::Data(d)) => (0b110001, d as u16, a as u16),
            _ => unimplemented!(),
        };
        let code = (0b1100 << 12) + (l << 9) + (mode << 3) + r;
        self.push_u16(code);
    }
    pub fn jmp(&mut self, src: EA) {
        self.tag4_reg3_mode3_ea(0b0100, 0b111, 0b011);
        if src.is_control_mode() {
            self.push_ea(Size::Long, src);
        } else {
            unimplemented!()
        }
    }
    pub fn jsr(&mut self, src: EA) {
        self.tag4_reg3_mode3_ea(0b0100, 0b111, 0b010);
        if src.is_control_mode() {
            self.push_ea(Size::Long, src);
        } else {
            unimplemented!()
        }
    }
    pub fn ret(&mut self, to_drop: u16) {
        if to_drop == 0 {
            self.push_u16(0b0100_1110_0111_0101);
        } else {
            self.push_u16(0b0100_1110_0111_0100);
            self.push_u16(to_drop);
        }
    }
    pub fn halt(&mut self) {
        // TRAP #0
        self.push_u16(0b0100_1110_0100_0000);
    }
    pub fn assert_eq(&mut self) {
        // TRAP #1
        self.push_u16(0b0100_1110_0100_0001);
    }
    // todo BSET, BCHG, BCLR, BTST
    pub fn data(&mut self, data: &[u8]) -> usize {
        let here = self.here();
        for byte in data {
            self.out.push(*byte);
        }
        if (self.here() & 1) == 1 {
            self.out.push(0);
        }
        here
    }
    pub fn data_16(&mut self, data: &[i16]) -> usize {
        let here = self.here();
        for d in data {
            self.out.extend(d.to_be_bytes())
        }
        here
    }
    pub fn data_32(&mut self, data: &[i32]) -> usize {
        let here = self.here();
        for d in data {
            self.out.extend(d.to_be_bytes());
        }
        here
    }

    pub fn here(&self) -> usize {
        self.out.len() + self.base_offset
    }
    pub fn fixup(&mut self, at: usize, f: impl Fn(&mut Asm)) {
        let mut a = Asm::org(at);
        f(&mut a);
        for (i, byte) in a.out.into_iter().enumerate() {
            self.out[at + i] = byte;
        }
    }
    pub fn fixup_branch_to_here(&mut self, at: usize) {
        let cond = Cond::from(self.out[at - self.base_offset] as u16 & 0x0F);
        let here = self.here();
        self.fixup(at, |asm| {
            // TODO: check that placeholder displacement slot can fit this value
            asm.branch(cond, Branch::Line(here));
        });
    }

    // instruction formats
    fn tag4_reg3_mode3_ea(&mut self, tag: u16, register: u16, mode: u16) {
        let base = (tag << 12) + (register << 9) + (mode << 6);
        self.push_u16(base);
    }
    fn tag4_cond_4_disp8(&mut self, tag: u8, cond: u8, disp: i8) {
        let disp_bytes = disp.to_be_bytes();
        let base = u16::from_be_bytes([(tag << 4) + cond, disp_bytes[0]]);
        self.push_u16(base)
    }
    fn mode_012(&self, size: Size) -> u16 {
        match size {
            Size::Byte => 0b000,
            Size::Short => 0b001,
            Size::Long => 0b010,
        }
    }
    fn mode_456(&self, size: Size) -> u16 {
        match size {
            Size::Byte => 0b100,
            Size::Short => 0b101,
            Size::Long => 0b110,
        }
    }
    fn mode_37(&self, size: Size) -> u16 {
        match size {
            Size::Byte => unimplemented!(),
            Size::Short => 0b011,
            Size::Long => 0b111,
        }
    }
    fn push_u16(&mut self, value: u16) {
        let bytes = value.to_be_bytes();
        self.out.push(bytes[0]);
        self.out.push(bytes[1]);
    }
    fn push_ea(&mut self, size: Size, ea: EA) {
        let here = self.here();
        ea.write(size, &mut self.out, here);
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
            Size::Long => {
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

    fn init_vm(asm: &Asm) -> VM {
        let mut vm = VM::new(256);
        let init_sp = 120;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        vm.load_memory(init_pc as usize, &asm.out);
        vm
    }

    enum T {
        Data(i32),
        Addr(i32),
        Immediate(i32),
        Memory(i32),
    }

    type TestOp = fn(&mut Asm, Size, EA, EA);
    fn test_case(op: TestOp, size: Size, src: T, dest: T, expected: i32) {
        let mut asm = Asm::new();
        asm.mov(Size::Long, Immediate(0), PreDec(A7));
        asm.mov(Size::Long, Immediate(0), PreDec(A7));
        let src_ea = match src {
            T::Data(x) => {
                asm.mov(size, Immediate(x), Data(D0));
                Data(D0)
            }
            T::Addr(x) => {
                asm.mov(size, Immediate(x), Addr(A0));
                Addr(A0)
            }
            T::Memory(x) => {
                asm.mov(size, Immediate(x), Offset(A7, 4));
                Offset(A7, 4)
            }
            T::Immediate(x) => Immediate(x),
        };
        let dest_ea = match dest {
            T::Data(x) => {
                asm.mov(size, Immediate(x), Data(D1));
                Data(D1)
            }
            T::Addr(x) => {
                asm.mov(size, Immediate(x), Addr(A1));
                Addr(A1)
            }
            T::Memory(x) => {
                asm.mov(size, Immediate(x), Offset(A7, 0));
                Offset(A7, 0)
            }
            T::Immediate(_) => unimplemented!(),
        };

        op(&mut asm, size, src_ea, dest_ea);
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();

        match dest {
            T::Data(_) => {
                assert_eq!(vm.data[1], expected);
            }
            T::Addr(_) => {
                assert_eq!(vm.addr[1], expected);
            }
            T::Memory(_) => {
                assert_eq!(vm.stack(0), expected);
            }
            _ => unimplemented!(),
        }
    }

    #[test]
    fn bytes() {
        let mut asm = Asm::new();
        asm.mov(Byte, Immediate(0), Data(D0));

        let instr = asm.here();
        asm.add(Byte, PCOffset(PC::Displacement(0)), Data(D0));
        asm.add(Byte, PCOffset(PC::Displacement(0)), Data(D0));
        asm.add(Byte, PCOffset(PC::Displacement(0)), Data(D0));
        asm.halt();

        let data = asm.here();
        asm.data(&[10, 20, 30]);
        asm.fixup(instr, |asm| {
            asm.add(Byte, PCOffset(PC::Line(data)), Data(D0));
            asm.add(Byte, PCOffset(PC::Line(data + 1)), Data(D0));
            asm.add(Byte, PCOffset(PC::Line(data + 2)), Data(D0));
        });

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 60);
    }

    #[test]
    fn and() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(0b1100), Data(D0));
        asm.mov(Long, Immediate(0b1010), Data(D1));
        asm.mov(Long, Immediate(0b1001), PreDec(A7));
        asm.mov(Long, Immediate(0b0110), PreDec(A7));

        // data to memory
        asm.and(Long, Data(D0), Offset(A7, 0));
        // memory to data
        asm.and(Long, Offset(A7, 4), Data(D1));
        // immediate to memory
        asm.and(Long, Immediate(0b0101), Offset(A7, 4));
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();

        // 1100 & 0110 -> 0100
        assert_eq!(vm.stack(0), 0b0100);
        // 1001 & 1010 -> 1000
        assert_eq!(vm.data[1], 0b1000);
        // 0101 & 1001 -> 0001
        assert_eq!(vm.stack(4), 0b0001);
    }

    #[test]
    fn shift() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(3), Data(D0));
        asm.mov(Long, Immediate(4), Data(D1));
        asm.asl(Long, Data(D1), Data(D0));
        asm.asl(Long, Immediate(1), Data(D0));
        asm.halt();

        asm.asr(Long, Data(D1), Data(D0));
        asm.asr(Long, Immediate(1), Data(D0));
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 3 << 5);
        vm.run();
        assert_eq!(vm.data[0], 3);
    }

    #[test]
    fn branch_unconditional() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(1), Data(D0));

        let branch = asm.branch(Cond::True, Branch::Placeholder(Size::Byte));
        asm.mov(Long, Immediate(2), Data(D0));
        asm.fixup_branch_to_here(branch);
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 1);
    }

    #[test]
    fn branch_conditional() {
        let mut asm = Asm::new();

        asm.mov(Long, Immediate(1), Data(D0));
        asm.cmp(Long, Immediate(3), Data(D0));

        let branch = asm.branch(Cond::Equal, Branch::Placeholder(Size::Byte));
        asm.mov(Long, Immediate(2), Data(D0));
        asm.fixup_branch_to_here(branch);
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 2);
    }

    #[test]
    fn chk() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(10), Data(D0));
        asm.chk(Long, Immediate(20), Data(D0));
        asm.halt();
        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 10);
    }

    #[test]
    #[should_panic]
    fn chk_overflow() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(100), Data(D0));
        asm.chk(Long, Immediate(20), Data(D0));
        asm.halt();
        let mut vm = init_vm(&asm);
        vm.run();
    }

    #[test]
    #[should_panic]
    fn chk_underflow() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(-1), Data(D0));
        asm.chk(Long, Immediate(20), Data(D0));
        asm.halt();
        let mut vm = init_vm(&asm);
        vm.run();
    }

    #[test]
    fn or() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(0b0001), Data(D0));
        asm.mov(Long, Immediate(0b0010), Data(D1));
        asm.or(Long, Data(D1), Data(D0));
        asm.or(Long, Immediate(0b1000), Data(D1));
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 0b0011);
        assert_eq!(vm.data[1], 0b1010);
    }

    #[test]
    fn xor() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(0b0001), Data(D0));
        asm.mov(Long, Immediate(0b0011), Data(D1));
        asm.xor(Long, Data(D1), Data(D0));
        asm.xor(Long, Immediate(0b1010), Data(D1));
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 0b0010);
        assert_eq!(vm.data[1], 0b1001);
    }

    #[test]
    fn neg_not() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(1), Data(D0));
        asm.neg(Long, Data(D0));
        asm.mov(Long, Immediate(1), Data(D1));
        asm.not(Long, Data(D1));
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], -1);
        assert_eq!(vm.data[1], !1);
    }

    #[test]
    fn add() {
        let cases = vec![
            (T::Immediate(5), T::Data(10), 15),
            (T::Immediate(20), T::Data(10), 30),
            (T::Data(5), T::Data(10), 15),
            (T::Memory(5), T::Addr(10), 15),
            (T::Memory(10), T::Data(20), 30),
            (T::Data(10), T::Memory(20), 30),
        ];

        for (src, dest, expected) in cases {
            test_case(Asm::add, Size::Long, src, dest, expected)
        }
    }

    #[test]
    fn sub() {
        let cases = vec![
            (T::Immediate(5), T::Data(10), 5),
            (T::Immediate(20), T::Data(10), -10),
            (T::Data(5), T::Data(10), 5),
            (T::Memory(5), T::Addr(10), 5),
            (T::Memory(10), T::Data(20), 10),
            (T::Data(10), T::Memory(20), 10),
        ];

        for (src, dest, expected) in cases {
            test_case(Asm::sub, Size::Long, src, dest, expected)
        }
    }

    #[test]
    fn load_ea() {
        let mut asm = Asm::new();
        asm.load_ea(EA::Absolute(1234), EA::Addr(A0));
        asm.load_ea(EA::Absolute(5678), EA::PreDec(A7));
        asm.halt();

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.addr[0], 1234);
        assert_eq!(vm.stack(0), 5678);
    }

    #[test]
    fn exg() {
        let cases = vec![
            (T::Data(1), T::Data(2)),
            (T::Addr(0x00F0_FF00), T::Addr(0x00C0_CC00)),
            (T::Data(1), T::Addr(0x00F0_FF00)),
            (T::Addr(0x00F0_FF00), T::Data(1)),
        ];

        for (src, dest) in cases {
            let mut asm = Asm::new();
            let size = Size::Long;
            let (left_ea, left_val) = match src {
                T::Data(x) => {
                    asm.mov(size, Immediate(x), Data(D0));
                    (Data(D0), x)
                }
                T::Addr(x) => {
                    asm.mov(size, Immediate(x), Addr(A0));
                    (Addr(A0), x)
                }
                _ => unreachable!(),
            };
            let (right_ea, right_val) = match dest {
                T::Data(x) => {
                    asm.mov(size, Immediate(x), Data(D1));
                    (Data(D1), x)
                }
                T::Addr(x) => {
                    asm.mov(size, Immediate(x), Addr(A1));
                    (Addr(A1), x)
                }
                _ => unreachable!(),
            };
            asm.exg(left_ea, right_ea);
            asm.halt();
            let mut vm = init_vm(&asm);
            vm.run();
            let left_result = match src {
                T::Data(_) => vm.data[0],
                T::Addr(_) => vm.addr[0],
                _ => unreachable!(),
            };
            let right_result = match dest {
                T::Data(_) => vm.data[1],
                T::Addr(_) => vm.addr[1],
                _ => unreachable!(),
            };
            assert_eq!(left_result, right_val);
            assert_eq!(right_result, left_val);
        }
    }

    #[test]
    fn subroutines() {
        let mut asm = Asm::new();

        let to_main = asm.here();
        asm.jmp(EA::PCOffset(PC::Displacement(0)));

        // inc(D0) -> D0
        let inc = asm.here();
        asm.add(Long, Immediate(1), Data(D0));
        asm.ret(0);

        // add2(M, M) -> M
        let add2 = asm.here();
        asm.mov(Long, Offset(A7, 4), Data(D0));
        asm.add(Long, Offset(A7, 8), Data(D0));
        asm.mov(Long, Data(D0), Offset(A7, 12));
        asm.ret(8);

        // main()
        let main = asm.here();
        asm.fixup(to_main, |asm| {
            asm.jmp(EA::PCOffset(PC::Line(main)));
        });

        asm.mov(Long, Immediate(10), Data(D0));
        asm.branch_sub(Branch::Line(inc));

        asm.sub(Long, Immediate(4), Addr(A7)); // ret slot
        asm.mov(Long, Immediate(4), PreDec(A7)); // lhs
        asm.mov(Long, Data(D0), PreDec(A7)); // rhs
        asm.jsr(PCOffset(PC::Line(add2)));

        asm.mov(Long, Offset(A7, 0), PreDec(A7));
        asm.mov(Long, Immediate(15), PreDec(A7));
        asm.assert_eq();

        asm.halt();

        init_vm(&asm).run();
    }
}
