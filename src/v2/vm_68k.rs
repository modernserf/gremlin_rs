use super::ea::*;

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

struct VM {
    memory: Vec<u8>,
    data: [i32; 8],
    addr: [i32; 8],
    pc: i32,
    status: u16,
    run_state: RunState,
}

impl VM {
    fn new(size: usize) -> Self {
        Self {
            memory: vec![0; size],
            data: [0; 8],
            addr: [0; 8],
            pc: 0,
            status: 0,
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
                let src = self.ea_read(Size::Long);
                let dest = self.ea_read_inner(Size::Long, mode, reg);
                self.ea_apply(dest, src, Size::Long, |_, r| r);
            }
            // TRAP
            (0b0100, 0b111, 0b001) => {
                self.pc += 2;
                // TODO: "in-universe" halt
                self.run_state = RunState::Halt;
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
            // ADDI
            (0b0000, 0b011, _) => {
                let (size, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte)),
                    1 => (Size::Short, self.ea_read(Size::Short)),
                    2 => (Size::Long, self.ea_read(Size::Long)),
                    _ => unreachable!(),
                };
                let src = self.immediate_read(size);
                self.ea_apply(dest, src, size, |l, r| l + r);
            }
            // ANDI
            (0b0000, 0b001, _) => {
                let (size, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte)),
                    1 => (Size::Short, self.ea_read(Size::Short)),
                    2 => (Size::Long, self.ea_read(Size::Long)),
                    _ => unreachable!(),
                };
                let src = self.immediate_read(size);
                self.ea_apply(dest, src, size, |l, r| l & r);
            }
            // ADDQ
            (0b0101, _, _) => {
                let quick = (instr >> 9) & 0b111;
                let quick = if quick == 0 { 8 } else { quick as i32 };
                let mode = (instr >> 6) & 0b11;
                let (size, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte)),
                    1 => (Size::Short, self.ea_read(Size::Short)),
                    2 => (Size::Long, self.ea_read(Size::Long)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, EAView::Immediate(quick), size, |l, r| l + r);
            }
            // AND
            (0b1100, _, _) => {
                let (size, src, dest) = match mode {
                    0b000 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    0b001 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    0b010 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),
                    0b100 => (Size::Byte, EAView::Data(reg), self.ea_read(Size::Byte)),
                    0b101 => (Size::Short, EAView::Data(reg), self.ea_read(Size::Short)),
                    0b110 => (Size::Long, EAView::Data(reg), self.ea_read(Size::Long)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, src, size, |l, r| l & r);
            }
            // ADD / ADDA
            (0b1101, _, _) => {
                let (size, src, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    1 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    2 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),
                    3 => (Size::Byte, EAView::Data(reg), self.ea_read(Size::Byte)),
                    4 => (Size::Short, EAView::Data(reg), self.ea_read(Size::Short)),
                    5 => (Size::Long, EAView::Data(reg), self.ea_read(Size::Long)),
                    6 => (Size::Short, self.ea_read(Size::Short), EAView::Addr(reg)),
                    7 => (Size::Long, self.ea_read(Size::Long), EAView::Addr(reg)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, src, size, |l, r| l + r);
            }
            // CMP / CMPA / CMPM
            (0b1011, _, _) => {
                let reg_ay = (instr & 0b111) as usize;

                let (size, src, dest) = match mode {
                    0b000 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg)),
                    0b001 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg)),
                    0b010 => (Size::Long, self.ea_read(Size::Long), EAView::Data(reg)),
                    0b011 => (Size::Short, self.ea_read(Size::Short), EAView::Addr(reg)),
                    0b111 => (Size::Long, self.ea_read(Size::Long), EAView::Addr(reg)),
                    0b100 => {
                        self.pc += 2;
                        let src = EAView::Memory(self.addr[reg]);
                        self.addr[reg] += 1;
                        let dest = EAView::Memory(self.addr[reg_ay]);
                        self.addr[reg] += 1;
                        (Size::Byte, src, dest)
                    }
                    0b101 => {
                        self.pc += 2;
                        let src = EAView::Memory(self.addr[reg]);
                        self.addr[reg] += 2;
                        let dest = EAView::Memory(self.addr[reg_ay]);
                        self.addr[reg] += 2;
                        (Size::Short, src, dest)
                    }
                    0b110 => {
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
                self.set_status_cond(left - right);
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
            // BRA, BSR, Bcc
            (0b0110, _, _) => {
                let cond = (instr & 0x0F00) >> 8;
                if cond == 1 {
                    todo!("BSR")
                }

                let disp_8 = i8::from_be_bytes([instr.to_be_bytes()[1]]);
                self.pc += 2;

                let (if_true, if_false) = if disp_8 == 0 {
                    (self.pc + (self.mem_i16(self.pc) as i32), self.pc + 2)
                } else if disp_8 == -1 {
                    (self.pc + self.mem_i32(self.pc), self.pc + 4)
                } else {
                    (self.pc + disp_8 as i32, self.pc)
                };
                if self.check_cond(Cond::from(cond)) {
                    self.pc = if_true
                } else {
                    self.pc = if_false
                }
            }
            _ => unimplemented!(),
        }
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

            GreaterEqual => (n && v) || (!n && !v),
            Less => (n && !v) || (!n && v),
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
    fn set_status_cond(&mut self, value: i32) {
        // zero
        self.status = bit_set(self.status as isize, 2, value == 0) as u16;
        // negative
        self.status = bit_set(self.status as isize, 3, value == 0) as u16;
        // TODO: carry, overflow flags
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
enum Cond {
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
                    Size::Long => 2,
                };
                self.tag4_reg3_mode3_ea(0b1101, d as u16, op_mode);
                self.push_ea(size, src);
            }
            (src, EA::Addr(a)) => {
                let op_mode = match size {
                    Size::Byte => unimplemented!(),
                    Size::Short => 6,
                    Size::Long => 7,
                };
                self.tag4_reg3_mode3_ea(0b1101, a as u16, op_mode);
                self.push_ea(size, src);
            }
            (EA::Data(d), dest) => {
                let op_mode = match size {
                    Size::Byte => 3,
                    Size::Short => 4,
                    Size::Long => 5,
                };
                self.tag4_reg3_mode3_ea(0b1101, d as u16, op_mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                if let Some(q) = quick_positive(x) {
                    let mode = match size {
                        Size::Byte => 0,
                        Size::Short => 1,
                        Size::Long => 2,
                    };
                    self.tag4_reg3_mode3_ea(0b0101, q, mode);
                    self.push_ea(size, dest);
                } else {
                    let mode = match size {
                        Size::Byte => 0,
                        Size::Short => 1,
                        Size::Long => 2,
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
            Size::Long => 0b0010,
        };
        let mut dest_dummy = vec![0];
        dest.write(size, &mut dest_dummy);
        let reg = dest_dummy[0] & 0b111;
        let mode = (dest_dummy[0] >> 3) & 0b111;
        self.tag4_reg3_mode3_ea(tag, reg as u16, mode as u16);
        self.push_ea(size, src);
        self.out.extend(dest_dummy[1..].iter());
    }
    pub fn and(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(d), dest) => {
                let mode = match size {
                    Size::Byte => 0b100,
                    Size::Short => 0b101,
                    Size::Long => 0b110,
                };
                self.tag4_reg3_mode3_ea(0b1100, d as u16, mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                let mode = match size {
                    Size::Byte => 0b000,
                    Size::Short => 0b001,
                    Size::Long => 0b010,
                };
                self.tag4_reg3_mode3_ea(0, 1, mode);
                self.push_ea(size, dest);
                self.push_immediate(size, x);
            }
            (src, EA::Data(d)) => {
                let mode = match size {
                    Size::Byte => 0b000,
                    Size::Short => 0b001,
                    Size::Long => 0b010,
                };
                self.tag4_reg3_mode3_ea(0b1100, d as u16, mode);
                self.push_ea(size, src);
            }
            _ => unimplemented!(),
        }
    }
    pub fn asl(&mut self, size: Size, src: EA, dest: EA) {
        match (src, dest) {
            (EA::Data(src), EA::Data(dest)) => {
                let mode = match size {
                    Size::Byte => 0b100,
                    Size::Short => 0b101,
                    Size::Long => 0b110,
                };
                self.tag4_reg3_mode3_ea(0b1110, src as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + 0b100000 + dest as u8);
            }
            (EA::Immediate(x), EA::Data(d)) if x > 0 && x <= 8 => {
                let mode = match size {
                    Size::Byte => 0b100,
                    Size::Short => 0b101,
                    Size::Long => 0b110,
                };
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
                let mode = match size {
                    Size::Byte => 0b000,
                    Size::Short => 0b001,
                    Size::Long => 0b010,
                };
                self.tag4_reg3_mode3_ea(0b1110, src as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + 0b100000 + dest as u8);
            }
            (EA::Immediate(x), EA::Data(d)) if x > 0 && x <= 8 => {
                let mode = match size {
                    Size::Byte => 0b000,
                    Size::Short => 0b001,
                    Size::Long => 0b010,
                };
                let count = x & 0b111;
                self.tag4_reg3_mode3_ea(0b1110, count as u16, mode);
                let byte = self.out.pop().unwrap();
                self.out.push(byte + d as u8);
            }
            // ASR <ea> not implemented
            _ => unimplemented!(),
        }
    }
    pub fn branch(&mut self, cond: Cond, offset: i32) {
        assert_ne!(cond, Cond::False);
        self.branch_internal(cond, offset)
    }
    pub fn branch_sub(&mut self, offset: i32) {
        self.branch_internal(Cond::False, offset)
    }
    fn branch_internal(&mut self, cond: Cond, offset: i32) {
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
                let mode = match size {
                    Size::Byte => 0b000,
                    Size::Short => 0b001,
                    Size::Long => 0b010,
                };
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
                let mode = match size {
                    Size::Byte => 0b000,
                    Size::Short => 0b001,
                    Size::Long => 0b010,
                };
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
    pub fn halt(&mut self) {
        // TRAP #0
        self.push_u16(0b0100_1110_0100_0000);
    }
    // todo BSET, BCHG, BCLR, BTST
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
    fn tag4_cond_4_disp8(&mut self, tag: u8, cond: u8, disp: i8) {
        let disp_bytes = disp.to_be_bytes();
        let base = u16::from_be_bytes([(tag << 4) + cond, disp_bytes[0]]);
        self.push_u16(base)
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

    impl VM {
        fn stack(&self, offset: i32) -> i32 {
            self.mem_i32(self.addr[7] + offset)
        }
    }

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

    #[test]
    fn smoke_test() {
        let mut asm = Asm::new();
        asm.mov(Long, Immediate(10), Data(D0));
        asm.mov(Long, Immediate(20), Data(D1));
        asm.add(Long, Data(D0), Data(D1));
        asm.halt();

        asm.mov(Long, Immediate(30), Data(D0));
        asm.mov(Long, Immediate(10), Addr(A0));
        asm.add(Long, Data(D0), Addr(A0));
        asm.halt();

        asm.mov(Long, Immediate(10), PreDec(A7));
        asm.add(Long, Immediate(5), Offset(A7, 0));
        asm.halt();

        asm.mov(Long, Immediate(10), Offset(A7, 0));
        asm.mov(Long, Immediate(20), Data(D2));
        asm.add(Long, PostInc(A7), Data(D2));
        asm.halt();

        asm.mov(Long, Immediate(10), PreDec(A7));
        asm.mov(Long, Immediate(0), PreDec(A7));
        asm.add(Long, Immediate(20), Offset(A7, 4));
        asm.halt();

        let mut vm = init_vm(&asm);
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
            let base_offset = (data - instr - 2) as i16;
            asm.add(Byte, PCOffset(base_offset), Data(D0));
            asm.add(Byte, PCOffset(base_offset - 4 + 1), Data(D0));
            asm.add(Byte, PCOffset(base_offset - 8 + 2), Data(D0));
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

        let branch_before = asm.here();
        asm.branch(Cond::True, 1);
        asm.mov(Long, Immediate(2), Data(D0));
        let branch_dest = asm.here();
        asm.halt();
        asm.fixup(branch_before, |asm| {
            let disp = branch_dest - branch_before - 2;
            asm.branch(Cond::True, disp as i32);
        });

        let mut vm = init_vm(&asm);
        vm.run();
        assert_eq!(vm.data[0], 1);
    }

    #[test]
    fn branch_conditional() {
        let mut asm = Asm::new();

        asm.mov(Long, Immediate(1), Data(D0));
        asm.cmp(Long, Immediate(3), Data(D0));

        let branch_before = asm.here();
        asm.branch(Cond::Equal, 1);
        asm.mov(Long, Immediate(2), Data(D0));
        let branch_dest = asm.here();
        asm.halt();
        asm.fixup(branch_before, |asm| {
            let disp = branch_dest - branch_before - 2;
            asm.branch(Cond::Equal, disp as i32);
        });

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
}
