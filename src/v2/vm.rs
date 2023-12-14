pub type Word = i32;
pub type Byte = u8;
pub const WORD_BYTES: Word = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EA {
    Immediate(Word),
    Data(Data),
    Address(Address),
    Offset(Address, Word),
    OffsetIndexed(Address, Data, Word),
    PreDec(Address),
    PostInc(Address),
    PCIndexed(Data, Word),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Data {
    D0 = 0, // 1st data arg or return value
    D1,     // 2nd data arg or array indexing
    D2,     // locals
    D3,
    D4,
    D5,
    D6,
    D7,
}

pub const DATA_IDX: [Data; 8] = [
    Data::D0,
    Data::D1,
    Data::D2,
    Data::D3,
    Data::D4,
    Data::D5,
    Data::D6,
    Data::D7,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Address {
    A0 = 0, // 1st ptr arg or return value
    A1,     // 2nd ptr arg or dereferencing ptr on stack
    A2,     // locals
    A3,
    A4,
    A5, // context pointer
    A6, // frame pointer
    A7, // stack pointer
}

pub const ADDR_IDX: [Address; 8] = [
    Address::A0,
    Address::A1,
    Address::A2,
    Address::A3,
    Address::A4,
    Address::A5,
    Address::A6,
    Address::A7,
];

#[derive(Debug)]
pub enum Cond {
    Always,
    Zero,
    NotZero,
    Negative,
    NegativeZero,
    Positive,
    PositiveZero,
}

enum Op {
    Halt = 0x00,
    MovI = 0x01,
    Mov,
    Lea,
    Pea,
    MoveMPush,
    MoveMPop,
    LeaPC,
    AddI = 0x10,
    Add,
    SubI,
    Sub,
    MulI,
    Mul,
    DivI,
    Div,
    CmpI,
    Cmp,
    Neg,
    AndI = 0x20,
    And,
    OrI,
    Or,
    XorI,
    Xor,
    Not,
    Branch = 0x30,
    BranchZero = 0x31,
    BranchNotZero = 0x32,
    BranchNegative = 0x33,
    BranchNegativeZero = 0x34,
    BranchPositive = 0x35,
    BranchPositiveZero = 0x36,

    BranchSubroutine = 0x3F,
    JumpAddress = 0x40,
    // JumpSubroutineAddress = 0x41,
    Return = 0x42,
    ReturnAndDeallocate = 0x43,
    Link = 0x44,
    Unlink = 0x45,
}

impl Op {
    fn from_byte(byte: Byte) -> Self {
        use Op::*;
        match byte {
            0 => Halt,
            1 => MovI,
            2 => Mov,
            3 => Lea,
            4 => Pea,
            5 => MoveMPush,
            6 => MoveMPop,
            7 => LeaPC,
            0x05..=0x0F => unimplemented!(),
            0x10 => AddI,
            0x11 => Add,
            0x12 => SubI,
            0x13 => Sub,
            0x14 => MulI,
            0x15 => Mul,
            0x16 => DivI,
            0x17 => Div,
            0x18 => CmpI,
            0x19 => Cmp,
            0x1A => Neg,
            0x1B..=0x1F => unimplemented!(),
            0x20 => AndI,
            0x21 => And,
            0x22 => OrI,
            0x23 => Or,
            0x24 => XorI,
            0x25 => Xor,
            0x26 => Not,
            0x27..=0x2F => unimplemented!(),
            0x30 => Branch,
            0x31 => BranchZero,         // ==
            0x32 => BranchNotZero,      // !=
            0x33 => BranchNegative,     // <
            0x34 => BranchNegativeZero, // <=
            0x35 => BranchPositive,     // >
            0x36 => BranchPositiveZero, // >=
            0x37..=0x3E => unimplemented!(),
            0x3f => BranchSubroutine,
            0x40 => JumpAddress,
            0x42 => Return,
            0x43 => ReturnAndDeallocate,
            0x44 => Link,
            0x45 => Unlink,
            _ => unreachable!(),
        }
    }
}

fn write_32_le(value: Word, out: &mut Vec<Byte>) {
    let bytes = value.to_le_bytes();
    out.extend(bytes);
}

fn read_32_le(i: &mut Word, data: &[Byte]) -> Word {
    let idx = *i as usize;
    let out = Word::from_le_bytes([data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]);
    *i += 4;
    out
}

fn write_16_le(value_32: Word, out: &mut Vec<Byte>) {
    let value = value_32 as i16;
    out.extend(value.to_le_bytes());
}

fn read_16_le(i: &mut Word, data: &[Byte]) -> Word {
    let idx = *i as usize;
    let out = i16::from_le_bytes([data[idx], data[idx + 1]]);
    *i += 2;
    out as Word
}

fn pack_lea_pair(src: Address, dest: Address, out: &mut Vec<Byte>) {
    let packed = ((dest as Byte) << 4) + src as Byte;
    out.push(packed);
}

fn unpack_lea_pair(i: &mut Word, data: &[Byte]) -> (Address, Address) {
    let byte = data[*i as usize];
    *i += 1;
    let src = ADDR_IDX[(byte & 0x0F) as usize];
    let dest = ADDR_IDX[((byte & 0xF0) >> 4) as usize];
    (src, dest)
}

impl EA {
    pub fn offset(self, offset: Word) -> Option<Self> {
        match self {
            Self::Offset(a, o) => Some(Self::Offset(a, o + offset)),
            ea => {
                if offset == 0 {
                    Some(ea)
                } else {
                    None
                }
            }
        }
    }
    fn write(self, out: &mut Vec<Byte>) {
        match self {
            EA::Data(data) => {
                out.push(data as Byte);
            }
            EA::Address(addr) => out.push(addr as Byte + 8),
            EA::PreDec(addr) => out.push(addr as Byte + 16),
            EA::PostInc(addr) => out.push(addr as Byte + 24),
            EA::Offset(addr, offset) => {
                out.push(addr as Byte + 32);
                write_16_le(offset, out);
            }
            EA::PCIndexed(data, offset) => {
                out.push(data as Byte + 40);
                write_16_le(offset, out);
            }
            EA::OffsetIndexed(addr, data, offset) => {
                let pair = ((addr as Byte) << 3) + data as Byte;
                out.push(pair + 0b11000000);
                write_16_le(offset, out);
            }
            _ => unimplemented!(),
        }
    }
    fn read(i: &mut Word, data: &[Byte]) -> Self {
        let byte = data[*i as usize];
        *i += 1;
        if byte & 0b11000000 != 0 {
            let a = ADDR_IDX[((byte >> 3) & 0b111) as usize];
            let d = DATA_IDX[(byte & 0b111) as usize];
            let offset = read_16_le(i, data);
            return Self::OffsetIndexed(a, d, offset);
        }

        let reg = byte & 0b111;
        let mode = byte & 0b11111000;
        let out = match mode {
            0 => Self::Data(DATA_IDX[reg as usize]),
            8 => Self::Address(ADDR_IDX[reg as usize]),
            16 => Self::PreDec(ADDR_IDX[reg as usize]),
            24 => Self::PostInc(ADDR_IDX[reg as usize]),
            32 => {
                let offset = read_16_le(i, data);
                Self::Offset(ADDR_IDX[reg as usize], offset)
            }
            40 => {
                let offset = read_16_le(i, data);
                Self::PCIndexed(DATA_IDX[reg as usize], offset)
            }
            _ => unreachable!(),
        };
        out
    }
    fn read_immediate(i: &mut Word, data: &[Byte]) -> Self {
        let out = read_32_le(i, data);
        EA::Immediate(out)
    }
}

pub struct Writer {
    out: Vec<Byte>,
}

impl Default for Writer {
    fn default() -> Self {
        Self { out: Vec::new() }
    }
}

impl Writer {
    pub fn new() -> Self {
        Self { out: Vec::new() }
    }
    pub fn done(mut self) -> Vec<Byte> {
        self.halt();
        self.out
    }
    pub fn halt(&mut self) {
        self.out.push(Op::Halt as Byte)
    }
    pub fn mov(&mut self, dest: EA, src: EA) {
        self.op2(Op::Mov, Op::MovI, dest, src);
    }
    pub fn fixup_pc_indexed_mov(&mut self, at: Word, to: Word) {
        let mut patch = Vec::new();
        let next_idx = at + 4;
        write_16_le(to - next_idx, &mut patch);
        self.out[at as usize + 2] = patch[0];
        self.out[at as usize + 3] = patch[1];
    }
    pub fn load_address(&mut self, dest: Address, src: Address, offset: Word) {
        self.out.push(Op::Lea as Byte);
        pack_lea_pair(src, dest, &mut self.out);
        write_16_le(offset, &mut self.out);
    }
    pub fn push_address(&mut self, src: Address, offset: Word) {
        self.out.push(Op::Pea as Byte);
        pack_lea_pair(src, Address::A7, &mut self.out);
        write_16_le(offset, &mut self.out);
    }
    pub fn load_address_pc(&mut self, dest: Address) {
        self.out.push(Op::LeaPC as Byte);
        self.out.push(dest as Byte);
    }
    pub fn add(&mut self, dest: EA, src: EA) {
        self.op2(Op::Add, Op::AddI, dest, src);
    }
    pub fn sub(&mut self, dest: EA, src: EA) {
        self.op2(Op::Sub, Op::SubI, dest, src);
    }
    pub fn mul(&mut self, dest: EA, src: EA) {
        // TODO: use shifts with constant src
        self.op2(Op::Mul, Op::MulI, dest, src);
    }
    pub fn div(&mut self, dest: EA, src: EA) {
        self.op2(Op::Div, Op::DivI, dest, src);
    }
    pub fn cmp(&mut self, dest: EA, src: EA) {
        self.op2(Op::Cmp, Op::CmpI, dest, src);
    }
    pub fn neg(&mut self, dest: EA) {
        self.out.push(Op::Neg as Byte);
        dest.write(&mut self.out);
    }
    pub fn and(&mut self, dest: EA, src: EA) {
        self.op2(Op::And, Op::AndI, dest, src);
    }
    pub fn or(&mut self, dest: EA, src: EA) {
        self.op2(Op::Or, Op::OrI, dest, src);
    }
    pub fn xor(&mut self, dest: EA, src: EA) {
        self.op2(Op::Xor, Op::XorI, dest, src);
    }
    pub fn not(&mut self, dest: EA) {
        self.out.push(Op::Not as Byte);
        dest.write(&mut self.out);
    }
    pub fn branch_dest(&self) -> Word {
        self.out.len() as Word
    }

    pub fn jmp_addr(&mut self, addr: Address) {
        self.out.push(Op::JumpAddress as Byte);
        self.out.push(addr as Byte);
    }

    pub fn branch(&mut self, cond: Cond, idx: Word) -> Word {
        let fixup = self.out.len() as Word;
        let op = match cond {
            Cond::Always => Op::Branch,
            Cond::Zero => Op::BranchZero,
            Cond::NotZero => Op::BranchNotZero,
            Cond::Negative => Op::BranchNegative,
            Cond::NegativeZero => Op::BranchNegativeZero,
            Cond::Positive => Op::BranchPositive,
            Cond::PositiveZero => Op::BranchPositiveZero,
        };
        self.out.push(op as Byte);
        let next_idx = (self.out.len() + 2) as Word;
        write_16_le(idx - next_idx, &mut self.out);
        fixup
    }
    pub fn fixup_branch(&mut self, at: Word, to: Word) {
        let mut patch = Vec::new();
        let next_idx = at + 3;
        write_16_le(to - next_idx, &mut patch);
        self.out[at as usize + 1] = patch[0];
        self.out[at as usize + 2] = patch[1];
    }
    pub fn branch_subroutine(&mut self, idx: Word) -> Word {
        let fixup = self.out.len() as Word;
        self.out.push(Op::BranchSubroutine as Byte);
        let next_idx = (self.out.len() + 4) as Word;
        write_32_le(idx - next_idx, &mut self.out);
        fixup
    }
    pub fn fixup_branch_subroutine(&mut self, at: Word, to: Word) {
        let mut patch = Vec::new();
        let next_idx = at + 5;
        write_32_le(to - next_idx, &mut patch);
        for i in 0..4 {
            self.out[at as usize + 1 + i] = patch[i];
        }
    }
    pub fn ret(&mut self, to_drop: Word) {
        if to_drop == 0 {
            self.out.push(Op::Return as Byte);
        } else {
            self.out.push(Op::ReturnAndDeallocate as Byte);
            write_16_le(to_drop, &mut self.out);
        }
    }

    pub fn link(&mut self, addr: Address, displacement: Word) {
        self.out.push(Op::Link as Byte);
        self.out.push(addr as Byte);
        self.out.push(-displacement as Byte);
    }
    pub fn fixup_link(&mut self, at: Word, addr: Address, displacement: Word) {
        self.out[(at + 1) as usize] = addr as Byte;
        self.out[(at + 2) as usize] = -displacement as Byte;
    }
    pub fn unlink(&mut self, addr: Address) {
        self.out.push(Op::Unlink as Byte);
        self.out.push(addr as Byte);
    }

    pub fn movem_push(&mut self, dest: EA, data: &[Data], addr: &[Address]) -> Word {
        let fixup = self.out.len() as Word;
        self.out.push(Op::MoveMPush as Byte);
        let (data, addr) = self.movem_register_list(data, addr);
        self.out.push(data);
        self.out.push(addr);

        dest.write(&mut self.out);
        fixup
    }
    pub fn fixup_movem_push(&mut self, at: Word, dest: EA, data: &[Data], addr: &[Address]) {
        let at = at as usize;
        let (data, addr) = self.movem_register_list(data, addr);
        self.out[at + 1] = data;
        self.out[at + 2] = addr;
        let mut patch = Vec::new();
        dest.write(&mut patch);
        for (i, byte) in patch.into_iter().enumerate() {
            self.out[at + i + 3] = byte;
        }
    }
    pub fn movem_pop(&mut self, data: &[Data], addr: &[Address], src: EA) -> Word {
        let fixup = self.out.len() as Word;
        self.out.push(Op::MoveMPop as Byte);
        let (data, addr) = self.movem_register_list(data, addr);
        self.out.push(data);
        self.out.push(addr);
        src.write(&mut self.out);
        fixup
    }
    pub fn fixup_movem_pop(&mut self, at: Word, data: &[Data], addr: &[Address], src: EA) {
        let at = at as usize;
        let (data, addr) = self.movem_register_list(data, addr);
        self.out[at + 1] = data;
        self.out[at + 2] = addr;
        let mut patch = Vec::new();
        src.write(&mut patch);
        for (i, byte) in patch.into_iter().enumerate() {
            self.out[at + i + 3] = byte;
        }
    }
    fn movem_register_list(&mut self, data: &[Data], addr: &[Address]) -> (Byte, Byte) {
        let mut data_byte = 0;
        for d in data {
            data_byte |= 1 << *d as Byte;
        }
        let mut addr_byte = 0;
        for a in addr {
            addr_byte |= 1 << *a as Byte;
        }
        (data_byte, addr_byte)
    }

    pub fn data_32(&mut self, data: &[Word]) -> Word {
        let fixup = self.out.len() as Word;
        for item in data.iter() {
            write_32_le(*item, &mut self.out);
        }
        fixup
    }
    pub fn string(&mut self, data: &str) -> Word {
        let fixup = self.out.len() as Word;
        for b in data.as_bytes() {
            self.out.push(*b);
        }
        fixup
    }
    fn op2(&mut self, op: Op, op_i: Op, dest: EA, src: EA) {
        match src {
            EA::Immediate(value) => {
                self.out.push(op_i as Byte);
                write_32_le(value, &mut self.out);
                dest.write(&mut self.out);
            }
            src => {
                self.out.push(op as Byte);
                src.write(&mut self.out);
                dest.write(&mut self.out);
            }
        }
    }
    pub fn fixup_jump_table(&mut self, idx: Word) {
        let here = self.branch_dest();
        let displacement = here - idx;
        let mut out = Vec::new();
        write_32_le(displacement, &mut out);
        for i in 0..4 {
            self.out[(idx + i) as usize] = out[i as usize];
        }
    }
    #[cfg(test)]
    pub fn expect_output(&self, other: Writer) {
        assert_eq!(self.out, other.out);
    }
}

pub struct VM {
    data: [Word; 8],
    addr: [Word; 8],
    pc: Word,
    memory: Vec<Word>,
    program: Vec<Byte>,
    run_mode: RunMode,
    flags: Flags,
}

#[derive(Debug, PartialEq, Eq)]
enum RunMode {
    Halt,
    Run,
}

#[derive(Debug)]
struct Flags {
    extend: bool,
    negative: bool,
    zero: bool,
    overflow: bool,
    carry: bool,
}

impl VM {
    pub fn new(memory_bytes: usize, program: Vec<Byte>) -> Self {
        Self {
            data: [0; 8],
            addr: [0, 0, 0, 0, 0, 0, 0, memory_bytes as Word],
            pc: 0,
            memory: vec![0; memory_bytes >> 2],
            program,
            run_mode: RunMode::Halt,
            flags: Flags {
                extend: false,
                negative: false,
                zero: false,
                overflow: false,
                carry: false,
            },
        }
    }
    pub fn run(&mut self) {
        self.run_mode = RunMode::Run;
        loop {
            self.run_1();
            if self.run_mode == RunMode::Halt {
                break;
            }
        }
    }
    fn run_1(&mut self) {
        use Op::*;
        match self.get_op() {
            Halt => {
                self.run_mode = RunMode::Halt;
            }
            MovI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                *dest = src;
                let res = *dest;
                self.set_flags(res);
            }
            Mov => {
                let src = self.get_src();
                let dest = self.get_dest();
                *dest = src;
                let res = *dest;
                self.set_flags(res);
            }
            Lea => {
                let (src, dest) = unpack_lea_pair(&mut self.pc, &self.program);
                let offset = read_16_le(&mut self.pc, &self.program);
                self.addr[dest as usize] = self.addr[src as usize] + offset;
            }
            LeaPC => {
                self.addr[self.get_byte() as usize] = self.pc;
            }
            JumpAddress => {
                self.pc = self.addr[self.get_byte() as usize];
            }
            Pea => {
                let (src, _) = unpack_lea_pair(&mut self.pc, &self.program);
                let offset = read_16_le(&mut self.pc, &self.program);
                let address = self.addr[src as usize] + offset;
                self.dec_address(Address::A7);
                let dest = self.get_memory_word(self.addr[7]);
                *dest = address
            }
            AddI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                let sum = *dest as i64 + src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            Add => {
                let src = self.get_src();
                let dest = self.get_dest();
                let sum = *dest as i64 + src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            SubI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                let sum = *dest as i64 - src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            Sub => {
                let src = self.get_src();
                let dest = self.get_dest();
                let sum = *dest as i64 - src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            MulI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                let sum = *dest as i64 * src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            Mul => {
                let src = self.get_src();
                let dest = self.get_dest();
                let sum = *dest as i64 * src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            DivI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                // TODO: div by zero trap
                let sum = *dest as i64 / src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            Div => {
                let src = self.get_src();
                let dest = self.get_dest();
                let sum = *dest as i64 / src as i64;
                *dest = sum as Word;
                self.set_math_flags(sum);
            }
            CmpI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                let sum = *dest as i64 - src as i64;
                self.set_math_flags(sum);
            }
            Cmp => {
                let src = self.get_src();
                let dest = self.get_dest();
                let sum = *dest as i64 - src as i64;
                self.set_math_flags(sum);
            }
            Neg => {
                let dest = self.get_dest();
                *dest = -*dest;
                let res = *dest;
                self.set_flags(res);
            }
            AndI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                *dest &= src;
                let res = *dest;
                self.set_flags(res);
            }
            And => {
                let src = self.get_src();
                let dest = self.get_dest();
                *dest &= src;
                let res = *dest;
                self.set_flags(res);
            }
            OrI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                *dest |= src;
                let res = *dest;
                self.set_flags(res);
            }
            Or => {
                let src = self.get_src();
                let dest = self.get_dest();
                *dest |= src;
                let res = *dest;
                self.set_flags(res);
            }
            XorI => {
                let src = self.get_src_immediate();
                let dest = self.get_dest();
                *dest ^= src;
                let res = *dest;
                self.set_flags(res);
            }
            Xor => {
                let src = self.get_src();
                let dest = self.get_dest();
                *dest ^= src;
                let res = *dest;
                self.set_flags(res);
            }
            Not => {
                let dest = self.get_dest();
                *dest = !*dest;
                let res = *dest;
                self.set_flags(res);
            }
            Branch => {
                let disp = read_16_le(&mut self.pc, &self.program);
                self.pc += disp;
            }
            BranchZero => {
                let disp = read_16_le(&mut self.pc, &self.program);
                if self.flags.zero {
                    self.pc += disp;
                }
            }
            BranchNotZero => {
                let disp = read_16_le(&mut self.pc, &self.program);
                if !self.flags.zero {
                    self.pc += disp;
                }
            }
            BranchNegative => {
                let disp = read_16_le(&mut self.pc, &self.program);
                if self.flags.negative && !self.flags.zero {
                    self.pc += disp;
                }
            }
            BranchNegativeZero => {
                let disp = read_16_le(&mut self.pc, &self.program);
                if self.flags.negative || self.flags.zero {
                    self.pc += disp;
                }
            }
            BranchPositive => {
                let disp = read_16_le(&mut self.pc, &self.program);
                if !self.flags.negative && !self.flags.zero {
                    self.pc += disp;
                }
            }
            BranchPositiveZero => {
                let disp = read_16_le(&mut self.pc, &self.program);
                if !self.flags.negative || self.flags.zero {
                    self.pc += disp;
                }
            }
            BranchSubroutine => {
                let disp = read_32_le(&mut self.pc, &self.program);
                self.dec_address(Address::A7);
                *self.get_memory_word(self.addr[7]) = self.pc;
                self.pc += disp;
            }
            Return => {
                let addr = *self.get_memory_word(self.addr[7]);
                self.inc_address(Address::A7);
                self.pc = addr
            }
            ReturnAndDeallocate => {
                let to_drop = read_16_le(&mut self.pc, &self.program);
                let addr = *self.get_memory_word(self.addr[7]);
                self.addr[7] += to_drop + 4;
                self.pc = addr
            }
            Link => {
                let a = self.get_byte();
                let displacement = self.get_byte() as Word;
                // push old fp onto stack
                self.addr[7] -= WORD_BYTES;
                *self.get_memory_word(self.addr[7]) = self.addr[a as usize];
                // set current fp to sp
                self.addr[a as usize] = self.addr[7];
                // allocate displacement
                self.addr[7] -= displacement;
            }
            Unlink => {
                let a = self.get_byte();
                // deallocate stack
                self.addr[7] = self.addr[a as usize];
                // restore previous frame pointer
                self.addr[a as usize] = *self.get_memory_word(self.addr[7]);
                self.addr[7] += WORD_BYTES;
            }
            MoveMPush => {
                let data = self.get_byte();
                let addr = self.get_byte();
                let mut idx = self.get_ea_addr();
                for i in 0..8 {
                    if (data & (1 << i)) != 0 {
                        *self.get_memory_word(idx) = self.data[i as usize];
                        idx += WORD_BYTES;
                    }
                }
                for i in 0..8 {
                    if (addr & (1 << i)) != 0 {
                        *self.get_memory_word(idx) = self.addr[i as usize];
                        idx += WORD_BYTES;
                    }
                }
            }
            MoveMPop => {
                let data = self.get_byte();
                let addr = self.get_byte();
                let mut idx = self.get_ea_addr();
                for i in 0..8 {
                    if (data & (1 << i)) != 0 {
                        self.data[i as usize] = *self.get_memory_word(idx);
                        idx += WORD_BYTES;
                    }
                }
                for i in 0..8 {
                    if (addr & (1 << i)) != 0 {
                        self.addr[i as usize] = *self.get_memory_word(idx);
                        idx += WORD_BYTES;
                    }
                }
            }
        }
    }

    fn set_flags(&mut self, dest: i32) {
        self.flags.negative = dest < 0;
        self.flags.zero = dest == 0;
        self.flags.overflow = false;
        self.flags.carry = false;
    }
    fn set_math_flags(&mut self, value: i64) {
        self.flags.negative = value < 0;
        self.flags.zero = value == 0;
        self.flags.overflow = (value > 0) && ((value & (1 << 31)) > 0);
        self.flags.carry = (value & (1 << 32)) > 0;
        self.flags.extend = self.flags.carry;
    }
    fn get_op(&mut self) -> Op {
        let op = Op::from_byte(self.program[self.pc as usize]);
        self.pc += 1;
        op
    }
    fn get_src_immediate(&mut self) -> Word {
        read_32_le(&mut self.pc, &mut self.program)
    }
    // memory is word aligned, but addresses are in bytes
    fn get_memory_word(&mut self, address: Word) -> &mut Word {
        assert_eq!(address, address & !0b11);
        &mut self.memory[(address >> 2) as usize]
    }
    fn get_byte(&mut self) -> Byte {
        let byte = self.program[self.pc as usize];
        self.pc += 1;
        byte
    }
    fn get_src(&mut self) -> Word {
        let src = EA::read(&mut self.pc, &mut self.program);
        match src {
            EA::Immediate(_) => unreachable!(),
            EA::Data(d) => self.data[d as usize],
            EA::Address(a) => self.addr[a as usize],
            EA::Offset(a, offset) => {
                let addr = self.addr[a as usize];
                *self.get_memory_word(addr + offset)
            }
            EA::OffsetIndexed(a, d, offset) => {
                let addr = self.addr[a as usize];
                let index = self.data[d as usize];
                *self.get_memory_word(addr + index + offset)
            }
            EA::PreDec(a) => {
                self.dec_address(a);
                let addr = self.addr[a as usize];
                *self.get_memory_word(addr)
            }
            EA::PostInc(a) => {
                let addr = self.addr[a as usize];
                self.inc_address(a);
                *self.get_memory_word(addr)
            }
            // FIXME: this just gets a byte, should have separate word/byte modes
            EA::PCIndexed(d, offset) => {
                let idx = self.data[d as usize];
                let byte = self.program[(self.pc + idx + offset) as usize];
                byte as Word
            }
        }
    }
    fn get_dest(&mut self) -> &mut Word {
        let dest = EA::read(&mut self.pc, &mut self.program);
        match dest {
            EA::Immediate(_) => unreachable!(),
            EA::Data(d) => &mut self.data[d as usize],
            EA::Address(a) => &mut self.addr[a as usize],
            EA::Offset(a, offset) => {
                let addr = self.addr[a as usize];
                self.get_memory_word(addr + offset)
            }
            EA::OffsetIndexed(a, d, offset) => {
                let addr = self.addr[a as usize];
                let index = self.data[d as usize];
                self.get_memory_word(addr + index + offset)
            }
            EA::PreDec(a) => {
                self.dec_address(a);
                let addr = self.addr[a as usize];
                self.get_memory_word(addr)
            }
            EA::PostInc(a) => {
                let addr = self.addr[a as usize];
                self.inc_address(a);
                self.get_memory_word(addr)
            }
            EA::PCIndexed(_, _) => unreachable!(),
        }
    }
    fn get_ea_addr(&mut self) -> Word {
        let dest = EA::read(&mut self.pc, &mut self.program);
        match dest {
            EA::Immediate(_) => unreachable!(),
            EA::Data(_) => unreachable!(),
            EA::Address(_) => unreachable!(),
            EA::Offset(a, offset) => {
                let addr = self.addr[a as usize];
                addr + offset
            }
            EA::OffsetIndexed(a, d, offset) => {
                let addr = self.addr[a as usize];
                let index = self.data[d as usize];
                addr + index + offset
            }
            EA::PreDec(_) => unreachable!(),
            EA::PostInc(_) => unreachable!(),
            EA::PCIndexed(_, _) => unreachable!(),
        }
    }
    fn inc_address(&mut self, a: Address) {
        self.addr[a as usize] += 4;
    }
    fn dec_address(&mut self, a: Address) {
        self.addr[a as usize] -= 4;
    }
}

#[cfg(test)]
mod test {
    use super::{Address::*, Data::*, EA::*, *};

    impl VM {
        fn expect_stack(&self, expected: Vec<Word>) {
            let sp = self.addr[A7 as usize];
            let sp_aligned = (sp >> 2) as usize;

            let received = self.memory.as_slice()[sp_aligned..].to_vec();
            assert_eq!(received, expected);
        }
        fn expect_data(&self, data: super::Data, expected: Word) {
            assert_eq!(self.data[data as usize], expected);
        }
    }
    impl Writer {
        fn run(self) -> VM {
            let program = self.done();
            let mut vm = VM::new(128, program);
            vm.run();
            vm
        }
    }

    #[test]
    fn init_state() {
        let vm = VM::new(128, vec![]);
        vm.expect_stack(vec![]);
    }

    #[test]
    fn mov() {
        let mut w = Writer::new();
        w.mov(PreDec(A7), Immediate(123));
        w.mov(PreDec(A7), Immediate(456));
        w.mov(Data(D0), Immediate(789));
        w.mov(Data(D1), Data(D0));
        w.halt();
        w.mov(Data(D2), PostInc(A7));

        let mut vm = w.run();
        vm.expect_stack(vec![456, 123]);
        vm.expect_data(D0, 789);
        vm.expect_data(D1, 789);

        vm.run();
        vm.expect_stack(vec![123]);
        vm.expect_data(D2, 456);
    }

    #[test]
    fn load_address() {
        let mut w = Writer::new();
        w.mov(PreDec(A7), Immediate(123));
        w.load_address(A0, A7, 0);
        w.mov(PreDec(A7), Immediate(456));
        w.mov(PreDec(A7), Offset(A0, 0));
        w.halt();
        w.push_address(A7, 4);
        w.mov(Address(A1), PostInc(A7));
        w.mov(PreDec(A7), Offset(A1, 0));

        let mut vm = w.run();
        vm.expect_stack(vec![123, 456, 123]);

        vm.run();
        vm.expect_stack(vec![456, 123, 456, 123]);
    }

    #[test]
    fn add_overflow() {
        let mut w = Writer::new();
        w.mov(Data(D0), Immediate(100));
        w.add(Data(D0), Immediate(200));
        w.mov(Data(D1), Immediate(300));
        w.add(Data(D0), Data(D1));
        w.halt();
        w.mov(Data(D3), Immediate(i32::MAX));
        w.add(Data(D3), Immediate(1));

        let mut vm = w.run();
        vm.expect_data(D0, 600);

        vm.run();
        assert!(vm.flags.overflow);
    }

    #[test]
    fn arithmetic() {
        let mut w = Writer::new();
        w.mov(Data(D0), Immediate(100));
        w.sub(Data(D0), Immediate(50));
        w.mul(Data(D0), Immediate(3));
        w.div(Data(D0), Immediate(10));
        w.neg(Data(D0));

        let vm = w.run();
        vm.expect_data(D0, -15);
    }

    #[test]
    fn logic() {
        let mut w = Writer::new();
        w.mov(Data(D0), Immediate(0b01));
        w.or(Data(D0), Immediate(0b11));
        w.and(Data(D0), Immediate(0b10));
        w.not(Data(D0));
        w.not(Data(D0));
        w.xor(Data(D0), Immediate(0b11));

        let vm = w.run();
        vm.expect_data(D0, 0b001);
    }

    #[test]
    fn cmp() {
        let mut w = Writer::new();
        w.mov(Data(D0), Immediate(100));
        w.cmp(Data(D0), Immediate(100));
        let vm = w.run();
        vm.expect_data(D0, 100);
        assert!(vm.flags.zero);
    }

    #[test]
    fn branch() {
        let mut w = Writer::new();
        w.mov(Data(D0), Immediate(0));

        let begin = w.branch_dest();
        w.cmp(Data(D0), Immediate(3));
        let fixup = w.branch(Cond::Zero, 0);
        w.add(Data(D0), Immediate(1));
        w.branch(Cond::Always, begin);
        let end = w.branch_dest();
        w.fixup_branch(fixup, end);

        w.mov(Data(D1), Immediate(123));

        let vm = w.run();
        vm.expect_data(D0, 3);
        vm.expect_data(D1, 123);
    }

    #[test]
    fn subroutine() {
        let mut w = Writer::new();
        let fixup_main = w.branch(Cond::Always, 0);

        // sub add_1 (a: Int) -> Int do return a + 1 end
        let add_1 = w.branch_dest();
        w.add(Data(D0), Immediate(1));
        w.ret(0);
        w.halt();

        // sub main() do add_1(123) end
        let main = w.branch_dest();
        w.fixup_branch(fixup_main, main);
        w.mov(Data(D0), Immediate(123));
        w.branch_subroutine(add_1);

        let vm = w.run();
        vm.expect_data(D0, 124);
    }

    #[test]
    fn subroutine_stack_args() {
        let mut w = Writer::new();
        let fixup_main = w.branch(Cond::Always, 0);

        // sub add_1 (a: Int) -> Int do return a + 1 end
        let add_1 = w.branch_dest();
        w.mov(Data(D0), Offset(A7, 4));
        w.add(Data(D0), Immediate(1));
        w.mov(Offset(A7, 8), Data(D0));
        w.ret(4);
        w.halt();

        // sub main() do add_1(123) end
        let main = w.branch_dest();
        w.fixup_branch(fixup_main, main);
        w.sub(Address(A7), Immediate(4));
        w.mov(PreDec(A7), Immediate(123));
        w.branch_subroutine(add_1);

        let vm = w.run();
        vm.expect_stack(vec![124]);
    }

    #[test]
    fn subroutine_save_registers() {
        let mut w = Writer::new();
        let fixup_main = w.branch(Cond::Always, 0);

        // sub origin_distance(p: Point) -> Int
        let sub = w.branch_dest();
        w.link(A6, -4);
        w.movem_push(Offset(A6, -4), &[D2], &[]);

        w.mov(Data(D2), Offset(A6, 8));
        w.add(Data(D2), Offset(A6, 12));
        w.mov(Data(D0), Data(D2));

        w.movem_pop(&[D2], &[], Offset(A6, -4));
        w.unlink(A6);
        w.ret(8);
        w.halt();

        // sub main()
        let main = w.branch_dest();
        w.fixup_branch(fixup_main, main);

        w.mov(Data(D2), Immediate(20));
        w.mov(PreDec(A7), Immediate(3));
        w.mov(PreDec(A7), Immediate(5));
        w.branch_subroutine(sub);
        w.mov(PreDec(A7), Data(D0));
        w.mov(PreDec(A7), Data(D2));

        let vm = w.run();
        vm.expect_stack(vec![20, 8]);
    }

    #[test]
    fn read_data() {
        let mut w = Writer::new();
        let fixup_main = w.branch(Cond::Always, 0);
        w.halt();
        let str = w.string("Hello");

        let main = w.branch_dest();
        w.fixup_branch(fixup_main, main);
        w.mov(Data(D0), Immediate(0));
        let loop_ = w.branch_dest();
        // TODO: use mov_byte, write forward instead of backward
        w.mov(PreDec(A7), PCIndexed(D0, 0));
        w.fixup_pc_indexed_mov(loop_, str);
        w.add(Data(D0), Immediate(1));
        w.cmp(Data(D0), Immediate(5));
        w.branch(Cond::NotZero, loop_);

        let vm = w.run();
        vm.expect_stack(vec![111, 108, 108, 101, 72]);
    }

    #[test]
    fn array() {
        let mut w = Writer::new();
        let array = A0;
        w.mov(PreDec(A7), Immediate(50));
        w.mov(PreDec(A7), Immediate(40));
        w.mov(PreDec(A7), Immediate(30));
        w.mov(PreDec(A7), Immediate(20));
        w.mov(PreDec(A7), Immediate(10));
        w.mov(Address(array), Address(A7));

        let sum = D0;
        w.mov(Data(sum), Immediate(0));
        let index = D1;
        w.mov(Data(index), Immediate(0));
        let to_do_while = w.branch(Cond::Always, 0);

        let begin = w.branch_dest();
        w.add(Data(sum), OffsetIndexed(array, index, 0));
        w.add(Data(index), Immediate(WORD_BYTES));

        let do_while = w.branch_dest();
        w.fixup_branch(to_do_while, do_while);
        w.cmp(Data(index), Immediate(5 * WORD_BYTES));
        w.branch(Cond::NotZero, begin);

        let vm = w.run();
        vm.expect_data(D0, 150);
    }

    #[test]
    fn bump_allocator() {
        let mut w = Writer::new();
        let heap_tip = Offset(A5, 0);
        w.mov(Address(A5), Immediate(0));
        w.mov(heap_tip, Immediate(WORD_BYTES));

        let to_main = w.branch(Cond::Always, 0);

        // sub alloc (size: D0) -> A0
        let alloc = w.branch_dest();
        {
            let size = Data(D0);
            let ret = Address(A0);
            w.mov(ret, heap_tip);
            w.add(heap_tip, size);
            w.ret(0);
        };

        // sub free (ptr: A0)
        let _free = w.branch_dest();
        {
            let ptr = Address(A0);
            w.mov(heap_tip, ptr);
            w.ret(0);
        };

        // sub cons(h: D0, t: A0) -> A0
        let cons = w.branch_dest();
        {
            w.mov(PreDec(A7), Address(A0));
            w.mov(PreDec(A7), Data(D0));
            w.mov(Data(D0), Immediate(WORD_BYTES * 2));
            w.branch_subroutine(alloc);
            w.mov(Offset(A0, 0), PostInc(A7));
            w.mov(Offset(A0, WORD_BYTES), PostInc(A7));
            w.ret(0);
        };

        // sub sum(list: A0) -> D0
        let sum = w.branch_dest();
        {
            w.cmp(Address(A0), Immediate(0));
            let to_else = w.branch(Cond::NotZero, 0);
            w.mov(Data(D0), Immediate(0));
            w.ret(0);

            let else_ = w.branch_dest();
            w.fixup_branch(to_else, else_);
            // let value = list[].head
            w.mov(PreDec(A7), Offset(A0, 0));
            // list = list[].tail
            w.mov(Address(A0), Offset(A0, WORD_BYTES));
            w.branch_subroutine(sum);
            w.add(Data(D0), PostInc(A7));
            w.ret(0);
        }

        let main = w.branch_dest();
        w.fixup_branch(to_main, main);
        {
            w.mov(Address(A0), Immediate(0));
            w.mov(Data(D0), Immediate(10));
            w.branch_subroutine(cons);
            w.mov(Data(D0), Immediate(20));
            w.branch_subroutine(cons);
            w.mov(Data(D0), Immediate(30));
            w.branch_subroutine(cons);

            w.branch_subroutine(sum);
        };

        let vm = w.run();
        vm.expect_data(D0, 60);
    }
}
