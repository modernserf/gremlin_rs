pub type Word = i32;
pub type Byte = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EA {
    Immediate(Word),
    Data(Data),
    Address(Address),
    Offset(Address, Word),
    PreDec(Address),
    PostInc(Address),
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

const DATA_IDX: [Data; 8] = [
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

const ADDR_IDX: [Address; 8] = [
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
    // JumpAddress = 0x40,
    // JumpSubroutineAddress = 0x41,
    Return = 0x42,
    ReturnAndDeallocate = 0x43,
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
            0x42 => Return,
            0x43 => ReturnAndDeallocate,
            _ => unreachable!(),
        }
    }
}

fn write_32_le(value: Word, out: &mut Vec<Byte>) {
    out.push((value & 0xFF) as Byte);
    out.push(((value >> 8) & 0xFF) as Byte);
    out.push(((value >> 16) & 0xFF) as Byte);
    out.push(((value >> 24) & 0xFF) as Byte);
}

fn read_32_le(i: &mut Word, data: &[Byte]) -> Word {
    let out = data[*i as usize] as Word
        + ((data[*i as usize + 1] as Word) << 8)
        + ((data[*i as usize + 2] as Word) << 16)
        + ((data[*i as usize + 3] as Word) << 24);
    *i += 4;
    out
}

fn write_16_le(value_32: Word, out: &mut Vec<Byte>) {
    let value = value_32 as i16;
    out.push((value & 0xFF) as Byte);
    out.push(((value >> 8) & 0xFF) as Byte);
}

fn read_16_le(i: &mut Word, data: &[Byte]) -> Word {
    let out = data[*i as usize] as i16 + ((data[*i as usize + 1] as i16) << 8);
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
            _ => unimplemented!(),
        }
    }
    fn read(i: &mut Word, data: &[Byte]) -> Self {
        let byte = data[*i as usize];
        *i += 1;
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
    pub fn add(&mut self, dest: EA, src: EA) {
        self.op2(Op::Add, Op::AddI, dest, src);
    }
    pub fn sub(&mut self, dest: EA, src: EA) {
        self.op2(Op::Sub, Op::SubI, dest, src);
    }
    pub fn mul(&mut self, dest: EA, src: EA) {
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
            MulI => todo!(),
            Mul => todo!(),
            DivI => todo!(),
            Div => todo!(),
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
}
