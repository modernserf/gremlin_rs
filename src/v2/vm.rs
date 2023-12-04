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

enum Op {
    Halt = 0,
    MovI,
    Mov,
    Lea,
    Pea,
    AddI,
    Add,
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
            5 => AddI,
            6 => Add,
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

fn write_16_le(value: Word, out: &mut Vec<Byte>) {
    assert_eq!(value, value & 0xFFFF);
    out.push((value & 0xFF) as Byte);
    out.push(((value >> 8) & 0xFF) as Byte);
}

fn read_32_le(i: &mut usize, data: &[Byte]) -> Word {
    let out = data[*i] as Word
        + ((data[*i + 1] as Word) << 8)
        + ((data[*i + 2] as Word) << 16)
        + ((data[*i + 3] as Word) << 24);
    *i += 4;
    out
}
fn read_16_le(i: &mut usize, data: &[Byte]) -> Word {
    let out = data[*i] as Word + ((data[*i + 1] as Word) << 8);
    *i += 2;
    out
}

fn pack_lea_pair(src: Address, dest: Address, out: &mut Vec<Byte>) {
    let packed = ((dest as Byte) << 4) + src as Byte;
    out.push(packed);
}

fn unpack_lea_pair(i: &mut usize, data: &[Byte]) -> (Address, Address) {
    let byte = data[*i];
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
    fn read(i: &mut usize, data: &[Byte]) -> Self {
        let byte = data[*i];
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
    fn read_immediate(i: &mut usize, data: &[Byte]) -> Self {
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
        match src {
            EA::Immediate(value) => {
                self.out.push(Op::MovI as Byte);
                write_32_le(value, &mut self.out);
                dest.write(&mut self.out);
            }
            src => {
                self.out.push(Op::Mov as Byte);
                src.write(&mut self.out);
                dest.write(&mut self.out);
            }
        }
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
        match src {
            EA::Immediate(value) => {
                self.out.push(Op::AddI as Byte);
                write_32_le(value, &mut self.out);
                dest.write(&mut self.out);
            }
            src => {
                self.out.push(Op::Add as Byte);
                src.write(&mut self.out);
                dest.write(&mut self.out);
            }
        }
    }
}

pub struct VM {
    data: [Word; 8],
    addr: [Word; 8],
    pc: usize,
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
                *dest += src;
                // TODO: overflow, carry
                let res = *dest;
                self.set_flags(res);
            }
            Add => {
                let src = self.get_src();
                let dest = self.get_dest();
                *dest += src;
                // TODO: overflow, carry
                let res = *dest;
                self.set_flags(res);
            }
        }
    }

    fn set_flags(&mut self, dest: i32) {
        self.flags.negative = dest < 0;
        self.flags.zero = dest == 0;
        self.flags.overflow = false;
        self.flags.carry = false;
    }

    fn get_op(&mut self) -> Op {
        let op = Op::from_byte(self.program[self.pc]);
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
}
