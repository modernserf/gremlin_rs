use super::ea::*;

const ADDRESS_MASK: i32 = 0x7FFF_FFFF;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EAView {
    Data(usize),
    Addr(usize),
    Memory(i32),
    Immediate(i32),
}

struct VM {
    memory: Vec<u8>,
    data: [i32; 8],
    addr: [i32; 8],
    pc: i32,
}

impl VM {
    fn new(size: usize) -> Self {
        Self {
            memory: vec![0; size],
            data: [0; 8],
            addr: [0; 8],
            pc: 0,
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
    }
    fn run_1(&mut self) {
        let instr = self.mem_u16(self.pc);
        match Tag0::from_instr(instr) {
            Tag0::Zero => todo!(),
            Tag0::AddQ => todo!(),
            Tag0::Add => {
                let reg_id = (instr >> 9 & 0b111) as usize;
                let mode = (instr >> 6) & 0b111;
                let (size, src, dest) = match mode {
                    0 => (Size::Byte, self.ea_read(Size::Byte), EAView::Data(reg_id)),
                    1 => (Size::Short, self.ea_read(Size::Short), EAView::Data(reg_id)),
                    2 => (Size::Word, self.ea_read(Size::Word), EAView::Data(reg_id)),
                    3 => (Size::Byte, EAView::Data(reg_id), self.ea_read(Size::Byte)),
                    4 => (Size::Short, EAView::Data(reg_id), self.ea_read(Size::Short)),
                    5 => (Size::Word, EAView::Data(reg_id), self.ea_read(Size::Word)),
                    6 => (Size::Short, self.ea_read(Size::Short), EAView::Addr(reg_id)),
                    7 => (Size::Word, self.ea_read(Size::Word), EAView::Addr(reg_id)),
                    _ => unreachable!(),
                };
                self.ea_apply(dest, src, size, |l, r| l + r);
            }
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
                let disp = self.mem_i16(self.pc) as i32;
                self.pc += 2;
                EAView::Memory(self.pc + disp)
            }
            (7, 3) => {
                let register = self.mem_u8(self.pc) as usize;
                let offset = self.mem_u8(self.pc + 1) as i32;
                self.pc += 2;
                let id = register & !0b1000;
                if register == id {
                    EAView::Memory(self.pc + self.data[id] + offset)
                } else {
                    EAView::Memory(self.pc + self.addr[id] + offset)
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
enum Tag0 {
    Zero = 0,
    Add = 0b1101,
    AddQ = 0b0101,
}
impl Tag0 {
    fn from_instr(instr: u16) -> Self {
        match instr >> 12 {
            0 => Self::Zero,
            0b1101 => Self::Add,
            0b0101 => Self::AddQ,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tag1 {
    AddI = 0b0110,
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
                self.tag4_reg3_mode3_ea(Tag0::Add, d as u16, op_mode);
                self.push_ea(size, src);
            }
            (src, EA::Addr(a)) => {
                let op_mode = match size {
                    Size::Byte => unimplemented!(),
                    Size::Short => 6,
                    Size::Word => 7,
                };
                self.tag4_reg3_mode3_ea(Tag0::Add, a as u16, op_mode);
                self.push_ea(size, src);
            }
            (EA::Data(d), dest) => {
                let op_mode = match size {
                    Size::Byte => 3,
                    Size::Short => 4,
                    Size::Word => 5,
                };
                self.tag4_reg3_mode3_ea(Tag0::Add, d as u16, op_mode);
                self.push_ea(size, dest);
            }
            (EA::Immediate(x), dest) => {
                if let Some(q) = quick_positive(x) {
                    self.tag4_quick3_size3_ea(Tag0::AddQ, q, size);
                    self.push_ea(size, dest);
                } else {
                    self.tag4_tag4_size3_ea_imm(Tag0::Zero, Tag1::AddI, size);
                    self.push_ea(size, dest);
                    self.push_immediate(size, x);
                }
            }
            _ => unimplemented!(),
        }
    }
    // instruction formats
    fn tag4_reg3_mode3_ea(&mut self, tag0: Tag0, register: u16, op_mode: u16) {
        let base = ((tag0 as u16) << 12) + (register << 9) + (op_mode << 6);
        self.push_u16(base);
    }
    fn tag4_quick3_size3_ea(&mut self, tag0: Tag0, quick: Quick, size: Size) {
        let size_code = match size {
            Size::Byte => 0,
            Size::Short => 1,
            Size::Word => 2,
        };
        let base = ((tag0 as u16) << 12) + (quick << 9) + (size_code << 6);
        self.push_u16(base);
    }
    fn tag4_tag4_size3_ea_imm(&mut self, tag0: Tag0, tag1: Tag1, size: Size) {
        let size_code = match size {
            Size::Byte => 0,
            Size::Short => 1,
            Size::Word => 2,
        };
        let base = ((tag0 as u16) << 12) + ((tag1 as u16) << 8) + (size_code << 6);
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
    use crate::v2::register::*;

    impl VM {
        fn set_memory_i32(&mut self, addr: i32, value: i32) {
            self.ea_apply(
                EAView::Memory(addr),
                EAView::Immediate(value),
                Size::Word,
                |_, value| value,
            );
        }
    }

    #[test]
    fn smoke_test() {
        let mut vm = VM::new(256);
        let init_sp = 124;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        let mut asm = Asm::new();

        asm.add(Size::Word, EA::Data(Data::D0), EA::Data(Data::D1));
        asm.add(Size::Word, EA::Data(Data::D0), EA::Addr(Addr::A0));
        asm.add(Size::Word, EA::Data(Data::D0), EA::Offset(Addr::A7, 0));
        asm.add(Size::Word, EA::Offset(Addr::A7, 0), EA::Data(Data::D2));

        vm.load_memory(init_pc as usize, &asm.out);

        vm.data[0] = 10;
        vm.data[1] = 20;
        vm.run_1();
        assert_eq!(vm.data[1], 30);

        vm.addr[0] = 30;
        vm.run_1();
        assert_eq!(vm.addr[0], 40);

        vm.data[0] = 10;
        vm.set_memory_i32(vm.addr[7], 10);
        vm.run_1();
        assert_eq!(vm.mem_i32(vm.addr[7]), 20);

        vm.set_memory_i32(vm.addr[7], 10);
        vm.data[2] = 20;
        vm.run_1();
        assert_eq!(vm.data[2], 30);
    }
}
