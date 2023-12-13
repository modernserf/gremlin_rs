use super::register::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    Byte,
    Short,
    Word,
}

impl Size {
    pub fn bytes(&self) -> i32 {
        match self {
            Self::Byte => 1,
            Self::Short => 2,
            Self::Word => 4,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EA {
    Data(Data),
    Addr(Addr),
    Offset(Addr, i32),
    PreDec(Addr),
    PostInc(Addr),
    IdxData(Addr, Data, u8),
    IdxAddr(Addr, Addr, u8),
    Absolute(i32),
    PCOffset(i32),
    PCIdxData(Data, u8),
    PCIdxAddr(Addr, u8),
    Immediate(i32),
}

impl EA {
    pub fn read(memory: &[u8], pc: &mut usize, size: Size) -> Option<EA> {
        let byte = memory[*pc];
        *pc += 1;
        let mode = (byte >> 3) & 0b111;
        let reg = byte & 0b111;
        let ea = match (mode, reg) {
            (0, r) => EA::Data(Data::from(r)),
            (1, r) => EA::Addr(Addr::from(r)),
            (2, r) => EA::Offset(Addr::from(r), 0),
            (3, r) => EA::PreDec(Addr::from(r)),
            (4, r) => EA::PostInc(Addr::from(r)),
            (5, r) => {
                let offset = i32::from_be_bytes([0, 0, memory[*pc], memory[*pc + 1]]);
                *pc += 2;
                EA::Offset(Addr::from(r), offset)
            }
            (6, r) => {
                let d = memory[*pc];
                let offset = memory[*pc + 1];
                *pc += 2;
                if d > 8 {
                    EA::IdxAddr(Addr::from(r), Addr::from(d - 8), offset)
                } else {
                    EA::IdxData(Addr::from(r), Data::from(d), offset)
                }
            }
            (7, 0) => {
                let offset = i32::from_be_bytes([0, 0, memory[*pc], memory[*pc + 1]]);
                *pc += 2;
                EA::Absolute(offset)
            }
            (7, 1) => {
                let offset = i32::from_be_bytes([
                    memory[*pc],
                    memory[*pc + 1],
                    memory[*pc + 2],
                    memory[*pc + 3],
                ]);
                *pc += 4;
                EA::Absolute(offset)
            }
            (7, 2) => {
                let offset = i32::from_be_bytes([0, 0, memory[*pc], memory[*pc + 1]]);
                *pc += 2;
                EA::PCOffset(offset)
            }
            (7, 3) => {
                let d = memory[*pc];
                let offset = memory[*pc + 1];
                *pc += 2;
                if d > 8 {
                    EA::PCIdxAddr(Addr::from(d - 8), offset)
                } else {
                    EA::PCIdxData(Data::from(d), offset)
                }
            }
            (7, 4) => {
                let (bytes, value) = match size {
                    Size::Byte => (2, i32::from_be_bytes([0, 0, 0, memory[*pc + 1]])),
                    Size::Short => (2, i32::from_be_bytes([0, 0, memory[*pc], memory[*pc + 1]])),
                    Size::Word => (
                        4,
                        i32::from_be_bytes([
                            memory[*pc],
                            memory[*pc + 1],
                            memory[*pc + 2],
                            memory[*pc + 3],
                        ]),
                    ),
                };
                *pc += bytes;
                EA::Immediate(value)
            }
            _ => return None,
        };
        Some(ea)
    }

    pub fn write(&self, size: Size, out: &mut Vec<u8>) {
        // Overwrites the bottom 6 bits of the previously-written byte
        let prev_byte = out.pop().unwrap();
        match *self {
            Self::Data(d) => out.push(prev_byte + d as u8),
            Self::Addr(a) => out.push(prev_byte + (1 << 3) + a as u8),
            Self::Offset(a, o) => {
                if o == 0 {
                    out.push(prev_byte + (2 << 3) + a as u8);
                } else {
                    out.push(prev_byte + (5 << 3) + a as u8);
                    out.extend((o as i16).to_be_bytes())
                }
            }
            Self::PreDec(a) => out.push(prev_byte + (3 << 3) + a as u8),
            Self::PostInc(a) => out.push(prev_byte + (4 << 3) + a as u8),
            Self::IdxData(a, d, o) => {
                out.push(prev_byte + (6 << 3) + a as u8);
                out.push(d as u8 + 8);
                out.push(o);
            }
            Self::IdxAddr(a, a2, o) => {
                out.push(prev_byte + (6 << 3) + a as u8);
                out.push(a2 as u8);
                out.push(o);
            }
            Self::Absolute(o) => {
                if (o & 0xFFFF) == o {
                    out.push(prev_byte + (7 << 3));
                    out.extend((o as u16).to_be_bytes());
                } else {
                    out.push(prev_byte + (7 << 3) + 1);
                    out.extend(o.to_be_bytes());
                }
            }
            Self::PCOffset(o) => {
                out.push(prev_byte + (7 << 3) + 2);
                out.extend((o as i16).to_be_bytes());
            }
            Self::PCIdxData(d, o) => {
                out.push(prev_byte + (7 << 3) + 3);
                out.push(d as u8 + 8);
                out.push(o);
            }
            Self::PCIdxAddr(a, o) => {
                out.push(prev_byte + (7 << 3) + 3);
                out.push(a as u8);
                out.push(o);
            }
            Self::Immediate(x) => {
                out.push(prev_byte + (7 << 3) + 4);
                match size {
                    Size::Byte => out.push(x as u8),
                    Size::Short => out.extend((x as u16).to_be_bytes()),
                    Size::Word => out.extend(x.to_be_bytes()),
                };
            }
        }
    }
}
