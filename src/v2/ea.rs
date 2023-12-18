use super::register::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Size {
    Byte,
    Short,
    Long,
}

impl Size {
    pub fn bytes(&self) -> i32 {
        match self {
            Self::Byte => 1,
            Self::Short => 2,
            Self::Long => 4,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PC {
    Displacement(i16),
    Line(usize),
}

impl PC {
    fn resolve_16(&self, here: usize) -> [u8; 2] {
        match *self {
            Self::Displacement(x) => x.to_be_bytes(),
            Self::Line(line) => ((line as isize - here as isize) as i16).to_be_bytes(),
        }
    }
    fn resolve_8(&self, here: usize) -> u8 {
        match *self {
            Self::Displacement(x) => (x as i8).to_be_bytes()[0],
            Self::Line(line) => ((line as isize - here as isize) as i8).to_be_bytes()[0],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EA {
    Data(Data),
    Addr(Addr),
    Offset(Addr, i16),
    PreDec(Addr),
    PostInc(Addr),
    IdxData(Addr, Data, u8),
    IdxAddr(Addr, Addr, u8),
    Absolute(i32),
    PCOffset(PC),
    PCIdxData(Data, PC),
    PCIdxAddr(Addr, PC),
    Immediate(i32),
}

impl EA {
    pub fn offset(self, offset: i32) -> Self {
        match self {
            EA::Offset(a, o) => EA::Offset(a, o + offset as i16),
            EA::IdxData(a, d, o) => EA::IdxData(a, d, o + offset as u8),
            EA::IdxAddr(a, a2, o) => EA::IdxAddr(a, a2, o + offset as u8),
            _ => unimplemented!(),
        }
    }
    pub fn is_control_mode(&self) -> bool {
        match self {
            Self::Offset(_, _)
            | Self::IdxData(_, _, _)
            | Self::IdxAddr(_, _, _)
            | Self::Absolute(_)
            | Self::PCOffset(_)
            | Self::PCIdxData(_, _)
            | Self::PCIdxAddr(_, _) => true,
            _ => false,
        }
    }
    pub fn read(memory: &[u8], pc: &mut usize, size: Size) -> Option<EA> {
        let byte = memory[*pc];
        *pc += 1;
        let mode = (byte >> 3) & 0b111;
        let reg = (byte & 0b111) as usize;
        let ea = match (mode, reg) {
            (0, r) => EA::Data(Data::from(r)),
            (1, r) => EA::Addr(Addr::from(r)),
            (2, r) => EA::Offset(Addr::from(r), 0),
            (3, r) => EA::PostInc(Addr::from(r)),
            (4, r) => EA::PreDec(Addr::from(r)),
            (5, r) => {
                let offset = i16::from_be_bytes([memory[*pc], memory[*pc + 1]]);
                *pc += 2;
                EA::Offset(Addr::from(r), offset)
            }
            (6, r) => {
                let d = memory[*pc] as usize;
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
                let offset = i16::from_be_bytes([memory[*pc], memory[*pc + 1]]);
                *pc += 2;
                EA::PCOffset(PC::Displacement(offset))
            }
            (7, 3) => {
                let d = memory[*pc] as usize;
                let offset = memory[*pc + 1] as i16;
                *pc += 2;
                if d > 8 {
                    EA::PCIdxAddr(Addr::from(d - 8), PC::Displacement(offset))
                } else {
                    EA::PCIdxData(Data::from(d), PC::Displacement(offset))
                }
            }
            (7, 4) => {
                let (bytes, value) = match size {
                    Size::Byte => (2, i32::from_be_bytes([0, 0, 0, memory[*pc + 1]])),
                    Size::Short => (2, i32::from_be_bytes([0, 0, memory[*pc], memory[*pc + 1]])),
                    Size::Long => (
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

    pub fn write(&self, size: Size, out: &mut Vec<u8>, here: usize) {
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
            Self::PostInc(a) => out.push(prev_byte + (3 << 3) + a as u8),
            Self::PreDec(a) => out.push(prev_byte + (4 << 3) + a as u8),
            // FIXME: "brief extension word" format
            Self::IdxData(a, d, o) => {
                out.push(prev_byte + (6 << 3) + a as u8);
                out.push(d as u8);
                out.push(o);
            }
            Self::IdxAddr(a, a2, o) => {
                out.push(prev_byte + (6 << 3) + a as u8);
                out.push(a2 as u8 + 8);
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
                out.extend(o.resolve_16(here));
            }
            Self::PCIdxData(d, o) => {
                out.push(prev_byte + (7 << 3) + 3);
                out.push(d as u8);
                out.push(o.resolve_8(here));
            }
            Self::PCIdxAddr(a, o) => {
                out.push(prev_byte + (7 << 3) + 3);
                out.push(a as u8 + 8);
                out.push(o.resolve_8(here));
            }
            Self::Immediate(x) => {
                out.push(prev_byte + (7 << 3) + 4);
                match size {
                    Size::Byte => {
                        out.push(0);
                        out.push(x as u8);
                    }
                    Size::Short => out.extend((x as u16).to_be_bytes()),
                    Size::Long => out.extend(x.to_be_bytes()),
                };
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn roundtrip() {
        let sizes = vec![Size::Byte, Size::Short, Size::Long];
        let eas = vec![
            EA::Data(Data::D1),
            EA::Addr(Addr::A2),
            EA::Offset(Addr::A6, 0),
            EA::PostInc(Addr::A7),
            EA::PreDec(Addr::A7),
            EA::Offset(Addr::A5, -4),
            EA::IdxData(Addr::A1, Data::D3, 8),
            EA::IdxAddr(Addr::A3, Addr::A4, 24),
            EA::Absolute(0xFF00),
            EA::Absolute(0x00CC_0000),
            EA::PCOffset(PC::Displacement(-512)),
            EA::PCIdxData(Data::D1, PC::Displacement(16)),
            EA::PCIdxAddr(Addr::A1, PC::Displacement(64)),
            EA::Immediate(69),
        ];

        for ea in eas.iter() {
            for size in sizes.iter() {
                let mut out = vec![0];
                let mut i = 0;
                ea.write(*size, &mut out, 0);
                let res = EA::read(&out, &mut i, *size);
                assert_eq!(res, Some(*ea));
            }
        }
    }
}
