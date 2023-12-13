#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Data {
    D0 = 0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}

impl From<u8> for Data {
    fn from(value: u8) -> Self {
        use Data::*;
        match value {
            0 => D0,
            1 => D1,
            2 => D2,
            3 => D3,
            4 => D4,
            5 => D5,
            6 => D6,
            7 => D7,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Addr {
    A0 = 0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
}

pub const FP: Addr = Addr::A6;
pub const SP: Addr = Addr::A7;

impl From<u8> for Addr {
    fn from(value: u8) -> Self {
        use Addr::*;
        match value {
            0 => A0,
            1 => A1,
            2 => A2,
            3 => A3,
            4 => A4,
            5 => A5,
            6 => A6,
            7 => A7,
            _ => unimplemented!(),
        }
    }
}
