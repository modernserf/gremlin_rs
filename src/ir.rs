pub type Word = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IR {
    pub kind: IRKind,
}

impl IR {
    pub fn mov(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::Move(dest, src),
        }
    }
    pub fn load_address(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::LoadAddress(dest, src),
        }
    }
    pub fn add(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::Add(dest, src),
        }
    }
    pub fn sub(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::Sub(dest, src),
        }
    }
    pub fn mult(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::Mult(dest, src),
        }
    }
    pub fn and(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::And(dest, src),
        }
    }
    pub fn or(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::Or(dest, src),
        }
    }
    pub fn xor(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::Xor(dest, src),
        }
    }
    pub fn bit_test(dest: IRDest, src: IRSrc) -> Self {
        IR {
            kind: IRKind::BitTest(dest, src),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IRKind {
    Move(IRDest, IRSrc),
    LoadAddress(IRDest, IRSrc),
    Add(IRDest, IRSrc),
    Sub(IRDest, IRSrc),
    Mult(IRDest, IRSrc),
    And(IRDest, IRSrc),
    Or(IRDest, IRSrc),
    Xor(IRDest, IRSrc),
    BitTest(IRDest, IRSrc),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IRDest {
    PushStack,
    R0,
    SP,
    StackOffset(Word),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IRSrc {
    Immediate(Word),
    StackOffset(Word),
    R0,
    R0Offset(Word),
    #[allow(dead_code)]
    PopStack,
}
