pub type Word = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IR {
    pub kind: IRKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IRKind {
    Move(IRDest, IRSrc),
    LoadAddress(IRDest, IRSrc),
    Add(IRDest, IRSrc),
    Mult(IRDest, IRSrc),
    And(IRDest, IRSrc),
    Or(IRDest, IRSrc),
    Not(IRDest, IRSrc),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IRDest {
    PushStack,
    R0,
    StackOffset(Word),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IRSrc {
    Immediate(Word),
    Address(Word),
    StackOffset(Word),
    R0,
    AtR0,
    PopStack,
}
