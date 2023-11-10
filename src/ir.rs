pub type Word = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IR {
    pub kind: IRKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IRKind {
    // pop an address from the stack & push the value at that address
    // Deref,
    Move(IRDest, IRSrc),
    Add(IRDest, IRSrc),
    Mult(IRDest, IRSrc),
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
    StackPointer,
    PopStack,
}
