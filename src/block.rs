use crate::runtime::*;
use crate::ty::*;

#[derive(Debug, Copy, Clone)]
pub struct Slice {
    pub offset: Word,
    pub size: Word,
}

impl Slice {
    pub fn with_size(size: Word) -> Self {
        Self { offset: 0, size }
    }
    pub fn from_array_index(item_ty: &Ty, index: Word) -> Self {
        Self {
            size: item_ty.size(),
            offset: item_ty.size() * index,
        }
    }
    fn focus(&self, other: Slice) -> Self {
        assert!(other.size <= self.size);
        Self {
            offset: self.offset - other.offset,
            size: other.size,
        }
    }
}

// a location in memory, which may span multiple words
#[derive(Debug, Copy, Clone)]
pub enum Block {
    // relative to (imaginary) frame pointer, converted to stack-relative
    Frame(Slice),
    // relative to the address in the specified register
    Offset(Register, Slice),
    // a single word in a register
    Register(Register),
}

impl Block {
    pub fn frame(offset: Word, size: Word) -> Self {
        Self::Frame(Slice { offset, size })
    }
    pub fn size(&self) -> Word {
        match &self {
            Self::Frame(slice) => slice.size,
            Self::Register(_) => 1,
            Self::Offset(_, slice) => slice.size,
        }
    }
    pub fn frame_slice(self) -> Option<Slice> {
        match self {
            Self::Frame(slice) => Some(slice),
            _ => None,
        }
    }
    pub fn to_ea(self, current_frame_offset: Word, index: Word) -> EA {
        match &self {
            Self::Frame(slice) => {
                EA::Offset(Register::SP, current_frame_offset - slice.offset + index)
            }
            Self::Register(register) => {
                assert_eq!(index, 0);
                EA::Register(*register)
            }
            Self::Offset(register, slice) => EA::Offset(*register, slice.offset + index),
        }
    }
    pub fn focus(&self, focus: Slice) -> Block {
        match &self {
            Self::Frame(slice) => Self::Frame(slice.focus(focus)),
            _ => unimplemented!(),
        }
    }
}
