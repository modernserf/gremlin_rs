use crate::runtime::*;
use crate::ty::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    pub fn focus_direct(&self, other: Slice) -> Self {
        Self {
            offset: self.offset + other.offset,
            size: other.size,
        }
    }
}

// a location on the stack, which may span multiple words
#[derive(Debug, Copy, Clone)]
pub struct Block {
    // relative to (imaginary) frame pointer
    offset: Word,
    size: Word,
}

impl Block {
    pub fn new(offset: Word, size: Word) -> Self {
        Self { offset, size }
    }
    pub fn size(&self) -> Word {
        self.size
    }
    pub fn frame_offset(&self) -> Word {
        self.offset
    }
    // invert frame offset to get stack offset
    pub fn to_ea(self, current_frame_offset: Word) -> EA {
        EA::Offset(Register::SP, current_frame_offset - self.offset)
    }
    // a frame offset needs to be focused in the opposite direction of a slice,
    // e.g. a struct field within a block will have a _lower_ frame offset
    pub fn focus(&self, focus: Slice) -> Block {
        assert!(focus.size <= self.size);
        Self {
            offset: self.offset - focus.offset,
            size: focus.size,
        }
    }
}
