use crate::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemLocation {
    kind: MemLocationKind,
    size: Word,
}
impl MemLocation {
    pub fn local(frame_offset: Word, size: Word) -> Self {
        Self {
            kind: MemLocationKind::FrameOffset(frame_offset),
            size,
        }
    }
    pub fn r0() -> Self {
        Self {
            kind: MemLocationKind::R0,
            size: 1,
        }
    }
    pub fn r0_offset(field: StructField) -> Self {
        Self {
            kind: MemLocationKind::R0Offset(field.offset),
            size: field.ty.size(),
        }
    }
    fn to_dest(&self) -> Dest {
        Dest::Mem(self.kind)
    }
    pub fn to_src(&self) -> Src {
        match self.kind {
            MemLocationKind::FrameOffset(offset) => Src::FrameOffset(offset),
            _ => unimplemented!(),
        }
    }
    pub fn struct_field(self, field: &StructField) -> MemLocation {
        match self.kind {
            MemLocationKind::FrameOffset(parent_offset) => {
                MemLocation::local(parent_offset - field.offset, field.ty.size())
            }
            _ => unimplemented!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MemLocationKind {
    FrameOffset(Word),
    R0,
    R0Offset(Word),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Dest {
    Stack,
    Mem(MemLocationKind),
    // FrameOffset(Word),
    // R0,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Src {
    Immediate(Word),
    FrameOffset(Word),
    PopStack,
    R0,
    R0Offset(Word),
    Indexed(Word),
}

pub struct Writer {
    output: Vec<IR>,
    current_frame_size: Word,
}

impl Writer {
    pub fn new() -> Self {
        Self {
            output: Vec::new(),
            current_frame_size: 0,
        }
    }
    pub fn done(self) -> Vec<IR> {
        self.output
    }
    pub fn get_current_frame_size(&self) -> Word {
        self.current_frame_size
    }
    pub fn write(&mut self, op: IROp, dest: &MemLocation, src: Src) {
        if dest.size == 1 {
            self.write_inner(op, dest.to_dest(), src, 0);
            return;
        }

        for i in 0..dest.size {
            self.write_inner(op, dest.to_dest(), src, i);
        }
    }
    pub fn to_stack(&mut self, op: IROp, src: Src, dest_ty: &Ty) -> MemLocation {
        if dest_ty.size() == 1 {
            self.write_inner(op, Dest::Stack, src, 0);
            return MemLocation::local(self.current_frame_size, dest_ty.size());
        }
        let loc = self.allocate(dest_ty);
        self.write(op, &loc, src);
        loc
    }
    pub fn from_stack(&mut self, op: IROp, dest: &MemLocation, src: &MemLocation) {
        match src.kind {
            MemLocationKind::FrameOffset(frame_offset) => {
                let stack_offset = self.current_frame_size - frame_offset;
                if stack_offset == 0 && dest.size == 1 {
                    self.write_inner(op, dest.to_dest(), Src::PopStack, 0);
                    return;
                }
                unimplemented!()
            }
            _ => unimplemented!(),
        }
    }
    fn write_inner(&mut self, op: IROp, dest: Dest, src: Src, i: Word) {
        let ir_src = match src {
            Src::Immediate(value) => EA::Immediate(value),
            Src::FrameOffset(offset) => {
                let stack_offset = self.current_frame_size - offset + i;
                EA::StackOffset(stack_offset)
            }
            Src::PopStack => {
                self.current_frame_size -= 1;
                EA::PopStack
            }
            Src::R0Offset(offset) => EA::R0Offset(offset),
            Src::R0 => EA::R0,
            Src::Indexed(offset) => EA::Indexed(offset),
        };
        let ir_dest = match dest {
            Dest::Stack => {
                self.current_frame_size += 1;
                EA::PushStack
            }
            Dest::Mem(mem) => match mem {
                MemLocationKind::FrameOffset(offset) => {
                    let stack_offset = self.current_frame_size - offset + i;
                    EA::StackOffset(stack_offset)
                }
                MemLocationKind::R0 => EA::R0,
                MemLocationKind::R0Offset(offset) => EA::R0Offset(offset),
            },
        };
        self.output.push(op(ir_dest, ir_src));
    }
    pub fn allocate(&mut self, ty: &Ty) -> MemLocation {
        self.current_frame_size += ty.size();
        self.output.push(IR::Sub(EA::SP, EA::Immediate(ty.size())));
        MemLocation::local(self.current_frame_size, ty.size())
    }
}
