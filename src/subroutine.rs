use std::rc::Rc;

use crate::memory::*;
use crate::runtime::Word;
use crate::ty::*;

// Calling convention
// [return value, args..., return addr, locals...] top
// caller:
// Sub(SP, sizeof return)
// Mov(Push, arg) ...
// JSR sub
// ...
// Add(SP, sizeof args)
// callee:
// ...
// Mov(SP+return, result)
// Add(SP, sizeof locals)
// RTS

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TySub {
    pub params: Vec<Ty>,
    pub ret: Ty,
}

impl TySub {
    pub fn new(params: Vec<Ty>, ret: Ty) -> Self {
        Self { params, ret }
    }
    pub fn return_frame_offset(&self) -> Word {
        -(self.params.iter().map(|p| p.size()).sum::<Word>() + 1)
    }
}

pub struct CallBuilder {
    sub_index: SubIndex,
    ty_sub: Rc<TySub>,
}

pub struct SubBuilder {
    name: String,
    params: Vec<(String, Ty)>,
    return_type: Ty,
}

impl SubBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            return_type: Ty::void(),
        }
    }
    pub fn add_param(&mut self, name: String, ty: Ty) {
        self.params.push((name, ty));
    }
    pub fn returns(&mut self, ty: Ty) {
        self.return_type = ty;
    }
    pub fn push_sub_scope(self, memory: &mut Memory, scope: &mut Scope) {
        let params = self.params.iter().map(|(_, ty)| ty.clone()).collect();
        let ty_sub = TySub::new(params, self.return_type);
        // frame offset is negative for args & return slot
        let mut frame_offset = ty_sub.return_frame_offset();
        scope.store_sub(self.name, Ty::sub(ty_sub), memory.sub());
        scope.push_scope(memory);

        for (key, ty) in self.params {
            let size = ty.size();
            scope.store_local(key, ty, frame_offset);
            frame_offset += size;
        }

        debug_assert!(frame_offset == -1);
    }
}
