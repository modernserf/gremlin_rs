use std::rc::Rc;

use crate::compiler::*;
use crate::expr::ResolvedExpr;
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
    pub fn args_size(&self) -> Word {
        self.params.iter().map(|p| p.size()).sum::<Word>()
    }
}

pub struct CallBuilder {
    pub sub_index: SubIndex,
    pub ty_sub: Rc<TySub>,
    pub return_block: Block,
    pub current_arg: usize,
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
    pub fn push_sub_scope(self, compiler: &mut Compiler) {
        let params = self.params.iter().map(|(_, ty)| ty.clone()).collect();
        let ty_sub = TySub::new(params, self.return_type.clone());
        // frame offset is negative for args & return slot
        let mut frame_offset = -(ty_sub.args_size() + 1);
        let return_expr = ResolvedExpr {
            block: Block::stack(frame_offset, self.return_type.size()),
            ty: self.return_type,
        };
        compiler.enter_sub(self.name, Ty::sub(ty_sub), return_expr);

        for (key, ty) in self.params {
            frame_offset += ty.size();
            compiler.sub_param(key, frame_offset, ty);
        }

        debug_assert!(frame_offset == -1);
    }
}
