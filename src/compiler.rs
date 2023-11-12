use std::collections::HashMap;

use crate::typed_ast as t;
use crate::{ast, ir::*};

type FrameOffset = isize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ScopeRecord {
    frame_offset: FrameOffset,
    width: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExprContext {
    dest: ExprDest,
}

impl ExprContext {
    fn to_stack() -> Self {
        ExprContext {
            dest: ExprDest::push_stack(1),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct ExprDest {
    kind: ExprDestKind,
    width: usize,
}

impl ExprDest {
    fn frame_offset(offset: FrameOffset, width: usize) -> Self {
        ExprDest {
            kind: ExprDestKind::FrameOffset(offset),
            width,
        }
    }
    fn push_stack(width: usize) -> Self {
        ExprDest {
            kind: ExprDestKind::PushStack,
            width,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ExprDestKind {
    PushStack,
    FrameOffset(FrameOffset),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct ExprSrc {
    kind: ExprSrcKind,
    width: usize,
}

impl ExprSrc {
    fn imm(value: Word) -> Self {
        ExprSrc {
            kind: ExprSrcKind::Immediate(value),
            width: 1,
        }
    }
    fn frame_offset(offset: FrameOffset, width: usize) -> Self {
        ExprSrc {
            kind: ExprSrcKind::FrameOffset(offset),
            width,
        }
    }
    fn pop_stack(width: usize) -> Self {
        ExprSrc {
            kind: ExprSrcKind::PopStack,
            width,
        }
    }
    fn at_r0(width: usize) -> Self {
        ExprSrc {
            kind: ExprSrcKind::AtR0,
            width,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ExprSrcKind {
    Immediate(Word),
    FrameOffset(FrameOffset),
    PopStack,
    AtR0,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct WriteResult {
    kind: WriteResultKind,
    width: usize,
}

impl WriteResult {
    fn to_dest(&self) -> ExprDest {
        match self.kind {
            WriteResultKind::FrameOffset(frame_offset) => {
                ExprDest::frame_offset(frame_offset, self.width)
            }
        }
    }
    fn to_scope_record(&self) -> ScopeRecord {
        match self.kind {
            WriteResultKind::FrameOffset(frame_offset) => ScopeRecord {
                frame_offset,
                width: self.width,
            },
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum WriteResultKind {
    FrameOffset(FrameOffset),
}

type Compile<T> = Result<T, CompileError>;
pub struct Compiler {
    output: Vec<IR>,
    scope: HashMap<usize, ScopeRecord>,
    current_frame_offset: FrameOffset,
}

impl Compiler {
    pub fn compile(body: &[t::Stmt]) -> Compile<Vec<IR>> {
        let mut compiler = Self::new();
        compiler.body(body)?;
        Ok(compiler.output)
    }
    fn new() -> Self {
        Self {
            output: Vec::new(),
            scope: HashMap::new(),
            current_frame_offset: 0,
        }
    }

    fn body(&mut self, body: &[t::Stmt]) -> Compile<()> {
        for stmt in body {
            self.stmt(stmt)?;
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &t::Stmt) -> Compile<()> {
        match stmt {
            t::Stmt::Let(t) => {
                let res = self.expr(&t.expr, ExprContext::to_stack())?;
                self.scope.insert(t.bind_id, res.to_scope_record());
                Ok(())
            }
            t::Stmt::Assign(t) => {
                let dest = self.get_scope_dest(t.bind_id);
                self.expr(&t.expr, ExprContext { dest })?;
                Ok(())
            }
            t::Stmt::Expr(expr) => {
                self.expr(expr, ExprContext::to_stack())?;
                Ok(())
            }
        }
    }
    fn expr(&mut self, expr: &t::Expr, context: ExprContext) -> Compile<WriteResult> {
        match &expr.kind {
            t::ExprKind::Constant(value) => {
                let res = self.write(IR::mov, context.dest, ExprSrc::imm(*value));
                Ok(res)
            }
            t::ExprKind::Ident(id) => {
                let src = self.get_scope_src(*id);
                let res = self.write(IR::mov, context.dest, src);
                Ok(res)
            }
            t::ExprKind::RefIdent(id) => {
                let src = self.get_scope_src(*id);
                let res = self.write(IR::load_address, context.dest, src);
                Ok(res)
            }
            t::ExprKind::Deref(t) => {
                self.expr(t, ExprContext::to_stack())?;
                self.write_ir(IR::mov(IRDest::R0, IRSrc::PopStack));
                self.current_frame_offset -= 1;
                let res = self.write(IR::mov, context.dest, ExprSrc::at_r0(t.ty.size()));
                Ok(res)
            }
            t::ExprKind::Not(t) => {
                let res = self.expr(t, context)?;
                let res = self.write(IR::xor, res.to_dest(), ExprSrc::imm(1));
                Ok(res)
            }
            t::ExprKind::BinaryOp(t) => {
                let left_res = self.expr(&t.left, context)?;
                self.expr(&t.right, ExprContext::to_stack())?;
                // TODO: ops on larger than word-sized values
                let op = match t.operator {
                    ast::BinOpKind::Add => IR::add,
                    ast::BinOpKind::Mult => IR::mult,
                    ast::BinOpKind::And => IR::and,
                    ast::BinOpKind::Or => IR::or,
                };
                let res = self.write(op, left_res.to_dest(), ExprSrc::pop_stack(1));
                Ok(res)
            }
        }
    }
    fn get_scope_dest(&self, id: usize) -> ExprDest {
        let rec = self.scope.get(&id).expect("scope record");
        ExprDest::frame_offset(rec.frame_offset, rec.width)
    }
    fn get_scope_src(&self, id: usize) -> ExprSrc {
        let rec = self.scope.get(&id).expect("scope record");
        ExprSrc::frame_offset(rec.frame_offset, rec.width)
    }
    fn write_ir(&mut self, ir: IR) {
        self.output.push(ir);
    }
    fn frame_to_stack_offset(&self, frame_offset: FrameOffset, width: usize, index: usize) -> Word {
        (self.current_frame_offset - frame_offset - (width as isize) + (index as isize)) as Word
    }
    fn write(&mut self, op: fn(IRDest, IRSrc) -> IR, dest: ExprDest, src: ExprSrc) -> WriteResult {
        debug_assert!(dest.width == src.width);
        use ExprDestKind as D;
        use ExprSrcKind as S;
        let init_frame_offset = self.current_frame_offset;
        for i in 0..src.width {
            let ir_src = match src.kind {
                S::Immediate(value) => IRSrc::Immediate(value),
                S::FrameOffset(src_offset) => {
                    let stack_offset = self.frame_to_stack_offset(src_offset, src.width, i);
                    IRSrc::StackOffset(stack_offset)
                }
                S::PopStack => {
                    self.current_frame_offset -= 1;
                    IRSrc::PopStack
                }
                S::AtR0 => IRSrc::R0Offset(i as Word),
            };

            let ir_dest = match dest.kind {
                D::PushStack => {
                    self.current_frame_offset += 1;
                    IRDest::PushStack
                }
                D::FrameOffset(dest_offset) => {
                    let stack_offset = self.frame_to_stack_offset(dest_offset, dest.width, i);
                    IRDest::StackOffset(stack_offset)
                }
            };

            self.write_ir(op(ir_dest, ir_src));
        }
        match dest.kind {
            D::FrameOffset(offset) => WriteResult {
                kind: WriteResultKind::FrameOffset(offset),
                width: dest.width,
            },
            D::PushStack => WriteResult {
                kind: WriteResultKind::FrameOffset(init_frame_offset),
                width: dest.width,
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::{IRDest, IRSrc, IR};

    #[test]
    fn frame_offset_push() {
        let mut compiler = Compiler::new();
        let left_res = compiler.write(IR::mov, ExprDest::push_stack(1), ExprSrc::imm(1));
        compiler.write(IR::mov, ExprDest::push_stack(1), ExprSrc::imm(2));
        assert_eq!(compiler.current_frame_offset, 2);
        compiler.write(IR::add, left_res.to_dest(), ExprSrc::pop_stack(1));
        assert_eq!(compiler.current_frame_offset, 1);
        assert_eq!(
            compiler.output,
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(1)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(2)),
                IR::add(IRDest::StackOffset(0), IRSrc::PopStack),
            ],
        );
    }

    #[test]
    fn identifiers() {
        let mut compiler = Compiler::new();
        // let x = 123
        compiler.write(IR::mov, ExprDest::push_stack(1), ExprSrc::imm(123));
        // let y = 456
        compiler.write(IR::mov, ExprDest::push_stack(1), ExprSrc::imm(456));
        // x
        compiler.write(
            IR::mov,
            ExprDest::push_stack(1),
            ExprSrc::frame_offset(0, 1),
        );
        assert_eq!(compiler.current_frame_offset, 3);
        assert_eq!(
            compiler.output,
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(123)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(456)),
                IR::mov(IRDest::PushStack, IRSrc::StackOffset(1)),
            ]
        );
    }

    #[test]
    fn assignment() {
        let mut compiler = Compiler::new();
        // let x = 123
        compiler.write(IR::mov, ExprDest::push_stack(1), ExprSrc::imm(123));
        // let y = 456
        compiler.write(IR::mov, ExprDest::push_stack(1), ExprSrc::imm(456));
        // x  = 789
        compiler.write(IR::mov, ExprDest::frame_offset(0, 1), ExprSrc::imm(789));

        assert_eq!(compiler.current_frame_offset, 2);
        assert_eq!(
            compiler.output,
            vec![
                IR::mov(IRDest::PushStack, IRSrc::Immediate(123)),
                IR::mov(IRDest::PushStack, IRSrc::Immediate(456)),
                IR::mov(IRDest::StackOffset(1), IRSrc::Immediate(789)),
            ]
        );
    }
}
