use std::collections::HashMap;

use crate::typed_ast as t;
use crate::{ast, ir::*};

type FrameOffset = isize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ScopeRecord {
    frame_offset: FrameOffset,
    width: Word,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExprContext {
    dest: ExprDest,
}

impl ExprContext {
    fn to_stack() -> Self {
        ExprContext {
            dest: ExprDest::push_stack(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct ExprDest {
    kind: ExprDestKind,
    width: Word,
}

impl ExprDest {
    fn frame_offset(offset: FrameOffset, width: Word) -> Self {
        ExprDest {
            kind: ExprDestKind::FrameOffset(offset),
            width,
        }
    }
    fn push_stack() -> Self {
        ExprDest {
            kind: ExprDestKind::PushStack,
            width: 1,
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
    // separate src width & dest width
    // for converting reference<->value
    src_width: Word,
    dest_width: Word,
}

impl ExprSrc {
    fn imm(value: Word) -> Self {
        ExprSrc {
            kind: ExprSrcKind::Immediate(value),
            src_width: 1,
            dest_width: 1,
        }
    }
    fn long(hi: Word, lo: Word) -> Self {
        ExprSrc {
            kind: ExprSrcKind::Long(hi, lo),
            src_width: 2,
            dest_width: 2,
        }
    }
    fn frame_offset(offset: FrameOffset, width: Word) -> Self {
        ExprSrc {
            kind: ExprSrcKind::FrameOffset(offset),
            src_width: width,
            dest_width: width,
        }
    }
    fn pop_stack(width: Word) -> Self {
        ExprSrc {
            kind: ExprSrcKind::PopStack,
            src_width: width,
            dest_width: width,
        }
    }
    fn at_r0(width: Word) -> Self {
        ExprSrc {
            kind: ExprSrcKind::AtR0,
            src_width: width,
            dest_width: width,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ExprSrcKind {
    Immediate(Word),
    Long(Word, Word),
    FrameOffset(FrameOffset),
    PopStack,
    AtR0,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct WriteResult {
    kind: WriteResultKind,
    width: Word,
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
            t::ExprKind::Long(hi, lo) => {
                let res = self.write(IR::mov, context.dest, ExprSrc::long(*hi, *lo));
                Ok(res)
            }
            t::ExprKind::Ident(id) => {
                let src = self.get_scope_src(*id);
                let res = self.write(IR::mov, context.dest, src);
                Ok(res)
            }
            t::ExprKind::RefIdent(id) => {
                let mut src = self.get_scope_src(*id);
                src.dest_width = 1;
                let res = self.write(IR::load_address, context.dest, src);
                Ok(res)
            }
            t::ExprKind::Deref(t) => {
                self.expr(t, ExprContext::to_stack())?;
                self.write_ir(IR::mov(IRDest::R0, IRSrc::PopStack));
                self.current_frame_offset -= 1;
                let deref_size = t.ty.deref().expect("deref").size();
                let res = self.write(IR::mov, context.dest, ExprSrc::at_r0(deref_size as Word));
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
    fn frame_to_stack_offset(&self, frame_offset: FrameOffset, width: Word, index: Word) -> Word {
        let base_offset = self.current_frame_offset - frame_offset;
        base_offset as Word + index - width
    }
    fn write(&mut self, op: fn(IRDest, IRSrc) -> IR, dest: ExprDest, src: ExprSrc) -> WriteResult {
        use ExprDestKind as D;
        use ExprSrcKind as S;
        let init_frame_offset = self.current_frame_offset;

        // if pushing more than 1 word onto stack, allocate space first (instead of writing data in reverse)
        if src.dest_width > 1 && dest.kind == D::PushStack {
            self.write_ir(IR::sub(IRDest::SP, IRSrc::Immediate(src.dest_width)));
            self.current_frame_offset += src.dest_width as isize;
            return self.write(
                op,
                ExprDest::frame_offset(init_frame_offset, src.dest_width),
                src,
            );
        }
        debug_assert!(dest.width == src.dest_width);

        // TODO: inline a memcpy loop here for "large" data

        for i in 0..src.dest_width {
            let ir_src = match src.kind {
                S::Immediate(value) => IRSrc::Immediate(value),
                S::Long(hi, lo) => IRSrc::Immediate([lo, hi][i as usize]),
                S::FrameOffset(src_offset) => {
                    IRSrc::StackOffset(self.frame_to_stack_offset(src_offset, src.src_width, i))
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
        let left_res = compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::imm(1));
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::imm(2));
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
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::imm(123));
        // let y = 456
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::imm(456));
        // x
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::frame_offset(0, 1));
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
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::imm(123));
        // let y = 456
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::imm(456));
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

    #[test]
    fn longs() {
        let mut compiler = Compiler::new();
        compiler.write(IR::mov, ExprDest::push_stack(), ExprSrc::long(123, 456));
        assert_eq!(compiler.current_frame_offset, 2);
        assert_eq!(
            compiler.output,
            vec![
                IR::sub(IRDest::SP, IRSrc::Immediate(2)),
                IR::mov(IRDest::StackOffset(0), IRSrc::Immediate(456)),
                IR::mov(IRDest::StackOffset(1), IRSrc::Immediate(123)),
            ]
        );
    }
}
