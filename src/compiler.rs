use crate::{
    ast::{
        BinOpKind, Bind, BindKind, Expr, ExprKind, Stmt, StmtKind, TyExpr, TyExprKind, UnOpKind,
    },
    ir::{IRDest, IRKind, IRSrc, Word, IR},
    source_info::SourceInfo,
};
use std::collections::HashMap;

pub type Compile<T> = Result<T, CompileError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {
    kind: CmpErrKind,
    source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CmpErrKind {
    UnknownIdentifier,
    UnknownTypeIdentifier,
    IntOutOfRange,
    InvalidReference,
    TypeError { expected: TypeId, received: TypeId },
}

// offset from value of stack pointer when function begins
// starts at 0 and increases as stack grows
// args are negative, locals are positive
// current stack pointer - current frame offset = original stack pointer
type FrameOffset = isize;

pub struct Compiler {
    scope: HashMap<String, ScopeRec>,
    program: Vec<IR>,
    current_frame_offset: FrameOffset,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ScopeRec {
    frame_offset: FrameOffset,
    type_id: TypeId,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TypeId(usize);
const INT_TYPE: TypeId = TypeId(1);
const BOOL_TYPE: TypeId = TypeId(2);

impl TypeId {
    fn check(&self, expected: TypeId, source_info: SourceInfo) -> Compile<()> {
        if *self != expected {
            Err(CompileError {
                kind: CmpErrKind::TypeError {
                    expected,
                    received: *self,
                },
                source_info,
            })
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExprResult {
    type_id: TypeId,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct LocalResult {
    type_id: TypeId,
    stack_offset: Word,
}

impl Compiler {
    pub fn compile(program: &[Stmt]) -> Compile<Vec<IR>> {
        let mut compiler = Self::new();
        for stmt in program {
            compiler.stmt(stmt)?;
        }
        Ok(compiler.program)
    }
    fn new() -> Self {
        Self {
            scope: HashMap::new(),
            program: Vec::new(),
            current_frame_offset: 0,
        }
    }
    fn inc_frame_offset(&mut self) {
        self.current_frame_offset += 1;
    }
    fn dec_frame_offset(&mut self) {
        self.current_frame_offset -= 1;
    }
    fn stmt(&mut self, stmt: &Stmt) -> Compile<()> {
        match &stmt.kind {
            StmtKind::Let(payload) => {
                let res = self.expr(&payload.expr)?;

                if let Some(ty) = &payload.ty {
                    let type_id = self.get_type_id(ty)?;
                    type_id.check(res.type_id, payload.expr.source_info)?;
                }

                self.init_local(&payload.binding, res.type_id)?;
            }
            StmtKind::Assign(payload) => {
                self.expr(&payload.expr)?;
                // dest is relative to stack _after_ value is popped, so dec before getting offset
                self.dec_frame_offset();
                let dest = self.get_assign_dest(&payload.target)?;
                self.program.push(IR {
                    kind: IRKind::Move(dest, IRSrc::PopStack),
                });
            }
            StmtKind::Expr(expr) => {
                self.expr(expr)?;
            }
            StmtKind::Noop => {}
        };
        Ok(())
    }
    fn expr(&mut self, expr: &Expr) -> Compile<ExprResult> {
        match &expr.kind {
            ExprKind::Int(payload) => {
                if payload.value >= (u32::MAX as u128) {
                    return Err(CompileError {
                        kind: CmpErrKind::IntOutOfRange,
                        source_info: expr.source_info,
                    });
                }
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::Immediate(payload.value as Word)),
                });
                self.inc_frame_offset();
                Ok(ExprResult { type_id: INT_TYPE })
            }
            ExprKind::True => {
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::Immediate(1)),
                });
                self.inc_frame_offset();
                Ok(ExprResult { type_id: BOOL_TYPE })
            }
            ExprKind::False => {
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::Immediate(0)),
                });
                self.inc_frame_offset();
                Ok(ExprResult { type_id: BOOL_TYPE })
            }
            ExprKind::Ident(payload) => {
                let local = self.get_local(&payload.value, expr.source_info)?;
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::StackOffset(local.stack_offset)),
                });
                self.inc_frame_offset();
                Ok(ExprResult {
                    type_id: local.type_id,
                })
            }
            ExprKind::As(payload) => {
                // TODO: check for compatible sizes
                self.expr(&payload.expr)?;
                let type_id = self.get_type_id(&payload.ty)?;
                Ok(ExprResult { type_id })
            }
            ExprKind::UnaryOp(payload) => {
                match &payload.operator {
                    UnOpKind::Deref => {
                        self.expr(&payload.expr)?;
                        self.program.push(IR {
                            kind: IRKind::Move(IRDest::R0, IRSrc::PopStack),
                        });
                        self.program.push(IR {
                            kind: IRKind::Move(IRDest::PushStack, IRSrc::AtR0),
                        });
                        // FIXME
                        Ok(ExprResult { type_id: INT_TYPE })
                    }
                    UnOpKind::Ref => {
                        let src = self.get_ref_src(&payload.expr)?;
                        self.program.push(IR {
                            kind: IRKind::LoadAddress(IRDest::PushStack, src),
                        });
                        self.inc_frame_offset();
                        // FIXME
                        Ok(ExprResult { type_id: INT_TYPE })
                    }
                    UnOpKind::Not => {
                        let res = self.expr(&payload.expr)?;
                        res.type_id.check(BOOL_TYPE, payload.expr.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Not(IRDest::StackOffset(0), IRSrc::StackOffset(0)),
                        });
                        Ok(ExprResult { type_id: BOOL_TYPE })
                    }
                }
            }
            ExprKind::BinaryOp(payload) => {
                let right = self.expr(&payload.right)?;
                let left = self.expr(&payload.left)?;
                let res = match &payload.operator {
                    BinOpKind::Add => {
                        right.type_id.check(INT_TYPE, payload.right.source_info)?;
                        left.type_id.check(INT_TYPE, payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Add(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult { type_id: INT_TYPE })
                    }
                    BinOpKind::Mult => {
                        right.type_id.check(INT_TYPE, payload.right.source_info)?;
                        left.type_id.check(INT_TYPE, payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Mult(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult { type_id: INT_TYPE })
                    }
                    BinOpKind::And => {
                        right.type_id.check(BOOL_TYPE, payload.right.source_info)?;
                        left.type_id.check(BOOL_TYPE, payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::And(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult { type_id: BOOL_TYPE })
                    }
                    BinOpKind::Or => {
                        right.type_id.check(BOOL_TYPE, payload.right.source_info)?;
                        left.type_id.check(BOOL_TYPE, payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Or(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult { type_id: BOOL_TYPE })
                    }
                };
                self.dec_frame_offset();
                res
            }
        }
    }
    fn init_local(&mut self, binding: &Bind, type_id: TypeId) -> Compile<()> {
        match &binding.kind {
            BindKind::Ident(ident) => {
                let _prev = self.scope.insert(
                    ident.value.to_string(),
                    ScopeRec {
                        frame_offset: self.current_frame_offset,
                        type_id,
                    },
                );
            }
        };
        Ok(())
    }
    fn get_assign_dest(&mut self, expr: &Expr) -> Compile<IRDest> {
        match &expr.kind {
            ExprKind::Ident(payload) => {
                let local = self.get_local(&payload.value, expr.source_info)?;
                Ok(IRDest::StackOffset(local.stack_offset))
            }
            _ => Err(CompileError {
                kind: CmpErrKind::InvalidReference,
                source_info: expr.source_info,
            }),
        }
    }
    fn get_ref_src(&mut self, expr: &Expr) -> Compile<IRSrc> {
        match &expr.kind {
            ExprKind::Ident(payload) => {
                let local = self.get_local(&payload.value, expr.source_info)?;
                Ok(IRSrc::StackOffset(local.stack_offset))
            }
            _ => Err(CompileError {
                kind: CmpErrKind::InvalidReference,
                source_info: expr.source_info,
            }),
        }
    }
    fn get_local(&mut self, key: &str, source_info: SourceInfo) -> Compile<LocalResult> {
        match self.scope.get(key) {
            None => Err(CompileError {
                kind: CmpErrKind::UnknownIdentifier,
                source_info,
            }),
            Some(scope_rec) => {
                let stack_offset = self.current_frame_offset - scope_rec.frame_offset;
                debug_assert!(stack_offset >= 0);
                Ok(LocalResult {
                    type_id: scope_rec.type_id,
                    stack_offset: stack_offset as Word,
                })
            }
        }
    }
    fn get_type_id(&mut self, ty: &TyExpr) -> Compile<TypeId> {
        match &ty.kind {
            TyExprKind::Identifier(payload) => {
                // TODO: table lookup
                match payload.value.as_str() {
                    "bool" => Ok(BOOL_TYPE),
                    "int" => Ok(INT_TYPE),
                    _ => Err(CompileError {
                        kind: CmpErrKind::UnknownTypeIdentifier,
                        source_info: ty.source_info,
                    }),
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn check_err(str: &str, expected: CompileError) {
        let result = Compiler::compile(&Parser::parse_body(Lexer::lex(str)).expect("expr"))
            .expect_err("compile error");

        assert_eq!(result, expected);
    }

    #[test]
    fn unknown_identifier() {
        check_err(
            "  foo  ",
            CompileError {
                kind: CmpErrKind::UnknownIdentifier,
                source_info: SourceInfo {
                    start: 2,
                    length: 3,
                },
            },
        )
    }

    #[test]
    fn int_out_of_range() {
        check_err(
            "12345678901234567890",
            CompileError {
                kind: CmpErrKind::IntOutOfRange,
                source_info: SourceInfo {
                    start: 0,
                    length: 20,
                },
            },
        )
    }

    #[test]
    fn invalid_reference() {
        check_err(
            "&3",
            CompileError {
                kind: CmpErrKind::InvalidReference,
                source_info: SourceInfo {
                    start: 1,
                    length: 1,
                },
            },
        )
    }

    #[test]
    fn type_error() {
        check_err(
            "not 1",
            CompileError {
                kind: CmpErrKind::TypeError {
                    expected: BOOL_TYPE,
                    received: INT_TYPE,
                },
                source_info: SourceInfo {
                    start: 4,
                    length: 1,
                },
            },
        );

        check_err(
            "true + 2",
            CompileError {
                kind: CmpErrKind::TypeError {
                    expected: INT_TYPE,
                    received: BOOL_TYPE,
                },
                source_info: SourceInfo {
                    start: 0,
                    length: 4,
                },
            },
        );

        check_err(
            "let x := false; x + 1",
            CompileError {
                kind: CmpErrKind::TypeError {
                    expected: INT_TYPE,
                    received: BOOL_TYPE,
                },
                source_info: SourceInfo {
                    start: 16,
                    length: 1,
                },
            },
        );

        check_err(
            "let x : bool := 1",
            CompileError {
                kind: CmpErrKind::TypeError {
                    expected: INT_TYPE,
                    received: BOOL_TYPE,
                },
                source_info: SourceInfo {
                    start: 16,
                    length: 1,
                },
            },
        );
    }

    #[test]
    fn unknown_type_identifier() {
        check_err(
            "123 as foobar",
            CompileError {
                kind: CmpErrKind::UnknownTypeIdentifier,
                source_info: SourceInfo {
                    start: 7,
                    length: 6,
                },
            },
        )
    }
}
