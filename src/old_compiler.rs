use crate::{
    ast::{
        BinOpKind, Bind, BindKind, Expr, ExprKind, FieldKind, Stmt, StmtKind, TyExpr, TyExprKind,
        UnOpKind,
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

impl CompileError {
    fn type_error(expected: &Ty, received: &Ty, source_info: SourceInfo) -> Self {
        Self {
            kind: CmpErrKind::TypeError(Box::new(TypeError {
                expected: expected.clone(),
                received: received.clone(),
            })),
            source_info,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CmpErrKind {
    UnknownIdentifier,
    UnknownTypeIdentifier,
    IntOutOfRange,
    InvalidReference,
    TypeError(Box<TypeError>),
    DuplicateStructField,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
    pub expected: Ty,
    pub received: Ty,
}

// offset from value of stack pointer when function begins
// starts at 0 and increases as stack grows
// args are negative, locals are positive
// current stack pointer - current frame offset = original stack pointer
type FrameOffset = isize;

#[derive(Clone, Debug, PartialEq, Eq)]
struct ScopeRec {
    frame_offset: FrameOffset,
    ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ty {
    id: usize,
    kind: TyKind,
    width: usize,
    ref_level: usize,
}

impl Ty {
    fn int_type() -> Self {
        Self {
            id: 1,
            kind: TyKind::Primitive,
            width: 1,
            ref_level: 0,
        }
    }
    fn bool_type() -> Self {
        Self {
            id: 2,
            kind: TyKind::Primitive,
            width: 1,
            ref_level: 0,
        }
    }
    fn add_ref(&self) -> Self {
        let mut next = self.clone();
        next.ref_level += 1;
        next
    }
    fn deref(&self, source_info: SourceInfo) -> Compile<Self> {
        if self.ref_level == 0 {
            return Err(CompileError {
                kind: CmpErrKind::InvalidReference,
                source_info,
            });
        }
        let mut next = self.clone();
        next.ref_level -= 1;
        Ok(next)
    }
    fn check(&self, expected: &Ty, source_info: SourceInfo) -> Compile<()> {
        if self != expected {
            Err(CompileError::type_error(expected, self, source_info))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Primitive,
    Struct(Struct),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Struct {
    fields: HashMap<String, StructTyField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct StructTyField {
    offset: usize,
    ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExprResult {
    ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct LocalResult {
    ty: Ty,
    stack_offset: Word,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Compiler {
    scope: HashMap<String, ScopeRec>,
    ty_scope: HashMap<String, Ty>,
    last_ty_id: usize,
    program: Vec<IR>,
    current_frame_offset: FrameOffset,
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
            ty_scope: HashMap::from_iter(vec![
                ("int".to_string(), Ty::int_type()),
                ("bool".to_string(), Ty::bool_type()),
            ]),
            last_ty_id: 2,
        }
    }
    fn inc_frame_offset(&mut self) {
        self.current_frame_offset += 1;
    }
    fn dec_frame_offset(&mut self) {
        self.current_frame_offset -= 1;
    }
    fn next_ty_id(&mut self) -> usize {
        self.last_ty_id += 1;
        self.last_ty_id
    }
    fn stmt(&mut self, stmt: &Stmt) -> Compile<()> {
        match &stmt.kind {
            StmtKind::Let(payload) => {
                let res = self.expr(&payload.expr)?;

                if let Some(ty) = &payload.ty {
                    let type_id = self.get_ty(ty)?;
                    type_id.check(&res.ty, payload.expr.source_info)?;
                }

                self.init_local(&payload.binding, res.ty)?;
            }
            StmtKind::TypeDef(payload) => {
                let ty = self.get_ty(&payload.ty)?;
                self.ty_scope.insert(payload.identifier.to_string(), ty);
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
                Ok(ExprResult { ty: Ty::int_type() })
            }
            ExprKind::True => {
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::Immediate(1)),
                });
                self.inc_frame_offset();
                Ok(ExprResult {
                    ty: Ty::bool_type(),
                })
            }
            ExprKind::False => {
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::Immediate(0)),
                });
                self.inc_frame_offset();
                Ok(ExprResult {
                    ty: Ty::bool_type(),
                })
            }
            ExprKind::Ident(payload) => {
                let local = self.get_local(&payload.value, expr.source_info)?;
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::StackOffset(local.stack_offset)),
                });
                self.inc_frame_offset();
                Ok(ExprResult { ty: local.ty })
            }
            ExprKind::As(payload) => {
                // TODO: check for compatible sizes
                self.expr(&payload.expr)?;
                let ty = self.get_ty(&payload.ty)?;
                Ok(ExprResult { ty })
            }
            ExprKind::UnaryOp(payload) => match &payload.operator {
                UnOpKind::Deref => {
                    let res = self.expr(&payload.expr)?;
                    let ty = res.ty.deref(payload.expr.source_info)?;
                    self.program.push(IR {
                        kind: IRKind::Move(IRDest::R0, IRSrc::PopStack),
                    });
                    self.program.push(IR {
                        kind: IRKind::Move(IRDest::PushStack, IRSrc::R0Offset(0)),
                    });
                    Ok(ExprResult { ty })
                }
                UnOpKind::Ref => {
                    let (src, local) = self.get_ref_src(&payload.expr)?;
                    self.program.push(IR {
                        kind: IRKind::LoadAddress(IRDest::PushStack, src),
                    });
                    self.inc_frame_offset();
                    Ok(ExprResult {
                        ty: local.ty.add_ref(),
                    })
                }
                UnOpKind::Not => {
                    let res = self.expr(&payload.expr)?;
                    res.ty.check(&Ty::bool_type(), payload.expr.source_info)?;
                    self.program.push(IR {
                        kind: IRKind::Xor(IRDest::StackOffset(0), IRSrc::Immediate(1)),
                    });
                    Ok(ExprResult {
                        ty: Ty::bool_type(),
                    })
                }
            },
            ExprKind::BinaryOp(payload) => {
                let right = self.expr(&payload.right)?;
                let left = self.expr(&payload.left)?;
                let res = match &payload.operator {
                    BinOpKind::Add => {
                        right.ty.check(&Ty::int_type(), payload.right.source_info)?;
                        left.ty.check(&Ty::int_type(), payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Add(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult { ty: Ty::int_type() })
                    }
                    BinOpKind::Mult => {
                        right.ty.check(&Ty::int_type(), payload.right.source_info)?;
                        left.ty.check(&Ty::int_type(), payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Mult(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult { ty: Ty::int_type() })
                    }
                    BinOpKind::And => {
                        right
                            .ty
                            .check(&Ty::bool_type(), payload.right.source_info)?;
                        left.ty.check(&Ty::bool_type(), payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::And(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult {
                            ty: Ty::bool_type(),
                        })
                    }
                    BinOpKind::Or => {
                        right
                            .ty
                            .check(&Ty::bool_type(), payload.right.source_info)?;
                        left.ty.check(&Ty::bool_type(), payload.left.source_info)?;
                        self.program.push(IR {
                            kind: IRKind::Or(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                        Ok(ExprResult {
                            ty: Ty::bool_type(),
                        })
                    }
                };
                self.dec_frame_offset();
                res
            }
            ExprKind::Struct(payload) => {
                let ty = self.get_ty_name(&payload.name, expr.source_info)?.clone();
                let struct_type = match &ty.kind {
                    TyKind::Struct(struct_type) => struct_type,
                    _ => {
                        panic!("todo not a struct type")
                    }
                };

                let struct_frame_offset = self.current_frame_offset;
                // Allocate struct space
                self.program.push(IR {
                    kind: IRKind::Sub(IRDest::SP, IRSrc::Immediate(ty.width as Word)),
                });
                self.current_frame_offset += ty.width as isize;

                for field in &payload.fields {
                    // write field value onto stack
                    // TODO: pass dest to self.expr() to get correct placement instead of moving off stack
                    let res = self.expr(&field.value)?;
                    // check against matching struct field
                    let ty_field = {
                        match struct_type.fields.get(&field.key) {
                            Some(t) => t,
                            None => {
                                panic!("todo unknown field")
                            }
                        }
                    };
                    res.ty.check(&ty_field.ty, expr.source_info)?;
                    // move to correct position
                    // TODO: move multiple words
                    let struct_stack_offset = self.to_stack_offset(struct_frame_offset);
                    self.program.push(IR {
                        kind: IRKind::Move(
                            IRDest::StackOffset(struct_stack_offset),
                            IRSrc::PopStack,
                        ),
                    });
                    self.dec_frame_offset();
                }
                // TODO: check for missing / duplicate fields
                Ok(ExprResult { ty })
            }
            ExprKind::Field(payload) => {
                // TODO: I want to make space on the stack for this field, but I need to have the type info _before_ i compile

                // let res = self.expr(&payload.expr)?;
                // let field_ty = match &res.ty.kind {
                //     TyKind::Struct(st) => match &payload.field {
                //         FieldKind::Identifier(key) => match st.fields.get(key) {
                //             Some(t) => t,
                //             None => {
                //                 panic!("todo unknown field")
                //             }
                //         },
                //     },
                //     _ => panic!("todo not a struct"),
                // };
                // // allocate sapc

                // self.program.push(IR {
                //     kind: IRKind::Move(
                //         IRDest::PushStack,
                //         IRSrc::StackOffset(field_ty.offset as Word),
                //     ),
                // });
                unimplemented!()
            }
        }
    }
    fn init_local(&mut self, binding: &Bind, ty: Ty) -> Compile<()> {
        match &binding.kind {
            BindKind::Ident(ident) => {
                let _prev = self.scope.insert(
                    ident.value.to_string(),
                    ScopeRec {
                        frame_offset: self.current_frame_offset,
                        ty,
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
    fn get_ref_src(&mut self, expr: &Expr) -> Compile<(IRSrc, LocalResult)> {
        match &expr.kind {
            ExprKind::Ident(payload) => {
                let local = self.get_local(&payload.value, expr.source_info)?;
                Ok((IRSrc::StackOffset(local.stack_offset), local))
            }
            _ => Err(CompileError {
                kind: CmpErrKind::InvalidReference,
                source_info: expr.source_info,
            }),
        }
    }
    fn to_stack_offset(&self, frame_offset: FrameOffset) -> Word {
        let stack_offset = self.current_frame_offset - frame_offset;
        debug_assert!(stack_offset >= 0);
        stack_offset as Word
    }
    fn get_local(&mut self, key: &str, source_info: SourceInfo) -> Compile<LocalResult> {
        match self.scope.get(key) {
            None => Err(CompileError {
                kind: CmpErrKind::UnknownIdentifier,
                source_info,
            }),
            Some(scope_rec) => {
                let stack_offset = self.to_stack_offset(scope_rec.frame_offset);
                Ok(LocalResult {
                    ty: scope_rec.ty.clone(),
                    stack_offset: stack_offset as Word,
                })
            }
        }
    }
    fn get_ty_name(&self, name: &str, source_info: SourceInfo) -> Compile<&Ty> {
        match self.ty_scope.get(name) {
            Some(value) => Ok(value),
            None => Err(CompileError {
                kind: CmpErrKind::UnknownTypeIdentifier,
                source_info,
            }),
        }
    }
    fn get_ty(&mut self, ty: &TyExpr) -> Compile<Ty> {
        match &ty.kind {
            TyExprKind::Identifier(payload) => {
                let found = self.get_ty_name(&payload.value, ty.source_info)?;
                let mut out = found.clone();
                out.ref_level = ty.ref_level;
                Ok(out)
            }
            TyExprKind::Struct(payload) => {
                let next_ty_id = self.next_ty_id();
                let mut width = 0;
                let mut fields = HashMap::new();
                for field in &payload.fields {
                    let field_ty = self.get_ty(&field.ty)?;
                    let offset = width;
                    width += field_ty.width;
                    let found = fields.insert(
                        field.key.to_string(),
                        StructTyField {
                            offset,
                            ty: field_ty,
                        },
                    );
                    if found.is_some() {
                        return Err(CompileError {
                            kind: CmpErrKind::DuplicateStructField,
                            source_info: ty.source_info,
                        });
                    }
                }
                Ok(Ty {
                    id: next_ty_id,
                    kind: TyKind::Struct(Struct { fields }),
                    width,
                    ref_level: 0,
                })
            }
        }
    }
}

// #[cfg(test)]
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
        );
        check_err(
            "let x := 3; @x",
            CompileError {
                kind: CmpErrKind::InvalidReference,
                source_info: SourceInfo {
                    start: 13,
                    length: 1,
                },
            },
        );
    }

    #[test]
    fn type_error() {
        check_err(
            "not 1",
            CompileError::type_error(
                &Ty::bool_type(),
                &Ty::int_type(),
                SourceInfo {
                    start: 4,
                    length: 1,
                },
            ),
        );

        check_err(
            "true + 2",
            CompileError::type_error(
                &Ty::int_type(),
                &Ty::bool_type(),
                SourceInfo {
                    start: 0,
                    length: 4,
                },
            ),
        );

        check_err(
            "let x := false; x + 1",
            CompileError::type_error(
                &Ty::int_type(),
                &Ty::bool_type(),
                SourceInfo {
                    start: 16,
                    length: 1,
                },
            ),
        );

        check_err(
            "let x : bool := 1",
            CompileError::type_error(
                &Ty::int_type(),
                &Ty::bool_type(),
                SourceInfo {
                    start: 16,
                    length: 1,
                },
            ),
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
