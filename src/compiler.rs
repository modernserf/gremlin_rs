use crate::{
    ast::{BinOpKind, Bind, BindKind, Expr, ExprKind, Stmt, StmtKind, UnOpKind},
    source_info::SourceInfo,
};
use std::collections::HashMap;

pub type Word = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IR {
    kind: IRKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum IRKind {
    // pop an address from the stack & push the value at that address
    // Deref,
    Move(IRDest, IRSrc),
    Add(IRDest, IRSrc),
    Mult(IRDest, IRSrc),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IRDest {
    PushStack,
    R0,
    StackOffset(Word),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum IRSrc {
    Immediate(Word),
    Address(Word),
    StackOffset(Word),
    R0,
    AtR0,
    StackPointer,
    PopStack,
}

pub type Compile<T> = Result<T, CompileError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {
    kind: CmpErrKind,
    source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CmpErrKind {
    UnknownIdentifier,
    IntOutOfRange,
    InvalidReference,
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
                self.expr(&payload.expr)?;
                self.init_local(&payload.binding)?;
            }
            StmtKind::Assign(payload) => {
                self.expr(&payload.expr)?;
            }
            StmtKind::Expr(expr) => self.expr(expr)?,
        };
        Ok(())
    }
    fn expr(&mut self, expr: &Expr) -> Compile<()> {
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
            }
            ExprKind::Ident(payload) => {
                let stack_offset = self.get_stack_offset(&payload.value, expr.source_info)?;
                self.program.push(IR {
                    kind: IRKind::Move(IRDest::PushStack, IRSrc::StackOffset(stack_offset)),
                });
                self.inc_frame_offset();
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
                    }
                    UnOpKind::Ref => {
                        match &payload.expr.kind {
                            ExprKind::Ident(payload) => {
                                let stack_offset =
                                    self.get_stack_offset(&payload.value, expr.source_info)?;
                                self.program.push(IR {
                                    kind: IRKind::Move(IRDest::PushStack, IRSrc::StackPointer),
                                });
                                self.program.push(IR {
                                    kind: IRKind::Add(
                                        IRDest::StackOffset(0),
                                        IRSrc::Immediate(stack_offset),
                                    ),
                                });
                                self.inc_frame_offset();
                            }
                            _ => {
                                return Err(CompileError {
                                    kind: CmpErrKind::InvalidReference,
                                    source_info: expr.source_info,
                                })
                            }
                        };
                    }
                };
            }
            ExprKind::BinaryOp(payload) => {
                self.expr(&payload.right)?;
                self.expr(&payload.left)?;
                match &payload.operator {
                    BinOpKind::Add => {
                        self.program.push(IR {
                            kind: IRKind::Add(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                    }
                    BinOpKind::Mult => {
                        self.program.push(IR {
                            kind: IRKind::Mult(IRDest::StackOffset(0), IRSrc::PopStack),
                        });
                    }
                }
                self.dec_frame_offset();
            }
        };
        Ok(())
    }
    fn init_local(&mut self, binding: &Bind) -> Compile<()> {
        match &binding.kind {
            BindKind::Ident(ident) => {
                let _prev = self.scope.insert(
                    ident.value.to_string(),
                    ScopeRec {
                        frame_offset: self.current_frame_offset,
                    },
                );
            }
        };
        Ok(())
    }
    fn get_stack_offset(&mut self, key: &str, source_info: SourceInfo) -> Compile<Word> {
        match self.scope.get(key) {
            None => Err(CompileError {
                kind: CmpErrKind::UnknownIdentifier,
                source_info,
            }),
            Some(scope_rec) => {
                let stack_offset = self.current_frame_offset - scope_rec.frame_offset;
                debug_assert!(stack_offset >= 0);
                Ok(stack_offset as Word)
            }
        }
    }
}

pub struct IRRuntime {
    r0: Word,
    sp: Word,
    ip: usize,
    memory: Vec<Word>,
}

impl IRRuntime {
    pub fn eval(program: &[IR]) -> Word {
        let mut runtime = Self::new(128);
        runtime.run_program(program);
        runtime.memory[runtime.sp as usize]
    }
    fn new(memory_size: Word) -> Self {
        Self {
            r0: 0,
            sp: memory_size,
            ip: 0,
            memory: vec![0; memory_size as usize],
        }
    }
    fn run_program(&mut self, program: &[IR]) {
        while self.ip < program.len() {
            let instruction = &program[self.ip];
            self.ip += 1;
            self.run_instr(instruction);
        }
    }
    fn run_instr(&mut self, instr: &IR) {
        match &instr.kind {
            IRKind::Move(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr = value;
            }
            IRKind::Add(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr += value;
            }
            IRKind::Mult(dest, src) => {
                let value = self.get_src(*src);
                let dest_ptr = self.get_dest(*dest);
                *dest_ptr *= value;
            }
        }
    }
    fn get_src(&mut self, src: IRSrc) -> Word {
        match src {
            IRSrc::Immediate(value) => value,
            IRSrc::Address(addr) => self.memory[addr as usize],
            IRSrc::R0 => self.r0,
            IRSrc::AtR0 => self.memory[self.r0 as usize],
            IRSrc::StackPointer => self.sp,
            IRSrc::StackOffset(offset) => self.memory[(self.sp + offset) as usize],
            IRSrc::PopStack => {
                let value = self.memory[self.sp as usize];
                self.sp += 1;
                value
            }
        }
    }
    fn get_dest<'a>(&'a mut self, dest: IRDest) -> &'a mut Word {
        match dest {
            IRDest::R0 => &mut self.r0,
            IRDest::StackOffset(offset) => &mut self.memory[(self.sp + offset) as usize],
            IRDest::PushStack => {
                self.sp -= 1;
                &mut self.memory[self.sp as usize]
            }
        }
    }
}
