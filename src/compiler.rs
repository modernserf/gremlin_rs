use std::collections::HashMap;

use crate::ast;
use crate::ir::{IRDest, IRSrc, Word, IR};
use crate::typed_ast as t;

/*
FrameOffset
...
top ->  127: 3
        126: 2   // let p = Point { x: 2, y: 3 }
(SP) -> 125: 100 // let q = 100
        124:
...
p : frame_offset = 2, width = 2, current stack_offset = 1
q : frame_offset = 3, width = 1, current stack_offset = 0

arguments, return value & return address are at negative frame offsets
*/
type FrameOffset = isize;
type Id = usize;
type IROp = fn(IRDest, IRSrc) -> IR;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileError {}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Local {
    frame_offset: FrameOffset,
    size: usize,
}
impl Local {
    fn at(&self, idx: usize) -> Self {
        debug_assert!(idx < self.size);
        Self {
            frame_offset: self.frame_offset - idx as FrameOffset,
            size: 1,
        }
    }
    fn to_dest(&self) -> Dest {
        Dest::Local(*self)
    }
    fn to_src(&self) -> Src {
        Src::Local(*self)
    }
    fn subset(&self, offset: usize, size: usize) -> Self {
        debug_assert!(self.size > offset);
        debug_assert!(self.size - offset > size);
        Self {
            frame_offset: self.frame_offset - offset as FrameOffset,
            size,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Dest {
    Stack,
    Local(Local),
}
impl Dest {
    fn at(&self, idx: usize) -> Self {
        match self {
            Dest::Local(local) => Dest::Local(local.at(idx)),
            _ => panic!("invalid dest index"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Src {
    Local(Local),
    Immediate(Word),
    R0Offset(Word),
    R0,
}
impl Src {
    fn at(&self, idx: usize) -> Self {
        match self {
            Src::Local(local) => Src::Local(local.at(idx)),
            Src::R0Offset(offset) => Src::R0Offset(offset + idx as Word),
            _ => panic!("invalid src index"),
        }
    }
}

type Compile<T> = Result<T, CompileError>;

pub struct Compiler {
    scope: HashMap<Id, Local>,
    current_frame_offset: FrameOffset,
    out: Vec<IR>,
}

impl Compiler {
    pub fn compile(body: &[t::Stmt]) -> Compile<Vec<IR>> {
        let mut compiler = Self::new();
        compiler.body(body)?;
        Ok(compiler.out)
    }
    fn new() -> Self {
        Compiler {
            scope: HashMap::new(),
            current_frame_offset: 0,
            out: Vec::new(),
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
            t::Stmt::Let(x) => {
                let mem = self.expr(&x.expr, Dest::Stack)?;
                self.scope.insert(x.bind_id, mem);
                Ok(())
            }
            t::Stmt::Assign(x) => {
                let mem = self.lvalue(&x.lvalue)?;
                self.expr(&x.expr, mem.to_dest())?;
                Ok(())
            }
            t::Stmt::Expr(x) => {
                self.expr(&x, Dest::Stack)?;
                Ok(())
            }
        }
    }
    fn expr(&mut self, expr: &t::Expr, dest: Dest) -> Compile<Local> {
        match &expr.kind {
            t::ExprKind::Constant(value) => self.write(IR::mov, dest, Src::Immediate(*value)),
            t::ExprKind::Long(hi, lo) => {
                let mem = self.allocate(2, dest)?;
                self.write(IR::mov, mem.at(0).to_dest(), Src::Immediate(*lo))?;
                self.write(IR::mov, mem.at(1).to_dest(), Src::Immediate(*hi))?;
                Ok(mem)
            }
            t::ExprKind::Ident(bind_id) => {
                let mem = *self.scope.get(&bind_id).expect("local");
                self.write_multiple(IR::mov, dest, Src::Local(mem), mem.size)
            }
            t::ExprKind::RefIdent(place) => {
                let mem = self.lvalue(place)?;
                self.write(IR::load_address, dest, Src::Local(mem))
            }
            t::ExprKind::Deref(x) => {
                let size = x.ty.deref().expect("deref").size();
                let mem = self.allocate(size, dest)?;
                let addr = self.expr(&x, Dest::Stack)?;
                debug_assert!(addr.size == 1);
                self.write_ir(IR::mov(
                    IRDest::R0,
                    IRSrc::StackOffset(self.stack_offset(addr)),
                ));
                let out = self.write_multiple(IR::mov, mem.to_dest(), Src::R0Offset(0), size)?;
                self.deallocate(addr)?;
                Ok(out)
            }
            t::ExprKind::Not(x) => {
                let mem = self.expr(&x, dest)?;
                self.write(IR::xor, mem.to_dest(), Src::Immediate(1))
            }
            t::ExprKind::BinaryOp(x) => {
                let op = match x.operator {
                    ast::BinOpKind::Add => IR::add,
                    ast::BinOpKind::Mult => IR::mult,
                    ast::BinOpKind::And => IR::and,
                    ast::BinOpKind::Or => IR::or,
                };
                let left = self.expr(&x.left, dest)?;
                let right = self.expr(&x.right, Dest::Stack)?;
                let res = self.write(op, left.to_dest(), right.to_src())?;
                self.deallocate(right)?;
                Ok(res)
            }
            t::ExprKind::Struct(fields) => {
                let size = expr.ty.size();
                let mem = self.allocate(size, dest)?;
                for field in fields {
                    let offset = field.offset;
                    let size = field.expr.ty.size();
                    let field_dest = Local {
                        frame_offset: mem.frame_offset - offset as FrameOffset,
                        size,
                    }
                    .to_dest();
                    self.expr(&field.expr, field_dest)?;
                }
                Ok(mem)
            }
            t::ExprKind::StructField(field) => {
                let size = expr.ty.size();

                // fast path for <ident>.foo over <expr>.foo
                if let Some(lvalue) = field.expr.as_lvalue() {
                    let base = self.lvalue(&lvalue)?;
                    let field_src = Src::Local(Local {
                        frame_offset: base.frame_offset - field.offset as FrameOffset,
                        size,
                    });
                    return self.write_multiple(IR::mov, dest, field_src, size);
                }

                let mem = self.allocate(size, dest)?;
                let target = self.expr(&field.expr, dest)?;
                let src = Src::Local(Local {
                    frame_offset: target.frame_offset - field.offset as FrameOffset,
                    size,
                });
                let out = self.write_multiple(IR::mov, mem.to_dest(), src, size)?;
                self.deallocate(target)?;
                Ok(out)
            }
            t::ExprKind::OneOfField(field) => {
                if let Some(lvalue) = field.expr.as_lvalue() {
                    let target = self.lvalue(&lvalue)?.to_dest();
                    self.write(
                        IR::bit_test,
                        target,
                        Src::Immediate(field.bit_index as Word),
                    )?;
                    return self.write(IR::mov, dest, Src::R0);
                }
                let mem = self.allocate(1, dest)?;
                let target = self.expr(&field.expr, Dest::Stack)?;
                self.write(
                    IR::bit_test,
                    target.to_dest(),
                    Src::Immediate(field.bit_index as Word),
                )?;
                let out = self.write(IR::mov, mem.to_dest(), Src::R0)?;
                self.deallocate(target)?;
                Ok(out)
            }
        }
    }
    fn lvalue(&mut self, lvalue: &t::LValue) -> Compile<Local> {
        match lvalue {
            t::LValue::Id(id) => {
                let mem = *self.scope.get(&id).expect("place");
                Ok(mem)
            }
            t::LValue::Field(x) => {
                let parent = self.lvalue(&x.lvalue)?;
                let child = parent.subset(x.offset, x.size);
                Ok(child)
            }
        }
    }
    fn allocate(&mut self, size: usize, dest: Dest) -> Compile<Local> {
        match dest {
            Dest::Stack => {
                self.current_frame_offset += size as isize;
                self.write_ir(IR::sub(IRDest::SP, IRSrc::Immediate(size as Word)));
                Ok(Local {
                    frame_offset: self.current_frame_offset,
                    size,
                })
            }
            Dest::Local(local) => {
                assert_eq!(local.size, size);
                Ok(local)
            }
        }
    }
    fn deallocate(&mut self, local: Local) -> Compile<()> {
        let stack_offset = self.stack_offset(local);
        if stack_offset > 0 {
            panic!("must deallocate last element of stack");
        }
        self.write_ir(IR::add(IRDest::SP, IRSrc::Immediate(local.size as Word)));
        self.current_frame_offset -= local.size as FrameOffset;
        Ok(())
    }
    fn write_multiple(&mut self, op: IROp, dest: Dest, src: Src, size: usize) -> Compile<Local> {
        if size == 1 {
            return self.write(op, dest, src);
        }
        match dest {
            Dest::Stack => {
                let mem = self.allocate(size, dest)?;
                self.write_multiple(op, mem.to_dest(), src, size)
            }
            Dest::Local(local) => {
                for i in 0..size {
                    self.write(op, dest.at(i), src.at(i))?;
                }
                Ok(local)
            }
        }
    }
    fn write(&mut self, op: IROp, dest: Dest, src: Src) -> Compile<Local> {
        let ir_src = match src {
            Src::Immediate(value) => IRSrc::Immediate(value),
            Src::Local(local) => IRSrc::StackOffset(self.stack_offset(local)),
            Src::R0Offset(offset) => IRSrc::R0Offset(offset),
            Src::R0 => IRSrc::R0,
        };

        let (ir_dest, res) = match dest {
            Dest::Stack => {
                self.current_frame_offset += 1;
                (
                    IRDest::PushStack,
                    Local {
                        frame_offset: self.current_frame_offset,
                        size: 1,
                    },
                )
            }
            Dest::Local(local) => (IRDest::StackOffset(self.stack_offset(local)), local),
        };
        self.write_ir(op(ir_dest, ir_src));
        Ok(res)
    }
    fn write_ir(&mut self, ir: IR) {
        self.out.push(ir);
    }
    fn stack_offset(&self, local: Local) -> Word {
        let offset = self.current_frame_offset - local.frame_offset;
        debug_assert!(offset >= 0);
        offset as Word
    }
}
