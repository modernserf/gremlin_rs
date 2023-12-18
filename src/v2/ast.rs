use std::collections::HashMap;

use crate::v2::memory5::Memory;

use super::vm_68k::Asm;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {}

impl Ty {
    fn int() -> Self {
        Self {}
    }
    fn void() -> Self {
        Self {}
    }
    fn string() -> Self {
        Self {}
    }
    fn expected(&self, expected: &Ty) -> Compile<()> {
        if self == expected {
            Ok(())
        } else {
            Err(CompileError::ExpectedType {
                expected: expected.clone(),
                received: self.clone(),
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(String),
    Int(i32),
    Call(Box<CallExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    target: Expr,
    args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let(String, Expr),
    Expr(Expr),
    Log(&'static str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyExpr {}

#[derive(Default, Debug)]
struct Scope {
    frames: Vec<ScopeFrame>,
}

impl Scope {
    fn begin(&mut self) {
        self.frames.push(ScopeFrame::default())
    }
    fn end(&mut self) {
        self.frames.pop().unwrap();
    }
    fn insert(&mut self, name: String, id: usize, ty: Ty) {
        let top = self.top_mut();
        top.scope.insert(name, ScopeRecord { id, ty });
    }
    fn get(&mut self, name: &str) -> Compile<ScopeRecord> {
        for frame in self.frames.iter().rev() {
            if let Some(rec) = frame.scope.get(name) {
                return Ok(rec.clone());
            }
        }
        Err(CompileError::UnknownIdent(name.to_string()))
    }
    fn top_mut(&mut self) -> &mut ScopeFrame {
        let idx = self.frames.len() - 1;
        &mut self.frames[idx]
    }
}

#[derive(Default, Debug)]
struct ScopeFrame {
    scope: HashMap<String, ScopeRecord>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ScopeRecord {
    id: usize,
    ty: Ty,
}

#[derive(Default, Debug)]
struct TyScope {}

type Compile<T> = Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    ExpectedType { expected: Ty, received: Ty },
    InvalidCallArgs,
    UnknownIdent(String),
}

#[derive(Default, Debug)]
pub struct Compiler {
    scope: Scope,
    ty_scope: TyScope,
    memory: Memory,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn program(body: Vec<Stmt>) -> Compile<Asm> {
        let mut c = Self::new();
        c.scope.begin();
        for stmt in body {
            c.stmt(stmt)?;
        }
        c.scope.end();
        Ok(c.memory.end())
    }
    pub fn stmt(&mut self, stmt: Stmt) -> Compile<()> {
        match stmt {
            Stmt::Let(name, expr) => {
                let ty = self.expr(expr)?;
                let id = self.memory.local();
                self.scope.insert(name, id, ty);
            }
            Stmt::Expr(expr) => {
                self.expr(expr)?;
            }
            Stmt::Log(str) => {
                self.memory.log(&str);
            }
        };
        Ok(())
    }
    pub fn expr(&mut self, expr: Expr) -> Compile<Ty> {
        match expr {
            Expr::Int(value) => {
                self.memory.push_i32(value);
                Ok(Ty::int())
            }
            Expr::Ident(name) => {
                let rec = self.scope.get(&name)?;
                self.memory.push_ident(rec.id);
                Ok(rec.ty)
            }
            Expr::Call(call) => self.call_expr(*call),
        }
    }
    fn call_expr(&mut self, call: CallExpr) -> Compile<Ty> {
        match &call.target {
            Expr::Ident(str) if str == "assert_eq" => {
                let mut arg_tys = Vec::new();
                for arg in call.args {
                    let ty = self.expr(arg)?;
                    ty.expected(&Ty::int())?;
                    arg_tys.push(ty);
                }
                if arg_tys.len() != 2 {
                    return Err(CompileError::InvalidCallArgs);
                }

                self.memory.assert_eq();
                Ok(Ty::void())
            }
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::vm_68k::VM;

    fn run_vm(asm: Asm) -> VM {
        let mut vm = VM::new(256);
        let init_sp = 120;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        vm.load_memory(init_pc as usize, &asm.out);
        vm.run();
        vm
    }

    #[test]
    fn smoke_test() {
        run_vm(
            Compiler::program(vec![
                Stmt::Let("foo".to_string(), Expr::Int(123)),
                Stmt::Expr(Expr::Call(Box::new(CallExpr {
                    target: Expr::Ident("assert_eq".to_string()),
                    args: vec![Expr::Ident("foo".to_string()), Expr::Int(123)],
                }))),
                Stmt::Log("Hello, world!"),
            ])
            .unwrap(),
        );
    }
}
