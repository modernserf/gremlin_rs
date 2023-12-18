use std::collections::HashMap;

use super::vm_68k::Asm;
use crate::v2::memory5::Memory;

type Compile<T> = Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    ExpectedType { expected: Ty, received: Ty },
    InvalidCallArgs,
    UnknownIdent(String),
    InvalidLValue,
    InvalidRef,
    InvalidDeref,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    ref_level: usize,
    kind: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TyKind {
    Void,
    Int,
    String,
}

impl Ty {
    fn void() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::Void,
        }
    }
    fn int() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::Int,
        }
    }
    fn string() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::String,
        }
    }
    fn pointer(mut self) -> Self {
        self.ref_level += 1;
        self
    }
    fn deref(mut self) -> Compile<Self> {
        if self.ref_level == 0 {
            return Err(CompileError::InvalidDeref);
        }
        self.ref_level -= 1;
        Ok(self)
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
    String(String),
    Call(Box<CallExpr>),
    Add(Box<Expr>, Box<Expr>),
    Pointer(Box<Expr>),
    Deref(Box<Expr>),
}

impl Expr {
    fn pointer(self) -> Expr {
        Expr::Pointer(Box::new(self))
    }
    fn deref(self) -> Expr {
        Expr::Deref(Box::new(self))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    target: Expr,
    args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Local(String, Expr),
    LocalRef(String, Expr),
    Assign(Expr, Expr),
    Expr(Expr),
    Log(String),
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
    fn insert(&mut self, name: String, id: usize, ty: Ty, can_ref: bool) {
        let top = self.top_mut();
        top.scope.insert(name, ScopeRecord { id, ty, can_ref });
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
    can_ref: bool,
}

#[derive(Default, Debug)]
struct TyScope {}

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
            Stmt::Local(name, expr) => {
                let ty = self.expr(expr)?;
                let id = self.memory.local();
                self.scope.insert(name, id, ty, false);
            }
            Stmt::LocalRef(name, expr) => {
                let ty = self.expr(expr)?;
                let id = self.memory.local_stack();
                self.scope.insert(name, id, ty, true);
            }
            Stmt::Assign(target, expr) => {
                let expected = self.lvalue(target)?;
                let ty = self.expr(expr)?;
                ty.expected(&expected.ty)?;
                self.memory.assign();
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
            Expr::String(name) => {
                self.memory.push_string(name);
                Ok(Ty::string())
            }
            Expr::Ident(name) => {
                let rec = self.scope.get(&name)?;
                self.memory.push_ident(rec.id);
                Ok(rec.ty)
            }
            Expr::Call(call) => self.call_expr(*call),
            Expr::Add(left, right) => {
                self.expr(*left)?.expected(&Ty::int())?;
                self.expr(*right)?.expected(&Ty::int())?;
                self.memory.add();
                Ok(Ty::int())
            }
            Expr::Pointer(expr) => {
                let lvalue = self.lvalue(*expr)?;
                if !lvalue.can_ref {
                    return Err(CompileError::InvalidRef);
                }
                self.memory.pointer();
                let ty = lvalue.ty.pointer();
                Ok(ty)
            }
            Expr::Deref(expr) => {
                let ty = self.expr(*expr)?.deref()?;
                self.memory.deref();
                Ok(ty)
            }
        }
    }
    fn call_expr(&mut self, call: CallExpr) -> Compile<Ty> {
        match &call.target {
            Expr::Ident(str) if str == "assert_eq" => {
                self.call_args(call.args, &vec![Ty::int(), Ty::int()])?;
                self.memory.assert_eq();
                Ok(Ty::void())
            }
            Expr::Ident(str) if str == "println" => {
                self.call_args(call.args, &vec![Ty::string()])?;
                self.memory.println();
                Ok(Ty::void())
            }
            _ => unimplemented!(),
        }
    }
    fn call_args(&mut self, args: Vec<Expr>, expected: &[Ty]) -> Compile<()> {
        if args.len() != expected.len() {
            return Err(CompileError::InvalidCallArgs);
        }
        for (i, arg) in args.into_iter().enumerate() {
            let ty = self.expr(arg)?;
            ty.expected(&expected[i])?;
        }
        Ok(())
    }
    fn lvalue(&mut self, expr: Expr) -> Compile<ScopeRecord> {
        match expr {
            Expr::Ident(name) => {
                let rec = self.scope.get(&name)?;
                self.memory.push_ident(rec.id);
                Ok(rec)
            }
            Expr::Deref(expr) => {
                let mut rec = self.lvalue(*expr)?;
                rec.ty = rec.ty.deref()?;
                self.memory.deref();
                Ok(rec)
            }
            _ => Err(CompileError::InvalidLValue),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::vm_68k::VM;

    fn local(name: &str, expr: Expr) -> Stmt {
        Stmt::Local(name.to_string(), expr)
    }
    fn local_ref(name: &str, expr: Expr) -> Stmt {
        Stmt::LocalRef(name.to_string(), expr)
    }
    fn log(name: &str) -> Stmt {
        Stmt::Log(name.to_string())
    }
    fn expr(expr: Expr) -> Stmt {
        Stmt::Expr(expr)
    }
    fn assign(lvalue: Expr, rvalue: Expr) -> Stmt {
        Stmt::Assign(lvalue, rvalue)
    }

    fn int(value: i32) -> Expr {
        Expr::Int(value)
    }
    fn string(value: &str) -> Expr {
        Expr::String(value.to_string())
    }
    fn ident(name: &str) -> Expr {
        Expr::Ident(name.to_string())
    }
    fn call(target: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call(Box::new(CallExpr { target, args }))
    }
    fn add(left: Expr, right: Expr) -> Expr {
        Expr::Add(Box::new(left), Box::new(right))
    }

    fn assert_eq(left: Expr, right: Expr) -> Stmt {
        expr(call(ident("assert_eq"), vec![left, right]))
    }

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
                log("log"),
                //
                local("foo", int(123)),
                expr(call(ident("assert_eq"), vec![ident("foo"), int(123)])),
                //
                local("bar", string("Hello, world!")),
                expr(call(ident("println"), vec![ident("bar")])),
            ])
            .unwrap(),
        );
    }

    #[test]
    fn add_expr() {
        run_vm(
            Compiler::program(vec![
                local("a", int(10)),
                local("b", int(20)),
                local("c", add(ident("a"), add(int(30), ident("b")))),
                //
                expr(call(ident("assert_eq"), vec![ident("c"), int(60)])),
            ])
            .unwrap(),
        );
    }

    #[test]
    fn add_type_err() {
        assert_eq!(
            Compiler::program(vec![expr(add(int(30), string("hello")))]),
            Err(CompileError::ExpectedType {
                expected: Ty::int(),
                received: Ty::string()
            })
        );
    }

    #[test]
    fn invalid_call_args() {
        assert_eq!(
            Compiler::program(vec![expr(call(ident("assert_eq"), vec![])),]),
            Err(CompileError::InvalidCallArgs)
        );
    }

    #[test]
    fn unknown_ident() {
        assert_eq!(
            Compiler::program(vec![expr(ident("foo"))]),
            Err(CompileError::UnknownIdent("foo".to_string()))
        )
    }

    #[test]
    fn assign_() {
        run_vm(
            Compiler::program(vec![
                local("a", int(10)),
                assign(ident("a"), int(20)),
                //
                expr(call(ident("assert_eq"), vec![ident("a"), int(20)])),
            ])
            .unwrap(),
        );
    }

    #[test]
    fn invalid_lvalue() {
        assert_eq!(
            Compiler::program(vec![assign(int(10), int(20))]),
            Err(CompileError::InvalidLValue)
        );
    }

    #[test]
    fn pointer() {
        run_vm(
            Compiler::program(vec![
                local_ref("a", int(123)),
                local("a_ptr", ident("a").pointer()),
                local_ref("a_stack_ptr", ident("a").pointer()),
                //
                assign(ident("a"), int(456)),
                assert_eq(ident("a_ptr").deref(), int(456)),
                assert_eq(ident("a_stack_ptr").deref(), int(456)),
                //
                assign(ident("a_ptr").deref(), int(789)),
                assert_eq(ident("a"), int(789)),
            ])
            .unwrap(),
        );
    }
}
