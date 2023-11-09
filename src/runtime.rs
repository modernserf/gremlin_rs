use crate::ast::{BinOpKind, Bind, BindKind, Expr, ExprKind, LetStmt, Stmt, StmtKind, UnOpKind};
use std::collections::HashMap;

// FIXME
type Word = u128;

#[derive(Clone, Debug, PartialEq, Eq)]
struct RuntimeError {
    kind: RunErrKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum RunErrKind {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Interpreter {
    sp: usize,
    memory: Vec<Word>,
    bindings: HashMap<String, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let size = 128;
        Self {
            sp: size - 1,
            memory: vec![0; size],
            bindings: HashMap::new(),
        }
    }
    pub fn poke(&mut self, index: usize, value: Word) {
        self.memory[index] = value;
    }
    pub fn peek(&self, index: usize) -> Word {
        self.memory[index]
    }
    pub fn push_stk(&mut self, value: Word) {
        self.memory[self.sp] = value;
        self.sp -= 1;
    }
    pub fn top_stk(&mut self) -> Word {
        self.memory[self.sp + 1]
    }
    pub fn pop_stk(&mut self) -> Word {
        self.sp += 1;
        self.memory[self.sp]
    }
    pub fn eval_body(&mut self, body: &[Stmt]) {
        for stmt in body {
            self.eval_stmt(stmt);
        }
    }
    fn set_binding(&mut self, binding: &Bind) {
        match &binding.kind {
            BindKind::Ident(ident) => {
                self.bindings.insert(ident.value.to_string(), self.sp);
            }
        }
    }
    fn get_binding_addr(&mut self, key: &str) -> usize {
        *self.bindings.get(key).expect("binding")
    }
    fn eval_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => self.eval_expr(&expr),
            StmtKind::Let(let_stmt) => {
                self.set_binding(&let_stmt.binding);
                self.eval_expr(&let_stmt.expr);
            }
            _ => unimplemented!(),
        }
    }
    fn eval_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Int(payload) => {
                self.push_stk(payload.value);
            }
            ExprKind::Ident(ident) => {
                let addr = self.get_binding_addr(&ident.value);
                let value = self.peek(addr);
                self.push_stk(value);
            }
            ExprKind::BinaryOp(payload) => {
                self.eval_expr(&payload.left);
                self.eval_expr(&payload.right);
                let right = self.pop_stk();
                let left = self.pop_stk();
                let val = match payload.operator {
                    BinOpKind::Add => left + right,
                    BinOpKind::Mult => left * right,
                };
                self.push_stk(val);
            }
            ExprKind::UnaryOp(payload) => match payload.operator {
                UnOpKind::Ref => match &payload.expr.kind {
                    ExprKind::Ident(ident) => {
                        let addr = self.get_binding_addr(&ident.value);
                        self.push_stk(addr as Word);
                    }
                    _ => panic!("expected place expr"),
                },
                UnOpKind::Deref => {
                    self.eval_expr(&payload.expr);
                    let val = self.pop_stk();
                    self.push_stk(self.peek(val as usize));
                }
            },
            _ => unimplemented!(),
        };
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn push_pop() {
        let mut interpreter = Interpreter::new();
        interpreter.push_stk(123);
        assert_eq!(interpreter.pop_stk(), 123);
    }
}
