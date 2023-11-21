use crate::expr::Expr;
use crate::memory::Memory;
use crate::runtime::{IROp, Word, IR};
use crate::ty::*;
use crate::Compile;

// a * b + c
// a: [a] []
// *: [a] [*]
// b: [a, b] [*]
// +: [a * b] [+]
// c: [a * b, c] [+]
// .. [(a * b) + c] []

// a + b * c
// a: [a] []
// +: [a] [+]
// b: [a, b] [+]
// *: [a, b] [+, *]
// c: [a, b, c] [+, *]
// .. [a, b * c] [+]
// .. [a + (b * c)] []

pub struct OpExpr {
    op_stack: Vec<Op>,
    operands: Vec<Expr>,
}

impl OpExpr {
    pub fn new(left: Expr) -> Self {
        Self {
            op_stack: Vec::new(),
            operands: vec![left],
        }
    }
    pub fn unwind(&mut self, memory: &mut Memory) -> Compile<Expr> {
        while let Some(op) = self.op_stack.pop() {
            self.apply(memory, op)?;
        }
        Ok(self.operands.pop().expect("op result"))
    }
    pub fn next(&mut self, memory: &mut Memory, op: Op, right: Expr) -> Compile<()> {
        match self.op_stack.pop() {
            Some(last_op) => {
                if last_op.precedence() > op.precedence() {
                    self.op_stack.push(last_op);
                    self.op_stack.push(op);
                } else {
                    self.apply(memory, last_op)?;
                    self.op_stack.push(op);
                }
            }
            None => {
                self.op_stack.push(op);
            }
        };
        self.operands.push(right);
        Ok(())
    }
    fn apply(&mut self, memory: &mut Memory, op: Op) -> Compile<()> {
        let right = self.operands.pop().expect("rhs");
        let left = self.operands.pop().expect("lhs");

        let out_ty = op.check_ty(&left.ty, &right.ty)?;
        let res = left.op(memory, op, right, out_ty);
        self.operands.push(res);
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Equal,
    NotEqual,
}

impl Op {
    fn precedence(&self) -> usize {
        match self {
            Op::Add => 1,
            Op::Sub => 1,
            Op::Mul => 0,
            Op::Equal => 2,
            Op::NotEqual => 2,
        }
    }
    fn check_ty(&self, left: &Ty, right: &Ty) -> Compile<Ty> {
        match self {
            Op::Add => self.arithmetic(left, right),
            Op::Sub => self.arithmetic(left, right),
            Op::Mul => self.arithmetic(left, right),
            Op::Equal => self.equal(left, right),
            Op::NotEqual => self.equal(left, right),
        }
    }
    fn arithmetic(&self, left: &Ty, right: &Ty) -> Compile<Ty> {
        Ty::int().check(left)?;
        Ty::int().check(right)?;
        Ok(Ty::int())
    }
    fn equal(&self, left: &Ty, right: &Ty) -> Compile<Ty> {
        left.check(right)?;
        Ok(Ty::bool())
    }
    pub fn inline(&self, left: Word, right: Word) -> Word {
        match self {
            Op::Add => left + right,
            Op::Mul => left * right,
            Op::Sub => left - right,
            Op::Equal => {
                if left == right {
                    1
                } else {
                    0
                }
            }
            Op::NotEqual => {
                if left == right {
                    0
                } else {
                    1
                }
            }
        }
    }
    // TODO: operators that aren't single instructions as `dest = dest • src`
    pub fn ir(&self) -> IROp {
        match self {
            Op::Add => IR::Add,
            Op::Sub => IR::Sub,
            Op::Mul => IR::Mult,
            Op::Equal => IR::Equal,
            Op::NotEqual => IR::NotEqual,
        }
    }
}
