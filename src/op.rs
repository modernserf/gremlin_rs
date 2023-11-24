use crate::expr::{Expr, ExprTarget};
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Equal,
    NotEqual,
}

impl Op {
    pub fn precedence(&self) -> usize {
        match self {
            Op::Add => 1,
            Op::Sub => 1,
            Op::Mul => 0,
            Op::Equal => 2,
            Op::NotEqual => 2,
        }
    }
    pub fn check_ty(&self, left: &Ty, right: &Ty) -> Compile<Ty> {
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
