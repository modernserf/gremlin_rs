use crate::expr::*;
use crate::memory::*;
use crate::runtime::*;
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

    pub fn apply(&self, memory: &mut Memory, ty: Ty, left: Src, right: Src) -> Expr {
        let block = match self {
            Op::Add => memory.write(IR::Add, left.as_dest(), right),
            Op::Sub => memory.write(IR::Sub, left.as_dest(), right),
            Op::Mul => memory.write(IR::Mult, left.as_dest(), right),
            // fixme: correctly handle consuming lhs
            Op::Equal => {
                memory.write_cmp(left, right);
                memory.drop(1);
                return Expr::cond(IRCond::Zero);
            }
            Op::NotEqual => {
                memory.write_cmp(left, right);
                memory.drop(1);
                return Expr::cond(IRCond::NotZero);
            }
        };
        Expr::resolved(ty, block)
    }
}
