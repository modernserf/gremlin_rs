use crate::expr::*;
use crate::memory::*;
use crate::runtime::*;
use crate::ty::*;
use crate::{Compile, CompileOpt};

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

impl ExprParser<'_, '_, '_, '_, '_, '_> {
    pub fn op_expr(&mut self) -> CompileOpt<Expr> {
        let left = match self.unary_op_expr()? {
            Some(expr) => expr,
            None => return Ok(None),
        };
        let mut op_expr = self.compiler.op_begin(left);

        while let Some(op) = self.lexer.op()? {
            self.compiler.op_next(&mut op_expr, op)?;
            let right = self.expect_unary_op_expr()?;
            self.compiler.op_push(&mut op_expr, right);
        }
        self.compiler.op_end(op_expr).map(Some)
    }
}

impl Expr {
    pub fn op_rhs(self, memory: &mut Memory, ty: Ty, op: Op, left: Expr) -> Expr {
        // TODO: in (a < b) | (c < d), how do we ensure that we've saved a < b to stack
        // by the time we execute c < d?

        match (&left.kind, self.kind) {
            (ExprKind::Constant(l), ExprKind::Constant(r)) => {
                let value = op.inline(*l, r);
                Expr::constant(ty, value)
            }
            (_, ExprKind::Constant(r)) => {
                let left = Src::Block(left.resolve_to_stack(memory).block);
                op.apply(memory, ty, left, Src::Immediate(r))
            }
            (_, ExprKind::Block(r)) => {
                let left = Src::Block(left.resolve_to_stack(memory).block);
                op.apply(memory, ty, left, Src::PopBlock(r))
            }
            (_, ExprKind::Cond(r)) => {
                let left = Src::Block(left.resolve_to_stack(memory).block);
                let right = Src::Block(memory.set_if(Dest::Stack, r));
                op.apply(memory, ty, left, right)
            }
            (_, ExprKind::Reference(r)) => {
                let left = Src::Block(left.resolve_to_stack(memory).block);
                // TODO
                let (src, register) = r.into_src_with_register(memory);
                let out = op.apply(memory, ty, left, src);
                if let Some(register) = register {
                    memory.free_register(register);
                }
                out
            }
            _ => unreachable!(),
        }
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

#[derive(Debug)]
enum OpKind {
    Accumulate(IROp),
    Cmp(IRCond),
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

    fn op_kind(&self) -> OpKind {
        match self {
            Op::Add => OpKind::Accumulate(IR::Add),
            Op::Sub => OpKind::Accumulate(IR::Sub),
            Op::Mul => OpKind::Accumulate(IR::Mult),
            Op::Equal => OpKind::Cmp(IRCond::Zero),
            Op::NotEqual => OpKind::Cmp(IRCond::NotZero),
        }
    }

    pub fn apply(&self, memory: &mut Memory, ty: Ty, left: Src, right: Src) -> Expr {
        match self.op_kind() {
            OpKind::Accumulate(ir_op) => {
                let block = memory.accumulate(ir_op, left.into_dest(), right);
                Expr::resolved(ty, block)
            }
            OpKind::Cmp(cond) => {
                memory.cmp(left, right);
                Expr::cond(cond)
            }
        }
    }
}

// Ops

pub struct OpExpr {
    op_stack: Vec<Op>,
    operands: Vec<Expr>,
}

impl ExprCompiler<'_, '_, '_> {
    fn op_begin(&mut self, left: Expr) -> OpExpr {
        OpExpr {
            op_stack: Vec::new(),
            operands: vec![left],
        }
    }
    fn op_next(&mut self, op_expr: &mut OpExpr, op: Op) -> Compile<()> {
        match op_expr.op_stack.pop() {
            Some(last_op) => {
                if last_op.precedence() > op.precedence() {
                    op_expr.op_stack.push(last_op);
                    op_expr.op_stack.push(op);
                } else {
                    self.op_apply(op_expr, op)?;
                    op_expr.op_stack.push(op);
                }
            }
            None => {
                op_expr.op_stack.push(op);
            }
        };
        Ok(())
    }
    fn op_push(&mut self, op_expr: &mut OpExpr, expr: Expr) {
        op_expr.operands.push(expr);
    }
    fn op_end(&mut self, mut op_expr: OpExpr) -> Compile<Expr> {
        while let Some(op) = op_expr.op_stack.pop() {
            self.op_apply(&mut op_expr, op)?;
        }
        Ok(op_expr.operands.pop().expect("op result"))
    }
    fn op_apply(&mut self, op_expr: &mut OpExpr, op: Op) -> Compile<()> {
        let right = op_expr.operands.pop().expect("rhs");
        let left = op_expr.operands.pop().expect("lhs");

        let out_ty = op.check_ty(&left.ty, &right.ty)?;
        let result = right.op_rhs(self.memory, out_ty, op, left);
        op_expr.operands.push(result);
        Ok(())
    }
    pub fn op_simple(&mut self, op: Op, left: Expr, right: Expr) -> Compile<Expr> {
        let op_expr = OpExpr {
            op_stack: vec![op],
            operands: vec![left, right],
        };
        self.op_end(op_expr)
    }
}

#[cfg(test)]
mod test {
    use crate::expr::test::ExprFixture;
    use crate::runtime::{Register::*, EA::*, IR::*};

    #[test]
    fn parens() {
        let mut fx = ExprFixture::new();
        fx.expect_ir_result(
            "volatile 3 * (volatile 4 + volatile 5)",
            vec![
                Mov(PreDec(SP), Immediate(3)),
                Mov(PreDec(SP), Immediate(4)),
                Mov(PreDec(SP), Immediate(5)),
                Add(Offset(SP, 1), PostInc(SP)),
                Mult(Offset(SP, 1), PostInc(SP)),
            ],
            27,
        );
    }

    #[test]
    fn constant_folding() {
        let mut fx = ExprFixture::new();
        fx.expect_ir_result("3 * (4 + 5)", vec![Mov(PreDec(SP), Immediate(27))], 27);
    }

    #[test]
    fn precedence() {
        let mut fx = ExprFixture::new();
        fx.expect_ir_result(
            "volatile 4 + volatile 5 * volatile 3",
            vec![
                Mov(PreDec(SP), Immediate(4)),
                Mov(PreDec(SP), Immediate(5)),
                Mov(PreDec(SP), Immediate(3)),
                Mult(Offset(SP, 1), PostInc(SP)),
                Add(Offset(SP, 1), PostInc(SP)),
            ],
            19,
        );
        fx.expect_ir_result("4 + 5 * 3", vec![Mov(PreDec(SP), Immediate(19))], 19);
    }

    #[test]
    fn negation() {
        let mut fx = ExprFixture::new();
        fx.expect_ir_result("-3", vec![Mov(PreDec(SP), Immediate(-3))], -3);
        fx.expect_ir_result(
            "-(volatile 3)",
            vec![
                Mov(PreDec(SP), Immediate(3)),
                Mov(PreDec(SP), Immediate(0)),
                Sub(Offset(SP, 0), Offset(SP, 1)),
            ],
            -3,
        );
    }
}
