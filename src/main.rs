mod lexer;
mod memory;
mod op;
mod runtime;
mod ty;

use lexer::*;
use memory::*;
use op::*;
use runtime::*;
use std::collections::HashMap;
use ty::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    UnexpectedChar,
    ExpectedToken(Token),
    UnknownIdentifier(String),
    ExpectedType(Ty, Ty),
    UnknownTypeIdentifier(String),
    Expected(&'static str),
    InvalidDeref,
    InvalidCast,
    DuplicateField,
    MissingField,
    InvalidRef,
}
use CompileError::*;

pub type Compile<T> = Result<T, CompileError>;
pub type CompileOpt<T> = Result<Option<T>, CompileError>;

struct TyScope {
    data: HashMap<String, Ty>,
    next_type_id: usize,
}

impl TyScope {
    fn new() -> Self {
        Self {
            data: HashMap::from_iter(
                vec![("Int", Ty::int()), ("Bool", Ty::bool())]
                    .into_iter()
                    .map(|(k, v)| (k.to_string(), v)),
            ),
            next_type_id: 1,
        }
    }
    fn get(&self, key: &str) -> Compile<Ty> {
        self.data
            .get(key)
            .cloned()
            .ok_or_else(|| UnknownTypeIdentifier(key.to_string()))
    }
    fn assign(&mut self, key: String, record: Ty) {
        self.data.insert(key, record);
    }
    fn new_type_id(&mut self) -> usize {
        let id = self.next_type_id;
        self.next_type_id += 1;
        id
    }
}

struct Compiler {
    memory: Memory,
    ty_scope: TyScope,
    lexer: Lexer,
}

impl Compiler {
    fn new(input: &str) -> Self {
        Self {
            lexer: Lexer::new(input),
            ty_scope: TyScope::new(),
            memory: Memory::new(),
        }
    }

    fn bitset_expr(&mut self, ty: Ty, ctx: ExprContext) -> Compile<Expr> {
        let mut value = 0;
        loop {
            let key = match self.lexer.type_ident_token()? {
                Some(key) => key,
                _ => break,
            };
            let field = ty.oneof_member(&key)?;
            // TODO: check for dupes
            value |= 1 << field.index;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(ctx.constant(ty.as_bitset()?, value))
    }

    fn struct_expr(&mut self, ty: Ty, ctx: ExprContext, case: Option<Word>) -> Compile<Expr> {
        let struct_block = ctx.allocate(ty.size(), &mut self.memory);

        loop {
            let field_name = match self.lexer.ident_token()? {
                Some(s) => s,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let field = ty.struct_field(&field_name, case)?;
            let field_ctx = ExprContext::Block(struct_block.struct_field(field));
            let expr = self.expect_expr(field_ctx)?.resolve(&mut self.memory);
            field.ty.check(&expr.ty)?;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(Expr::resolved(ty, struct_block))
    }

    fn oneof_member_expr(&mut self, name: &str, ctx: ExprContext) -> Compile<Expr> {
        let ty = self.ty_scope.get(name)?;

        self.lexer.expect_token(Token::Dot)?;
        let key = self
            .lexer
            .type_ident_token()?
            .ok_or(Expected("type identifier"))?;

        match self.lexer.peek()? {
            Token::CurlyLeft => {
                self.lexer.advance();
                let case = ty.struct_case(&key)?;
                let out = self.struct_expr(ty, ctx, Some(case))?;
                self.lexer.expect_token(Token::CurlyRight)?;
                Ok(out)
            }
            _ => {
                let member = ty.oneof_member(&key)?;
                Ok(ctx.constant(ty.clone(), member.index))
            }
        }
    }

    fn array_expr(&mut self, ctx: ExprContext) -> Compile<Expr> {
        self.lexer.expect_token(Token::SqLeft)?;
        let item_ty = self.expect_type_expr()?;
        self.lexer.expect_token(Token::Colon)?;
        let capacity = self.lexer.int_token()?.ok_or(Expected("array size"))?;
        self.lexer.expect_token(Token::SqRight)?;
        self.lexer.expect_token(Token::CurlyLeft)?;

        let ty = Ty::array(item_ty.clone(), capacity);
        let block = ctx.allocate(ty.size(), &mut self.memory);

        let mut i = 0;
        loop {
            let cell_ctx = ExprContext::Block(block.array_index(&item_ty, i));
            let expr = match self.expr(cell_ctx)? {
                Some(p) => p.resolve(&mut self.memory),
                None => break,
            };
            item_ty.check(&expr.ty)?;
            i += 1;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            };
        }
        if capacity != i {
            return Err(Expected("array count"));
        }

        self.lexer.expect_token(Token::CurlyRight)?;
        Ok(Expr::resolved(ty, block))
    }

    fn base_expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        let res = match self.lexer.peek()? {
            Token::ParLeft => {
                self.lexer.advance();
                let expr = self.expect_expr(ctx)?;
                self.lexer.expect_token(Token::ParRight)?;
                expr
            }
            Token::TypeIdentifier(name) => {
                self.lexer.advance();
                match self.lexer.peek()? {
                    Token::Dot => self.oneof_member_expr(&name, ctx)?,
                    Token::CurlyLeft => {
                        self.lexer.advance();
                        let ty = self.ty_scope.get(&name)?;
                        let out = match self.lexer.peek()? {
                            Token::TypeIdentifier(_) => self.bitset_expr(ty, ctx)?,
                            Token::Identifier(_) => self.struct_expr(ty, ctx, None)?,
                            _ => unimplemented!(),
                        };
                        self.lexer.expect_token(Token::CurlyRight)?;
                        out
                    }
                    _ => return Err(Expected("struct or bitset")),
                }
            }
            Token::Array => {
                self.lexer.advance();
                self.array_expr(ctx)?
            }
            Token::Integer(int) => {
                self.lexer.advance();
                ctx.constant(Ty::int(), int)
            }
            Token::True => {
                self.lexer.advance();
                ctx.constant(Ty::bool(), 1)
            }
            Token::False => {
                self.lexer.advance();
                ctx.constant(Ty::bool(), 0)
            }
            Token::Identifier(name) => {
                self.lexer.advance();
                self.memory.identifier(&name, ctx)?
            }
            _ => return Ok(None),
        };
        Ok(Some(res))
    }

    fn postfix_expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        let mut left = match self.base_expr(ctx)? {
            Some(e) => e,
            None => return Ok(None),
        };
        loop {
            match self.lexer.peek()? {
                Token::SqLeft => {
                    self.lexer.advance();
                    if self.lexer.token(Token::SqRight)?.is_some() {
                        left = left.deref(&mut self.memory)?;
                    } else {
                        let item_ty = left.ty.index_ty(&Ty::int())?.clone();
                        // get pointer to left
                        let ptr = left.add_ref(&mut self.memory)?;
                        // add (index * size) to value of pointer
                        let idx = self.expect_expr(ExprContext::Stack)?;

                        let offset = idx.op(
                            &mut self.memory,
                            Op::Mul,
                            ExprContext::Stack.constant(Ty::int(), item_ty.size()),
                            Ty::int(),
                        );
                        let out_ty = ptr.ty.clone();
                        let offset_ptr = ptr.op(&mut self.memory, Op::Add, offset, out_ty);
                        // deref pointer
                        left = offset_ptr.deref(&mut self.memory)?;
                        self.lexer.expect_token(Token::SqRight)?;
                    }
                }
                Token::Dot => {
                    self.lexer.advance();
                    left = match self.lexer.peek()? {
                        Token::Identifier(field_name) => {
                            self.lexer.advance();
                            left.struct_field(&field_name)?
                        }
                        Token::TypeIdentifier(field_name) => {
                            self.lexer.advance();
                            left.bitset_field(&field_name, &mut self.memory)?
                        }
                        _ => return Err(Expected("field")),
                    }
                }
                _ => return Ok(Some(left)),
            };
        }
    }

    fn unary_op_expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        let out = match self.lexer.peek()? {
            Token::Minus => {
                self.lexer.advance();
                let left = ctx.constant(Ty::int(), 0);
                let right = self.expect_unary_op_expr(ExprContext::Stack)?;
                left.op(&mut self.memory, Op::Sub, right, Ty::int())
            }
            Token::Ampersand => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr(ctx)?;
                operand.add_ref(&mut self.memory)?
            }
            Token::Volatile => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr(ctx)?;
                operand.resolve(&mut self.memory).to_expr()
            }
            _ => return self.postfix_expr(ctx),
        };
        Ok(Some(out))
    }

    fn expect_unary_op_expr(&mut self, ctx: ExprContext) -> Compile<Expr> {
        self.unary_op_expr(ctx)?.ok_or(Expected("expr"))
    }

    fn op_expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        let left = match self.unary_op_expr(ctx)? {
            Some(expr) => expr,
            None => return Ok(None),
        };
        let mut op_expr = OpExpr::new(left);

        loop {
            match self.lexer.op()? {
                Some(op) => {
                    let right = self.expect_unary_op_expr(ExprContext::Stack)?;
                    op_expr.next(&mut self.memory, op, right)?;
                }
                // None => return Ok(Some(op_parser.unwind(&mut self.memory)?)),
                None => return op_expr.unwind(&mut self.memory).map(Some),
            }
        }
    }

    fn assign_expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        let left = match self.op_expr(ctx)? {
            Some(x) => x,
            None => return Ok(None),
        };
        if self.lexer.token(Token::ColonEq)?.is_none() {
            return Ok(Some(left));
        }
        self.op_expr(left.assign_ctx()?)?
            .ok_or(Expected("expr"))
            .map(Some)
    }

    fn as_expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        let value = match self.assign_expr(ctx)? {
            Some(x) => x,
            None => return Ok(None),
        };
        if self.lexer.token(Token::As)?.is_none() {
            return Ok(Some(value));
        }
        let ty = self.expect_type_expr()?;
        Ok(Some(value.cast_ty(ty)?))
    }

    fn expr(&mut self, ctx: ExprContext) -> CompileOpt<Expr> {
        self.as_expr(ctx)
    }

    fn expect_expr(&mut self, ctx: ExprContext) -> Compile<Expr> {
        self.expr(ctx)?.ok_or(Expected("expr"))
    }

    // ### Bindings, TypeExprs, etc.

    fn binding(&mut self) -> CompileOpt<String> {
        self.lexer.ident_token()
    }

    fn type_binding(&mut self) -> CompileOpt<String> {
        self.lexer.type_ident_token()
    }

    fn struct_items(&mut self, fields: &mut TyStruct, case: Option<Word>) -> Compile<()> {
        self.lexer.expect_token(Token::CurlyLeft)?;
        loop {
            if self.lexer.token(Token::Case)?.is_some() {
                let case_name = self
                    .lexer
                    .type_ident_token()?
                    .ok_or(Expected("case identifier"))?;

                let case_id = fields.insert_case(case_name)?;
                self.struct_items(fields, Some(case_id))?;
                self.lexer.token(Token::Comma)?;
                continue;
            }

            let key = match self.lexer.ident_token()? {
                Some(s) => s,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let ty = self.expect_type_expr()?;
            fields.insert(key, ty, case)?;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::CurlyRight)?;
        Ok(())
    }

    fn struct_ty(&mut self) -> Compile<Ty> {
        let mut fields = TyStruct::new(self.ty_scope.new_type_id());

        self.struct_items(&mut fields, None)?;

        Ok(Ty::struct_(fields))
    }

    fn oneof_ty(&mut self) -> Compile<Ty> {
        let mut data = TyOneOf::new(self.ty_scope.new_type_id());

        self.lexer.expect_token(Token::CurlyLeft)?;
        let mut i = 0;
        loop {
            let key = match self.lexer.type_ident_token()? {
                Some(key) => key,
                _ => break,
            };

            // TODO: allow setting numeric values for oneof members
            // `oneof {Jan := 1, Feb, Mar, Apr}`
            // enforce that numbers are increasing order

            data.insert(key, i)?;
            i += 1;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::CurlyRight)?;

        Ok(Ty::oneof(data))
    }

    fn type_expr(&mut self) -> CompileOpt<Ty> {
        match self.lexer.peek()? {
            Token::TypeIdentifier(ident) => {
                self.lexer.advance();
                self.ty_scope.get(&ident).map(Some)
            }
            Token::Struct => {
                self.lexer.advance();
                self.struct_ty().map(Some)
            }
            Token::OneOf => {
                self.lexer.advance();
                self.oneof_ty().map(Some)
            }
            _ => Ok(None),
        }
    }

    fn expect_type_expr(&mut self) -> Compile<Ty> {
        self.type_expr()?.ok_or(Expected("type expr"))
    }

    // ### Statements

    fn type_def_stmt(&mut self) -> Compile<()> {
        let tb = self.type_binding()?.ok_or(Expected("type binding"))?;
        self.lexer.expect_token(Token::ColonEq)?;
        let te = self.expect_type_expr()?;
        self.ty_scope.assign(tb, te);
        Ok(())
    }

    fn let_stmt(&mut self) -> Compile<()> {
        let binding = self.binding()?.ok_or(Expected("binding"))?;
        let bind_ty = if self.lexer.token(Token::Colon)?.is_some() {
            let ty = self.expect_type_expr()?;
            Some(ty)
        } else {
            None
        };
        self.lexer.expect_token(Token::ColonEq)?;
        let expr = self
            .expect_expr(ExprContext::Stack)?
            .resolve(&mut self.memory);
        match bind_ty {
            Some(b) => b.check(&expr.ty)?,
            None => {}
        };
        self.memory.store_local(binding, expr);
        Ok(())
    }

    fn stmt(&mut self) -> CompileOpt<()> {
        match self.lexer.peek()? {
            Token::Let => {
                self.lexer.advance();
                self.let_stmt()?;
            }
            Token::Type => {
                self.lexer.advance();
                self.type_def_stmt()?;
            }
            _ => {
                match self.expr(ExprContext::Stack)? {
                    Some(expr) => {
                        let res = expr.resolve(&mut self.memory);
                        self.memory.compact(res.block);
                    }
                    None => return Ok(None),
                };
            }
        };
        Ok(Some(()))
    }

    pub fn program(input: &str) -> Compile<Vec<IR>> {
        let mut parse = Self::new(input);
        loop {
            if parse.stmt()?.is_none() {
                break;
            }
            if parse.lexer.token(Token::Semicolon)?.is_none() {
                break;
            }
        }
        parse.lexer.expect_token(Token::EndOfInput)?;
        Ok(parse.memory.done())
    }
}

fn main() {
    let ir = Compiler::program("").expect("compile");
    Runtime::eval(&ir);
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{EA::*, IR::*};

    fn expect_ir(code: &str, ir: Vec<IR>) {
        assert_eq!(Compiler::program(code), Ok(ir));
    }
    fn expect_result(code: &str, _ir: Vec<IR>, value: Word) {
        let ir = Compiler::program(code).expect("compile");
        let res = Runtime::eval(&ir);
        assert_eq!(res, value);
    }
    fn expect_ir_result(code: &str, ir: Vec<IR>, result: Word) {
        let actual_ir = Compiler::program(code).expect("compile");
        let actual_result: i32 = Runtime::eval(&actual_ir);
        assert_eq!(actual_ir, ir);
        assert_eq!(actual_result, result);
    }
    fn expect_err(code: &str, err: CompileError) {
        assert_eq!(Compiler::program(code), Err(err))
    }

    #[test]
    fn empty_program() {
        expect_ir("", vec![]);
    }

    #[test]
    fn integers() {
        expect_ir_result("123", vec![Mov(PushStack, Immediate(123))], 123);
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "
            123

            ",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123 # This is a comment",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn unexpected_char() {
        expect_err(" £ ", UnexpectedChar)
    }

    #[test]
    fn bools() {
        expect_ir("true", vec![Mov(PushStack, Immediate(1))]);
        expect_ir("false", vec![Mov(PushStack, Immediate(0))]);
    }

    #[test]
    fn let_stmts() {
        expect_ir_result(
            "
                let x := 1;
                let y := 2;
                x
            ",
            vec![
                Mov(PushStack, Immediate(1)),
                Mov(PushStack, Immediate(2)),
                Mov(PushStack, StackOffset(1)),
            ],
            1,
        );
    }

    #[test]
    fn type_exprs() {
        expect_ir(
            "
                let x : Int := 1;
            ",
            vec![Mov(PushStack, Immediate(1))],
        );
        expect_err(
            "
            let x : Bool := 1;
        ",
            ExpectedType(Ty::bool(), Ty::int()),
        )
    }

    #[test]
    fn assignment() {
        expect_ir_result(
            "
            let x := 1;
            let y := 3;
            x := y;
            x
        ",
            vec![
                Mov(PushStack, Immediate(1)),
                Mov(PushStack, Immediate(3)),
                Mov(StackOffset(1), StackOffset(0)),
                Mov(PushStack, StackOffset(1)),
            ],
            3,
        );
    }

    #[test]
    fn addition() {
        expect_ir_result(
            "
            let x := 1;
            let y := 2;
            x + 3 + y
        ",
            vec![
                Mov(PushStack, Immediate(1)),        // [x: 1]
                Mov(PushStack, Immediate(2)),        // [y: 2, x: 1]
                Mov(PushStack, StackOffset(1)),      // [1, y: 2, x: 1]
                Add(StackOffset(0), Immediate(3)),   // [4, y: 2, x: 1]
                Add(StackOffset(0), StackOffset(1)), // [6, y: 2, x: 1]
            ],
            6,
        );
    }

    #[test]
    fn typechecked_arithmetic() {
        expect_err("1 + true", ExpectedType(Ty::int(), Ty::bool()));
        expect_err("true + 1", ExpectedType(Ty::int(), Ty::bool()));
    }

    #[test]
    fn parens() {
        expect_ir_result(
            "volatile 3 * (volatile 4 + volatile 5)",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Add(StackOffset(1), StackOffset(0)),
                Mult(StackOffset(2), StackOffset(1)),
                Add(SP, Immediate(2)),
            ],
            27,
        );
    }

    #[test]
    fn constant_folding() {
        expect_ir_result("3 * (4 + 5)", vec![Mov(PushStack, Immediate(27))], 27);
    }

    #[test]
    fn precedence() {
        expect_ir_result(
            "volatile 4 + volatile 5 * volatile 3",
            vec![
                Mov(PushStack, Immediate(4)),
                Mov(PushStack, Immediate(5)),
                Mov(PushStack, Immediate(3)),
                Mult(StackOffset(1), StackOffset(0)),
                Add(StackOffset(2), StackOffset(1)),
                Add(SP, Immediate(2)),
            ],
            19,
        );
        expect_ir_result("4 + 5 * 3", vec![Mov(PushStack, Immediate(19))], 19);
    }

    #[test]
    fn negation() {
        expect_ir_result("-3", vec![Mov(PushStack, Immediate(-3))], -3);

        expect_ir_result(
            "let a := 3; -a",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Sub(StackOffset(0), StackOffset(1)),
            ],
            -3,
        );

        expect_ir_result(
            "-(volatile 3)",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Sub(StackOffset(0), StackOffset(1)),
                Mov(StackOffset(1), StackOffset(0)),
                Add(SP, Immediate(1)),
            ],
            -3,
        );
    }

    #[test]
    fn ref_deref() {
        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            (volatile ptr)[]
        ",
            vec![
                // let x := 1;
                Mov(PushStack, Immediate(1)),
                // let ptr := &x;
                LoadAddress(PushStack, StackOffset(0)),
                // volatile ptr
                Mov(PushStack, StackOffset(0)),
                // []
                Mov(R0, StackOffset(0)),
                Mov(StackOffset(0), R0Offset(0)),
            ],
            1,
        );

        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            ptr[]
        ",
            vec![
                // let x := 1;
                Mov(PushStack, Immediate(1)),
                // let ptr := &x;
                LoadAddress(PushStack, StackOffset(0)),
                // ptr[]
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(0)),
            ],
            1,
        );
    }

    #[test]
    fn ptr_assign() {
        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            ptr[] := 2;
            x
        ",
            vec![
                // let x := 1;
                Mov(PushStack, Immediate(1)),
                // let ptr := &x;
                LoadAddress(PushStack, StackOffset(0)),
                // ptr[] := 2;
                Mov(R0, StackOffset(0)),
                Mov(R0Offset(0), Immediate(2)),
                // x
                Mov(PushStack, StackOffset(1)),
            ],
            2,
        );
    }

    #[test]
    fn type_casting() {
        expect_ir_result(
            "(volatile true as Int) +  2",
            vec![
                Mov(PushStack, Immediate(1)),
                Add(StackOffset(0), Immediate(2)),
            ],
            3,
        );
    }

    #[test]
    fn type_alias() {
        expect_ir(
            "
                type Word := Int;
                let a : Word := 3
            ",
            vec![Mov(PushStack, Immediate(3))],
        )
    }

    #[test]
    fn struct_() {
        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            (volatile p).x
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                // volatile p
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // .x
                Mov(StackOffset(1), StackOffset(0)),
                Add(SP, Immediate(1)),
            ],
            123,
        );

        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            (volatile p).y
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                // volatile p
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // .y
                Add(SP, Immediate(1)),
            ],
            456,
        );

        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            p.x
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                // p.x
                Mov(PushStack, StackOffset(0)),
            ],
            123,
        );
    }

    #[test]
    fn struct_assignment() {
        expect_ir(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            p.y := 789;
        ",
            vec![
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(123)),
                Mov(StackOffset(1), Immediate(456)),
                Mov(StackOffset(1), Immediate(789)),
            ],
        );
    }

    #[test]
    fn pointer_to_struct_field() {
        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            (volatile ptr)[]
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                Mov(StackOffset(1), Immediate(2)),
                // let ptr := &p.x;
                LoadAddress(PushStack, StackOffset(0)),
                // p.x := 5;
                Mov(StackOffset(1), Immediate(5)),
                // volatile ptr
                Mov(PushStack, StackOffset(0)),
                // []
                Mov(R0, StackOffset(0)),
                Mov(StackOffset(0), R0Offset(0)),
            ],
            5,
        );

        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            ptr[]
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                Mov(StackOffset(1), Immediate(2)),
                // let ptr := &p.x;
                LoadAddress(PushStack, StackOffset(0)),
                // p.x := 5;
                Mov(StackOffset(1), Immediate(5)),
                // ptr[]
                Mov(R0, StackOffset(0)),
                Mov(PushStack, R0Offset(0)),
            ],
            5,
        );
    }

    #[test]
    fn deref_struct_pointer_field() {
        expect_ir_result(
            "
            type Point := struct { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p;
            ptr[].y
        ",
            vec![
                // let p := Point { x: 1, y: 2 };
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                Mov(StackOffset(1), Immediate(2)),
                // let ptr := &p;
                LoadAddress(PushStack, StackOffset(0)),
                Mov(R0, StackOffset(0)),
                // ptr[].y
                Mov(PushStack, R0Offset(1)),
            ],
            2,
        );
    }

    #[test]
    fn oneof() {
        expect_ir_result(
            "
            type TrafficLight := oneof {Red, Yellow, Green};
            TrafficLight.Yellow
        ",
            vec![Mov(PushStack, Immediate(1))],
            1,
        )
    }

    #[test]
    fn bitset() {
        expect_ir_result(
            "
            type Flags := oneof {Carry, Overflow, Zero, Negative, Extend};
            let flags := Flags{Carry, Zero};
            flags.Zero
        ",
            vec![
                Mov(PushStack, Immediate(0b101)),
                Mov(PushStack, StackOffset(0)),
                BitTest(StackOffset(0), Immediate(2)),
                Mov(StackOffset(0), R0),
            ],
            1,
        );
    }

    #[test]
    fn array() {
        expect_ir_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[volatile 1]
            ",
            vec![
                // let xs := array[Int: 4]{10, 20, 30, 40};
                Sub(SP, Immediate(4)),
                Mov(StackOffset(0), Immediate(10)),
                Mov(StackOffset(1), Immediate(20)),
                Mov(StackOffset(2), Immediate(30)),
                Mov(StackOffset(3), Immediate(40)),
                // &xs + (1 * sizeof Int)
                LoadAddress(PushStack, StackOffset(0)),
                Mov(PushStack, Immediate(1)),
                Mult(StackOffset(0), Immediate(1)),
                Add(StackOffset(1), StackOffset(0)),
                // deref
                Mov(R0, StackOffset(1)),
                Mov(StackOffset(1), R0Offset(0)),
                // cleanup
                Add(SP, Immediate(1)),
            ],
            20,
        );

        expect_ir_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[1]
            ",
            vec![
                // let xs := array[Int: 4]{10, 20, 30, 40};
                Sub(SP, Immediate(4)),
                Mov(StackOffset(0), Immediate(10)),
                Mov(StackOffset(1), Immediate(20)),
                Mov(StackOffset(2), Immediate(30)),
                Mov(StackOffset(3), Immediate(40)),
                // &xs + 1
                LoadAddress(PushStack, StackOffset(0)),
                Add(StackOffset(0), Immediate(1)),
                // deref
                Mov(R0, StackOffset(0)),
                Mov(StackOffset(0), R0Offset(0)),
            ],
            20,
        );
    }

    #[test]
    fn variants() {
        expect_result(
            "
                type Point := struct { x: Int, y: Int };
                type Shape := struct {
                    fill: Bool,
                    case Rect {
                        top_left: Point,
                        bottom_right: Point,
                    }
                    case Circle {
                        center: Point,
                        radius: Int
                    }
                };
                
                let disc := Shape.Circle{
                    fill: true,
                    center: Point{x: 0, y: 1},
                    radius: 3
                };

                disc.fill
            ",
            vec![],
            1,
        );
    }
}
