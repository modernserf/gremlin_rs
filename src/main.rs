mod expr;
mod lexer;
mod memory;
mod op;
mod record;
mod runtime;
mod ty;

use expr::*;
use lexer::*;
use memory::*;
use op::*;
use record::*;
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

// TODO: either use block scope or forbid in blocks
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
    scope: Scope,
    ty_scope: TyScope,
    lexer: Lexer,
}

impl Compiler {
    fn new(input: &str) -> Self {
        Self {
            lexer: Lexer::new(input),
            scope: Scope::new(),
            ty_scope: TyScope::new(),
            memory: Memory::new(),
        }
    }

    fn bitset_expr(&mut self, ty: Ty) -> Compile<Expr> {
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
        Ok(Expr::constant(ty.as_bitset()?, value))
    }

    fn record_expr(&mut self, ty: Ty, case: Option<Word>) -> Compile<Expr> {
        let block = self.memory.allocate(ty.size());
        let record = ty.get_record()?;

        if let Some(case_id) = case {
            let case_field = record.case_field.as_ref().ok_or(Expected("case field"))?;
            let field_ctx = ExprTarget::Block(block.record_field(case_field));
            Expr::constant(Ty::int(), case_id).resolve(&mut self.memory, field_ctx);
        }

        loop {
            let field_name = match self.lexer.ident_token()? {
                Some(s) => s,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let field = record.get(&field_name, case)?;
            let field_ctx = ExprTarget::Block(block.record_field(field));
            let expr = self.expect_expr()?.resolve(&mut self.memory, field_ctx);
            field.ty.check(&expr.ty)?;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(Expr::resolved(ty, block))
    }

    fn oneof_member_expr(&mut self, name: &str) -> Compile<Expr> {
        let ty = self.ty_scope.get(name)?;

        self.lexer.expect_token(Token::Dot)?;
        let key = self
            .lexer
            .type_ident_token()?
            .ok_or(Expected("type identifier"))?;

        match self.lexer.peek()? {
            Token::CurlyLeft => {
                self.lexer.advance();
                let case = ty.get_record()?.get_case(&key)?;
                let out = self.record_expr(ty, Some(case))?;
                self.lexer.expect_token(Token::CurlyRight)?;
                Ok(out)
            }
            _ => {
                let member = ty.oneof_member(&key)?;
                Ok(Expr::constant(ty.clone(), member.index))
            }
        }
    }

    fn array_expr(&mut self) -> Compile<Expr> {
        self.lexer.expect_token(Token::SqLeft)?;
        let item_ty = self.expect_type_expr()?;
        self.lexer.expect_token(Token::Colon)?;
        let capacity = self.lexer.int_token()?.ok_or(Expected("array size"))?;
        self.lexer.expect_token(Token::SqRight)?;
        self.lexer.expect_token(Token::CurlyLeft)?;

        let ty = Ty::array(item_ty.clone(), capacity);
        let block = self.memory.allocate(ty.size());

        let mut i = 0;
        loop {
            let cell_ctx = ExprTarget::Block(block.array_index(&item_ty, i));
            let expr = match self.expr()? {
                Some(p) => p.resolve(&mut self.memory, cell_ctx),
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

    fn base_expr(&mut self) -> CompileOpt<Expr> {
        let res = match self.lexer.peek()? {
            Token::ParLeft => {
                self.lexer.advance();
                let expr = self.expect_expr()?;
                self.lexer.expect_token(Token::ParRight)?;
                expr
            }
            Token::TypeIdentifier(name) => {
                self.lexer.advance();
                match self.lexer.peek()? {
                    Token::Dot => self.oneof_member_expr(&name)?,
                    Token::CurlyLeft => {
                        self.lexer.advance();
                        let ty = self.ty_scope.get(&name)?;
                        let out = match self.lexer.peek()? {
                            Token::TypeIdentifier(_) => self.bitset_expr(ty)?,
                            Token::Identifier(_) => self.record_expr(ty, None)?,
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
                self.array_expr()?
            }
            Token::Integer(int) => {
                self.lexer.advance();
                Expr::constant(Ty::int(), int)
            }
            Token::True => {
                self.lexer.advance();
                Expr::constant(Ty::bool(), 1)
            }
            Token::False => {
                self.lexer.advance();
                Expr::constant(Ty::bool(), 0)
            }
            Token::Identifier(name) => {
                self.lexer.advance();
                self.scope.identifier(&name)?
            }
            _ => return Ok(None),
        };
        Ok(Some(res))
    }

    fn array_index(&mut self, left: Expr) -> Compile<Expr> {
        let item_ty = left.ty.index_ty(&Ty::int())?.clone();
        // get pointer to first item from array
        let ptr = left
            .add_ref(&mut self.memory, ExprTarget::Stack)?
            .cast_ty(item_ty.add_ref())?;
        // add (index * size) to value of pointer
        let idx = self.expect_expr()?;
        let offset = OpExpr::simple(
            &mut self.memory,
            ExprTarget::Stack,
            Op::Mul,
            idx,
            Expr::constant(Ty::int(), item_ty.size()),
        )?;
        let ptr_ty = ptr.ty.clone();
        let offset_ptr = OpExpr::simple(
            &mut self.memory,
            ExprTarget::Stack,
            Op::Add,
            ptr.cast_ty(Ty::int())?,
            offset,
        )?
        .cast_ty(ptr_ty)?;
        // deref pointer
        offset_ptr.deref(&mut self.memory, ExprTarget::Stack)
    }

    fn postfix_expr(&mut self) -> CompileOpt<Expr> {
        let mut left = match self.base_expr()? {
            Some(e) => e,
            None => return Ok(None),
        };
        loop {
            match self.lexer.peek()? {
                Token::SqLeft => {
                    self.lexer.advance();
                    if self.lexer.token(Token::SqRight)?.is_some() {
                        left = left.deref(&mut self.memory, ExprTarget::Stack)?;
                    } else {
                        left = self.array_index(left)?;
                        self.lexer.expect_token(Token::SqRight)?;
                    }
                }
                Token::Dot => {
                    self.lexer.advance();
                    left = match self.lexer.peek()? {
                        Token::Identifier(field_name) => {
                            self.lexer.advance();
                            left.record_field(&field_name)?
                        }
                        Token::TypeIdentifier(field_name) => {
                            self.lexer.advance();
                            left.bitset_field(&field_name, &mut self.memory, ExprTarget::Stack)?
                        }
                        _ => return Err(Expected("field")),
                    }
                }
                _ => return Ok(Some(left)),
            };
        }
    }

    fn unary_op_expr(&mut self) -> CompileOpt<Expr> {
        let out = match self.lexer.peek()? {
            Token::Minus => {
                self.lexer.advance();
                let left = Expr::constant(Ty::int(), 0);
                let right = self.expect_unary_op_expr()?;
                OpExpr::simple(&mut self.memory, ExprTarget::Stack, Op::Sub, left, right)?
            }
            Token::Ampersand => {
                self.lexer.advance();
                let expr = self.expect_unary_op_expr()?;
                expr.add_ref(&mut self.memory, ExprTarget::Stack)?
            }
            Token::Volatile => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr()?;
                operand
                    .resolve(&mut self.memory, ExprTarget::Stack)
                    .to_expr()
            }
            _ => return self.postfix_expr(),
        };
        Ok(Some(out))
    }

    fn expect_unary_op_expr(&mut self) -> Compile<Expr> {
        self.unary_op_expr()?.ok_or(Expected("expr"))
    }

    fn op_expr(&mut self) -> CompileOpt<Expr> {
        let left = match self.unary_op_expr()? {
            Some(expr) => expr,
            None => return Ok(None),
        };
        let mut op_expr = OpExpr::new(left, ExprTarget::Stack);

        loop {
            match self.lexer.op()? {
                Some(op) => {
                    let right = self.expect_unary_op_expr()?;
                    op_expr.next(&mut self.memory, op, right)?;
                }
                // None => return Ok(Some(op_parser.unwind(&mut self.memory)?)),
                None => return op_expr.unwind(&mut self.memory).map(Some),
            }
        }
    }

    fn assign_expr(&mut self) -> CompileOpt<Expr> {
        let left = match self.op_expr()? {
            Some(x) => x,
            None => return Ok(None),
        };
        if self.lexer.token(Token::ColonEq)?.is_none() {
            return Ok(Some(left));
        }
        let resolved = self
            .op_expr()?
            .ok_or(Expected("expr"))?
            .resolve(&mut self.memory, left.assign_ctx()?);
        Ok(Some(resolved.to_expr()))
    }

    fn as_expr(&mut self) -> CompileOpt<Expr> {
        let value = match self.assign_expr()? {
            Some(x) => x,
            None => return Ok(None),
        };
        if self.lexer.token(Token::As)?.is_none() {
            return Ok(Some(value));
        }
        let ty = self.expect_type_expr()?;
        Ok(Some(value.cast_ty(ty)?))
    }

    fn expr(&mut self) -> CompileOpt<Expr> {
        self.as_expr()
    }

    fn expect_expr(&mut self) -> Compile<Expr> {
        self.expr()?.ok_or(Expected("expr"))
    }

    // ### Bindings, TypeExprs, etc.

    fn binding(&mut self) -> CompileOpt<String> {
        self.lexer.ident_token()
    }

    fn type_binding(&mut self) -> CompileOpt<String> {
        self.lexer.type_ident_token()
    }

    fn record_items(&mut self, fields: &mut TyRecord, case: Option<Word>) -> Compile<()> {
        self.lexer.expect_token(Token::CurlyLeft)?;
        loop {
            if self.lexer.token(Token::Case)?.is_some() {
                let case_name = self
                    .lexer
                    .type_ident_token()?
                    .ok_or(Expected("case identifier"))?;

                let case_id = fields.insert_case(case_name)?;
                self.record_items(fields, Some(case_id))?;
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

    fn record_ty(&mut self) -> Compile<Ty> {
        let mut fields = TyRecord::new(self.ty_scope.new_type_id());

        self.record_items(&mut fields, None)?;

        Ok(Ty::record(fields))
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
            Token::Record => {
                self.lexer.advance();
                self.record_ty().map(Some)
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
        self.lexer.expect_token(Token::Semicolon)?;
        Ok(())
    }

    fn if_cond(&mut self) -> Compile<CondIndex> {
        let cond = self.expect_expr()?;
        Ty::bool().check(&cond.ty)?;
        Ok(self.memory.begin_cond(cond, ExprTarget::Stack))
    }

    // TODO: if as expr
    fn if_stmt(&mut self) -> Compile<()> {
        let mut elses = Vec::new();
        let mut if_rec = self.if_cond()?;
        self.lexer.expect_token(Token::Then)?;
        self.scoped_block()?;
        loop {
            if self.lexer.token(Token::Else)?.is_none() {
                break;
            }

            if self.lexer.token(Token::If)?.is_some() {
                elses.push(self.memory.begin_else(if_rec));
                if_rec = self.if_cond()?;
                self.lexer.expect_token(Token::Then)?;
                self.scoped_block()?;
            } else {
                if_rec = self.memory.begin_else(if_rec);
                self.scoped_block()?;
                break;
            }
        }
        self.lexer.expect_token(Token::End)?;
        self.memory.end_if(if_rec);
        for rec in elses {
            self.memory.end_if(rec);
        }
        Ok(())
    }

    // TODO: expand into generalized struct destructuring
    fn match_bindings(&mut self, case: &mut MatchCaseBuilder) -> Compile<()> {
        if self.lexer.token(Token::CurlyLeft)?.is_none() {
            return Ok(());
        }
        loop {
            let binding = match self.lexer.ident_token()? {
                Some(b) => b,
                None => break,
            };
            case.add_binding(binding, &mut self.scope)?;
            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::CurlyRight)?;
        self.lexer.expect_token(Token::Colon)?;
        Ok(())
    }

    fn match_stmt(&mut self) -> Compile<()> {
        let target = self.expect_expr()?;
        self.lexer.expect_token(Token::Then)?;
        let mut match_builder = MatchBuilder::new(target, &mut self.memory, ExprTarget::Stack)?;
        loop {
            if self.lexer.token(Token::Case)?.is_none() {
                break;
            }
            let tag = self
                .lexer
                .type_ident_token()?
                .ok_or(Expected("match case"))?;
            let mut case = match_builder.add_case(&tag, &mut self.memory)?;

            self.scope.push_scope(&self.memory);
            self.match_bindings(&mut case)?;
            self.block()?;
            match_builder.end_case(&mut self.memory);
            self.scope.pop_scope(&mut self.memory);
        }
        self.lexer.expect_token(Token::End)?;
        match_builder.resolve(&mut self.memory);
        Ok(())
    }

    fn while_stmt(&mut self) -> Compile<()> {
        let while_idx = self.memory.begin_while();
        let cond = self.if_cond()?;
        self.lexer.expect_token(Token::Loop)?;
        self.scoped_block()?;
        self.lexer.expect_token(Token::End)?;
        self.memory.end_while(while_idx, cond);
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
            .expect_expr()?
            .resolve(&mut self.memory, ExprTarget::Stack);
        match bind_ty {
            Some(b) => b.check(&expr.ty)?,
            None => {}
        };
        self.memory.store_local(binding, expr, &mut self.scope);
        self.lexer.expect_token(Token::Semicolon)?;
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
            Token::If => {
                self.lexer.advance();
                self.if_stmt()?;
            }
            Token::Match => {
                self.lexer.advance();
                self.match_stmt()?;
            }
            Token::While => {
                self.lexer.advance();
                self.while_stmt()?;
            }
            _ => {
                match self.expr()? {
                    Some(expr) => {
                        let res = expr.resolve(&mut self.memory, ExprTarget::Stack);
                        self.memory.compact(res.block);
                        self.lexer.expect_token(Token::Semicolon)?;
                    }
                    None => return Ok(None),
                };
            }
        };
        Ok(Some(()))
    }

    fn scoped_block(&mut self) -> Compile<()> {
        self.scope.push_scope(&self.memory);
        self.block()?;
        self.scope.pop_scope(&mut self.memory);
        Ok(())
    }

    fn block(&mut self) -> Compile<()> {
        loop {
            if self.stmt()?.is_none() {
                return Ok(());
            }
        }
    }

    fn sub_params(&mut self) -> Compile<()> {
        self.lexer.expect_token(Token::ParLeft)?;
        loop {
            let binding = match self.binding()? {
                Some(b) => b,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let ty = self.type_expr()?.ok_or(Expected("type"))?;
            // TODO: do something with param

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::ParRight)?;

        let return_ty = if self.lexer.token(Token::Arrow)?.is_some() {
            self.type_expr()?.ok_or(Expected("return type"))?
        } else {
            Ty::void()
        };

        Ok(())
    }

    fn sub(&mut self) -> Compile<()> {
        let name = self.lexer.ident_token()?.ok_or(Expected("name"))?;
        self.sub_params()?;
        self.lexer.expect_token(Token::Do)?;
        self.scoped_block()?;
        self.lexer.expect_token(Token::End)?;

        Ok(())
    }

    fn module_stmt(&mut self) -> CompileOpt<()> {
        match self.lexer.peek()? {
            Token::Type => {
                self.lexer.advance();
                self.type_def_stmt()?;
                Ok(Some(()))
            }
            Token::Sub => {
                self.lexer.advance();
                self.sub()?;
                Ok(Some(()))
            }
            _ => Ok(None),
        }
    }

    fn module(&mut self) -> Compile<()> {
        loop {
            if self.module_stmt()?.is_none() {
                return Ok(());
            }
        }
    }

    pub fn program(input: &str) -> Compile<Vec<IR>> {
        let mut parse = Self::new(input);
        parse.module()?;
        parse.lexer.expect_token(Token::EndOfInput)?;
        Ok(parse.memory.done())
    }

    pub fn script(input: &str) -> Compile<Vec<IR>> {
        let mut parse = Self::new(input);
        parse.block()?;
        parse.lexer.expect_token(Token::EndOfInput)?;
        Ok(parse.memory.done())
    }
}

fn main() {
    let ir = Compiler::script("").expect("compile");
    Runtime::eval(&ir);
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{EA::*, IR::*};

    fn expect_ir(code: &str, ir: Vec<IR>) {
        assert_eq!(Compiler::script(code), Ok(ir));
    }
    fn expect_result(code: &str, _ir: Vec<IR>, value: Word) {
        let ir = Compiler::script(code).expect("compile");
        let res = Runtime::eval(&ir);
        assert_eq!(res, value);
    }
    fn expect_ir_result(code: &str, ir: Vec<IR>, result: Word) {
        let actual_ir = Compiler::script(code).expect("compile");
        assert_eq!(actual_ir, ir);
        let actual_result: i32 = Runtime::eval(&actual_ir);
        assert_eq!(actual_result, result);
    }
    fn expect_err(code: &str, err: CompileError) {
        assert_eq!(Compiler::script(code), Err(err))
    }
    #[allow(dead_code)]
    fn run_ir(ir: Vec<IR>) {
        let res = Runtime::eval(&ir);
        dbg!(res);
    }

    #[test]
    fn empty_program() {
        expect_ir("", vec![]);
    }

    #[test]
    fn integers() {
        expect_ir_result("123;", vec![Mov(PushStack, Immediate(123))], 123);
    }

    #[test]
    fn whitespace() {
        expect_ir(
            "
            123;

            ",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn comments() {
        expect_ir(
            "123; # This is a comment",
            vec![Mov(PushStack, Immediate(123))],
        )
    }

    #[test]
    fn unexpected_char() {
        expect_err(" £ ", UnexpectedChar)
    }

    #[test]
    fn bools() {
        expect_ir("true;", vec![Mov(PushStack, Immediate(1))]);
        expect_ir("false;", vec![Mov(PushStack, Immediate(0))]);
    }

    #[test]
    fn let_stmts() {
        expect_ir_result(
            "
                let x := 1;
                let y := 2;
                x;
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
            x;
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
            x + 3 + y;
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
        expect_err("1 + true;", ExpectedType(Ty::int(), Ty::bool()));
        expect_err("true + 1;", ExpectedType(Ty::int(), Ty::bool()));
    }

    #[test]
    fn parens() {
        expect_ir_result(
            "volatile 3 * (volatile 4 + volatile 5);",
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
        expect_ir_result("3 * (4 + 5);", vec![Mov(PushStack, Immediate(27))], 27);
    }

    #[test]
    fn precedence() {
        expect_ir_result(
            "volatile 4 + volatile 5 * volatile 3;",
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
        expect_ir_result("4 + 5 * 3;", vec![Mov(PushStack, Immediate(19))], 19);
    }

    #[test]
    fn negation() {
        expect_ir_result("-3;", vec![Mov(PushStack, Immediate(-3))], -3);

        expect_ir_result(
            "let a := 3; -a;",
            vec![
                Mov(PushStack, Immediate(3)),
                Mov(PushStack, Immediate(0)),
                Sub(StackOffset(0), StackOffset(1)),
            ],
            -3,
        );

        expect_ir_result(
            "-(volatile 3);",
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
        expect_result(
            "
            let x := 1;
            let ptr := &x;
            (volatile ptr)[];
        ",
            vec![
                // // let x := 1;
                // Mov(PushStack, Immediate(1)),
                // // let ptr := &x;
                // LoadAddress(PushStack, StackOffset(0)),
                // // volatile ptr
                // Mov(PushStack, StackOffset(0)),
                // // []
                // Mov(R0, StackOffset(0)),
                // Mov(StackOffset(0), R0Offset(0)),
            ],
            1,
        );

        expect_ir_result(
            "
            let x := 1;
            let ptr := &x;
            ptr[];
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
            x;
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
            "(volatile true as Int) +  2;",
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
                let a : Word := 3;
            ",
            vec![Mov(PushStack, Immediate(3))],
        )
    }

    #[test]
    fn record() {
        expect_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            (volatile p).x;
        ",
            vec![
                // // let p := Point { x: 1, y: 2 };
                // Sub(SP, Immediate(2)),
                // Mov(StackOffset(0), Immediate(123)),
                // Mov(StackOffset(1), Immediate(456)),
                // // volatile p
                // Sub(SP, Immediate(2)),
                // Mov(StackOffset(0), StackOffset(2)),
                // Mov(StackOffset(1), StackOffset(3)),
                // // .x
                // Mov(StackOffset(1), StackOffset(0)),
                // Add(SP, Immediate(1)),
            ],
            123,
        );

        expect_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            (volatile p).y;
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
            type Point := record { x: Int, y: Int };
            let p := Point { x: 123, y: 456 };
            p.x;
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
    fn record_assignment() {
        expect_ir(
            "
            type Point := record { x: Int, y: Int };
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
    fn pointer_to_record_field() {
        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            (volatile ptr)[];
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
                Mov(PushStack, R0Offset(0)),
                // cleanup
                Mov(StackOffset(1), StackOffset(0)),
                Add(SP, Immediate(1)),
            ],
            5,
        );

        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p.x;
            p.x := 5;
            ptr[];
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
    fn deref_record_pointer_field() {
        expect_ir_result(
            "
            type Point := record { x: Int, y: Int };
            let p := Point { x: 1, y: 2 };
            let ptr := &p;
            ptr[].y;
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
            TrafficLight.Yellow;
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
            flags.Zero;
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
        expect_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[volatile 1];
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

        expect_result(
            "
                let xs := array[Int: 4]{10, 20, 30, 40};
                xs[1];
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
                type Point := record { x: Int, y: Int };
                type Shape := record {
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

                disc.fill;
            ",
            vec![],
            1,
        );
    }

    #[test]
    fn if_stmt() {
        expect_ir_result(
            "
            let i := 1;
            if false then
                i := 3;
            end
            i;
        ",
            vec![
                // let i := 1;
                Mov(PushStack, Immediate(1)),
                // false
                Mov(PushStack, Immediate(0)),
                // if .. then
                BranchZero(Immediate(1), PopStack),
                // i := 3
                Mov(StackOffset(0), Immediate(3)),
                // i;
                Mov(PushStack, StackOffset(0)),
            ],
            1,
        );
        expect_result(
            "
            let i := 1;
            if true then
                i := 3;
            end
            i;
        ",
            vec![],
            3,
        );
    }

    #[test]
    fn if_else_stmt() {
        expect_ir_result(
            "
            let i := 0;
            if true then
                i := 3;
            else
                i := 4;
            end
            i;
        ",
            vec![
                // let i := 0
                Mov(PushStack, Immediate(0)),
                // true
                Mov(PushStack, Immediate(1)),
                // if .. then
                BranchZero(Immediate(2), PopStack),
                // i := 3
                Mov(StackOffset(0), Immediate(3)),
                // -> skip else
                BranchZero(Immediate(1), Immediate(0)),
                // else:
                // i := 4;
                Mov(StackOffset(0), Immediate(4)),
                // i
                Mov(PushStack, StackOffset(0)),
            ],
            3,
        );
        expect_result(
            "
            let i := 0;
            if false then
                i := 3;
            else
                i := 4;
            end
            i;
        ",
            vec![],
            4,
        );
    }

    #[test]
    fn else_if() {
        expect_ir_result(
            "
            let i := 0;
            if false then
                i := 3;
            else if true then
                i := 4;
            else 
                i := 5;
            end
            i;
        ",
            vec![
                // let i := 0;
                Mov(PushStack, Immediate(0)),
                // false
                Mov(PushStack, Immediate(0)),
                // if .. then
                BranchZero(Immediate(2), PopStack),
                // i := 3
                Mov(StackOffset(0), Immediate(3)),
                // -> end
                BranchZero(Immediate(5), Immediate(0)),
                // true
                Mov(PushStack, Immediate(1)),
                // if .. then
                BranchZero(Immediate(2), PopStack),
                // i := 4
                Mov(StackOffset(0), Immediate(4)),
                //  -> end
                BranchZero(Immediate(1), Immediate(0)),
                // i := 5
                Mov(StackOffset(0), Immediate(5)),
                // end: i
                Mov(PushStack, StackOffset(0)),
            ],
            4,
        );
    }

    #[test]
    fn while_stmt() {
        expect_ir_result(
            "
            let count := 0;
            while count != 10 loop
                count := count + 1;
            end
            count;
        ",
            vec![
                // let count := 0;
                Mov(PushStack, Immediate(0)),
                // begin: count
                Mov(PushStack, StackOffset(0)),
                // != 10
                NotEqual(StackOffset(0), Immediate(10)),
                // -> end
                BranchZero(Immediate(5), PopStack),
                // count
                Mov(PushStack, StackOffset(0)),
                // + 1
                Add(StackOffset(0), Immediate(1)),
                // count :=
                Mov(StackOffset(1), StackOffset(0)),
                // drop
                Add(SP, Immediate(1)),
                // -> begin
                BranchZero(Immediate(-8), Immediate(0)),
                // end: count
                Mov(PushStack, StackOffset(0)),
            ],
            10,
        );
    }

    #[test]
    fn match_stmt() {
        expect_ir_result(
            "
            type Option := record {
                case Some {
                    value: Int
                }
                case None {}
            };

            let result := 1;
            let opt := Option.Some{value: 3};

            match opt then
            case Some{value}:
                result := value;
            case None{}:
                result := 10;
            end

            result;
        ",
            vec![
                // let result := 0;
                Mov(PushStack, Immediate(1)),
                // let opt := Option.Some{value: 3};
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(0)),
                Mov(StackOffset(1), Immediate(3)),
                // opt (todo: don't need to force resolution)
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // match .. then
                BranchZero(StackOffset(0), Immediate(0)),
                BranchZero(Immediate(1), Immediate(0)),
                BranchZero(Immediate(2), Immediate(0)),
                // Some: result := value
                Mov(StackOffset(4), StackOffset(1)),
                BranchZero(Immediate(2), Immediate(0)),
                // None: result := 10
                Mov(StackOffset(4), Immediate(10)),
                BranchZero(Immediate(0), Immediate(0)),
                // end; result
                Mov(PushStack, StackOffset(4)),
                // cleanup
                Mov(StackOffset(2), StackOffset(0)),
                Add(SP, Immediate(2)),
            ],
            3,
        );

        expect_ir_result(
            "
            type Option := record {
                case Some {
                    value: Int
                }
                case None {}
            };

            let result := 1;
            let opt := Option.None{};

            match opt then
            case Some{value}:
                result := value;
            case None{}:
                result := 10;
            end

            result;
        ",
            vec![
                // let result := 0;
                Mov(PushStack, Immediate(1)),
                // let opt := Option.None{};
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), Immediate(1)),
                // opt (todo: don't need to force resolution)
                Sub(SP, Immediate(2)),
                Mov(StackOffset(0), StackOffset(2)),
                Mov(StackOffset(1), StackOffset(3)),
                // match .. then
                BranchZero(StackOffset(0), Immediate(0)),
                BranchZero(Immediate(1), Immediate(0)),
                BranchZero(Immediate(2), Immediate(0)),
                // Some: result := value
                Mov(StackOffset(4), StackOffset(1)),
                BranchZero(Immediate(2), Immediate(0)),
                // None: result := 10
                Mov(StackOffset(4), Immediate(10)),
                BranchZero(Immediate(0), Immediate(0)),
                // end; result
                Mov(PushStack, StackOffset(4)),
                // cleanup
                Mov(StackOffset(2), StackOffset(0)),
                Add(SP, Immediate(2)),
            ],
            10,
        );
    }

    #[test]
    fn block_scope() {
        expect_ir_result(
            "
            let x := 1;
            if true then
                let x := 2;
                x := 3;
            end
            x;
        ",
            vec![
                // let x:= 1
                Mov(PushStack, Immediate(1)),
                // true
                Mov(PushStack, Immediate(1)),
                // if .. then
                BranchZero(Immediate(3), PopStack),
                // let x := 2; (new x)
                Mov(PushStack, Immediate(2)),
                // x := 3;
                Mov(StackOffset(0), Immediate(3)),
                // drop scope
                Add(SP, Immediate(1)),
                // x
                Mov(PushStack, StackOffset(0)),
            ],
            1,
        );
    }

    #[test]
    fn subroutine() {
        assert!(Compiler::program(
            "
            type Option := record {
                case Some {
                    value: Int
                }
                case None {}
            };

            sub main() do
                let x := Option.Some{value: 3};
            end
        "
        )
        .is_ok());
    }
}
