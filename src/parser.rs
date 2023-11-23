use crate::compiler::*;
use crate::expr::*;
use crate::lexer::*;
use crate::memory::*;
use crate::op::*;
use crate::record::*;
use crate::runtime::*;
use crate::subroutine::*;
use crate::ty::*;
use crate::{Compile, CompileError::*, CompileOpt};

pub struct Parser {
    compiler: Compiler,
    lexer: Lexer,
}

impl Parser {
    pub fn program(input: &str) -> Compile<CompileResult> {
        let mut parse = Self::new(input);
        parse.module()?;
        parse.lexer.expect_token(Token::EndOfInput)?;
        parse.compiler.resolve()
    }
    #[allow(dead_code)]
    pub fn script(input: &str) -> Compile<Vec<IR>> {
        let mut parse = Self::new(input);
        parse.compiler.script_scope();
        parse.block()?;
        parse.lexer.expect_token(Token::EndOfInput)?;
        Ok(parse.compiler.done())
    }
    fn new(input: &str) -> Self {
        Self {
            lexer: Lexer::new(input),
            compiler: Compiler::new(),
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
        let block = self.compiler.allocate(&ty);
        let record = ty.get_record()?;

        if let Some(case_id) = case {
            let case_field = record.case_field.as_ref().ok_or(Expected("case field"))?;
            let field_ctx = ExprTarget::Block(block.record_field(case_field));
            self.compiler
                .resolve_expr(Expr::constant(Ty::int(), case_id), field_ctx);
        }

        loop {
            let field_name = match self.lexer.ident_token()? {
                Some(s) => s,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let field = record.get(&field_name, case)?;
            let field_ctx = ExprTarget::Block(block.record_field(field));
            let expr = self.expect_expr()?;
            let resolved = self.compiler.resolve_expr(expr, field_ctx);
            field.ty.check(&resolved.ty)?;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(Expr::resolved(ty, block))
    }

    fn oneof_member_expr(&mut self, name: &str) -> Compile<Expr> {
        let ty = self.compiler.get_ty(name)?;

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
        let block = self.compiler.allocate(&ty);

        let mut i = 0;
        loop {
            let cell_ctx = ExprTarget::Block(block.array_index(&item_ty, i));
            let expr = match self.expr()? {
                Some(p) => self.compiler.resolve_expr(p, cell_ctx),
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
                        let ty = self.compiler.get_ty(&name)?;
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
                self.compiler.get_expr(&name)?
            }
            _ => return Ok(None),
        };
        Ok(Some(res))
    }

    fn array_index(&mut self, array: Expr) -> Compile<Expr> {
        let item_ty = array.ty.index_ty(&Ty::int())?.clone();
        let array_ptr = self.compiler.array_ptr(array, &item_ty)?;
        let idx = self.expect_expr()?;
        self.compiler
            .add_index_to_array_ptr(array_ptr, idx, &item_ty)
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
                        left = self.compiler.deref_expr(left)?
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
                            self.compiler.bitset_field(left, &field_name)?
                        }
                        _ => return Err(Expected("field")),
                    }
                }
                Token::ParLeft => {
                    self.lexer.advance();
                    let mut builder = self.compiler.begin_call(left)?;
                    loop {
                        match self.expr()? {
                            Some(expr) => {
                                self.compiler.call_arg(&mut builder, expr)?;
                            }
                            None => break,
                        };
                        if self.lexer.token(Token::Comma)?.is_none() {
                            break;
                        }
                    }
                    self.lexer.expect_token(Token::ParRight)?;
                    left = self.compiler.end_call(builder)?;
                }
                _ => return Ok(Some(left)),
            };
        }
    }

    fn unary_op_expr(&mut self) -> CompileOpt<Expr> {
        let out = match self.lexer.peek()? {
            Token::Minus => {
                self.lexer.advance();
                let expr = self.expect_unary_op_expr()?;
                self.compiler.negate_expr(expr)?
            }
            Token::Ampersand => {
                self.lexer.advance();
                let expr = self.expect_unary_op_expr()?;
                self.compiler.add_ref_expr(expr)?
            }
            Token::Volatile => {
                self.lexer.advance();
                let operand = self.expect_unary_op_expr()?;
                self.compiler
                    .resolve_expr(operand, ExprTarget::Stack)
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
                    self.compiler.op_next(&mut op_expr, op, right)?;
                }
                None => return self.compiler.op_unwind(op_expr).map(Some),
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
        let expr = self.op_expr()?.ok_or(Expected("expr"))?;
        let resolved = self.compiler.resolve_expr(expr, left.assign_ctx()?);
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
        let mut fields = self.compiler.new_record_ty();
        self.record_items(&mut fields, None)?;
        Ok(Ty::record(fields))
    }

    fn oneof_ty(&mut self) -> Compile<Ty> {
        let mut data = self.compiler.new_oneof_ty();
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
                self.compiler.get_ty(&ident).map(Some)
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
        self.compiler.assign_ty(tb, te);
        self.lexer.expect_token(Token::Semicolon)?;
        Ok(())
    }

    fn if_cond(&mut self) -> Compile<CondIndex> {
        let expr = self.expect_expr()?;
        self.compiler.begin_cond(expr)
    }

    // TODO: if as expr
    fn if_stmt(&mut self) -> Compile<()> {
        let mut elses = Vec::new();
        let mut if_rec = self.if_cond()?;
        self.lexer.expect_token(Token::Then)?;
        self.compiler.push_scope();
        self.block()?;
        self.compiler.pop_scope();
        loop {
            if self.lexer.token(Token::Else)?.is_none() {
                break;
            }

            if self.lexer.token(Token::If)?.is_some() {
                elses.push(self.compiler.begin_else(if_rec));
                if_rec = self.if_cond()?;
                self.lexer.expect_token(Token::Then)?;
                self.compiler.push_scope();
                self.block()?;
                self.compiler.pop_scope();
            } else {
                if_rec = self.compiler.begin_else(if_rec);
                self.compiler.push_scope();
                self.block()?;
                self.compiler.pop_scope();
                break;
            }
        }
        self.lexer.expect_token(Token::End)?;
        self.compiler.end_if(if_rec);
        for rec in elses {
            self.compiler.end_if(rec);
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
            self.compiler.add_case_binding(case, binding)?;
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
        let mut match_builder = self.compiler.begin_match(target)?;

        loop {
            if self.lexer.token(Token::Case)?.is_none() {
                break;
            }
            let tag = self
                .lexer
                .type_ident_token()?
                .ok_or(Expected("match case"))?;
            let mut case = self.compiler.begin_match_case(&mut match_builder, &tag)?;
            self.compiler.push_scope();
            self.match_bindings(&mut case)?;
            self.block()?;
            self.compiler.pop_scope();
            self.compiler.end_match_case(&mut match_builder);
        }
        self.lexer.expect_token(Token::End)?;
        self.compiler.end_match(match_builder);
        Ok(())
    }

    fn while_stmt(&mut self) -> Compile<()> {
        let while_idx = self.compiler.begin_while();
        let cond = self.if_cond()?;
        self.lexer.expect_token(Token::Loop)?;
        self.compiler.push_scope();
        self.block()?;
        self.compiler.pop_scope();
        self.lexer.expect_token(Token::End)?;
        self.compiler.end_while(while_idx, cond);
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

        let expr = self.expect_expr()?;
        let resolved = self.compiler.resolve_expr(expr, ExprTarget::Stack);
        match bind_ty {
            Some(b) => b.check(&resolved.ty)?,
            None => {}
        };
        self.compiler.assign_expr(binding, resolved);
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
            Token::Debug => {
                self.lexer.advance();
                self.compiler.debug_stack();
                self.lexer.expect_token(Token::Semicolon)?;
            }
            Token::Return => {
                self.lexer.advance();
                let maybe_expr = self.expr()?;
                self.compiler.return_sub(maybe_expr)?;
                self.lexer.expect_token(Token::Semicolon)?;
            }

            _ => {
                match self.expr()? {
                    Some(expr) => {
                        let res = self.compiler.resolve_expr(expr, ExprTarget::Stack);
                        self.compiler.compact(res.block);
                        self.lexer.expect_token(Token::Semicolon)?;
                    }
                    None => return Ok(None),
                };
            }
        };
        Ok(Some(()))
    }

    fn block(&mut self) -> Compile<()> {
        loop {
            if self.stmt()?.is_none() {
                return Ok(());
            }
        }
    }

    fn sub_params(&mut self, builder: &mut SubBuilder) -> Compile<()> {
        self.lexer.expect_token(Token::ParLeft)?;
        loop {
            let binding = match self.binding()? {
                Some(b) => b,
                None => break,
            };
            self.lexer.expect_token(Token::Colon)?;
            let ty = self.type_expr()?.ok_or(Expected("type"))?;
            builder.add_param(binding, ty);

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        self.lexer.expect_token(Token::ParRight)?;

        if self.lexer.token(Token::Arrow)?.is_some() {
            let ty = self.type_expr()?.ok_or(Expected("return type"))?;
            builder.returns(ty);
        };

        Ok(())
    }

    fn sub(&mut self) -> Compile<()> {
        let name = self.lexer.ident_token()?.ok_or(Expected("name"))?;
        let mut builder = SubBuilder::new(name);
        self.sub_params(&mut builder)?;
        self.lexer.expect_token(Token::Do)?;
        self.compiler.begin_sub(builder);
        self.block()?;
        self.compiler.end_sub()?;
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
                break;
            }
        }
        self.lexer.expect_token(Token::EndOfInput)
    }
}
