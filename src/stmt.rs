use crate::block::*;
use crate::expr::*;
use crate::lexer::*;
use crate::memory::*;
use crate::module::*;
use crate::runtime::*;
use crate::ty::*;
use crate::{Compile, CompileError::*, CompileOpt};
use std::collections::HashMap;
use std::rc::Rc;

pub struct StmtParser<'lexer, 'compiler, 'memory, 'module_scope, 'ty_scope> {
    lexer: &'lexer mut Lexer,
    pub compiler: &'compiler mut StmtCompiler<'memory, 'module_scope>,
    ty_scope: &'ty_scope mut TyScope,
}

impl<'lexer, 'compiler, 'memory, 'module_scope, 'ty_scope>
    StmtParser<'lexer, 'compiler, 'memory, 'module_scope, 'ty_scope>
{
    pub fn new(
        lexer: &'lexer mut Lexer,
        compiler: &'compiler mut StmtCompiler<'memory, 'module_scope>,
        ty_scope: &'ty_scope mut TyScope,
    ) -> Self {
        Self {
            lexer,
            compiler,
            ty_scope,
        }
    }

    pub fn block(&mut self) -> Compile<()> {
        loop {
            if self.stmt()?.is_none() {
                return Ok(());
            }
        }
    }

    fn expr(&mut self) -> CompileOpt<Expr> {
        let mut expr_compiler = ExprCompiler::new(
            self.compiler.memory,
            self.compiler.module_scope,
            &mut self.compiler.scope,
        );
        let mut expr_parser = ExprParser::new(self.lexer, &mut expr_compiler, self.ty_scope);
        let expr = expr_parser.expr()?;
        Ok(expr)
    }

    fn expect_expr(&mut self) -> Compile<Expr> {
        self.expr()?.ok_or(Expected("expr"))
    }

    // ### Bindings, TypeExprs, etc.

    fn binding(&mut self) -> CompileOpt<String> {
        self.lexer.ident_token()
    }

    fn type_expr(&mut self) -> CompileOpt<Ty> {
        let mut p = TypeExprParser::new(self.lexer, self.ty_scope);
        let ty = p.type_expr()?;
        Ok(ty)
    }

    fn expect_type_expr(&mut self) -> Compile<Ty> {
        self.type_expr()?.ok_or(Expected("type expr"))
    }

    // ### Statements

    fn type_binding(&mut self) -> CompileOpt<String> {
        self.lexer.type_ident_token()
    }

    // TODO: remove statement-level typedefs?
    fn type_def_stmt(&mut self) -> Compile<()> {
        let tb = self.type_binding()?.ok_or(Expected("type binding"))?;
        self.lexer.expect_token(Token::ColonEq)?;
        let te = self.expect_type_expr()?;
        self.ty_scope.assign(tb, te);
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
        while let Some(binding) = self.lexer.ident_token()? {
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

        let frame_offset = self.compiler.begin_compact();
        let expr = self.expect_expr()?;
        let resolved = expr.resolve_to_stack(self.compiler.memory);
        if let Some(b) = bind_ty {
            b.check(&resolved.ty)?;
        }
        self.compiler.assign_expr(binding, resolved, frame_offset);
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
            Token::Panic => {
                self.lexer.advance();
                self.compiler.panic();
                self.lexer.expect_token(Token::Semicolon)?;
            }

            _ => {
                let state = self.compiler.begin_compact();
                match self.expr()? {
                    Some(expr) => {
                        let res = expr.resolve_to_stack(self.compiler.memory);
                        self.compiler.end_compact(state, res);
                        self.lexer.expect_token(Token::Semicolon)?;
                    }
                    None => return Ok(None),
                };
            }
        };
        Ok(Some(()))
    }
}

pub struct StmtCompiler<'memory, 'module_scope> {
    memory: &'memory mut Memory,
    module_scope: &'module_scope mut HashMap<String, ModuleRecord>,
    scope: Vec<ScopeFrame>,
    return_expr: ResolvedExpr,
    did_return: bool,
}

impl<'memory, 'module_scope> StmtCompiler<'memory, 'module_scope> {
    pub fn from_module(
        memory: &'memory mut Memory,
        module_scope: &'module_scope mut HashMap<String, ModuleRecord>,
        return_expr: ResolvedExpr,
    ) -> Self {
        Self {
            memory,
            module_scope,
            scope: vec![ScopeFrame::new(ScopeIndex::root())],
            return_expr,
            did_return: false,
        }
    }

    fn check_return(&mut self, ty: &Ty) -> Compile<()> {
        self.did_return = true;
        self.return_expr.ty.check(ty)
    }

    fn push_scope(&mut self) {
        self.scope.push(ScopeFrame::new(self.memory.begin_scope()));
    }
    fn pop_scope(&mut self) {
        let last_frame = self.scope.pop().expect("scope frame");
        self.memory.end_scope(last_frame.scope_index);
    }
    pub fn insert_scope_record(&mut self, key: String, value: ScopeRecord) {
        let len = self.scope.len();
        self.scope[len - 1].scope.insert(key, value);
    }
}

impl StmtCompiler<'_, '_> {
    fn return_sub(&mut self, expr: Option<Expr>) -> Compile<()> {
        match expr {
            Some(expr) => {
                self.check_return(&expr.ty)?;
                expr.resolve(
                    self.memory,
                    ExprTarget::Reference(Reference::block(self.return_expr.block)),
                );
            }
            None => {
                self.check_return(&Ty::void())?;
            }
        };
        self.memory.return_sub();
        Ok(())
    }
    pub fn end_sub(&mut self) -> Compile<()> {
        if !self.did_return {
            if self.return_expr.ty == Ty::void() {
                self.memory.return_sub();
            } else {
                return Err(Expected("return"));
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct MatchBuilder {
    case_index: CaseIndex,
    record: Rc<TyRecord>,
    end_addrs: Vec<usize>,
    block: Block,
}

#[derive(Debug)]
pub struct MatchCaseBuilder {
    record: Rc<TyRecord>,
    case_id: Word,
    parent_block: Block,
}

impl StmtCompiler<'_, '_> {
    fn begin_match(&mut self, expr: Expr) -> Compile<MatchBuilder> {
        let res = expr.resolve_to_stack(self.memory);
        let record = res.ty.get_record()?;
        let case_field = record.case_field.as_ref().ok_or(Expected("case field"))?;
        let case_value = res.block.focus(case_field.to_slice());
        let case_index = self.memory.begin_match(case_value, record.cases.len());
        Ok(MatchBuilder {
            record,
            block: res.block,
            case_index,
            end_addrs: Vec::new(),
        })
    }
    fn begin_match_case(
        &mut self,
        builder: &mut MatchBuilder,
        tag: &str,
    ) -> Compile<MatchCaseBuilder> {
        let case_id = *builder.record.cases.get(tag).ok_or(Expected("case"))?;
        self.memory
            .set_jump_target(builder.case_index, case_id as usize);

        Ok(MatchCaseBuilder {
            record: builder.record.clone(),
            case_id,
            parent_block: builder.block,
        })
    }
    fn add_case_binding(&mut self, case: &mut MatchCaseBuilder, name: String) -> Compile<()> {
        let field = case.record.get(&name, Some(case.case_id))?;
        let block = case.parent_block.focus(field.to_slice());

        self.insert_scope_record(
            name,
            ScopeRecord {
                frame_offset: block.frame_offset(),
                ty: field.ty.clone(),
            },
        );
        Ok(())
    }
    fn end_match_case(&mut self, builder: &mut MatchBuilder) {
        builder.end_addrs.push(self.memory.end_case());
    }
    fn end_match(&mut self, builder: MatchBuilder) {
        self.memory.set_jump_end_targets(
            builder.case_index,
            builder.record.cases.len(),
            &builder.end_addrs,
        );
    }
}

pub struct ScopeFrame {
    scope_index: ScopeIndex,
    pub scope: HashMap<String, ScopeRecord>,
}

impl ScopeFrame {
    fn new(scope_index: ScopeIndex) -> Self {
        Self {
            scope_index,
            scope: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct ScopeRecord {
    pub frame_offset: Word,
    pub ty: Ty,
}

impl StmtCompiler<'_, '_> {
    pub fn assign_expr(&mut self, name: String, expr: ResolvedExpr, prev_frame_offset: Word) {
        let frame_offset = self.memory.assign(expr.block, prev_frame_offset);
        self.insert_scope_record(
            name,
            ScopeRecord {
                frame_offset,
                ty: expr.ty,
            },
        );
    }
    fn begin_cond(&mut self, cond: Expr) -> Compile<CondIndex> {
        Ty::bool().check(&cond.ty)?;
        Ok(self.memory.begin_cond(cond))
    }
    fn begin_else(&mut self, if_rec: CondIndex) -> CondIndex {
        self.memory.begin_else(if_rec)
    }
    fn end_if(&mut self, if_rec: CondIndex) {
        self.memory.end_if(if_rec);
    }
    fn begin_while(&mut self) -> WhileIndex {
        self.memory.begin_while()
    }
    fn end_while(&mut self, while_index: WhileIndex, cond_index: CondIndex) {
        self.memory.end_while(while_index, cond_index)
    }
    fn debug_stack(&mut self) {
        self.memory.debug_stack()
    }
    fn panic(&mut self) {
        self.memory.panic();
    }
}

impl StmtCompiler<'_, '_> {
    fn begin_compact(&mut self) -> Word {
        self.memory.current_frame_offset
    }
    fn end_compact(&mut self, prev_frame_offset: Word, res: ResolvedExpr) {
        self.memory.compact(res.block, prev_frame_offset)
    }
}
