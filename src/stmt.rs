use crate::block::*;
use crate::expr::*;
use crate::lexer::*;
use crate::memory::*;
use crate::module::*;
use crate::op::*;
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
        let mut expr_parser = ExprParser::new(self.lexer, self.compiler, self.ty_scope);
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
        let resolved = self.compiler.resolve_stack(expr);
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
                        let res = self.compiler.resolve_stack(expr);
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

    pub fn check_return(&mut self, ty: &Ty) -> Compile<()> {
        self.did_return = true;
        self.return_expr.ty.check(ty)
    }

    pub fn push_scope(&mut self) {
        self.scope.push(ScopeFrame::new(self.memory.begin_scope()));
    }
    pub fn pop_scope(&mut self) {
        let last_frame = self.scope.pop().expect("scope frame");
        self.memory.end_scope(last_frame.scope_index);
    }
    fn get_local(&self, key: &str) -> Option<&ScopeRecord> {
        for frame in self.scope.iter().rev() {
            if let Some(rec) = frame.scope.get(key) {
                return Some(rec);
            }
        }
        None
    }
    pub fn insert_scope_record(&mut self, key: String, value: ScopeRecord) {
        let len = self.scope.len();
        self.scope[len - 1].scope.insert(key, value);
    }
}

// Calling convention
// [return value, args..., return addr, locals...] top
// caller:
// Sub(SP, sizeof return)
// Mov(Push, arg) ...
// JSR sub
// ...
// Add(SP, sizeof args)
// callee:
// ...
// Mov(SP+return, result)
// Add(SP, sizeof locals)
// RTS

pub struct CallBuilder {
    sub_index: SubIndex,
    ty_sub: Rc<TySub>,
    return_block: Block,
    current_arg: usize,
}

impl StmtCompiler<'_, '_> {
    pub fn begin_call(&mut self, callee: Expr) -> Compile<CallBuilder> {
        let sub_index = callee.sub_index()?;
        let ty_sub = callee.ty.get_sub()?;
        let return_block = self.memory.allocate(ty_sub.ret.size());
        Ok(CallBuilder {
            sub_index,
            ty_sub,
            return_block,
            current_arg: 0,
        })
    }
    pub fn call_arg(&mut self, builder: &mut CallBuilder, arg: Expr) -> Compile<()> {
        let params = &builder.ty_sub.params;
        if builder.current_arg > params.len() {
            return Err(Expected("fewer params"));
        }
        params[builder.current_arg].check(&arg.ty)?;
        self.resolve_expr(arg, ExprTarget::Stack);
        builder.current_arg += 1;
        Ok(())
    }
    pub fn end_call(&mut self, builder: CallBuilder) -> Compile<Expr> {
        if builder.current_arg != builder.ty_sub.params.len() {
            return Err(Expected("more params"));
        }
        self.memory
            .call_sub(builder.sub_index, builder.ty_sub.args_size());
        Ok(Expr::resolved(
            builder.ty_sub.ret.clone(),
            builder.return_block,
        ))
    }
}

impl StmtCompiler<'_, '_> {
    pub fn return_sub(&mut self, expr: Option<Expr>) -> Compile<()> {
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
    pub fn return_sub_default(&mut self) -> Compile<()> {
        self.memory.return_sub();
        Ok(())
    }
    pub fn end_sub(&mut self) -> Compile<()> {
        if !self.did_return {
            if self.return_expr.ty == Ty::void() {
                self.return_sub_default()?;
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
    pub fn begin_match(&mut self, expr: Expr) -> Compile<MatchBuilder> {
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
    pub fn begin_match_case(
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
    pub fn add_case_binding(&mut self, case: &mut MatchCaseBuilder, name: String) -> Compile<()> {
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
    pub fn end_match_case(&mut self, builder: &mut MatchBuilder) {
        builder.end_addrs.push(self.memory.end_case());
    }
    pub fn end_match(&mut self, builder: MatchBuilder) {
        self.memory.set_jump_end_targets(
            builder.case_index,
            builder.record.cases.len(),
            &builder.end_addrs,
        );
    }
}

// Ops

pub struct OpExpr {
    op_stack: Vec<Op>,
    operands: Vec<Expr>,
}

impl StmtCompiler<'_, '_> {
    pub fn op_begin(&mut self, left: Expr) -> OpExpr {
        OpExpr {
            op_stack: Vec::new(),
            operands: vec![left],
        }
    }
    // TODO: somewhere here need to
    pub fn op_next(&mut self, op_expr: &mut OpExpr, op: Op) -> Compile<()> {
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

    pub fn op_push(&mut self, op_expr: &mut OpExpr, expr: Expr) {
        op_expr.operands.push(expr);
    }

    pub fn op_end(&mut self, mut op_expr: OpExpr) -> Compile<Expr> {
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
    pub fn negate_expr(&mut self, expr: Expr) -> Compile<Expr> {
        self.op_simple(Op::Sub, Expr::constant(Ty::int(), 0), expr)
    }
    fn op_simple(&mut self, op: Op, left: Expr, right: Expr) -> Compile<Expr> {
        let op_expr = OpExpr {
            op_stack: vec![op],
            operands: vec![left, right],
        };
        self.op_end(op_expr)
    }
}

struct ScopeFrame {
    scope_index: ScopeIndex,
    scope: HashMap<String, ScopeRecord>,
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
    pub fn get_expr(&mut self, name: &str) -> Compile<Expr> {
        if let Some(record) = self.get_local(name) {
            let block = Block::new(record.frame_offset, record.ty.size());
            return Ok(Expr::lvalue(record.ty.clone(), block));
        }
        if let Some(record) = self.module_scope.get(name) {
            return Ok(Expr::sub(record.ty.clone(), record.sub_index));
        }

        Err(UnknownIdentifier(name.to_string()))
    }
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
    pub fn allocate(&mut self, ty: &Ty) -> Block {
        self.memory.allocate(ty.size())
    }
    pub fn resolve_expr(&mut self, expr: Expr, target: ExprTarget) {
        expr.resolve(self.memory, target)
    }
    pub fn resolve_stack(&mut self, expr: Expr) -> ResolvedExpr {
        expr.resolve_to_stack(self.memory)
    }
    // get pointer to first item from array
    pub fn array_ptr(&mut self, array: Expr, item_ty: &Ty) -> Compile<Expr> {
        array
            .add_ref(self.memory, ExprTarget::Stack)?
            .cast_ty(item_ty.add_ref())
    }
    // add (index * size) to value of pointer
    pub fn add_index_to_array_ptr(
        &mut self,
        array_ptr: Expr,
        idx: Expr,
        item_ty: &Ty,
    ) -> Compile<Expr> {
        let offset = self.op_simple(Op::Mul, idx, Expr::constant(Ty::int(), item_ty.size()))?;
        let ptr_ty = array_ptr.ty.clone();
        let offset_ptr = self
            .op_simple(Op::Add, array_ptr.cast_ty(Ty::int())?, offset)?
            .cast_ty(ptr_ty)?;
        // deref pointer
        offset_ptr.deref(self.memory, ExprTarget::Stack)
    }
    pub fn add_ref_expr(&mut self, expr: Expr) -> Compile<Expr> {
        expr.add_ref(self.memory, ExprTarget::Stack)
    }
    pub fn deref_expr(&mut self, expr: Expr) -> Compile<Expr> {
        expr.deref(self.memory, ExprTarget::Stack)
    }
    pub fn bitset_field(&mut self, expr: Expr, field_name: &str) -> Compile<Expr> {
        let field = expr.ty.oneof_member(field_name)?.clone();
        let block = self.resolve_stack(expr).block;
        self.memory.bit_test(block, Src::Immediate(field.index));
        self.memory.set_if(Dest::Block(block), IRCond::NotZero);
        Ok(Expr::resolved(Ty::bool(), block))
    }

    pub fn begin_cond(&mut self, cond: Expr) -> Compile<CondIndex> {
        Ty::bool().check(&cond.ty)?;
        Ok(self.memory.begin_cond(cond))
    }
    pub fn begin_else(&mut self, if_rec: CondIndex) -> CondIndex {
        self.memory.begin_else(if_rec)
    }
    pub fn end_if(&mut self, if_rec: CondIndex) {
        self.memory.end_if(if_rec);
    }
    pub fn begin_while(&mut self) -> WhileIndex {
        self.memory.begin_while()
    }
    pub fn end_while(&mut self, while_index: WhileIndex, cond_index: CondIndex) {
        self.memory.end_while(while_index, cond_index)
    }
    pub fn debug_stack(&mut self) {
        self.memory.debug_stack()
    }
    pub fn panic(&mut self) {
        self.memory.panic();
    }
}

impl StmtCompiler<'_, '_> {
    pub fn begin_compact(&mut self) -> Word {
        self.memory.current_frame_offset
    }
    pub fn end_compact(&mut self, prev_frame_offset: Word, res: ResolvedExpr) {
        self.memory.compact(res.block, prev_frame_offset)
    }
}
