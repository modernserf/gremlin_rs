use std::collections::HashMap;
use std::rc::Rc;

use crate::block::*;
use crate::lexer::*;
use crate::memory::*;
use crate::module::ModuleRecord;
use crate::op::Op;
use crate::runtime::*;
use crate::stmt::*;
use crate::ty::*;
use crate::{Compile, CompileError::*, CompileOpt};

pub struct ExprParser<'lexer, 'compiler, 'memory, 'module_scope, 'scope, 'ty_scope> {
    lexer: &'lexer mut Lexer,
    compiler: &'compiler mut ExprCompiler<'memory, 'module_scope, 'scope>,
    ty_scope: &'ty_scope mut TyScope,
}

impl<'lexer, 'compiler, 'memory, 'module_scope, 'scope, 'ty_scope>
    ExprParser<'lexer, 'compiler, 'memory, 'module_scope, 'scope, 'ty_scope>
{
    pub fn new(
        lexer: &'lexer mut Lexer,
        compiler: &'compiler mut ExprCompiler<'memory, 'module_scope, 'scope>,
        ty_scope: &'ty_scope mut TyScope,
    ) -> Self {
        Self {
            lexer,
            compiler,
            ty_scope,
        }
    }

    fn expect_type_expr(&mut self) -> Compile<Ty> {
        let mut p = TypeExprParser::new(self.lexer, self.ty_scope);
        let ty = p.expect_type_expr()?;
        Ok(ty)
    }

    fn bitset_expr(&mut self, ty: Ty) -> Compile<Expr> {
        let mut value = 0;
        while let Some(key) = self.lexer.type_ident_token()? {
            let field = ty.oneof_member(&key)?;
            // TODO: check for dupes
            value |= 1 << field.index;

            if self.lexer.token(Token::Comma)?.is_none() {
                break;
            }
        }
        Ok(Expr::constant(ty.into_bitset()?, value))
    }

    fn record_expr(&mut self, ty: Ty, case: Option<Word>) -> Compile<Expr> {
        let block = self.compiler.allocate(&ty);
        let record = ty.get_record()?;

        if let Some(case_id) = case {
            let case_field = record.case_field.as_ref().ok_or(Expected("case field"))?;
            let field_ctx =
                ExprTarget::Reference(Reference::block(block).focus(case_field.to_slice()));
            self.compiler
                .resolve_expr(Expr::constant(Ty::int(), case_id), field_ctx);
        }

        while let Some(field_name) = self.lexer.ident_token()? {
            self.lexer.expect_token(Token::Colon)?;
            let field = record.get(&field_name, case)?;
            let field_ctx = ExprTarget::Reference(Reference::block(block).focus(field.to_slice()));
            let expr = self.expect_expr()?;
            field.ty.check(&expr.ty)?;
            self.compiler.resolve_expr(expr, field_ctx);

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
        let block = self.compiler.allocate(&ty);

        let mut i = 0;
        loop {
            let cell_ctx = ExprTarget::Reference(
                Reference::block(block).focus(Slice::from_array_index(&item_ty, i)),
            );
            match self.expr()? {
                Some(p) => {
                    item_ty.check(&p.ty)?;
                    self.compiler.resolve_expr(p, cell_ctx);
                }
                None => break,
            };
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
                self.compiler.resolve_stack(operand).into_expr()
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
        let mut op_expr = self.compiler.op_begin(left);

        while let Some(op) = self.lexer.op()? {
            self.compiler.op_next(&mut op_expr, op)?;
            let right = self.expect_unary_op_expr()?;
            self.compiler.op_push(&mut op_expr, right);
        }
        self.compiler.op_end(op_expr).map(Some)
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
        self.compiler.resolve_expr(expr, left.assign_ctx()?);
        Ok(Some(ResolvedExpr::void().into_expr()))
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

    pub fn expr(&mut self) -> CompileOpt<Expr> {
        self.as_expr()
    }

    fn expect_expr(&mut self) -> Compile<Expr> {
        self.expr()?.ok_or(Expected("expr"))
    }
}

#[derive(Debug)]
pub struct Expr {
    pub ty: Ty,
    kind: ExprKind,
}

#[derive(Debug)]
pub struct Reference {
    // 0 = an identifier, 1 = a pointer, 2 = a pointer to a pointer...
    deref_level: usize,
    // ref_level 0 = the location of the referent, 1 = the location of the pointer to the referent, etc
    next: Block,
    // segment of referent that we want (e.g. a particular struct field within a struct)
    focus: Slice,
}

impl Reference {
    pub fn block(block: Block) -> Self {
        Self {
            deref_level: 0,
            next: block,
            focus: Slice::with_size(block.size()),
        }
    }
    pub fn focus(self, focus: Slice) -> Self {
        Self {
            deref_level: self.deref_level,
            next: self.next,
            focus: self.focus.focus_direct(focus), // hmmm
        }
    }
    pub fn inc_deref(self) -> Self {
        Self {
            deref_level: self.deref_level + 1,
            next: self.next,
            focus: self.focus,
        }
    }

    // TODO: can a src/dest "own" a register, so that it can be automatically released after use?
    fn into_dest_with_register(self, memory: &mut Memory) -> (Dest, Option<Register>) {
        match self.deref_level {
            0 => (Dest::Block(self.next.focus(self.focus)), None),
            deref_level => {
                let register = memory.load_ptr_iter(self.next, deref_level);
                let dest = Dest::Offset(register, self.focus);
                (dest, Some(register))
            }
        }
    }
    fn into_src_with_register(self, memory: &mut Memory) -> (Src, Option<Register>) {
        match self.deref_level {
            0 => (Src::Block(self.next.focus(self.focus)), None),
            deref_level => {
                let register = memory.load_ptr_iter(self.next, deref_level);
                let src = Src::Offset(register, self.focus);
                (src, Some(register))
            }
        }
    }
}

#[derive(Debug)]
enum ExprKind {
    // a value in memory, not assigned to a variable
    Block(Block),
    // a value known at compile time that has not yet been written to memory
    Constant(Word),
    // the result of a comparison that set the status register
    Cond(IRCond),
    // a value accessible through a variable (ie an "lvalue")
    Reference(Reference),
    // a subroutine
    Sub(SubIndex),
}

impl Expr {
    pub fn resolved(ty: Ty, block: Block) -> Self {
        Self {
            ty,
            kind: ExprKind::Block(block),
        }
    }
    pub fn constant(ty: Ty, value: Word) -> Self {
        Self {
            ty,
            kind: ExprKind::Constant(value),
        }
    }
    pub fn cond(cond: IRCond) -> Self {
        Self {
            ty: Ty::bool(),
            kind: ExprKind::Cond(cond),
        }
    }
    pub fn lvalue(ty: Ty, block: Block) -> Self {
        Self {
            ty,
            kind: ExprKind::Reference(Reference::block(block)),
        }
    }
    pub fn sub(ty: Ty, sub_index: SubIndex) -> Self {
        Self {
            ty,
            kind: ExprKind::Sub(sub_index),
        }
    }
    pub fn assign_ctx(self) -> Compile<ExprTarget> {
        match self.kind {
            ExprKind::Reference(r) => Ok(ExprTarget::Reference(r)),
            _ => Err(Expected("lvalue")),
        }
    }
    pub fn cast_ty(self, ty: Ty) -> Compile<Self> {
        Ok(Self {
            ty: self.ty.cast(ty)?,
            kind: self.kind,
        })
    }
    pub fn sub_index(&self) -> Compile<SubIndex> {
        match &self.kind {
            ExprKind::Sub(sub_index) => Ok(*sub_index),
            _ => Err(Expected("subroutine")),
        }
    }
    pub fn record_field(self, field_name: &str) -> Compile<Self> {
        let record = self.ty.get_record()?;
        let field = record.get(field_name, None)?;
        match self.kind {
            ExprKind::Constant(_) => unimplemented!(),
            ExprKind::Block(block) => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference(Reference::block(block).focus(field.to_slice())),
            }),
            ExprKind::Reference(r) => Ok(Self {
                ty: field.ty.clone(),
                kind: ExprKind::Reference(r.focus(field.to_slice())),
            }),
            _ => unimplemented!(),
        }
    }
    //
    pub fn add_ref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        match self.kind {
            ExprKind::Reference(r) => {
                let out = target.load_address(memory, r.next);
                Ok(Self::resolved(self.ty.add_ref(), out))
            }
            _ => Err(InvalidRef),
        }
    }
    pub fn deref(self, memory: &mut Memory, target: ExprTarget) -> Compile<Self> {
        let deref_ty = self.ty.deref()?;
        match self.kind {
            ExprKind::Block(block) => {
                let register = memory.load_ptr_iter(block, 1);
                let src = Src::Offset(register, Slice::with_size(deref_ty.size()));
                let out = target.mov(memory, src);
                memory.free_register(register);
                Ok(Self::resolved(deref_ty, out))
            }
            ExprKind::Reference(r) => Ok(Self {
                ty: deref_ty,
                kind: ExprKind::Reference(r.inc_deref()),
            }),
            _ => unreachable!(),
        }
    }
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
                // TODO: try popping left
                let left = Src::Block(left.resolve_to_stack(memory).block);
                op.apply(memory, ty, left, Src::Block(r))
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

    pub fn resolve_branch_cond(self, memory: &mut Memory) -> IRCond {
        Ty::bool().check(&self.ty).expect("bool");
        match self.kind {
            ExprKind::Constant(value) => {
                if value == 1 {
                    IRCond::Never // ie `if true then ... end` never branches at the `then`
                } else {
                    IRCond::Always
                }
            }
            ExprKind::Cond(cond) => cond,
            _ => {
                let res = self.resolve_to_stack(memory);
                memory.cmp_bool(res.block)
            }
        }
    }

    // expr resolved to stack has an addressable value, can be accumulated upon
    pub fn resolve_to_stack(self, memory: &mut Memory) -> ResolvedExpr {
        self.resolve_inner(memory, ExprTarget::Stack)
    }

    pub fn resolve(self, memory: &mut Memory, target: ExprTarget) {
        self.resolve_inner(memory, target);
    }
    fn resolve_inner(self, memory: &mut Memory, target: ExprTarget) -> ResolvedExpr {
        match self.kind {
            ExprKind::Block(block) => {
                let block = target.maybe_move_block(memory, block);
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Constant(value) => {
                let block = target.mov(memory, Src::Immediate(value));
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Cond(cond) => {
                let block = target.set_if(memory, cond);
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Reference(r) => {
                let (src, register) = r.into_src_with_register(memory);
                let block = target.mov(memory, src);
                if let Some(register) = register {
                    memory.free_register(register);
                }
                ResolvedExpr { ty: self.ty, block }
            }
            ExprKind::Sub(_) => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedExpr {
    pub ty: Ty,
    pub block: Block,
}

impl ResolvedExpr {
    pub fn void() -> Self {
        ResolvedExpr {
            ty: Ty::void(),
            block: Block::new(0, 0),
        }
    }
    fn into_expr(self) -> Expr {
        Expr::resolved(self.ty, self.block)
    }
}

#[derive(Debug)]
pub enum ExprTarget {
    Stack,
    Reference(Reference),
}

impl ExprTarget {
    fn into_dest(self, memory: &mut Memory) -> (Dest, Option<Register>) {
        match self {
            Self::Stack => (Dest::Stack, None),
            Self::Reference(r) => r.into_dest_with_register(memory),
        }
    }
    fn mov(self, memory: &mut Memory, src: Src) -> Block {
        let (dest, register) = self.into_dest(memory);
        let block = memory.mov(dest, src);
        if let Some(r) = register {
            memory.free_register(r);
        }
        block
    }
    fn set_if(self, memory: &mut Memory, cond: IRCond) -> Block {
        let (dest, register) = self.into_dest(memory);
        let block = memory.set_if(dest, cond);
        if let Some(r) = register {
            memory.free_register(r);
        }
        block
    }
    fn load_address(self, memory: &mut Memory, block: Block) -> Block {
        let src = Src::Block(block);
        let (dest, register) = self.into_dest(memory);
        let block = memory.load_address(dest, src);
        if let Some(r) = register {
            memory.free_register(r);
        }
        block
    }
    fn maybe_move_block(self, memory: &mut Memory, block: Block) -> Block {
        match self {
            Self::Stack => block,
            _ => self.mov(memory, Src::Block(block)),
        }
    }
}

pub struct ExprCompiler<'memory, 'module_scope, 'scope> {
    memory: &'memory mut Memory,
    module_scope: &'module_scope mut HashMap<String, ModuleRecord>,
    scope: &'scope Vec<ScopeFrame>,
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

impl<'memory, 'module_scope, 'scope> ExprCompiler<'memory, 'module_scope, 'scope> {
    pub fn new(
        memory: &'memory mut Memory,
        module_scope: &'module_scope mut HashMap<String, ModuleRecord>,
        scope: &'scope Vec<ScopeFrame>,
    ) -> Self {
        Self {
            memory,
            module_scope,
            scope,
        }
    }

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
        arg.resolve(self.memory, ExprTarget::Stack);
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

// Ops

pub struct OpExpr {
    op_stack: Vec<Op>,
    operands: Vec<Expr>,
}

impl ExprCompiler<'_, '_, '_> {
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

impl ExprCompiler<'_, '_, '_> {
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
    fn get_local(&self, key: &str) -> Option<&ScopeRecord> {
        for frame in self.scope.iter().rev() {
            if let Some(rec) = frame.scope.get(key) {
                return Some(rec);
            }
        }
        None
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
        let block = expr.resolve_to_stack(self.memory).block;
        self.memory.bit_test(block, Src::Immediate(field.index));
        self.memory.set_if(Dest::Block(block), IRCond::NotZero);
        Ok(Expr::resolved(Ty::bool(), block))
    }
}
