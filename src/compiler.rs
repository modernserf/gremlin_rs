use crate::block::*;
use crate::expr::*;
use crate::memory::*;
use crate::op::*;
use crate::runtime::*;
use crate::ty::*;
use crate::{Compile, CompileError::*};
use std::collections::HashMap;
use std::rc::Rc;

pub struct ModuleCompiler {
    memory: Memory,
    module_scope: HashMap<String, ModuleRecord>,
}

pub struct ModuleRecord {
    ty: Ty,
    sub_index: SubIndex,
}

impl ModuleCompiler {
    pub fn init() -> Self {
        Self {
            module_scope: HashMap::new(),
            memory: Memory::new(),
        }
    }
    pub fn resolve(self) -> Compile<CompileResult> {
        let entry_point = self
            .module_scope
            .get("main")
            .ok_or(Expected("main"))?
            .sub_index;
        Ok(self.memory.done_program(entry_point))
    }
    pub fn script_scope(&mut self) -> StmtCompiler {
        StmtCompiler::from_module(
            &mut self.memory,
            &mut self.module_scope,
            ResolvedExpr::void(),
        )
    }
}

pub struct SubBuilder {
    name: String,
    params: Vec<(String, Ty)>,
    return_type: Ty,
}

impl SubBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            params: Vec::new(),
            return_type: Ty::void(),
        }
    }
    pub fn add_param(&mut self, name: String, ty: Ty) {
        self.params.push((name, ty));
    }
    pub fn returns(&mut self, ty: Ty) {
        self.return_type = ty;
    }
}

impl ModuleCompiler {
    pub fn begin_sub(&mut self, builder: SubBuilder) -> StmtCompiler {
        let params = builder.params.iter().map(|(_, ty)| ty.clone()).collect();
        let ty_sub = TySub::new(params, builder.return_type.clone());
        // frame offset is negative for args & return slot
        let mut frame_offset = -(ty_sub.args_size() + 1);
        let return_expr = ResolvedExpr {
            block: Block::new(frame_offset, builder.return_type.size()),
            ty: builder.return_type,
        };

        let ty = Ty::sub(ty_sub);
        let sub_index = self.memory.sub();
        self.module_scope
            .insert(builder.name, ModuleRecord { ty, sub_index });

        let mut stmt_compiler =
            StmtCompiler::from_module(&mut self.memory, &mut self.module_scope, return_expr);

        for (key, ty) in builder.params {
            frame_offset += ty.size();
            stmt_compiler.insert_scope_record(key, ScopeRecord { frame_offset, ty });
        }

        debug_assert!(frame_offset == -1);
        stmt_compiler
    }
    pub fn done(self) -> Vec<IR> {
        self.memory.done()
    }
}

// Stmt side

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
            match frame.scope.get(key) {
                Some(rec) => return Some(rec),
                None => {}
            };
        }
        None
    }
    fn insert_scope_record(&mut self, key: String, value: ScopeRecord) {
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

impl<'a, 'b> StmtCompiler<'a, 'b> {
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

impl<'a, 'b> StmtCompiler<'a, 'b> {
    pub fn return_sub(&mut self, expr: Option<Expr>) -> Compile<()> {
        match expr {
            Some(expr) => {
                self.check_return(&expr.ty)?;
                expr.resolve(
                    &mut self.memory,
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

impl<'a, 'b> StmtCompiler<'a, 'b> {
    pub fn begin_match(&mut self, expr: Expr) -> Compile<MatchBuilder> {
        let res = expr.to_stack(&mut self.memory);
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

impl<'a, 'b> StmtCompiler<'a, 'b> {
    pub fn op_begin(&mut self, left: Expr) -> OpExpr {
        OpExpr {
            op_stack: Vec::new(),
            operands: vec![left],
        }
    }
    pub fn op_next(&mut self, op_expr: &mut OpExpr, op: Op, expr: Expr) -> Compile<()> {
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
        op_expr.operands.push(expr);
        Ok(())
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
        let result = right.op_rhs(&mut self.memory, out_ty, op, left);
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
struct ScopeRecord {
    frame_offset: Word,
    ty: Ty,
}

impl<'a, 'b> StmtCompiler<'a, 'b> {
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
        expr.resolve(&mut self.memory, target)
    }
    pub fn resolve_stack(&mut self, expr: Expr) -> ResolvedExpr {
        expr.to_stack(&mut self.memory)
    }
    // get pointer to first item from array
    pub fn array_ptr(&mut self, array: Expr, item_ty: &Ty) -> Compile<Expr> {
        array
            .add_ref(&mut self.memory, ExprTarget::Stack)?
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
        offset_ptr.deref(&mut self.memory, ExprTarget::Stack)
    }
    pub fn add_ref_expr(&mut self, expr: Expr) -> Compile<Expr> {
        expr.add_ref(&mut self.memory, ExprTarget::Stack)
    }
    pub fn deref_expr(&mut self, expr: Expr) -> Compile<Expr> {
        expr.deref(&mut self.memory, ExprTarget::Stack)
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

impl<'a, 'b> StmtCompiler<'a, 'b> {
    pub fn begin_compact(&mut self) -> Word {
        self.memory.current_frame_offset
    }
    pub fn end_compact(&mut self, prev_frame_offset: Word, res: ResolvedExpr) {
        self.memory.compact(res.block, prev_frame_offset)
    }
}
