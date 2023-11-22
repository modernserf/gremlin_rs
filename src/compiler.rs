use crate::expr::*;
use crate::memory::*;
use crate::op::*;
use crate::record::*;
use crate::runtime::*;
use crate::subroutine::*;
use crate::ty::*;
use crate::{Compile, CompileError::*};
use std::collections::HashMap;
enum ModuleOrSub {
    Module,
    Sub(SubContext),
}

impl ModuleOrSub {
    fn sub(&mut self) -> &mut SubContext {
        match self {
            Self::Module => unimplemented!(),
            Self::Sub(ctx) => ctx,
        }
    }
}
pub struct Compiler {
    memory: Memory,
    ty_scope: TyScope,
    module_scope: HashMap<String, ModuleRecord>,
    module_or_sub: ModuleOrSub,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module_scope: HashMap::new(),
            ty_scope: TyScope::new(),
            memory: Memory::new(),
            module_or_sub: ModuleOrSub::Sub(SubContext::new()),
        }
    }
    pub fn done(self) -> Vec<IR> {
        self.memory.done()
    }
    pub fn resolve(self) -> Compile<CompileResult> {
        let entry_point = self
            .module_scope
            .get("main")
            .ok_or(Expected("main"))?
            .sub_index;
        Ok(self.memory.done_program(entry_point))
    }
    pub fn new_record_ty(&mut self) -> TyRecord {
        TyRecord::new(self.ty_scope.new_type_id())
    }
    pub fn new_oneof_ty(&mut self) -> TyOneOf {
        TyOneOf::new(self.ty_scope.new_type_id())
    }
    pub fn get_ty(&mut self, name: &str) -> Compile<Ty> {
        self.ty_scope.get(name)
    }
    pub fn assign_ty(&mut self, name: String, ty: Ty) {
        self.ty_scope.assign(name, ty);
    }
    pub fn get_expr(&mut self, name: &str) -> Compile<Expr> {
        if let Some(record) = self.module_or_sub.sub().get_local(name) {
            let block = Block::local(record.frame_offset, record.ty.size());
            return Ok(Expr::lvalue(record.ty.clone(), block));
        }
        if let Some(record) = self.module_scope.get(name) {
            return Ok(Expr::sub(record.ty.clone(), record.sub_index));
        }

        Err(UnknownIdentifier(name.to_string()))
    }
    pub fn assign_expr(&mut self, name: String, expr: ResolvedExpr) {
        self.memory.compact(expr.block);
        self.memory.locals_offset = self.memory.current_frame_offset;
        self.module_or_sub.sub().insert(
            name,
            ScopeRecord {
                frame_offset: self.memory.locals_offset,
                ty: expr.ty,
            },
        );
    }
    pub fn store_sub(&mut self, key: String, ty: Ty) {
        let sub_index = self.memory.sub();
        self.module_scope
            .insert(key, ModuleRecord { ty, sub_index });
    }
    pub fn sub_param(&mut self, name: String, frame_offset: Word, ty: Ty) {
        self.module_or_sub
            .sub()
            .insert(name, ScopeRecord { frame_offset, ty });
    }
    pub fn push_sub_scope(&mut self, builder: SubBuilder) {
        builder.push_sub_scope(self);
    }
    pub fn push_scope(&mut self) {
        self.module_or_sub.sub().push_scope(&self.memory);
    }
    pub fn pop_scope(&mut self) {
        let last_frame = self.module_or_sub.sub().pop_scope();
        self.memory.end_scope(last_frame.scope_index);
    }
    pub fn allocate(&mut self, ty: &Ty) -> Block {
        self.memory.allocate(ty.size())
    }
    pub fn resolve_expr(&mut self, expr: Expr, target: ExprTarget) -> ResolvedExpr {
        expr.resolve(&mut self.memory, target)
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
        let offset = OpExpr::simple(
            &mut self.memory,
            ExprTarget::Stack,
            Op::Mul,
            idx,
            Expr::constant(Ty::int(), item_ty.size()),
        )?;
        let ptr_ty = array_ptr.ty.clone();
        let offset_ptr = OpExpr::simple(
            &mut self.memory,
            ExprTarget::Stack,
            Op::Add,
            array_ptr.cast_ty(Ty::int())?,
            offset,
        )?
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
        expr.bitset_field(&field_name, &mut self.memory, ExprTarget::Stack)
    }
    pub fn negate_expr(&mut self, expr: Expr) -> Compile<Expr> {
        OpExpr::simple(
            &mut self.memory,
            ExprTarget::Stack,
            Op::Sub,
            Expr::constant(Ty::int(), 0),
            expr,
        )
    }
    pub fn op_next(&mut self, op_expr: &mut OpExpr, op: Op, expr: Expr) -> Compile<()> {
        op_expr.next(&mut self.memory, op, expr)
    }
    pub fn op_unwind(&mut self, mut op_expr: OpExpr) -> Compile<Expr> {
        op_expr.unwind(&mut self.memory)
    }
    pub fn begin_cond(&mut self, cond: Expr) -> Compile<CondIndex> {
        Ty::bool().check(&cond.ty)?;
        Ok(self.memory.begin_cond(cond, ExprTarget::Stack))
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
    pub fn return_sub(&mut self) {
        self.memory.return_sub()
    }
    pub fn compact(&mut self, block: Block) {
        self.memory.compact(block)
    }
    pub fn begin_match(&mut self, target: Expr) -> Compile<MatchBuilder> {
        MatchBuilder::new(target, &mut self.memory, ExprTarget::Stack)
    }
    pub fn begin_match_case(
        &mut self,
        builder: &mut MatchBuilder,
        tag: &str,
    ) -> Compile<MatchCaseBuilder> {
        let case = builder.add_case(&tag, &mut self.memory)?;
        Ok(case)
    }
    pub fn add_case_binding(&mut self, case: &mut MatchCaseBuilder, name: String) -> Compile<()> {
        let field = case.record.get(&name, Some(case.case_id))?;
        let block = case.parent_block.record_field(field);

        self.module_or_sub.sub().insert(
            name,
            ScopeRecord {
                frame_offset: block.frame_offset().expect("frame offset"),
                ty: field.ty.clone(),
            },
        );
        Ok(())
    }
    pub fn end_match_case(&mut self, builder: &mut MatchBuilder) {
        builder.end_case(&mut self.memory);
    }
    pub fn end_match(&mut self, builder: MatchBuilder) {
        builder.resolve(&mut self.memory);
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

struct ModuleRecord {
    ty: Ty,
    sub_index: SubIndex,
}

struct SubContext {
    scope: Vec<ScopeFrame>,
}

impl SubContext {
    fn new() -> Self {
        Self {
            scope: vec![ScopeFrame::new(ScopeIndex::root())],
        }
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
    fn insert(&mut self, key: String, value: ScopeRecord) {
        let len = self.scope.len();
        self.scope[len - 1].scope.insert(key, value);
    }
    pub fn push_scope(&mut self, memory: &Memory) {
        self.scope.push(ScopeFrame::new(memory.begin_scope()));
    }
    pub fn pop_scope(&mut self) -> ScopeFrame {
        self.scope.pop().expect("scope frame")
    }
}

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
