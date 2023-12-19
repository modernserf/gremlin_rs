use std::{collections::HashMap, rc::Rc};

use super::vm_68k::{Asm, Cond};
use crate::v2::memory5::Memory;

type Compile<T> = Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    ExpectedType { expected: Ty, received: Ty },
    InvalidCallArgs,
    UnknownIdent(String),
    UnknownTypeIdent(String),
    InvalidLValue,
    InvalidRef,
    InvalidDeref,
    InvalidRecord,
    UnknownField(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub ref_level: usize,
    kind: TyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TyKind {
    Void,
    Int,
    Bool,
    String,
    Record(Rc<RecordTy>),
}

impl TyKind {
    fn size(&self) -> usize {
        match self {
            Self::Void => 0,
            Self::Bool => 1,
            Self::Int => 4,
            Self::String => 8,
            Self::Record(rec) => rec.size,
        }
    }
}

impl Ty {
    pub fn void() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::Void,
        }
    }
    pub fn int() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::Int,
        }
    }
    pub fn bool() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::Bool,
        }
    }
    pub fn string() -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::String,
        }
    }
    fn record(record: RecordTy) -> Self {
        Self {
            ref_level: 0,
            kind: TyKind::Record(Rc::new(record)),
        }
    }
    fn get_record(&self) -> Compile<Rc<RecordTy>> {
        match &self.kind {
            TyKind::Record(rec) => Ok(rec.clone()),
            _ => Err(CompileError::InvalidRecord),
        }
    }
    pub fn base_size(&self) -> usize {
        self.kind.size()
    }
    fn size(&self) -> usize {
        if self.ref_level > 0 {
            4
        } else {
            self.kind.size()
        }
    }
    fn pointer(mut self) -> Self {
        self.ref_level += 1;
        self
    }
    fn deref(mut self) -> Compile<Self> {
        if self.ref_level == 0 {
            return Err(CompileError::InvalidDeref);
        }
        self.ref_level -= 1;
        Ok(self)
    }
    fn expected(&self, expected: &Ty) -> Compile<()> {
        if self == expected {
            Ok(())
        } else {
            Err(CompileError::ExpectedType {
                expected: expected.clone(),
                received: self.clone(),
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RecordTy {
    size: usize,
    fields: HashMap<String, RecordTyField>,
}

impl RecordTy {
    fn new(rows: Vec<(String, Ty)>) -> Self {
        let mut fields = HashMap::new();
        let mut offset = 0;
        for (name, ty) in rows {
            let size = ty.size();
            if size == 1 {
                todo!("alignment of byte fields")
            }
            fields.insert(name, RecordTyField { offset, ty });
            offset += size
        }
        Self {
            fields,
            size: offset,
        }
    }
    fn get_field(&self, field_name: &str) -> Compile<RecordTyField> {
        self.fields
            .get(field_name)
            .cloned()
            .ok_or_else(|| CompileError::UnknownField(field_name.to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RecordTyField {
    offset: usize,
    ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Ident(String),
    Int(i32),
    Bool(bool),
    String(String),
    Call(Box<CallExpr>),
    Add(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    Pointer(Box<Expr>),
    Deref(Box<Expr>),
    Record(String, Vec<(String, Expr)>),
    Field(Box<Expr>, String),
}

impl Expr {
    fn pointer(self) -> Expr {
        Expr::Pointer(Box::new(self))
    }
    fn deref(self) -> Expr {
        Expr::Deref(Box::new(self))
    }
    fn field(self, field: &str) -> Expr {
        Expr::Field(Box::new(self), field.to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    target: Expr,
    args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyExpr {
    Ident(String),
}

#[derive(Default, Debug)]
struct Scope {
    frames: Vec<ScopeFrame>,
}

impl Scope {
    fn begin(&mut self) {
        self.frames.push(ScopeFrame::root())
    }
    fn end(&mut self) {
        self.frames.pop().unwrap();
    }
    fn insert(&mut self, name: String, id: usize, ty: Ty, can_ref: bool) {
        let top = self.top_mut();
        top.scope.insert(name, ScopeRecord { id, ty, can_ref });
    }
    fn insert_ty(&mut self, name: String, ty: Ty) {
        let top = self.top_mut();
        top.ty_scope.insert(name, TyRecord { ty });
    }
    fn get(&mut self, name: &str) -> Compile<ScopeRecord> {
        for frame in self.frames.iter().rev() {
            if let Some(rec) = frame.scope.get(name) {
                return Ok(rec.clone());
            }
        }
        Err(CompileError::UnknownIdent(name.to_string()))
    }
    fn get_ty(&mut self, name: &str) -> Compile<TyRecord> {
        for frame in self.frames.iter().rev() {
            if let Some(rec) = frame.ty_scope.get(name) {
                return Ok(rec.clone());
            }
        }
        Err(CompileError::UnknownTypeIdent(name.to_string()))
    }
    fn top_mut(&mut self) -> &mut ScopeFrame {
        let idx = self.frames.len() - 1;
        &mut self.frames[idx]
    }
}

#[derive(Default, Debug)]
struct ScopeFrame {
    scope: HashMap<String, ScopeRecord>,
    ty_scope: HashMap<String, TyRecord>,
}

impl ScopeFrame {
    fn root() -> Self {
        let mut ty_scope = HashMap::new();
        ty_scope.insert("Void".to_string(), TyRecord { ty: Ty::void() });
        ty_scope.insert("Int".to_string(), TyRecord { ty: Ty::int() });
        ty_scope.insert("Bool".to_string(), TyRecord { ty: Ty::bool() });
        ty_scope.insert("String".to_string(), TyRecord { ty: Ty::string() });
        Self {
            scope: HashMap::new(),
            ty_scope,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ScopeRecord {
    id: usize,
    ty: Ty,
    can_ref: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TyRecord {
    ty: Ty,
}

pub struct IfBuilder {
    if_idx: usize,
}

impl IfBuilder {
    fn new(compiler: &mut Compiler, cond: Cond) -> Self {
        let if_idx = compiler.memory.if_begin(cond);
        compiler.scope.begin();
        Self { if_idx }
    }
    pub fn else_(&mut self, compiler: &mut Compiler) {
        compiler.scope.end();
        let prev_if_idx = self.if_idx;
        self.if_idx = compiler.memory.if_else(prev_if_idx);
        compiler.scope.begin();
    }
    pub fn end(self, compiler: &mut Compiler) {
        compiler.scope.end();
        compiler.memory.if_end(self.if_idx);
    }
}

pub struct WhileBuilder {
    loop_idx: usize,
    expr: Expr,
}

impl WhileBuilder {
    fn new(compiler: &mut Compiler, expr: Expr) -> Self {
        let loop_idx = compiler.memory.loop_begin();
        compiler.scope.begin();
        Self { loop_idx, expr }
    }
    pub fn end(self, compiler: &mut Compiler) -> Compile<()> {
        compiler.scope.end();
        compiler.memory.loop_check(self.loop_idx);
        let cond = compiler.cond(self.expr)?;
        compiler.memory.loop_end(cond, self.loop_idx);
        Ok(())
    }
}

#[derive(Default, Debug)]
pub struct Compiler {
    scope: Scope,
    memory: Memory,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn script() -> Self {
        let mut c = Self::new();
        c.scope.begin();
        c
    }
    pub fn end(mut self) -> Asm {
        self.scope.end();
        self.memory.end()
    }
    fn ty_expr(&mut self, tyexpr: TyExpr) -> Compile<Ty> {
        match tyexpr {
            TyExpr::Ident(name) => {
                let rec = self.scope.get_ty(&name)?;
                Ok(rec.ty)
            }
        }
    }

    pub fn let_(&mut self, name: &str, expr: Expr) -> Compile<()> {
        let ty = self.expr(expr)?;
        let id = self.memory.local();
        self.scope.insert(name.to_string(), id, ty, false);
        Ok(())
    }
    pub fn let_ref(&mut self, name: &str, expr: Expr) -> Compile<()> {
        let ty = self.expr(expr)?;
        let id = self.memory.local_stack();
        self.scope.insert(name.to_string(), id, ty, true);
        Ok(())
    }
    pub fn assign(&mut self, target: Expr, expr: Expr) -> Compile<()> {
        let expected = self.lvalue(target)?;
        let ty = self.expr(expr)?;
        ty.expected(&expected.ty)?;
        self.memory.assign();
        Ok(())
    }
    pub fn add_assign(&mut self, target: Expr, expr: Expr) -> Compile<()> {
        let expected = self.lvalue(target)?;
        let ty = self.expr(expr)?;
        ty.expected(&expected.ty)?;
        self.memory.add_assign();
        Ok(())
    }
    pub fn log(&mut self, str: &str) -> Compile<()> {
        self.memory.log(str);
        Ok(())
    }
    pub fn record(&mut self, name: &str, fields: Vec<(&str, TyExpr)>) -> Compile<()> {
        let mut rows = Vec::new();
        for (name, ty_expr) in fields {
            let ty = self.ty_expr(ty_expr)?;
            rows.push((name.to_string(), ty));
        }
        let ty = Ty::record(RecordTy::new(rows));
        self.scope.insert_ty(name.to_string(), ty);
        Ok(())
    }
    pub fn if_(&mut self, expr: Expr) -> Compile<IfBuilder> {
        let cond = self.cond(expr)?;
        Ok(IfBuilder::new(self, cond))
    }
    pub fn while_(&mut self, expr: Expr) -> WhileBuilder {
        WhileBuilder::new(self, expr)
    }
    fn expr(&mut self, expr: Expr) -> Compile<Ty> {
        match expr {
            Expr::Int(value) => {
                self.memory.push_i32(value);
                Ok(Ty::int())
            }
            Expr::Bool(value) => {
                self.memory.push_bool(value);
                Ok(Ty::bool())
            }
            Expr::String(name) => {
                self.memory.push_string(name);
                Ok(Ty::string())
            }
            Expr::Ident(name) => {
                let rec = self.scope.get(&name)?;
                self.memory.push_ident(rec.id);
                Ok(rec.ty)
            }
            Expr::Call(call) => self.call_expr(*call),
            Expr::Add(left, right) => {
                self.expr(*left)?.expected(&Ty::int())?;
                self.expr(*right)?.expected(&Ty::int())?;
                self.memory.add();
                Ok(Ty::int())
            }
            Expr::Pointer(expr) => {
                let lvalue = self.lvalue(*expr)?;
                if !lvalue.can_ref {
                    return Err(CompileError::InvalidRef);
                }
                self.memory.pointer();
                let ty = lvalue.ty.pointer();
                Ok(ty)
            }
            Expr::Deref(expr) => {
                let ty = self.expr(*expr)?.deref()?;
                self.memory.deref();
                Ok(ty)
            }
            Expr::Record(name, fields) => {
                let t = self.scope.get_ty(&name)?;
                let rec = t.ty.get_record()?;
                self.memory.push_struct(rec.size);
                for (field_name, expr) in fields {
                    let field = rec.get_field(&field_name)?;
                    let ty = self.expr(expr)?;
                    ty.expected(&field.ty)?;
                    self.memory.assign_field(field.offset);
                }
                Ok(t.ty)
            }
            Expr::Field(expr, field_name) => {
                let expr_ty = self.expr(*expr)?;
                let rec = expr_ty.get_record()?;
                let field = rec.get_field(&field_name)?;
                self.memory.field(field.offset, &field.ty);
                Ok(field.ty)
            }
            Expr::Equal(_, _) | Expr::Less(_, _) => {
                let cond = self.cond(expr)?;
                self.memory.set_cond(cond);
                Ok(Ty::bool())
            }
        }
    }
    fn call_expr(&mut self, call: CallExpr) -> Compile<Ty> {
        match &call.target {
            Expr::Ident(str) if str == "assert_eq" => {
                self.call_args(call.args, &vec![Ty::int(), Ty::int()])?;
                self.memory.assert_eq();
                Ok(Ty::void())
            }
            Expr::Ident(str) if str == "assert_eq_u8" => {
                self.call_args(call.args, &vec![Ty::bool(), Ty::bool()])?;
                self.memory.assert_eq_u8();
                Ok(Ty::void())
            }
            Expr::Ident(str) if str == "println" => {
                self.call_args(call.args, &vec![Ty::string()])?;
                self.memory.println();
                Ok(Ty::void())
            }
            _ => unimplemented!(),
        }
    }
    fn call_args(&mut self, args: Vec<Expr>, expected: &[Ty]) -> Compile<()> {
        if args.len() != expected.len() {
            return Err(CompileError::InvalidCallArgs);
        }
        for (i, arg) in args.into_iter().enumerate() {
            let ty = self.expr(arg)?;
            ty.expected(&expected[i])?;
        }
        Ok(())
    }
    fn lvalue(&mut self, expr: Expr) -> Compile<ScopeRecord> {
        match expr {
            Expr::Ident(name) => {
                let rec = self.scope.get(&name)?;
                self.memory.push_ident(rec.id);
                Ok(rec)
            }
            Expr::Deref(expr) => {
                let mut rec = self.lvalue(*expr)?;
                rec.ty = rec.ty.deref()?;
                self.memory.deref();
                Ok(rec)
            }
            _ => Err(CompileError::InvalidLValue),
        }
    }
    fn cond(&mut self, expr: Expr) -> Compile<Cond> {
        match expr {
            Expr::Equal(l, r) => {
                let l = self.expr(*l)?;
                let r = self.expr(*r)?;
                r.expected(&l)?;
                self.memory.cmp2();
                Ok(Cond::Equal)
            }
            Expr::Less(l, r) => {
                self.expr(*l)?.expected(&Ty::int())?;
                self.expr(*r)?.expected(&Ty::int())?;
                self.memory.cmp2();
                Ok(Cond::Less)
            }
            expr => {
                self.expr(expr)?.expected(&Ty::bool())?;
                self.memory.test_bool();
                Ok(Cond::Equal)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::v2::vm_68k::VM;

    fn ty_ident(name: &str) -> TyExpr {
        TyExpr::Ident(name.to_string())
    }

    fn int(value: i32) -> Expr {
        Expr::Int(value)
    }
    fn string(value: &str) -> Expr {
        Expr::String(value.to_string())
    }
    fn ident(name: &str) -> Expr {
        Expr::Ident(name.to_string())
    }
    fn call(target: Expr, args: Vec<Expr>) -> Expr {
        Expr::Call(Box::new(CallExpr { target, args }))
    }
    fn add(left: Expr, right: Expr) -> Expr {
        Expr::Add(Box::new(left), Box::new(right))
    }
    fn equal(left: Expr, right: Expr) -> Expr {
        Expr::Equal(Box::new(left), Box::new(right))
    }
    fn less(left: Expr, right: Expr) -> Expr {
        Expr::Less(Box::new(left), Box::new(right))
    }
    fn record(name: &str, fields: Vec<(&str, Expr)>) -> Expr {
        Expr::Record(
            name.to_string(),
            fields
                .into_iter()
                .map(|(f, e)| (f.to_string(), e))
                .collect(),
        )
    }

    fn assert_eq(left: Expr, right: Expr) -> Expr {
        call(ident("assert_eq"), vec![left, right])
    }

    fn run_vm(asm: Asm) -> VM {
        let mut vm = VM::new(256);
        let init_sp = 120;
        let init_pc = 128;
        let init_memory = vec![0, 0, 0, init_sp, 0, 0, 0, init_pc];
        vm.load_memory(0, &init_memory);
        vm.reset();

        vm.load_memory(init_pc as usize, &asm.out);
        vm.run();
        vm
    }

    #[test]
    fn smoke_test() -> Compile<()> {
        let mut c = Compiler::script();
        c.log("log")?;
        //
        c.let_("foo", int(123))?;
        c.expr(assert_eq(ident("foo"), int(123)))?;
        //
        c.let_("bar", string("Hello, world!"))?;
        c.expr(call(ident("println"), vec![ident("bar")]))?;
        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn add_expr() -> Compile<()> {
        let mut c = Compiler::script();
        c.let_("a", int(10))?;
        c.let_("b", int(20))?;
        c.let_("c", add(ident("a"), add(int(30), ident("b"))))?;

        c.expr(assert_eq(ident("c"), int(60)))?;
        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn add_type_err() {
        let mut c = Compiler::script();
        assert_eq!(
            c.expr(add(int(30), string("hello"))),
            Err(CompileError::ExpectedType {
                expected: Ty::int(),
                received: Ty::string()
            })
        );
    }

    #[test]
    fn invalid_call_args() {
        let mut c = Compiler::script();
        assert_eq!(
            c.expr(call(ident("assert_eq"), vec![])),
            Err(CompileError::InvalidCallArgs)
        );
    }

    #[test]
    fn unknown_ident() {
        let mut c = Compiler::script();
        assert_eq!(
            c.expr(ident("foo")),
            Err(CompileError::UnknownIdent("foo".to_string()))
        )
    }

    #[test]
    fn assign_() -> Compile<()> {
        let mut c = Compiler::script();

        c.let_("a", int(10))?;
        c.assign(ident("a"), int(20))?;

        c.expr(assert_eq(ident("a"), int(20)))?;
        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn invalid_lvalue() {
        let mut c = Compiler::script();
        assert_eq!(
            c.assign(int(123), int(456)),
            Err(CompileError::InvalidLValue)
        );
    }

    #[test]
    fn pointer() -> Compile<()> {
        let mut c = Compiler::script();
        c.let_ref("a", int(123))?;
        c.let_("a_ptr", ident("a").pointer())?;
        c.let_ref("a_stack_ptr", ident("a").pointer())?;

        c.assign(ident("a"), int(456))?;
        c.expr(assert_eq(ident("a_ptr").deref(), int(456)))?;
        c.expr(assert_eq(ident("a_stack_ptr").deref(), int(456)))?;

        c.assign(ident("a_ptr").deref(), int(789))?;
        c.expr(assert_eq(ident("a"), int(789)))?;
        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn record_fields() -> Compile<()> {
        let mut c = Compiler::script();
        c.record(
            "Point",
            vec![("x", ty_ident("Int")), ("y", ty_ident("Int"))],
        )?;
        c.let_("p", record("Point", vec![("x", int(123)), ("y", int(456))]))?;
        c.expr(assert_eq(ident("p").field("x"), int(123)))?;
        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn if_() -> Compile<()> {
        let mut c = Compiler::script();
        c.let_("cmp", int(2))?;
        c.let_("res", int(0))?;
        let b = c.if_(equal(ident("cmp"), int(2)))?;
        c.assign(ident("res"), int(10))?;
        b.end(&mut c);
        c.expr(assert_eq(ident("res"), int(10)))?;

        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn if_else() -> Compile<()> {
        let mut c = Compiler::script();
        c.let_("cmp", int(3))?;
        c.let_("res", int(0))?;
        let mut b = c.if_(equal(ident("cmp"), int(2)))?;
        c.assign(ident("res"), int(10))?;
        b.else_(&mut c);
        c.assign(ident("res"), int(20))?;
        b.end(&mut c);
        c.expr(assert_eq(ident("res"), int(20)))?;

        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn while_loop() -> Compile<()> {
        let mut c = Compiler::script();
        c.let_("i", int(0))?;
        c.let_("sum", int(0))?;
        let b = c.while_(less(ident("i"), int(5)));
        c.add_assign(ident("sum"), ident("i"))?;
        c.add_assign(ident("i"), int(1))?;
        b.end(&mut c)?;
        c.expr(assert_eq(ident("sum"), int(10)))?;

        run_vm(c.end());
        Ok(())
    }

    #[test]
    fn bools() -> Compile<()> {
        let mut c = Compiler::script();
        c.let_("a", Expr::Bool(true))?;
        c.let_("b", less(int(1), int(2)))?;
        let b = c.if_(ident("b"))?;
        c.assign(ident("a"), Expr::Bool(false))?;
        b.end(&mut c);

        c.expr(call(
            ident("assert_eq_u8"),
            vec![ident("a"), Expr::Bool(false)],
        ))?;

        Ok(())
    }
}
