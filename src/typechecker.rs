use crate::ast;
use crate::ir::Word;
use crate::typed_ast::*;
use std::collections::HashMap;

type TypeRes<T> = Result<T, TypeError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {}

struct TypeScope {
    scope: HashMap<String, Ty>,
    next_id: usize,
}

impl TypeScope {
    pub fn new() -> Self {
        Self {
            scope: HashMap::from_iter(vec![
                ("int".to_string(), Ty::int()),
                ("bool".to_string(), Ty::bool()),
                ("long".to_string(), Ty::long()),
            ]),
            next_id: 128,
        }
    }
    pub fn new_id(&mut self) -> usize {
        let prev = self.next_id;
        self.next_id += 1;
        prev
    }
    pub fn get(&mut self, key: &str) -> Option<&Ty> {
        self.scope.get(key)
    }
    pub fn add(&mut self, key: String, ty: Ty) {
        self.scope.insert(key, ty);
    }
}

mod value_scope {
    use crate::typed_ast::{BindId, Ty};
    use std::collections::HashMap;

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct ScopeRecord {
        pub id: BindId,
        pub ty: Ty,
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    pub struct Update {
        pub id: BindId,
    }

    pub struct ValueScope {
        scope: HashMap<String, ScopeRecord>,
        next_id: BindId,
    }

    impl ValueScope {
        pub fn new() -> Self {
            Self {
                scope: HashMap::new(),
                next_id: 0,
            }
        }
        pub fn get(&self, key: &str) -> Option<&ScopeRecord> {
            self.scope.get(key)
        }
        pub fn add(&mut self, key: String, ty: Ty) -> Update {
            let record = ScopeRecord {
                id: self.next_id,
                ty,
            };
            let update = Update { id: self.next_id };
            self.next_id += 1;
            self.scope.insert(key, record);
            update
        }
    }
}

struct BinOpChecker {
    table: Vec<(ast::BinOpKind, Ty, Ty, Ty)>,
}

impl BinOpChecker {
    fn new() -> Self {
        use ast::BinOpKind::*;
        Self {
            table: vec![
                (Add, Ty::int(), Ty::int(), Ty::int()),
                (Mult, Ty::int(), Ty::int(), Ty::int()),
                (And, Ty::bool(), Ty::bool(), Ty::bool()),
                (Or, Ty::bool(), Ty::bool(), Ty::bool()),
            ],
        }
    }
    fn check(&self, op: ast::BinOpKind, left: &Ty, right: &Ty) -> Option<Ty> {
        for row in &self.table {
            if (row.0, &row.1, &row.2) == (op, left, right) {
                return Some(row.3.to_owned());
            }
        }
        return None;
    }
}

use self::value_scope::*;
pub struct TypeChecker {
    type_scope: TypeScope,
    value_scope: ValueScope,
    bin_op: BinOpChecker,
}

impl TypeChecker {
    pub fn check(body: &[ast::Stmt]) -> TypeRes<Vec<Stmt>> {
        Self::new().body(body)
    }
    fn new() -> Self {
        Self {
            type_scope: TypeScope::new(),
            value_scope: ValueScope::new(),
            bin_op: BinOpChecker::new(),
        }
    }
    fn body(&mut self, body: &[ast::Stmt]) -> TypeRes<Vec<Stmt>> {
        let mut out = Vec::new();
        for stmt in body {
            match self.stmt(stmt)? {
                Some(val) => out.push(val),
                None => {}
            };
        }
        Ok(out)
    }
    fn stmt(&mut self, stmt: &ast::Stmt) -> TypeRes<Option<Stmt>> {
        match &stmt.kind {
            ast::StmtKind::Let(x) => {
                let expr = self.expr(&x.expr)?;
                if let Some(type_expr) = &x.ty {
                    let ty = self.type_expr(type_expr)?;
                    Self::check_expr_type(&expr, &ty)?;
                }
                let res = self.add_binding(&x.binding, &expr.ty)?;
                Ok(Some(Stmt::let_stmt(res.id, expr)))
            }
            ast::StmtKind::Assign(x) => {
                let expr = self.expr(&x.expr)?;
                let res = self.update_binding(&x.target)?;
                Self::check_expr_type(&expr, &res.ty)?;
                Ok(Some(Stmt::assign(res.id, expr)))
            }
            ast::StmtKind::Expr(x) => {
                let expr = self.expr(&x)?;
                Ok(Some(Stmt::expr(expr)))
            }
            ast::StmtKind::Noop => Ok(None),
            ast::StmtKind::TypeDef(x) => {
                let ty = self.type_expr(&x.ty)?;
                self.type_scope.add(x.identifier.to_string(), ty);
                Ok(None)
            }
        }
    }
    fn expr(&mut self, expr: &ast::Expr) -> TypeRes<Expr> {
        match &expr.kind {
            ast::ExprKind::As(x) => {
                let from_expr = self.expr(&x.expr)?;
                let cast_to = self.type_expr(&x.ty)?;
                from_expr
                    .cast(cast_to)
                    .ok_or_else(|| panic!("todo invalid cast"))
            }
            ast::ExprKind::False => Ok(Expr::constant(0, Ty::bool())),
            ast::ExprKind::True => Ok(Expr::constant(1, Ty::bool())),
            ast::ExprKind::Int(x) => {
                if x.value > (u32::MAX as u128) {
                    panic!("todo int size error")
                }
                Ok(Expr::constant(x.value as Word, Ty::int()))
            }
            ast::ExprKind::Long(x) => {
                if x.value > (u64::MAX as u128) {
                    panic!("todo long size error")
                }
                let hi = (x.value >> 32) as Word;
                let lo = x.value as Word;
                Ok(Expr::long(hi, lo, Ty::long()))
            }
            ast::ExprKind::Ident(x) => {
                let res = self
                    .value_scope
                    .get(&x.value)
                    .ok_or_else(|| panic!("todo unknown key"))?;
                Ok(Expr::ident(res.id, res.ty.clone()))
            }
            ast::ExprKind::UnaryOp(x) => {
                let value = self.expr(&x.expr)?;
                match &x.operator {
                    ast::UnOpKind::Ref => {
                        Expr::add_ref(value).ok_or_else(|| panic!("todo ref error"))
                    }
                    ast::UnOpKind::Deref => {
                        Expr::deref(value).ok_or_else(|| panic!("todo deref error"))
                    }
                    ast::UnOpKind::Not => {
                        Self::check_expr_type(&value, &Ty::bool())?;
                        Ok(Expr::not(value))
                    }
                }
            }
            ast::ExprKind::BinaryOp(x) => {
                let left = self.expr(&x.left)?;
                let right = self.expr(&x.right)?;
                let ty = self
                    .bin_op
                    .check(x.operator, &left.ty, &right.ty)
                    .ok_or_else(|| panic!("todo type error"))?;
                Ok(Expr::bin_op(x.operator, left, right, ty))
            }
            ast::ExprKind::Struct(_) => unimplemented!(),
            ast::ExprKind::Field(_) => unimplemented!(),
        }
    }
    fn add_binding(&mut self, binding: &ast::Bind, ty: &Ty) -> TypeRes<Update> {
        match &binding.kind {
            ast::BindKind::Ident(x) => Ok(self.value_scope.add(x.value.to_string(), ty.clone())),
        }
    }
    fn update_binding(&mut self, target: &ast::Expr) -> TypeRes<&ScopeRecord> {
        match &target.kind {
            ast::ExprKind::Ident(x) => self
                .value_scope
                .get(&x.value)
                .ok_or_else(|| panic!("unknown binding")),
            _ => panic!("todo invalid lhs"),
        }
    }
    fn type_expr(&mut self, type_expr: &ast::TyExpr) -> TypeRes<Ty> {
        match &type_expr.kind {
            ast::TyExprKind::Identifier(x) => self
                .type_scope
                .get(&x.value)
                .map(|t| t.with_ref_level(type_expr.ref_level))
                .ok_or_else(|| panic!("unknown type")),
            ast::TyExprKind::Struct(_) => {
                let id = self.type_scope.new_id();
                unimplemented!()
            }
        }
    }
    fn check_expr_type(expr: &Expr, ty: &Ty) -> TypeRes<()> {
        if &expr.ty == ty {
            return Ok(());
        };
        panic!("todo type error")
    }
}

#[cfg(test)]
mod test {}
