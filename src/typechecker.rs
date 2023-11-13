use crate::ast;
use crate::ir::Word;
use crate::typed_ast::*;
use std::collections::{HashMap, HashSet};

type TypeRes<T> = Result<T, TypeError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
    pub kind: TypeErrorKind,
}

impl TypeError {
    fn invalid_cast() -> Self {
        Self {
            kind: TypeErrorKind::InvalidCast,
        }
    }
    fn invalid_number_literal() -> Self {
        Self {
            kind: TypeErrorKind::NumberLiteralTooLarge,
        }
    }
    fn unknown_identifier() -> Self {
        Self {
            kind: TypeErrorKind::UnknownIdentifier,
        }
    }
    fn unknown_type_identifier() -> Self {
        Self {
            kind: TypeErrorKind::UnknownTypeIdentifier,
        }
    }
    fn invalid_reference() -> Self {
        Self {
            kind: TypeErrorKind::InvalidReference,
        }
    }
    fn invalid_dereference() -> Self {
        Self {
            kind: TypeErrorKind::InvalidDereference,
        }
    }
    fn expected_type(received: &Ty, expected: &Ty) -> Self {
        Self {
            kind: TypeErrorKind::ExpectedType {
                received: received.clone(),
                expected: expected.clone(),
            },
        }
    }
    fn invalid_lvalue() -> Self {
        Self {
            kind: TypeErrorKind::InvalidLValue,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeErrorKind {
    UnknownIdentifier,
    UnknownTypeIdentifier,
    ExpectedType { expected: Ty, received: Ty },
    NumberLiteralTooLarge,
    InvalidCast,
    InvalidReference,
    InvalidDereference,
    InvalidLValue,
}

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
                let res = self.binding(&x.binding, &expr.ty)?;
                Ok(Some(Stmt::let_stmt(res.id, expr)))
            }
            ast::StmtKind::Assign(x) => {
                let expr = self.expr(&x.expr)?;
                let (res, ty) = self.lvalue(&x.target)?;
                Self::check_expr_type(&expr, &ty)?;
                Ok(Some(Stmt::assign(res, expr)))
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
                    .ok_or_else(|| TypeError::invalid_cast())
            }
            ast::ExprKind::False => Ok(Expr::constant(0, Ty::bool())),
            ast::ExprKind::True => Ok(Expr::constant(1, Ty::bool())),
            ast::ExprKind::Int(x) => {
                if x.value > (u32::MAX as u128) {
                    return Err(TypeError::invalid_number_literal());
                }
                Ok(Expr::constant(x.value as Word, Ty::int()))
            }
            ast::ExprKind::Long(x) => {
                if x.value > (u64::MAX as u128) {
                    return Err(TypeError::invalid_number_literal());
                }
                let hi = (x.value >> 32) as Word;
                let lo = x.value as Word;
                Ok(Expr::long(hi, lo, Ty::long()))
            }
            ast::ExprKind::Ident(x) => {
                let res = self
                    .value_scope
                    .get(&x.value)
                    .ok_or_else(|| TypeError::unknown_identifier())?;
                Ok(Expr::ident(res.id, res.ty.clone()))
            }
            ast::ExprKind::UnaryOp(x) => {
                let value = self.expr(&x.expr)?;
                match &x.operator {
                    ast::UnOpKind::Ref => {
                        Expr::add_ref(value).ok_or_else(|| TypeError::invalid_reference())
                    }
                    ast::UnOpKind::Deref => {
                        Expr::deref(value).ok_or_else(|| TypeError::invalid_dereference())
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
                    .ok_or_else(|| TypeError::expected_type(&right.ty, &left.ty))?;
                Ok(Expr::bin_op(x.operator, left, right, ty))
            }
            ast::ExprKind::Struct(x) => {
                let ty = self
                    .type_scope
                    .get(&x.name)
                    .ok_or_else(|| TypeError::unknown_type_identifier())?
                    .clone();
                let expected_fields = ty.fields().ok_or_else(|| panic!("todo expected struct"))?;
                let mut out_fields = Vec::new();
                let mut used_fields = HashSet::new();
                for field in &x.fields {
                    let found = expected_fields
                        .get(&field.key)
                        .ok_or_else(|| todo!("unknown field"))?;
                    if used_fields.contains(&found.offset) {
                        todo!("duplicate field")
                    }
                    used_fields.insert(found.offset);

                    let value = self.expr(&field.value)?;
                    Self::check_expr_type(&value, &found.ty)?;
                    out_fields.push(StructField {
                        offset: found.offset,
                        size: found.ty.size(),
                        expr: value,
                    });
                }
                if used_fields.len() < expected_fields.len() {
                    todo!("missing field")
                }

                Ok(Expr::struc(out_fields, ty))
            }
            ast::ExprKind::Field(x) => {
                let target = self.expr(&x.expr)?;
                let key = match &x.field {
                    ast::FieldKind::Identifier(k) => k,
                };
                let rec = target
                    .ty
                    .fields()
                    .ok_or_else(|| todo!("expected struct"))?
                    .get(key)
                    .ok_or_else(|| {
                        todo!("missing struct field");
                    })?
                    .clone();

                let out = StructField {
                    offset: rec.offset,
                    size: rec.ty.size(),
                    expr: target,
                };

                Ok(Expr::struct_field(out, rec.ty))
            }
        }
    }
    fn binding(&mut self, binding: &ast::Bind, ty: &Ty) -> TypeRes<Update> {
        match &binding.kind {
            ast::BindKind::Ident(x) => Ok(self.value_scope.add(x.value.to_string(), ty.clone())),
        }
    }
    fn lvalue(&mut self, expr: &ast::Expr) -> TypeRes<(LValue, Ty)> {
        let typed_expr = self.expr(expr)?;
        let lvalue = typed_expr
            .as_lvalue()
            .ok_or_else(|| TypeError::invalid_lvalue())?;
        Ok((lvalue, typed_expr.ty))
    }
    fn type_expr(&mut self, type_expr: &ast::TyExpr) -> TypeRes<Ty> {
        match &type_expr.kind {
            ast::TyExprKind::Identifier(x) => self
                .type_scope
                .get(&x.value)
                .map(|t| t.with_ref_level(type_expr.ref_level))
                .ok_or_else(|| TypeError::unknown_type_identifier()),
            ast::TyExprKind::Struct(x) => {
                let id = self.type_scope.new_id();
                let mut fields = Vec::new();
                for field in &x.fields {
                    let field_ty = self.type_expr(&field.ty)?;
                    fields.push((field.key.to_string(), field_ty));
                }
                Ok(Ty::structure(id, fields))
            }
        }
    }
    fn check_expr_type(expr: &Expr, ty: &Ty) -> TypeRes<()> {
        if &expr.ty == ty {
            return Ok(());
        };
        Err(TypeError::expected_type(&expr.ty, &ty))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn check_err(str: &str, expected: TypeError) {
        let tok = Lexer::lex(&str);
        let ast = Parser::parse_body(tok).expect("ast");
        let tc = TypeChecker::check(&ast).expect_err("typecheck");

        assert_eq!(tc, expected);
    }

    #[test]
    fn unknown_identifier() {
        check_err("foo", TypeError::unknown_identifier());
    }

    #[test]
    fn unknown_type_identifier() {
        check_err("let x : foo := 1", TypeError::unknown_type_identifier());
    }

    #[test]
    fn expected_type() {
        check_err(
            "let x : bool := 1",
            TypeError::expected_type(&Ty::int(), &Ty::bool()),
        );
        check_err(
            "5 + true",
            TypeError::expected_type(&Ty::bool(), &Ty::int()),
        );
        check_err("not 3", TypeError::expected_type(&Ty::int(), &Ty::bool()));
    }

    #[test]
    fn invalid_number_literal() {
        check_err("1234567890123455667", TypeError::invalid_number_literal());
        check_err(
            "12345678901234556671234567890123455667l",
            TypeError::invalid_number_literal(),
        );
    }

    #[test]
    fn invalid_cast() {
        check_err("123l as int", TypeError::invalid_cast());
    }

    #[test]
    fn invalid_reference() {
        check_err("&123", TypeError::invalid_reference());
    }

    #[test]
    fn invalid_dereference() {
        check_err("@123", TypeError::invalid_dereference());
    }

    #[test]
    fn invalid_assignment() {
        check_err("123 := 4", TypeError::invalid_lvalue())
    }
}
