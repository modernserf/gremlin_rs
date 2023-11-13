use std::collections::HashMap;

use crate::{ast, ir::Word};

pub type BindId = usize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Let(Box<LetStmt>),
    Assign(Box<AssignStmt>),
    Expr(Box<Expr>),
}

impl Stmt {
    pub fn let_stmt(bind_id: BindId, expr: Expr) -> Self {
        Stmt::Let(Box::new(LetStmt { bind_id, expr }))
    }
    pub fn assign(bind_id: BindId, expr: Expr) -> Self {
        Stmt::Assign(Box::new(AssignStmt { bind_id, expr }))
    }
    pub fn expr(expr: Expr) -> Self {
        Stmt::Expr(Box::new(expr))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetStmt {
    pub bind_id: BindId,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssignStmt {
    pub bind_id: BindId,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Ty,
}

impl Expr {
    pub fn constant(value: Word, ty: Ty) -> Self {
        Expr {
            kind: ExprKind::Constant(value),
            ty,
        }
    }
    pub fn long(hi: Word, lo: Word, ty: Ty) -> Self {
        Expr {
            kind: ExprKind::Long(hi, lo),
            ty,
        }
    }
    pub fn ident(id: usize, ty: Ty) -> Self {
        Expr {
            kind: ExprKind::Ident(id),
            ty,
        }
    }
    pub fn cast(&self, ty: Ty) -> Option<Self> {
        if self.ty.width == ty.width {
            Some(Expr {
                kind: self.kind.clone(),
                ty,
            })
        } else {
            None
        }
    }
    pub fn add_ref(expr: Expr) -> Option<Self> {
        match &expr.kind {
            ExprKind::Ident(id) => Some(Expr {
                kind: ExprKind::RefIdent(*id),
                ty: expr.ty.add_ref(),
            }),
            _ => None,
        }
    }
    pub fn deref(expr: Expr) -> Option<Self> {
        expr.ty.deref().map(|ty| Expr {
            kind: ExprKind::Deref(Box::new(expr)),
            ty,
        })
    }
    pub fn not(expr: Expr) -> Self {
        Expr {
            kind: ExprKind::Not(Box::new(expr)),
            ty: Ty::bool(),
        }
    }
    pub fn bin_op(operator: ast::BinOpKind, left: Expr, right: Expr, ty: Ty) -> Self {
        Expr {
            kind: ExprKind::BinaryOp(Box::new(BinaryOp {
                operator,
                left,
                right,
            })),
            ty,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Constant(Word),
    Long(Word, Word),
    Ident(BindId),
    RefIdent(BindId),
    Deref(Box<Expr>),
    Not(Box<Expr>),
    BinaryOp(Box<BinaryOp>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryOp {
    pub operator: ast::BinOpKind,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ty {
    id: usize,
    kind: TyKind,
    width: usize,
    ref_level: usize,
}

impl Ty {
    pub fn int() -> Self {
        Self {
            id: 1,
            kind: TyKind::Primitive,
            width: 1,
            ref_level: 0,
        }
    }
    pub fn bool() -> Self {
        Self {
            id: 2,
            kind: TyKind::Primitive,
            width: 1,
            ref_level: 0,
        }
    }
    pub fn long() -> Self {
        Self {
            id: 3,
            kind: TyKind::Primitive,
            width: 2,
            ref_level: 0,
        }
    }
    pub fn size(&self) -> usize {
        if self.ref_level > 0 {
            1
        } else {
            self.width
        }
    }
    pub fn with_ref_level(&self, ref_level: usize) -> Self {
        let mut next = self.clone();
        next.ref_level = ref_level;
        next
    }
    pub fn add_ref(&self) -> Self {
        let mut next = self.clone();
        next.ref_level += 1;
        next
    }
    pub fn deref(&self) -> Option<Self> {
        if self.ref_level == 0 {
            return None;
        }
        let mut next = self.clone();
        next.ref_level -= 1;
        Some(next)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
    Primitive,
    Struct(Struct),
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Struct {
    fields: HashMap<String, StructTyField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct StructTyField {
    offset: usize,
    ty: Ty,
}
