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
    pub fn assign(place: LValue, expr: Expr) -> Self {
        Stmt::Assign(Box::new(AssignStmt {
            lvalue: Box::new(place),
            expr,
        }))
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
    pub lvalue: Box<LValue>,
    pub expr: Expr,
}

// TODO: more lvalues
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LValue {
    Id(BindId),
    Field(Box<LValueField>),
}

impl LValue {
    pub fn id(id: BindId) -> Self {
        LValue::Id(id)
    }
    pub fn field(lvalue: LValue, offset: usize, size: usize) -> Self {
        LValue::Field(Box::new(LValueField {
            offset,
            size,
            lvalue,
        }))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LValueField {
    pub offset: usize,
    pub size: usize,
    pub lvalue: LValue,
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
    pub fn struc(fields: Vec<StructField>, ty: Ty) -> Self {
        Expr {
            kind: ExprKind::Struct(fields),
            ty,
        }
    }
    pub fn struct_field(field: StructField, ty: Ty) -> Self {
        Expr {
            ty,
            kind: ExprKind::StructField(Box::new(field)),
        }
    }
    pub fn oneof_field(field: OneOfField, ty: Ty) -> Self {
        Expr {
            ty,
            kind: ExprKind::OneOfField(Box::new(field)),
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
    pub fn as_lvalue(&self) -> Option<LValue> {
        match &self.kind {
            ExprKind::Ident(id) => Some(LValue::id(*id)),
            ExprKind::StructField(field) => {
                let lvalue = field.expr.as_lvalue()?;

                Some(LValue::field(lvalue, field.offset, field.size))
            }
            _ => None,
        }
    }
    pub fn add_ref(expr: Expr) -> Option<Self> {
        expr.as_lvalue().map(|lvalue| Expr {
            kind: ExprKind::RefIdent(Box::new(lvalue)),
            ty: expr.ty.add_ref(),
        })
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
    Struct(Vec<StructField>),
    StructField(Box<StructField>),
    OneOfField(Box<OneOfField>),
    Ident(BindId),
    RefIdent(Box<LValue>),
    Deref(Box<Expr>),
    Not(Box<Expr>),
    BinaryOp(Box<BinaryOp>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub offset: usize,
    pub size: usize,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OneOfField {
    pub bit_index: usize,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryOp {
    pub operator: ast::BinOpKind,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
    width: usize,
    ref_level: usize,
}

impl Ty {
    pub fn int() -> Self {
        Self {
            kind: TyKind::Int,
            width: 1,
            ref_level: 0,
        }
    }
    pub fn bool() -> Self {
        Self {
            kind: TyKind::Bool,
            width: 1,
            ref_level: 0,
        }
    }
    pub fn long() -> Self {
        Self {
            kind: TyKind::Long,
            width: 2,
            ref_level: 0,
        }
    }
    pub fn structure(id: usize, fields: Vec<(String, Ty)>) -> Self {
        let mut fields_map = HashMap::new();
        let mut current_width = 0;
        for (key, ty) in fields {
            let offset = current_width;
            current_width += ty.size();
            fields_map.insert(key, StructTyField { offset, ty });
        }
        Self {
            kind: TyKind::Struct(Struct {
                id,
                fields: fields_map,
            }),
            width: current_width,
            ref_level: 0,
        }
    }
    pub fn oneof(id: usize, fields: HashMap<String, OneOfCase>) -> Self {
        Self {
            kind: TyKind::OneOf(OneOf { id, fields }),
            width: 1,
            ref_level: 0,
        }
    }
    pub fn oneof_set(value: &OneOf) -> Self {
        // TODO: handle bitsets > 32 bits
        Self {
            kind: TyKind::OneOfSet(value.clone()),
            width: 1,
            ref_level: 0,
        }
    }
    pub fn get_field(&self, key: &str) -> Option<&StructTyField> {
        match &self.kind {
            TyKind::Struct(s) => s.fields.get(key),
            _ => None,
        }
    }
    pub fn get_oneof_case(&self, key: &str) -> Option<&OneOfCase> {
        match &self.kind {
            TyKind::OneOf(o) => o.fields.get(key),
            _ => None,
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
pub enum TyKind {
    Int,
    Bool,
    Long,
    Struct(Struct),
    OneOf(OneOf),
    OneOfSet(OneOf),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct {
    id: usize,
    pub fields: HashMap<String, StructTyField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructTyField {
    pub offset: usize,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OneOf {
    id: usize,
    pub fields: HashMap<String, OneOfCase>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OneOfCase {
    pub value: usize,
}
