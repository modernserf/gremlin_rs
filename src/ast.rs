use crate::source_info::SourceInfo;
use crate::token::IntLiteral;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StmtKind {
    Let(Box<LetStmt>),
    Assign(Box<AssignStmt>),
    Expr(Box<Expr>),
    Noop,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetStmt {
    pub binding: Bind,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AssignStmt {
    pub target: Expr,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Bind {
    pub kind: BindKind,
    pub source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BindKind {
    Ident(IdentBind),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdentBind {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Int(IntLiteral),
    Ident(IdentExpr),
    UnaryOp(Box<UnaryOp>),
    BinaryOp(Box<BinaryOp>),
    As(Box<AsExpr>),
    True,
    False,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdentExpr {
    pub value: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnaryOp {
    pub operator: UnOpKind,
    pub expr: Expr,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOpKind {
    Ref,
    Deref,
    Not,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinaryOp {
    pub operator: BinOpKind,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Mult,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AsExpr {
    pub expr: Expr,
    pub ty: TyExpr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TyExpr {
    pub kind: TyExprKind,
    pub source_info: SourceInfo,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyExprKind {
    Identifier(IdentTyExpr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IdentTyExpr {
    pub value: String,
}
