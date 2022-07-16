use crate::text::*;

pub struct Item {
    span: Span,
    kind: ItemKind,
}

pub enum ItemKind {
    Expr(Expr),
    Stmt(Stmt),
}

pub struct Expr {
    span: Span,
    kind: ExprKind,
}

pub enum ExprKind {
    Lit(f64),
    Var(String),
    Ref(String),
    Neg(Box<Expr>),
    Pos(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Fn(String, Vec<Expr>)
}

pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

pub enum StmtKind {
    Assign(String, Box<Expr>),
    FnDecl(String, Vec<String>, Vec<Item>),
}