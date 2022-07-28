use crate::text::*;

pub struct Item {
    span: Span,
    kind: ItemKind,
}

impl Item {
    pub fn get_expr(&self) -> Option<Expr> {
        if let ItemKind::Expr(e) = self.kind {
            Some(e)
        } else {
            None
        }
    }

    pub fn get_stmt(&self) -> Option<Stmt> {
        if let ItemKind::Stmt(s) = self.kind {
            Some(s)
        } else {
            None
        }
    }
}

pub enum ItemKind {
    Expr(Expr),
    Stmt(Stmt),
}

pub struct Expr {
    span: Span,
    kind: ExprKind,
}

impl Expr {
    pub fn span(&self) -> Span {
        self.span
    }

    pub fn is_value(&self) -> bool {
        match self.kind {
            ExprKind::Lit(..) | ExprKind::Var(..) | ExprKind::Ref(..) | ExprKind::Fn(..) => true,
            _ => false,
        }
    }

    pub fn is_infix(&self) -> bool {
        match self.kind {
            ExprKind::Add(..)
            | ExprKind::Sub(..)
            | ExprKind::Mul(..)
            | ExprKind::Div(..)
            | ExprKind::Pow(..) => true,
            _ => false,
        }
    }
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
    Fn(String, Vec<Expr>),
}

pub struct Stmt {
    span: Span,
    kind: StmtKind,
}

pub enum StmtKind {
    Assign(String, Box<Expr>),
    FnDecl(String, Vec<String>, Vec<Item>),
}
