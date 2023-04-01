#[derive(Debug)]
pub enum Expr {
    Number(f64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
}

impl Expr {
    pub fn number(&self) -> Option<&f64> {
        if let Expr::Number(number) = self {
            Some(number)
        } else {
            None
        }
    }

    pub fn add(&self) -> Option<(&Expr, &Expr)> {
        if let Expr::Add(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn sub(&self) -> Option<(&Expr, &Expr)> {
        if let Expr::Sub(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn mul(&self) -> Option<(&Expr, &Expr)> {
        if let Expr::Mul(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn div(&self) -> Option<(&Expr, &Expr)> {
        if let Expr::Div(lhs, rhs) = self {
            Some((lhs, rhs))
        } else {
            None
        }
    }

    pub fn pow(&self) -> Option<(&Expr, &Expr)> {
        if let Expr::Pow(base, exponent) = self {
            Some((base, exponent))
        } else {
            None
        }
    }

    pub fn neg(&self) -> Option<&Expr> {
        if let Expr::Neg(expr) = self {
            Some(expr)
        } else {
            None
        }
    }
}
