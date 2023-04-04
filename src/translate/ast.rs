use rust_decimal::Decimal;

#[derive(Debug)]
pub enum Expr {
    Number(Decimal),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    BitNot(Box<Expr>),
}

impl Expr {
    pub fn number(&self) -> &Decimal {
        let Expr::Number(number) = self else {
            panic!("not a number");
        };

        number
    }

    pub fn add(&self) -> (&Expr, &Expr) {
        let Expr::Add(lhs, rhs) = self else {
            panic!("not an addition");
        };

        (lhs, rhs)
    }

    pub fn sub(&self) -> (&Expr, &Expr) {
        let Expr::Sub(lhs, rhs) = self else {
            panic!("not a subtraction");
        };

        (lhs, rhs)
    }

    pub fn mul(&self) -> (&Expr, &Expr) {
        let Expr::Mul(lhs, rhs) = self else {
            panic!("not a multiplication");
        };

        (lhs, rhs)
    }

    pub fn div(&self) -> (&Expr, &Expr) {
        let Expr::Div(lhs, rhs) = self else {
            panic!("not a division");
        };

        (lhs, rhs)
    }

    pub fn pow(&self) -> (&Expr, &Expr) {
        let Expr::Pow(base, exponent) = self else {
            panic!("not an exponent")
        };

        (base, exponent)
    }

    pub fn neg(&self) -> &Expr {
        let Expr::Neg(expr) = self else {
            panic!("not a negation");
        };

        expr
    }
}
