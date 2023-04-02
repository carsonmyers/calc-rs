use crate::ir::instruction::Inst;
use crate::parse::Expr;

pub trait Translate {
    fn translate(&self) -> Vec<Inst>;
}

impl Translate for Expr {
    fn translate(&self) -> Vec<Inst> {
        match self {
            Expr::Number(num) => vec![Inst::PushNumber(*num)],
            Expr::Add(lhs, rhs) => {
                let mut inst = lhs.translate();
                inst.append(&mut rhs.translate());
                inst.push(Inst::Add);
                inst
            }
            Expr::Sub(lhs, rhs) => {
                let mut inst = lhs.translate();
                inst.append(&mut rhs.translate());
                inst.push(Inst::Sub);
                inst
            }
            Expr::Mul(lhs, rhs) => {
                let mut inst = lhs.translate();
                inst.append(&mut rhs.translate());
                inst.push(Inst::Mul);
                inst
            }
            Expr::Div(lhs, rhs) => {
                let mut inst = lhs.translate();
                inst.append(&mut rhs.translate());
                inst.push(Inst::Div);
                inst
            }
            Expr::Pow(base, exponent) => {
                let mut inst = base.translate();
                inst.append(&mut exponent.translate());
                inst.push(Inst::Pow);
                inst
            }
            Expr::Neg(expr) => {
                let mut inst = expr.translate();
                inst.push(Inst::Neg);
                inst
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use super::*;

    #[test]
    fn translate() {
        let ast = Expr::Add(
            Box::new(Expr::Div(
                Box::new(Expr::Neg(Box::new(Expr::Pow(
                    Box::new(Expr::Number(dec!(12))),
                    Box::new(Expr::Sub(
                        Box::new(Expr::Number(dec!(4))),
                        Box::new(Expr::Number(dec!(3))),
                    )),
                )))),
                Box::new(Expr::Sub(
                    Box::new(Expr::Number(dec!(3))),
                    Box::new(Expr::Pow(
                        Box::new(Expr::Number(dec!(9))),
                        Box::new(Expr::Number(dec!(2))),
                    )),
                )),
            )),
            Box::new(Expr::Mul(
                Box::new(Expr::Number(dec!(5))),
                Box::new(Expr::Number(dec!(10))),
            )),
        );

        let expected = vec![
            Inst::PushNumber(dec!(12)),
            Inst::PushNumber(dec!(4)),
            Inst::PushNumber(dec!(3)),
            Inst::Sub,
            Inst::Pow,
            Inst::Neg,
            Inst::PushNumber(dec!(3)),
            Inst::PushNumber(dec!(9)),
            Inst::PushNumber(dec!(2)),
            Inst::Pow,
            Inst::Sub,
            Inst::Div,
            Inst::PushNumber(dec!(5)),
            Inst::PushNumber(dec!(10)),
            Inst::Mul,
            Inst::Add,
        ];

        let actual = ast.translate();

        assert_eq!(expected, actual);
    }
}
