use rust_decimal::Decimal;

use crate::translate::Expr;

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    PushNumber(Decimal),
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Neg,
}

pub trait ToIr {
    fn to_ir(&self) -> Vec<Instruction>;
}

impl ToIr for Expr {
    fn to_ir(&self) -> Vec<Instruction> {
        match self {
            Expr::Number(num) => vec![Instruction::PushNumber(*num)],
            Expr::Add(lhs, rhs) => {
                let mut inst = lhs.to_ir();
                inst.append(&mut rhs.to_ir());
                inst.push(Instruction::Add);
                inst
            }
            Expr::Sub(lhs, rhs) => {
                let mut inst = lhs.to_ir();
                inst.append(&mut rhs.to_ir());
                inst.push(Instruction::Sub);
                inst
            }
            Expr::Mul(lhs, rhs) => {
                let mut inst = lhs.to_ir();
                inst.append(&mut rhs.to_ir());
                inst.push(Instruction::Mul);
                inst
            }
            Expr::Div(lhs, rhs) => {
                let mut inst = lhs.to_ir();
                inst.append(&mut rhs.to_ir());
                inst.push(Instruction::Div);
                inst
            }
            Expr::Pow(base, exponent) => {
                let mut inst = base.to_ir();
                inst.append(&mut exponent.to_ir());
                inst.push(Instruction::Pow);
                inst
            }
            Expr::Neg(expr) => {
                let mut inst = expr.to_ir();
                inst.push(Instruction::Neg);
                inst
            }
            _ => todo!(),
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
            Instruction::PushNumber(dec!(12)),
            Instruction::PushNumber(dec!(4)),
            Instruction::PushNumber(dec!(3)),
            Instruction::Sub,
            Instruction::Pow,
            Instruction::Neg,
            Instruction::PushNumber(dec!(3)),
            Instruction::PushNumber(dec!(9)),
            Instruction::PushNumber(dec!(2)),
            Instruction::Pow,
            Instruction::Sub,
            Instruction::Div,
            Instruction::PushNumber(dec!(5)),
            Instruction::PushNumber(dec!(10)),
            Instruction::Mul,
            Instruction::Add,
        ];

        let actual = ast.to_ir();

        assert_eq!(expected, actual);
    }
}
