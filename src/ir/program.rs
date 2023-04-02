use rust_decimal::Decimal;

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
