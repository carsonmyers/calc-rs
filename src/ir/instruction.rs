use rust_decimal::Decimal;

#[derive(Debug, PartialEq, Eq)]
pub enum Inst {
    PushNumber(Decimal),
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Neg,
}
