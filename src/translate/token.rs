pub use rust_decimal::Decimal;

use crate::input::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Number(Decimal),
    OpenParen,
    CloseParen,
    Plus,
    Minus,
    Star,
    Slash,
    StarStar,
    And,
    Pipe,
    Caret,
    Tilde,
}

#[cfg(test)]
impl Token {
    pub fn number(&self) -> &Decimal {
        let TokenKind::Number(decimal) = &self.kind else {
            panic!("token is not a number: {:?}", self);
        };

        decimal
    }
}
