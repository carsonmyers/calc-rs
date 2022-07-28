use crate::text::Span;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{1} unexpected character `{0}`")]
    UnexpectedChar(char, Span),

    #[error("{1} invalid number `{0}`")]
    InvalidNumber(String, Span),

    #[error("unexpected end of input")]
    UnexpectedEOF,

    #[error("invalid parser state")]
    InvalidParserState,
}
