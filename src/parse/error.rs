use crate::text::Span;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{1} unexpected character `{0}`")]
    UnexpectedChar(char, Span),
}