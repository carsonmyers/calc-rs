use crate::translate::token::Token;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("unexpected end of file")]
    UnexpectedEOF,
    #[error("unexpected character '{0}'")]
    UnexpectedChar(char),
    #[error("unexpected token {0:?}")]
    UnexpectedToken(Token),
}
