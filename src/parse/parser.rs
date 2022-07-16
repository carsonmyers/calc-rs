use crate::parse::ast::*;
use crate::parse::ParseError;
use crate::text::*;

use anyhow::Result;

use std::iter::Peekable;
use std::str::Chars;

pub struct Parser<'a> {
    input: Peekable<Chars<'a>>,
    p: Pos,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            input: input.chars().peekable(),
            p: Pos::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Item>> {
        let mut items = Vec::new();

        loop {
            let res = match self.input.peek() {
                Some(c) if c.is_numeric() => self.parse_literal(),
                Some(c) => Err(ParseError::UnexpectedChar(c, Span::from_pos(self.p, 1))),
            }?
        }

        todo!();
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn parse_group() {
        panic!("oh no");
    }
}