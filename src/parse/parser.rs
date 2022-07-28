use crate::parse::ast::*;
use crate::parse::state::*;
use crate::parse::ParseError;
use crate::text::*;

use anyhow::Result;

use std::iter::Peekable;
use std::mem;
use std::str::Chars;

pub struct Parser<'a> {
    input: Peekable<Chars<'a>>,
    state: State,
    n: Pos,
    s: Pos,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            input: input.chars().peekable(),
            state: State::new(),
            n: Pos::new(),
            s: Pos::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Item>> {
        let mut items: Vec<Item> = Vec::new();

        loop {
            let res = match self.state.get() {
                StateKind::Group => self.parse_group(),
                StateKind::Expr(..) => self.parse_expr(),
                _ => Err(anyhow::Error::from(ParseError::UnexpectedEOF)),
            }?;

            if res.is_some() {
                items.push(res.unwrap())
            }
            break;
        }

        Ok(items)
    }

    fn parse_group(&mut self) -> Result<Option<Item>> {
        match self.input.peek() {
            Some(c) if c.is_numeric() => {
                let expr = self.parse_literal()?;
                self.state.push(StateKind::Expr(expr))
            }
            Some(c) => {
                return Err(anyhow::Error::from(ParseError::UnexpectedChar(
                    *c,
                    self.span(),
                )))
            }
            None => return Err(ParseError::UnexpectedEOF.into()),
        };

        Ok(None)
    }

    fn parse_expr(&mut self) -> Result<Option<Item>> {
        if let StateKind::Expr(expr) = self.state.pop()? {
            match self.peek() {
                Some('+') => self.state.push(StateKind::Infix(InfixOp::Add, expr)),
                Some('-') => self.state.push(StateKind::Infix(InfixOp::Sub, expr)),
                Some('*') => self.state.push(StateKind::Infix(InfixOp::Mul, expr)),
                Some('/') => self.state.push(StateKind::Infix(InfixOp::Div, expr)),
                Some('^') => self.state.push(StateKind::Infix(InfixOp::Pow, expr)),
                Some(c) => {
                    return Err(anyhow::Error::from(ParseError::UnexpectedChar(
                        *c,
                        self.span(),
                    )))
                }
                None => {
                    return Ok(Some(Item {
                        span: expr.span(),
                        kind: ItemKind::Expr(expr),
                    }))
                }
            }
        } else {
            return Err(anyhow::Error::from(ParseError::InvalidParserState));
        };

        Ok(None)
    }

    pub fn parse_infix(&mut self) -> Result<Option<Item>> {
        if let StateKind::Infix(op, expr) = self.state.pop()? {
            match self.peek() {
                Some(c) if c.is_numeric() => {
                    let lit = self.parse_literal()?;
                }
                Some(c) if c.is_alphabetic() || *c == '_' => self.parse_name()?,
            };
        } else {
            return Err(anyhow::Error::from(ParseError::InvalidParserState));
        }

        Ok(None)
    }

    pub fn parse_literal(&mut self) -> Result<Expr> {
        let mut lexeme = String::new();
        self.read_number(&mut lexeme);

        // Read a decimal part
        if self.read_char(&mut lexeme, &['.']) {
            if self.read_number(&mut lexeme) == 0 {
                return Err(ParseError::InvalidNumber(lexeme, self.span()).into());
            }
        }

        // Read an exponent part
        if self.read_char(&mut lexeme, &['e', 'E']) {
            self.read_char(&mut lexeme, &['+', '-']);
            if self.read_number(&mut lexeme) == 0 {
                return Err(ParseError::InvalidNumber(lexeme, self.span()).into());
            }
        }

        let span = self.span();
        let value = lexeme
            .parse::<f64>()
            .map_err(|_| ParseError::InvalidNumber(lexeme, span))?;

        Ok(Expr {
            span,
            kind: ExprKind::Lit(value),
        })
    }

    fn parse_name(&mut self) -> String {
        let mut lexeme = String::new();
        loop {
            match self.peek() {
                Some(c) if c.is_alphabetic() || *c == '_' => {
                    lexeme.push(*c);
                    self.next();
                }
                _ => break,
            }
        }

        lexeme
    }

    fn read_number(&mut self, lexeme: &mut String) -> usize {
        let mut count: usize = 0;
        loop {
            match self.peek() {
                Some(c) if c.is_numeric() => {
                    lexeme.push(*c);
                    self.next();
                    count += 1;
                }
                _ => break,
            }
        }

        count
    }

    fn read_char(&mut self, lexeme: &mut String, target: &[char]) -> bool {
        match self.peek() {
            Some(c) if c == target => {
                lexeme.push(*c);
                self.next();
                true
            }
            _ => false,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.input.next();
        if c.is_some() {
            self.n.next(c.unwrap());
        }

        c
    }

    fn span(&mut self) -> Span {
        Span {
            start: mem::replace(&mut self.s, self.n.clone()),
            end: self.n.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn parse_group() {
        panic!("oh no");
    }
}
