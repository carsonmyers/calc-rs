use std::iter::Peekable;
use std::str::Chars;

use eyre::Result;

use crate::parse::ast::*;
use crate::parse::error::Error;

/// Parse a calculator statement or expression
///
/// ```ignore
/// expr ->
///     | term add_sub?
/// add_sub ->
///     | '+' term add_sub?
///     | '-' term add_sub?
///     | \e
/// term ->
///     | factor mul_div?
/// mul_div ->
///     | '*' factor mul_div?
///     | '/' factor mul_div?
///     | \e
/// factor ->
///     | exponent power?
/// power ->
///     | '^' exponent power?
///     | \e
/// exponent ->
///     | '-'? '(' expr ')'
///     | '-'? NUMBER
/// ```
pub struct Parser<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Option<Expr>> {
        let res = self.parse_expr()?;

        if let Some(c) = self.input.next() {
            Err(Error::UnexpectedChar(c).into())
        } else {
            Ok(res)
        }
    }

    fn parse_expr(&mut self) -> Result<Option<Expr>> {
        let term = self.parse_term()?;

        if let Some(term) = term {
            self.parse_add_sub(term)
        } else {
            Ok(None)
        }
    }

    fn parse_term(&mut self) -> Result<Option<Expr>> {
        let factor = self.parse_factor()?;

        if let Some(factor) = factor {
            self.parse_mul_div(factor)
        } else {
            Ok(None)
        }
    }

    fn parse_add_sub(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        self.skip_whitespace();

        let res = match self.input.peek() {
            Some('+') => {
                self.input.next();
                let rhs = self.parse_term()?.ok_or(Error::UnexpectedChar('+'))?;

                Expr::Add(Box::new(lhs), Box::new(rhs))
            }
            Some('-') => {
                self.input.next();
                let rhs = self.parse_term()?.ok_or(Error::UnexpectedChar('-'))?;

                Expr::Sub(Box::new(lhs), Box::new(rhs))
            }
            _ => lhs,
        };

        Ok(Some(res))
    }

    fn parse_factor(&mut self) -> Result<Option<Expr>> {
        let exponent = self.parse_exponent()?;

        if let Some(exponent) = exponent {
            self.parse_power(exponent)
        } else {
            Ok(None)
        }
    }

    fn parse_mul_div(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        self.skip_whitespace();

        let res = match self.input.peek() {
            Some('*') => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or(Error::UnexpectedChar('*'))?;

                Expr::Mul(Box::new(lhs), Box::new(rhs))
            }
            Some('/') => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or(Error::UnexpectedChar('/'))?;

                Expr::Div(Box::new(lhs), Box::new(rhs))
            }
            _ => lhs,
        };

        Ok(Some(res))
    }

    fn parse_exponent(&mut self) -> Result<Option<Expr>> {
        self.skip_whitespace();

        match self.input.peek() {
            Some('-') => {
                self.input.next();
                let expr = self.parse_exponent()?;

                if let Some(expr) = expr {
                    Ok(Some(Expr::Neg(Box::new(expr))))
                } else {
                    Err(Error::UnexpectedChar('-').into())
                }
            }
            Some('(') => {
                self.input.next();
                let expr = self.parse_expr()?;

                let Some(c) = self.input.next() else {
                    return Err(Error::UnexpectedEOF.into());
                };

                if c != ')' {
                    return Err(Error::UnexpectedChar(c).into());
                }

                Ok(expr)
            }
            Some(c) if c.is_numeric() => self.parse_number(),
            _ => Ok(None),
        }
    }

    fn parse_power(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        self.skip_whitespace();

        let res = match self.input.peek() {
            Some('^') => {
                self.input.next();
                let rhs = self.parse_exponent()?.ok_or(Error::UnexpectedChar('^'))?;

                Expr::Pow(Box::new(lhs), Box::new(rhs))
            }
            _ => lhs,
        };

        Ok(Some(res))
    }

    fn parse_number(&mut self) -> Result<Option<Expr>> {
        let mut chars = Vec::with_capacity(32);
        self.read_numeric_chars(&mut chars);

        match self.input.peek() {
            Some('.') => {
                self.input.next();
                chars.push('.');
                self.read_numeric_chars(&mut chars);
            }
            _ => (),
        }

        if chars.len() == 0 {
            Ok(None)
        } else {
            let value = chars.into_iter().collect::<String>().parse::<f64>()?;
            Ok(Some(Expr::Number(value)))
        }
    }

    fn parse_decimal_number(&mut self, mut chars: Vec<char>) -> Result<Option<Expr>> {
        self.read_numeric_chars(false, &mut chars);

        match self.input.peek() {
            Some('.') => {
                self.input.next();
                chars.push('.');
                self.read_numeric_chars(false, &mut chars);
            }
            _ => (),
        }

        if chars.len() == 0 {
            Ok(None)
        } else {
            let value = chars.into_iter().collect::<String>().parse::<f64>()?;
            Ok(Some(Expr::Number(value)))
        }
    }

    fn parse_hex_number(&mut self, mut chars: Vec<char>) -> Result<Option<Expr>> {
        self.read_numeric_chars(true, &mut chars);

        if chars.len() == 0 {
            Ok(None)
        } else {
            let value = u64::from_str_radix(&chars.into_iter().collect::<String>(), 16)?;
            Ok(Some(Expr::Number(value as f64)))
        }
    }

    fn parse_octal_number(&mut self, mut chars: Vec<char>) -> Result<Option<Expr>> {
        self.read_numeric_chars(false, &mut chars);

        if chars.len() == 0 {
            Ok(None)
        } else {
            let value = u64::from_str_radix(&chars.into_iter().collect::<String>(), 8)?;
            Ok(Some(Expr::Number(value as f64)))
        }
    }

    fn read_numeric_chars(&mut self, include_hex: bool, chars: &mut Vec<char>) {
        while let Some(c) = self.input.peek() {
            if c.is_numeric() {
                chars.push(*c);
            } else if include_hex && c.is_ascii_hexdigit() {
                chars.push(*c);
            } else if c != &'_' {
                break;
            }

            self.input.next();
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.peek() {
            if c.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_expr(res: Result<Option<Expr>>) -> Expr {
        assert!(res.is_ok(), "unexpected error");
        let res = res.unwrap();
        assert!(res.is_some(), "expected an expression");
        res.unwrap()
    }

    #[test]
    fn number() {
        let mut p = Parser::new("3.24");
        let expr = get_expr(p.parse_number());
        assert!(matches!(expr, Expr::Number(_)));
        assert!(f64::abs(expr.number().unwrap() - 3.24) < 0.001);

        let mut p = Parser::new(".123");
        let expr = get_expr(p.parse_number());
        assert!(matches!(expr, Expr::Number(_)));
        assert!(f64::abs(expr.number().unwrap() - 0.123) < 0.001);

        let mut p = Parser::new("24.");
        let expr = get_expr(p.parse_number());
        assert!(matches!(expr, Expr::Number(_)));
        assert!(f64::abs(expr.number().unwrap() - 24.0) < 0.001);

        let mut p = Parser::new("123_456_._01_23_");
        let expr = get_expr(p.parse_number());
        assert!(matches!(expr, Expr::Number(_)));
        assert!(f64::abs(expr.number().unwrap() - 123456.0123) < 0.001);
    }

    #[test]
    fn decimal_number() {
        let mut p = Parser::new("3.13");
        let mut prefix = vec!['1'];
        let expr = get_expr(p.parse_decimal_number(prefix));
        assert!(matches!(expr, Expr::Number(_)));
        assert!(f64::abs(expr.number().unwrap() - 13.13) < 0.001);

        let mut p = Parser::new(".123");
        let mut prefix = vec![];
        let expr = get_expr(p.parse_decimal_number(prefix));
        assert!(matches!(expr, Expr::Number(_)));
        assert!(f64::abs(expr.number().unwrap() - 0.123) < 0.001)
    }
}
