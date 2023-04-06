use std::iter::Peekable;

use eyre::Result;

use crate::translate::ast::*;
use crate::translate::error::Error;
use crate::translate::token::*;
use crate::translate::tokenizer::Tokens;

/// Parse a calculator statement or expression
///
/// ```ignore
/// expr ->
///     | bitwise bitwise_op?
/// bitwise ->
///     | term add_sub?
/// bitwise_op ->
///     | '&' bitwise bitwise_op?
///     | '|' bitwise bitwise_op?
///     | '^' bitwise bitwise_op?
/// term ->
///     | factor mul_div?
/// add_sub ->
///     | '+' term add_sub?
///     | '-' term add_sub?
///     | \e
/// factor ->
///     | exponent power?
/// mul_div ->
///     | '*' factor mul_div?
///     | '/' factor mul_div?
///     | '%' factor mul_div?
///     | \e
/// exponent ->
///     | '-' unit
///     | '~' unit
///     | unit
/// power ->
///     | '**' exponent power?
///     | \e
/// unit ->
///     | NUMBER
///     | '(' expr ')'
///     | \e
/// ```
pub struct Parser<'a> {
    input: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: Tokens<'a>) -> Self {
        Self {
            input: input.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Option<Expr>> {
        let res = self.parse_expr()?;

        match self.input.next() {
            Some(Ok(t)) => Err(Error::UnexpectedToken(t).into()),
            Some(Err(err)) => Err(err),
            None => Ok(res),
        }
    }

    pub fn next(&mut self) -> Result<Option<Token>> {
        match self.input.next() {
            Some(Ok(t)) => Ok(Some(t)),
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }

    pub fn peek_kind(&mut self) -> Result<Option<&TokenKind>> {
        match self.input.peek() {
            Some(Ok(t)) => Ok(Some(&t.kind)),

            // matching an error here, like in `next`, gives an &Report which does
            // not implement `Clone` so can't really be reported here; it will be
            // reported when `next` is called
            _ => Ok(None),
        }
    }

    fn parse_expr(&mut self) -> Result<Option<Expr>> {
        let bitwise = self.parse_bitwise()?;

        if let Some(bitwise) = bitwise {
            self.parse_bitwise_op(bitwise)
        } else {
            Ok(None)
        }
    }

    fn parse_bitwise_op(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        let res = match self.peek_kind()? {
            Some(TokenKind::And) => {
                self.next()?;
                let rhs = self.parse_bitwise()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::BitAnd(Box::new(lhs), Box::new(rhs));

                self.parse_bitwise_op(lhs)?.unwrap()
            }
            Some(TokenKind::Pipe) => {
                self.next()?;
                let rhs = self.parse_bitwise()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::BitOr(Box::new(lhs), Box::new(rhs));

                self.parse_bitwise_op(lhs)?.unwrap()
            }
            Some(TokenKind::Caret) => {
                self.next()?;
                let rhs = self.parse_bitwise()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::BitXor(Box::new(lhs), Box::new(rhs));

                self.parse_bitwise_op(lhs)?.unwrap()
            }
            _ => lhs,
        };

        Ok(Some(res))
    }

    fn parse_bitwise(&mut self) -> Result<Option<Expr>> {
        let term = self.parse_term()?;

        if let Some(term) = term {
            self.parse_add_sub(term)
        } else {
            Ok(None)
        }
    }

    fn parse_add_sub(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        let res = match self.peek_kind()? {
            Some(TokenKind::Plus) => {
                self.input.next();
                let rhs = self.parse_term()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Add(Box::new(lhs), Box::new(rhs));

                self.parse_add_sub(lhs)?.unwrap()
            }
            Some(TokenKind::Minus) => {
                self.input.next();
                let rhs = self.parse_term()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Sub(Box::new(lhs), Box::new(rhs));

                self.parse_add_sub(lhs)?.unwrap()
            }
            _ => lhs,
        };

        Ok(Some(res))
    }

    fn parse_term(&mut self) -> Result<Option<Expr>> {
        let factor = self.parse_factor()?;

        if let Some(factor) = factor {
            self.parse_mul_div(factor)
        } else {
            Ok(None)
        }
    }

    fn parse_mul_div(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        let res = match self.peek_kind()? {
            Some(TokenKind::Star) => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Mul(Box::new(lhs), Box::new(rhs));

                self.parse_mul_div(lhs)?.unwrap()
            }
            Some(TokenKind::Slash) => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Div(Box::new(lhs), Box::new(rhs));

                self.parse_mul_div(lhs)?.unwrap()
            }
            Some(TokenKind::Percent) => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Rem(Box::new(lhs), Box::new(rhs));

                self.parse_mul_div(lhs)?.unwrap()
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

    fn parse_power(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        let res = match self.peek_kind()? {
            Some(TokenKind::StarStar) => {
                self.input.next();

                let rhs = self.parse_exponent()?.ok_or_else(|| self.input_error())?;
                let rhs = self.parse_power(rhs)?.unwrap();

                Expr::Pow(Box::new(lhs), Box::new(rhs))
            }
            _ => lhs,
        };

        Ok(Some(res))
    }

    fn parse_exponent(&mut self) -> Result<Option<Expr>> {
        match self.peek_kind()? {
            Some(TokenKind::Minus) => {
                self.next()?;
                let expr = self.parse_unit()?.ok_or_else(|| self.input_error())?;

                Ok(Some(Expr::Neg(Box::new(expr))))
            }
            Some(TokenKind::Tilde) => {
                self.next()?;
                let expr = self.parse_unit()?.ok_or_else(|| self.input_error())?;

                Ok(Some(Expr::BitNot(Box::new(expr))))
            }
            _ => self.parse_unit(),
        }
    }

    fn parse_unit(&mut self) -> Result<Option<Expr>> {
        match self.peek_kind()? {
            Some(TokenKind::OpenParen) => {
                self.next()?;
                let expr = self.parse_expr()?.ok_or_else(|| self.input_error())?;

                let Some(c) = self.next()? else {
                    return Err(Error::UnexpectedEOF.into());
                };

                if c.kind != TokenKind::CloseParen {
                    return Err(Error::UnexpectedToken(c).into());
                }

                Ok(Some(expr))
            }
            Some(TokenKind::Number(number)) => {
                let expr = Expr::Number(*number);
                self.next()?;

                Ok(Some(expr))
            }
            _ => Ok(None),
        }
    }

    fn input_error(&mut self) -> eyre::Report {
        match self.input.next() {
            Some(Ok(t)) => Error::UnexpectedToken(t).into(),
            Some(Err(err)) => err,
            None => Error::UnexpectedEOF.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use super::*;

    #[test]
    fn parse() {
        let tok = tokens(vec![
            TokenKind::Number(dec!(12.0)),
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Number(dec!(400)),
            TokenKind::Star,
            TokenKind::Number(dec!(3)),
            TokenKind::StarStar,
            TokenKind::Number(dec!(0.0006)),
            TokenKind::Slash,
            TokenKind::Number(dec!(5.45)),
            TokenKind::Minus,
            TokenKind::Minus,
            TokenKind::Number(dec!(2)),
        ]);
        let mut p = Parser::new(tok);
        let expr = get_expr(p.parse());
        let (lhs, rhs) = expr.sub();
        assert_eq!(*rhs.neg().number(), dec!(2));
        let (lhs, rhs) = lhs.add();
        assert_eq!(*lhs.number(), dec!(12));
        let (lhs, rhs) = rhs.div();
        assert_eq!(*rhs.number(), dec!(5.45));
        let (lhs, rhs) = lhs.mul();
        assert_eq!(*lhs.neg().number(), dec!(400));
        let (base, exponent) = rhs.pow();
        assert_eq!(*base.number(), dec!(3));
        assert_eq!(*exponent.number(), dec!(0.0006));

        let tok = tokens(vec![
            TokenKind::Number(dec!(12)),
            TokenKind::Minus,
            TokenKind::Number(dec!(4)),
            TokenKind::Number(dec!(6)),
            TokenKind::Plus,
            TokenKind::Number(dec!(3)),
        ]);
        let mut p = Parser::new(tok);
        let err = get_err(p.parse());
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::Number(_),
                ..
            })
        ));
    }

    #[test]
    fn expr() {
        let tok = tokens(vec![
            TokenKind::Number(dec!(4)),
            TokenKind::Pipe,
            TokenKind::Number(dec!(5)),
            // ---
            TokenKind::Number(dec!(2)),
            TokenKind::And,
            TokenKind::Minus,
            TokenKind::Number(dec!(7)),
            TokenKind::Caret,
            TokenKind::Number(dec!(10)),
            TokenKind::Star,
            TokenKind::Number(dec!(3)),
            // ---
            TokenKind::CloseParen,
        ]);
        let mut p = Parser::new(tok);

        let expr = get_expr(p.parse_expr());
        let (lhs, rhs) = expr.bit_or();
        assert_eq!(*lhs.number(), dec!(4));
        assert_eq!(*rhs.number(), dec!(5));

        let expr = get_expr(p.parse_expr());
        let (lhs, rhs) = expr.bit_xor();
        {
            let (lhs, rhs) = rhs.mul();
            assert_eq!(*lhs.number(), dec!(10));
            assert_eq!(*rhs.number(), dec!(3));
        }
        let (lhs, rhs) = lhs.bit_and();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.neg().number(), dec!(7));

        let res = p.parse_expr();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn bitwise_op() {
        let tok = tokens(vec![
            TokenKind::And,
            TokenKind::Number(dec!(2)),
            TokenKind::Plus,
            TokenKind::Number(dec!(3)),
            TokenKind::Pipe,
            TokenKind::Number(dec!(4)),
            TokenKind::Caret,
            TokenKind::Minus,
            TokenKind::Number(dec!(5)),
        ]);
        let mut p = Parser::new(tok);

        let expr = get_expr(p.parse_bitwise_op(Expr::Number(dec!(1))));
        let (lhs, rhs) = expr.bit_xor();
        assert_eq!(*rhs.neg().number(), dec!(5));
        let (lhs, rhs) = lhs.bit_or();
        assert_eq!(*rhs.number(), dec!(4));
        let (lhs, rhs) = lhs.bit_and();
        assert_eq!(*lhs.number(), dec!(1));
        let (lhs, rhs) = rhs.add();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.number(), dec!(3));
    }

    #[test]
    fn bitwise() {
        let tok = tokens(vec![
            TokenKind::Number(dec!(1)),
            TokenKind::Plus,
            TokenKind::Number(dec!(2)),
            TokenKind::Plus,
            TokenKind::Number(dec!(3)),
            // ---
            TokenKind::Number(dec!(2)),
            TokenKind::Slash,
            TokenKind::Number(dec!(3)),
            TokenKind::Plus,
            TokenKind::Number(dec!(4)),
            TokenKind::Minus,
            TokenKind::Number(dec!(5)),
            TokenKind::Star,
            TokenKind::Number(dec!(6)),
            // ---
            TokenKind::CloseParen,
        ]);
        let mut p = Parser::new(tok);

        let expr = get_expr(p.parse_bitwise());
        let (lhs, rhs) = expr.add();
        assert_eq!(*rhs.number(), dec!(3));
        let (lhs, rhs) = lhs.add();
        assert_eq!(*lhs.number(), dec!(1));
        assert_eq!(*rhs.number(), dec!(2));

        let expr = get_expr(p.parse_bitwise());
        let (lhs, rhs) = expr.sub();
        {
            let (lhs, rhs) = rhs.mul();
            assert_eq!(*lhs.number(), dec!(5));
            assert_eq!(*rhs.number(), dec!(6));
        }
        let (lhs, rhs) = lhs.add();
        assert_eq!(*rhs.number(), dec!(4));
        let (lhs, rhs) = lhs.div();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.number(), dec!(3));

        let res = p.parse_bitwise();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn add_sub() {
        let tok = tokens(vec![
            TokenKind::Plus,
            TokenKind::Number(dec!(2)),
            TokenKind::Minus,
            TokenKind::OpenParen,
            TokenKind::Number(dec!(3)),
            TokenKind::Plus,
            TokenKind::Number(dec!(4)),
            TokenKind::CloseParen,
        ]);
        let mut p = Parser::new(tok);

        let expr = get_expr(p.parse_add_sub(Expr::Number(dec!(1))));
        let (lhs, rhs) = expr.sub();
        {
            let (lhs, rhs) = rhs.add();
            assert_eq!(*lhs.number(), dec!(3));
            assert_eq!(*rhs.number(), dec!(4));
        }
        {
            let (lhs, rhs) = lhs.add();
            assert_eq!(*lhs.number(), dec!(1));
            assert_eq!(*rhs.number(), dec!(2));
        }

        let mut p = Parser::new(tokens(vec![TokenKind::Plus, TokenKind::Caret]));
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(2))));
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::Caret,
                ..
            })
        ));

        let mut p = Parser::new(tokens(vec![TokenKind::Plus]));
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedEOF));

        let mut p = Parser::new(tokens(vec![TokenKind::Minus, TokenKind::Caret]));
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(1))));
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::Caret,
                ..
            })
        ));

        let mut p = Parser::new(tokens(vec![TokenKind::Minus]));
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(3))));
        assert!(matches!(err, Error::UnexpectedEOF));
    }

    #[test]
    fn term() {
        let tok = tokens(vec![
            TokenKind::Number(dec!(3)),
            TokenKind::Slash,
            TokenKind::Number(dec!(4)),
            TokenKind::Star,
            TokenKind::Number(dec!(2)),
            // ---
            TokenKind::Minus,
            TokenKind::Number(dec!(2)),
            TokenKind::Star,
            TokenKind::OpenParen,
            TokenKind::Number(dec!(4)),
            TokenKind::Star,
            TokenKind::Number(dec!(3)),
            TokenKind::CloseParen,
            TokenKind::StarStar,
            TokenKind::Number(dec!(6.1)),
            // ---
            TokenKind::Number(dec!(4)),
            TokenKind::Percent,
            TokenKind::Number(dec!(2)),
            TokenKind::StarStar,
            TokenKind::Number(dec!(8)),
            // ---
            TokenKind::Caret,
        ]);
        let mut p = Parser::new(tok);

        let expr = get_expr(p.parse_term());
        let (lhs, rhs) = expr.mul();
        assert_eq!(*rhs.number(), dec!(2));
        let (lhs, rhs) = lhs.div();
        assert_eq!(*lhs.number(), dec!(3));
        assert_eq!(*rhs.number(), dec!(4));

        let expr = get_expr(p.parse_term());
        let (lhs, rhs) = expr.mul();
        assert_eq!(*lhs.neg().number(), dec!(2));
        let (base, exponent) = rhs.pow();
        assert_eq!(*exponent.number(), dec!(6.1));
        let (lhs, rhs) = base.mul();
        assert_eq!(*lhs.number(), dec!(4));
        assert_eq!(*rhs.number(), dec!(3));

        let expr = get_expr(p.parse_term());
        let (lhs, rhs) = expr.rem();
        assert_eq!(*lhs.number(), dec!(4));
        let (lhs, rhs) = rhs.pow();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.number(), dec!(8));

        let res = p.parse_term();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn mul_div() {
        let tok = tokens(vec![
            TokenKind::Star,
            TokenKind::Number(dec!(0.032)),
            TokenKind::Slash,
            TokenKind::Number(dec!(4)),
            TokenKind::StarStar,
            TokenKind::Minus,
            TokenKind::Number(dec!(2)),
            TokenKind::Star,
            TokenKind::Number(dec!(5)),
            TokenKind::Percent,
            TokenKind::Number(dec!(7)),
        ]);
        let mut p = Parser::new(tok);
        let expr = get_expr(p.parse_mul_div(Expr::Number(dec!(2))));

        let (lhs, rhs) = expr.rem();
        assert_eq!(*rhs.number(), dec!(7));

        let (lhs, rhs) = lhs.mul();
        assert_eq!(*rhs.number(), dec!(5));

        let (lhs, rhs) = lhs.div();
        assert_eq!(*rhs.pow().0.number(), dec!(4));
        assert_eq!(*rhs.pow().1.neg().number(), dec!(2));

        let (lhs, rhs) = lhs.mul();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.number(), dec!(0.032));

        let mut p = Parser::new(tokens(vec![TokenKind::Star, TokenKind::CloseParen]));
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(2))));
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::CloseParen,
                ..
            })
        ));

        let mut p = Parser::new(tokens(vec![TokenKind::Star]));
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(99))));
        assert!(matches!(err, Error::UnexpectedEOF));

        let mut p = Parser::new(tokens(vec![TokenKind::Slash, TokenKind::Caret]));
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(2))));
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::Caret,
                ..
            })
        ));

        let mut p = Parser::new(tokens(vec![TokenKind::Slash]));
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedEOF));
    }

    #[test]
    fn factor() {
        let tok = tokens(vec![
            TokenKind::Number(dec!(2)),
            // ---
            TokenKind::Number(dec!(0.031)),
            TokenKind::StarStar,
            TokenKind::Number(dec!(4)),
            // ---
            TokenKind::Minus,
            TokenKind::OpenParen,
            TokenKind::Number(dec!(2)),
            TokenKind::StarStar,
            TokenKind::Minus,
            TokenKind::Number(dec!(4.1)),
            TokenKind::CloseParen,
            TokenKind::StarStar,
            TokenKind::Number(dec!(3.14)),
            // ---
            TokenKind::Star,
        ]);
        let mut p = Parser::new(tok);

        let expr = get_expr(p.parse_factor());
        assert_eq!(*expr.number(), dec!(2));

        let expr = get_expr(p.parse_factor());
        let (base, exponent) = expr.pow();
        assert_eq!(*base.number(), dec!(0.031));
        assert_eq!(*exponent.number(), dec!(4));

        let expr = get_expr(p.parse_factor());
        let (base, exponent) = expr.pow();
        assert_eq!(*exponent.number(), dec!(3.14));
        let (base, exponent) = base.neg().pow();
        assert_eq!(*base.number(), dec!(2));
        assert_eq!(*exponent.neg().number(), dec!(4.1));

        let res = p.parse_factor();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn power() {
        let tok = tokens(vec![
            TokenKind::StarStar,
            TokenKind::Number(dec!(3)),
            TokenKind::StarStar,
            TokenKind::Number(dec!(4)),
        ]);
        let mut p = Parser::new(tok);
        let expr = get_expr(p.parse_power(Expr::Number(dec!(2))));
        let (base, exponent) = expr.pow();
        assert_eq!(*base.number(), dec!(2));
        let (base, exponent) = exponent.pow();
        assert_eq!(*base.number(), dec!(3));
        assert_eq!(*exponent.number(), dec!(4));

        let mut p = Parser::new(tokens(vec![TokenKind::StarStar, TokenKind::Caret]));
        let err = get_err(p.parse_power(Expr::Number(dec!(2))));
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::Caret,
                ..
            })
        ));

        let mut p = Parser::new(tokens(vec![TokenKind::StarStar]));
        let err = get_err(p.parse_power(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedEOF));
    }

    #[test]
    fn exponent() {
        let tok = tokens(vec![
            TokenKind::Minus,
            TokenKind::Number(dec!(4)),
            // ---
            TokenKind::Tilde,
            TokenKind::OpenParen,
            TokenKind::Number(dec!(1)),
            TokenKind::CloseParen,
            // ---
            TokenKind::Minus,
            TokenKind::OpenParen,
            TokenKind::Minus,
            TokenKind::Number(dec!(1.1)),
            TokenKind::CloseParen,
            // ---
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            // ---
            TokenKind::OpenParen,
            TokenKind::Star,
            TokenKind::CloseParen,
        ]);
        let mut p = Parser::new(tok);
        let expr = get_expr(p.parse_exponent());
        assert_eq!(*expr.neg().number(), dec!(4));
        let expr = get_expr(p.parse_exponent());
        assert_eq!(*expr.bit_not().number(), dec!(1));
        let expr = get_expr(p.parse_exponent());
        assert_eq!(*expr.neg().neg().number(), dec!(1.1));
        let err = get_err(p.parse_exponent());
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::CloseParen,
                ..
            })
        ));
        let err = get_err(p.parse_exponent());
        assert!(matches!(
            err,
            Error::UnexpectedToken(Token {
                kind: TokenKind::Star,
                ..
            })
        ));
    }

    #[test]
    fn unit() {
        let tok = tokens(vec![
            TokenKind::Number(dec!(1)),
            // ---
            TokenKind::OpenParen,
            TokenKind::Number(dec!(2)),
            TokenKind::Slash,
            TokenKind::Number(dec!(3)),
            TokenKind::CloseParen,
            // ---
            TokenKind::CloseParen,
        ]);
        let mut p = Parser::new(tok);
        let expr = get_expr(p.parse_unit());
        assert_eq!(*expr.number(), dec!(1));
        let expr = get_expr(p.parse_unit());
        let (lhs, rhs) = expr.div();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.number(), dec!(3));

        let res = p.parse_unit();
        assert!(matches!(res, Ok(None)));
    }

    fn get_expr(res: Result<Option<Expr>>) -> Expr {
        let res = match res {
            Err(err) => panic!("unexpected error: {:?}", err),
            Ok(res) => res,
        };

        assert!(res.is_some(), "result was None");
        res.unwrap()
    }

    fn get_err(res: Result<Option<Expr>>) -> Error {
        let res = match res {
            Ok(res) => panic!("not an error: {:#?}", res),
            Err(err) => err,
        };

        match res.downcast() {
            Ok(err) => err,
            Err(err) => panic!("not a parser error: {:?}", err),
        }
    }

    fn tokens(kinds: Vec<TokenKind>) -> Tokens<'static> {
        kinds
            .into_iter()
            .map(|kind| Token {
                span: Default::default(),
                kind,
            })
            .collect::<Vec<_>>()
            .into()
    }
}
