use std::iter::Peekable;
use std::str::Chars;

use eyre::Result;
use rust_decimal::Decimal;

use crate::translate::ast::*;
use crate::translate::error::Error;

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

    fn parse_add_sub(&mut self, lhs: Expr) -> Result<Option<Expr>> {
        self.skip_whitespace();

        let res = match self.input.peek() {
            Some('+') => {
                self.input.next();
                let rhs = self.parse_term()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Add(Box::new(lhs), Box::new(rhs));

                self.parse_add_sub(lhs)?.unwrap()
            }
            Some('-') => {
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
        self.skip_whitespace();

        let res = match self.input.peek() {
            Some('*') => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Mul(Box::new(lhs), Box::new(rhs));

                self.parse_mul_div(lhs)?.unwrap()
            }
            Some('/') => {
                self.input.next();
                let rhs = self.parse_factor()?.ok_or_else(|| self.input_error())?;
                let lhs = Expr::Div(Box::new(lhs), Box::new(rhs));

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
        self.skip_whitespace();

        let res = match self.input.peek() {
            Some('^') => {
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
        self.skip_whitespace();

        match self.input.peek() {
            Some('-') => {
                self.input.next();
                let expr = self.parse_exponent()?.ok_or_else(|| self.input_error())?;

                Ok(Some(Expr::Neg(Box::new(expr))))
            }
            Some('(') => {
                self.input.next();
                let expr = self.parse_expr()?.ok_or_else(|| self.input_error())?;

                let Some(c) = self.input.next() else {
                    return Err(Error::UnexpectedEOF.into());
                };

                if c != ')' {
                    return Err(Error::UnexpectedChar(c).into());
                }

                Ok(Some(expr))
            }
            Some(c) if c.is_numeric() => self.parse_number(),
            _ => Ok(None),
        }
    }

    fn parse_number(&mut self) -> Result<Option<Expr>> {
        match self.input.peek() {
            Some('0') => {
                self.input.next();
                match self.input.peek() {
                    Some('x') => {
                        self.input.next();
                        self.parse_hex_number()
                    }
                    Some('o') => {
                        self.input.next();
                        self.parse_octal_number()
                    }
                    Some('b') => {
                        self.input.next();
                        self.parse_binary_number()
                    }
                    Some(c) if c.is_numeric() => self.parse_octal_number(),
                    _ => Ok(Some(Expr::Number(Decimal::ZERO))),
                }
            }
            _ => self.parse_decimal_number(),
        }
    }

    // parse a number in decimal format, e.g. `12`, `5.5`, `4.01e-8`
    fn parse_decimal_number(&mut self) -> Result<Option<Expr>> {
        // integer part of the number `[12].34e-12`
        let int_part = self
            .read_numeric_string(10)
            .expect("number starts with number");

        // decimal part of the number `12[.34]e-12`
        let dec_part = match self.input.peek() {
            Some('.') => {
                self.input.next();
                self.read_numeric_string(10)
            }
            _ => None,
        };

        // exponent part of the number `12.34[e-12]`
        let exp_part = match self.input.peek() {
            Some('e') | Some('E') => {
                self.input.next();
                let neg = match self.input.peek() {
                    Some('-') => {
                        self.input.next();
                        true
                    }
                    _ => false,
                };

                self.read_numeric_string(10)
                    .map(|input| i32::from_str_radix(&input, 10).unwrap())
                    .map(|num| if neg { num * -1 } else { num })
            }
            _ => None,
        };

        // get the number of decimals from the parsed number; important for calculating the scale
        // of the Decimal type later
        let decimal_places = dec_part.as_ref().map(|dec| dec.len() as i32).unwrap_or(0);

        // all of the parsed numerical data (excluding the exponent)
        let mut number_part = if let Some(dec_part) = dec_part {
            int_part + &dec_part
        } else {
            int_part
        };

        // "scale" is the number of decimal places into the numerical part of the number. `1234`
        // with a scale of 2 is `12.34`. Going the other way, the scale is a combination of the
        // number of decimal places plus the inverse of the exponent part:
        //
        // * `12.34e-1` = `1.234`: 2 + 1 (scale): `1234` scale 3
        // * `12.34e1` = `123.4`: 2 - 1 (scale): `1234` scale 1
        let mut scale = exp_part
            .map(|exp| decimal_places + exp * -1)
            .unwrap_or(decimal_places);

        // scale must be a positive number, so the number part can be padded with zeroes
        // to correctly align the decimal point:
        //
        // * `12.34e4` has a scale of -2 (2 decimal places minus 4 exponent). It will be changed
        //   to `123400` with a scale of 0
        if scale < 0 {
            for _ in 0..(scale * -1) {
                number_part.push('0');
            }

            scale = 0;
        }

        let value = i128::from_str_radix(&number_part, 10).unwrap();
        Ok(Some(Expr::Number(Decimal::try_from_i128_with_scale(
            value,
            scale as u32,
        )?)))
    }

    fn parse_hex_number(&mut self) -> Result<Option<Expr>> {
        let Some(number) = self.read_numeric_string(16) else {
            return match self.input.next() {
                Some(c) => Err(Error::UnexpectedChar(c).into()),
                None => Err(Error::UnexpectedEOF.into()),
            };
        };

        let value = Decimal::from_str_radix(&number, 16)?;
        Ok(Some(Expr::Number(value)))
    }

    fn parse_octal_number(&mut self) -> Result<Option<Expr>> {
        let Some(number) = self.read_numeric_string(8) else {
            return match self.input.next() {
                Some(c) => Err(Error::UnexpectedChar(c).into()),
                None => Err(Error::UnexpectedEOF.into()),
            };
        };

        let value = Decimal::from_str_radix(&number, 8)?;
        Ok(Some(Expr::Number(value)))
    }

    fn parse_binary_number(&mut self) -> Result<Option<Expr>> {
        let Some(number) = self.read_numeric_string(2) else {
            return match self.input.next() {
                Some(c) => Err(Error::UnexpectedChar(c).into()),
                None => Err(Error::UnexpectedEOF.into()),
            };
        };

        let value = Decimal::from_str_radix(&number, 2)?;
        Ok(Some(Expr::Number(value)))
    }

    const LOWER_DIGITS: [char; 16] = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    ];
    const UPPER_DIGITS: [char; 16] = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
    ];
    fn read_numeric_string(&mut self, radix: u32) -> Option<String> {
        assert!(radix == 2 || radix == 8 || radix == 10 || radix == 16);
        let mut chars = Vec::new();

        let lower_digits = &Parser::LOWER_DIGITS[0..radix as usize];
        let upper_digits = &Parser::UPPER_DIGITS[0..radix as usize];

        loop {
            match self.input.peek() {
                Some('_') => (),
                Some(c) if c.is_numeric() && lower_digits.contains(c) => {
                    chars.push(*c);
                }
                Some(c) if c.is_lowercase() && lower_digits.contains(c) => {
                    chars.push(*c);
                }
                Some(c) if c.is_uppercase() && upper_digits.contains(c) => {
                    chars.push(*c);
                }
                _ => break,
            }

            self.input.next();
        }

        if chars.len() == 0 {
            None
        } else {
            Some(chars.into_iter().collect())
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

    fn input_error(&mut self) -> eyre::Report {
        match self.input.next() {
            Some(c) => Error::UnexpectedChar(c),
            None => Error::UnexpectedEOF,
        }
        .into()
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use super::*;

    #[test]
    fn parse() {
        let mut p = Parser::new("12.0 + -4e2 * 3^6E-4 / 5.45 - -2");
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

        let mut p = Parser::new("12 - 4 6 + 3");
        let err = get_err(p.parse());
        assert!(matches!(err, Error::UnexpectedChar('6')));
    }

    #[test]
    fn expr() {
        let mut p = Parser::new("1+2+3 2/3+4-5*6 abc");

        let expr = get_expr(p.parse_expr());
        let (lhs, rhs) = expr.add();
        assert_eq!(*rhs.number(), dec!(3));
        let (lhs, rhs) = lhs.add();
        assert_eq!(*lhs.number(), dec!(1));
        assert_eq!(*rhs.number(), dec!(2));

        let expr = get_expr(p.parse_expr());
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

        let res = p.parse_expr();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn add_sub() {
        let mut p = Parser::new("+2-(3+4)");

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

        let mut p = Parser::new("+a");
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedChar('a')));

        let mut p = Parser::new("+");
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedEOF));

        let mut p = Parser::new("-b");
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(1))));
        assert!(matches!(err, Error::UnexpectedChar('b')));

        let mut p = Parser::new("-");
        let err = get_err(p.parse_add_sub(Expr::Number(dec!(3))));
        assert!(matches!(err, Error::UnexpectedEOF));
    }

    #[test]
    fn term() {
        let mut p = Parser::new("3/4*2 -2*(4*3)^6.1 abc");

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

        let res = p.parse_term();
        assert!(matches!(res, Ok(None)));
    }

    #[test]
    fn mul_div() {
        let mut p = Parser::new("*3.2E-2/4^-2*5");
        let expr = get_expr(p.parse_mul_div(Expr::Number(dec!(2))));

        let (lhs, rhs) = expr.mul();
        assert_eq!(*rhs.number(), dec!(5));

        let (lhs, rhs) = lhs.div();
        assert_eq!(*rhs.pow().0.number(), dec!(4));
        assert_eq!(*rhs.pow().1.neg().number(), dec!(2));

        let (lhs, rhs) = lhs.mul();
        assert_eq!(*lhs.number(), dec!(2));
        assert_eq!(*rhs.number(), dec!(0.032));

        let mut p = Parser::new("*)");
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedChar(')')));

        let mut p = Parser::new("*");
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(99))));
        assert!(matches!(err, Error::UnexpectedEOF));

        let mut p = Parser::new("/^");
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedChar('^')));

        let mut p = Parser::new("/");
        let err = get_err(p.parse_mul_div(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedEOF));
    }

    #[test]
    fn factor() {
        let mut p = Parser::new("2 3.1e-2^4 -(2^-4.1)^3.14 *");

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
        let mut p = Parser::new("^3^4");
        let expr = get_expr(p.parse_power(Expr::Number(dec!(2))));
        let (base, exponent) = expr.pow();
        assert_eq!(*base.number(), dec!(2));
        let (base, exponent) = exponent.pow();
        assert_eq!(*base.number(), dec!(3));
        assert_eq!(*exponent.number(), dec!(4));

        let mut p = Parser::new("^$");
        let err = get_err(p.parse_power(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedChar('$')));

        let mut p = Parser::new("^");
        let err = get_err(p.parse_power(Expr::Number(dec!(2))));
        assert!(matches!(err, Error::UnexpectedEOF));
    }

    #[test]
    fn exponent() {
        let mut p = Parser::new("-4 (1) -(-1.1) () (#)");
        let expr = get_expr(p.parse_exponent());
        assert_eq!(*expr.neg().number(), dec!(4));
        let expr = get_expr(p.parse_exponent());
        assert_eq!(*expr.number(), dec!(1));
        let expr = get_expr(p.parse_exponent());
        assert_eq!(*expr.neg().neg().number(), dec!(1.1));
        let err = get_err(p.parse_exponent());
        assert!(matches!(err, Error::UnexpectedChar(')')));
        let err = get_err(p.parse_exponent());
        assert!(matches!(err, Error::UnexpectedChar('#')));
    }

    #[test]
    fn number() {
        let mut p = Parser::new("12.34 0x1a 0b11 0o11 011 11 0");

        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(12.34));

        p.input.next();
        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(26));

        p.input.next();
        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(3));

        p.input.next();
        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(9));

        p.input.next();
        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(9));

        p.input.next();
        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(11));

        p.input.next();
        let expr = get_expr(p.parse_number());
        assert_eq!(*expr.number(), dec!(0));
    }

    #[test]
    fn decimal_number() {
        let mut p = Parser::new("12.34e-1 12.34e1 12.34e4 12. 0.34 1e-2");

        let expr = get_expr(p.parse_decimal_number());
        assert_eq!(*expr.number(), dec!(1.234));

        p.input.next();
        let expr = get_expr(p.parse_decimal_number());
        assert_eq!(*expr.number(), dec!(123.4));

        p.input.next();
        let expr = get_expr(p.parse_decimal_number());
        assert_eq!(*expr.number(), dec!(123400));

        p.input.next();
        let expr = get_expr(p.parse_decimal_number());
        assert_eq!(*expr.number(), dec!(12));

        p.input.next();
        let expr = get_expr(p.parse_decimal_number());
        assert_eq!(*expr.number(), dec!(0.34));

        p.input.next();
        let expr = get_expr(p.parse_decimal_number());
        assert_eq!(*expr.number(), dec!(0.01));
    }

    #[test]
    fn hex_number() {
        // parser should read "FF_ab_01_" and produce number 0xFFAB01 (16755457 dec)
        let mut p = Parser::new("FF_ab_01_xyz");
        let expr = get_expr(p.parse_hex_number());
        assert_eq!(*expr.number(), dec!(16_755_457))
    }

    #[test]
    fn octal_number() {
        // parser should read "6_17" and produce number 0617 (399 dec)
        let mut p = Parser::new("6_179");
        let expr = get_expr(p.parse_octal_number());
        assert_eq!(*expr.number(), dec!(399));
    }

    #[test]
    fn binary_number() {
        // parser should read "11_01" and produce number 0b1101 (13 dec)
        let mut p = Parser::new("11_012");
        let expr = get_expr(p.parse_binary_number());
        assert_eq!(*expr.number(), dec!(13));
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
}
