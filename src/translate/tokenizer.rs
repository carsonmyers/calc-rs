use eyre::Result;
use rust_decimal::Decimal;

use crate::input::Input;
use crate::translate::error::*;
use crate::translate::token::*;

pub struct Tokenizer<'a> {
    input: &'a mut Input,
}

impl<'a> Tokenizer<'a> {
    const LOWER_DIGITS: [char; 16] = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    ];
    const UPPER_DIGITS: [char; 16] = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
    ];

    pub fn new(input: &'a mut Input) -> Self {
        Self { input }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        self.skip_whitespace();

        match self.input.peek() {
            Some('+') => self.simple_token(TokenKind::Plus),
            Some('-') => self.simple_token(TokenKind::Minus),
            Some('/') => self.simple_token(TokenKind::Slash),
            Some('&') => self.simple_token(TokenKind::And),
            Some('|') => self.simple_token(TokenKind::Pipe),
            Some('^') => self.simple_token(TokenKind::Caret),
            Some('*') => {
                self.input.next();
                if let Some('*') = self.input.peek() {
                    self.simple_token(TokenKind::StarStar)
                } else {
                    let span = self.input.take_lexeme().to_span();
                    Ok(Some(Token {
                        span,
                        kind: TokenKind::Star,
                    }))
                }
            }
            Some(c) if c.is_numeric() => self.parse_number(),
            _ => unimplemented!(),
        }
    }

    fn parse_number(&mut self) -> Result<Option<Token>> {
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
                    _ => Ok(Some(Token {
                        span: self.input.take_lexeme().to_span(),
                        kind: TokenKind::Number(Decimal::ZERO),
                    })),
                }
            }
            _ => self.parse_decimal_number(),
        }
    }

    // parse a number in decimal format, e.g. `12`, `5.5`, `4.01e-8`
    fn parse_decimal_number(&mut self) -> Result<Option<Token>> {
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

        // get the number of decimals from the parsed number; important for calculating
        // the scale of the Decimal type later
        let decimal_places = dec_part.as_ref().map(|dec| dec.len() as i32).unwrap_or(0);

        // all of the parsed numerical data (excluding the exponent)
        let mut number_part = if let Some(dec_part) = dec_part {
            int_part + &dec_part
        } else {
            int_part
        };

        // "scale" is the number of decimal places into the numerical part of the
        // number. `1234` with a scale of 2 is `12.34`. Going the other way, the
        // scale is a combination of the number of decimal places plus the
        // inverse of the exponent part:
        //
        // * `12.34e-1` = `1.234`: 2 + 1 (scale): `1234` scale 3
        // * `12.34e1` = `123.4`: 2 - 1 (scale): `1234` scale 1
        let mut scale = exp_part
            .map(|exp| decimal_places + exp * -1)
            .unwrap_or(decimal_places);

        // scale must be a positive number, so the number part can be padded with zeroes
        // to correctly align the decimal point:
        //
        // * `12.34e4` has a scale of -2 (2 decimal places minus 4 exponent). It will be
        //   changed to `123400` with a scale of 0
        if scale < 0 {
            for _ in 0..(scale * -1) {
                number_part.push('0');
            }

            scale = 0;
        }

        let int_value = i128::from_str_radix(&number_part, 10).unwrap();
        let value = Decimal::try_from_i128_with_scale(int_value, scale as u32)?;
        Ok(Some(Token {
            span: self.input.take_lexeme().to_span(),
            kind: TokenKind::Number(value),
        }))
    }

    fn parse_hex_number(&mut self) -> Result<Option<Token>> {
        let Some(number) = self.read_numeric_string(16) else {
            return match self.input.next() {
                Some(c) => Err(Error::UnexpectedChar(c).into()),
                None => Err(Error::UnexpectedEOF.into()),
            };
        };

        let value = Decimal::from_str_radix(&number, 16)?;
        Ok(Some(Token {
            span: self.input.take_lexeme().to_span(),
            kind: TokenKind::Number(value),
        }))
    }

    fn parse_octal_number(&mut self) -> Result<Option<Token>> {
        let Some(number) = self.read_numeric_string(8) else {
            return match self.input.next() {
                Some(c) => Err(Error::UnexpectedChar(c).into()),
                None => Err(Error::UnexpectedEOF.into()),
            };
        };

        let value = Decimal::from_str_radix(&number, 8)?;
        Ok(Some(Token {
            span: self.input.take_lexeme().to_span(),
            kind: TokenKind::Number(value),
        }))
    }

    fn parse_binary_number(&mut self) -> Result<Option<Token>> {
        let Some(number) = self.read_numeric_string(2) else {
            return match self.input.next() {
                Some(c) => Err(Error::UnexpectedChar(c).into()),
                None => Err(Error::UnexpectedEOF.into()),
            };
        };

        let value = Decimal::from_str_radix(&number, 2)?;
        Ok(Some(Token {
            span: self.input.take_lexeme().to_span(),
            kind: TokenKind::Number(value),
        }))
    }

    fn read_numeric_string(&mut self, radix: u32) -> Option<String> {
        assert!(radix == 2 || radix == 8 || radix == 10 || radix == 16);
        let mut chars = Vec::new();

        let lower_digits = &Tokenizer::LOWER_DIGITS[0..radix as usize];
        let upper_digits = &Tokenizer::UPPER_DIGITS[0..radix as usize];

        loop {
            match self.input.peek() {
                Some('_') => (),
                Some(c) if c.is_numeric() && lower_digits.contains(&c) => {
                    chars.push(c);
                }
                Some(c) if c.is_lowercase() && lower_digits.contains(&c) => {
                    chars.push(c);
                }
                Some(c) if c.is_uppercase() && upper_digits.contains(&c) => {
                    chars.push(c);
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
        loop {
            match self.input.peek() {
                Some(c) if c.is_whitespace() => {
                    self.input.next();
                }
                _ => break,
            }
        }

        self.input.take_lexeme();
    }

    fn simple_token(&mut self, kind: TokenKind) -> Result<Option<Token>> {
        self.input.next();

        let span = self.input.take_lexeme().to_span();
        Ok(Some(Token { span, kind }))
    }
}

pub struct Tokens {
    token_iter: Box<dyn Iterator<Item = Result<Token>>>,
}

impl<'a> From<Tokenizer<'a>> for Tokens {
    fn from(value: Tokenizer<'a>) -> Self {
        Self {
            token_iter: Box::new(value),
        }
    }
}

impl From<Vec<Token>> for Tokens {
    fn from(value: Vec<Token>) -> Self {
        Self {
            token_iter: Box::new(value.into_iter().map(|tok| Ok(tok))),
        }
    }
}

impl Iterator for Tokens {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_iter.next()
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Some(t)) => Some(Ok(t)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use rust_decimal_macros::dec;

    use super::*;

    #[test]
    fn number() {
        let mut p = Tokenizer::new(&mut Input::new("12.34 0x1a 0b11 0o11 011 11 0"));

        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(12.34));

        p.skip_whitespace();
        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(26));

        p.skip_whitespace();
        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(3));

        p.skip_whitespace();
        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(9));

        p.skip_whitespace();
        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(9));

        p.skip_whitespace();
        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(11));

        p.skip_whitespace();
        let tok = get_tok(p.parse_number());
        assert_eq!(*tok.number(), dec!(0));
    }

    #[test]
    fn decimal_number() {
        let mut input = Input::new("12.34e-1 12.34e1 12.34e4 12. 0.34 1e-2");
        let mut t = Tokenizer::new(&mut input);

        let tok = get_tok(t.parse_decimal_number());
        assert_eq!(*tok.number(), dec!(1.234));

        t.skip_whitespace();
        let tok = get_tok(t.parse_decimal_number());
        assert_eq!(*tok.number(), dec!(123.4));

        t.skip_whitespace();
        let tok = get_tok(t.parse_decimal_number());
        assert_eq!(*tok.number(), dec!(123400));

        t.skip_whitespace();
        let tok = get_tok(t.parse_decimal_number());
        assert_eq!(*tok.number(), dec!(12));

        t.skip_whitespace();
        let tok = get_tok(t.parse_decimal_number());
        assert_eq!(*tok.number(), dec!(0.34));

        t.skip_whitespace();
        let tok = get_tok(t.parse_decimal_number());
        assert_eq!(*tok.number(), dec!(0.01));
    }

    #[test]
    fn hex_number() {
        // parser should read "FF_ab_01_" and produce number 0xFFAB01 (16755457 dec)
        let mut t = Tokenizer::new(&mut Input::new("FF_ab_01_xyz"));
        let tok = get_tok(t.parse_hex_number());
        assert_eq!(*tok.number(), dec!(16_755_457))
    }

    #[test]
    fn octal_number() {
        // parser should read "6_17" and produce number 0617 (399 dec)
        let mut t = Tokenizer::new(&mut Input::new("6_179"));
        let tok = get_tok(t.parse_octal_number());
        assert_eq!(*tok.number(), dec!(399));
    }

    #[test]
    fn binary_number() {
        // parser should read "11_01" and produce number 0b1101 (13 dec)
        let mut t = Tokenizer::new(&mut Input::new("11_012"));
        let tok = get_tok(t.parse_binary_number());
        assert_eq!(*tok.number(), dec!(13));
    }

    fn get_tok(res: Result<Option<Token>>) -> Token {
        let res = match res {
            Err(err) => panic!("unexpected error: {:?}", err),
            Ok(res) => res,
        };

        assert!(res.is_some(), "result was None");
        res.unwrap()
    }
}
