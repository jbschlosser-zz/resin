use lexer::{self, Token};
use std::iter::Peekable;
use value::Value;

struct Parser<I: Iterator<Item=Token>> {
    tokens: Peekable<I>
}

struct ParseError {
    msg: String
}

macro_rules! parse_error {
    ($($arg:tt)*) => (
        return Err(ParseError{msg: format!($($arg)*)})
    )
}

impl<I: Iterator<Item=Token>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Parser {tokens: tokens.peekable()}
    }

    pub fn parse_all(&mut self) -> Result<Vec<Value>, ParseError> {
        let mut values = Vec::new();
        loop {
            match try!(self.parse_value()) {
                Some(v) => values.push(v),
                None => return Ok(values)
            }
        }
    }

    pub fn parse_value(&mut self) -> Result<Option<Value>, ParseError> {
        match self.tokens.next() {
            Some(Token::Identifier(s)) => Ok(Some(Value::Symbol(s))),
            Some(Token::String(s)) => Ok(Some(Value::String(s))),
            Some(Token::Character(c)) => Ok(Some(Value::Character(c))),
            Some(Token::Number(n)) => Ok(Some(Value::Number(n))),
            Some(Token::Boolean(b)) => Ok(Some(Value::Boolean(b))),
            Some(Token::OpenParen) => Ok(Some(try!(self.parse_list()))),
            Some(_) => unimplemented!(),
            None => Ok(None)
        }
    }

    fn consume_if(&mut self, token: &Token) -> bool {
        let res = self.tokens.peek().map_or(false, |t| t == token);
        if res {
            self.tokens.next().unwrap();
        }
        res
    }

    fn parse_list(&mut self) -> Result<Value, ParseError> {
        return Ok(Value::Nil);
    }
}
