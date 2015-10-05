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
            Some(Token::CloseParen) => parse_error!("Unexpected closing paren"),
            Some(_) => unimplemented!(),
            None => Ok(None)
        }
    }

    fn parse_list(&mut self) -> Result<Value, ParseError> {
        match self.tokens.peek() {
            Some(&Token::CloseParen) => {
                // Consume closing paren.
                self.tokens.next();
                Ok(Value::Nil)
            },
            Some(_) => {
                let car = try!(self.parse_value()).unwrap();
                let cdr = try!(self.parse_list());
                Ok(Value::Pair(Box::new(car), Box::new(cdr)))
            },
            None => parse_error!("Expected closing paren")
        }
    }
}
