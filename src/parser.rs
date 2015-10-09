use datum::Datum;
use lexer::{self, Token};
use std::cell::RefCell;
use std::iter::Peekable;
use std::rc::Rc;

struct Parser<I: Iterator<Item=Token>> {
    tokens: Peekable<I>
}

#[derive(Debug, PartialEq, Eq)]
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

    pub fn parse_all(&mut self) -> Result<Vec<Datum>, ParseError> {
        let mut values = Vec::new();
        loop {
            if self.tokens.peek() == None { return Ok(values); }
            values.push(try!(self.parse_datum()));
        }
    }

    pub fn parse_datum(&mut self) -> Result<Datum, ParseError> {
        match self.tokens.next() {
            Some(Token::Identifier(s)) => Ok(Datum::Symbol(s)),
            Some(Token::String(s)) => Ok(Datum::String(s)),
            Some(Token::Character(c)) => Ok(Datum::Character(c)),
            Some(Token::Number(n)) => Ok(Datum::Number(n)),
            Some(Token::Boolean(b)) => Ok(Datum::Boolean(b)),
            Some(Token::OpenParen) => Ok(try!(self.parse_list())),
            Some(Token::CloseParen) => parse_error!("Unexpected closing paren"),
            Some(Token::OpenVectorParen) => Ok(try!(self.parse_vector())),
            Some(Token::Dot) => parse_error!("Unexpected dot"),
            Some(Token::Quote) => {
                let car = Datum::Symbol("quote".to_string());
                let cdar = try!(self.parse_datum());
                Ok(Datum::pair(car, Datum::pair(cdar, Datum::EmptyList)))
            },
            Some(Token::Quasiquote) => {
                let car = Datum::Symbol("quasiquote".to_string());
                let cdar = try!(self.parse_datum());
                Ok(Datum::pair(car, Datum::pair(cdar, Datum::EmptyList)))
            },
            Some(Token::Unquote) => {
                let car = Datum::Symbol("unquote".to_string());
                let cdar = try!(self.parse_datum());
                Ok(Datum::pair(car, Datum::pair(cdar, Datum::EmptyList)))
            },
            Some(Token::UnquoteList) => {
                let car = Datum::Symbol("unquote-splicing".to_string());
                let cdar = try!(self.parse_datum());
                Ok(Datum::pair(car, Datum::pair(cdar, Datum::EmptyList)))
            },
            None => parse_error!("Expected datum or closing parenthesis")
        }
    }

    fn parse_list(&mut self) -> Result<Datum, ParseError> {
        if self.consume_if(|t| t == Token::CloseParen) {
            return Ok(Datum::EmptyList);
        }

        let car = try!(self.parse_datum());
        let cdr = if self.consume_if(|t| t == Token::Dot) {
            let cdr = try!(self.parse_datum());
            if !self.consume_if(|t| t == Token::CloseParen) {
                parse_error!("Expected closing parenthesis");
            }
            cdr
        } else {
            try!(self.parse_list())
        };
        Ok(Datum::Pair(Box::new(car), Box::new(cdr)))
    }

    fn parse_vector(&mut self) -> Result<Datum, ParseError> {
        let mut vec = Vec::new();
        loop {
            if self.consume_if(|t| t == Token::CloseParen) {
                return Ok(Datum::Vector(Rc::new(RefCell::new(vec))));
            }
            vec.push(try!(self.parse_datum()));
        }
    }

    // Consumes if the token matches the given predicate; does nothing
    // otherwise.
    fn consume_if<F: Fn(Token) -> bool>(&mut self, pred: F) -> bool {
        let consume = match self.tokens.peek() {
            Some(t) => pred(t.clone()),
            _ => false
        };
        if consume {
            self.tokens.next().unwrap();
        }
        consume
    }
}

macro_rules! check_parse {
    ($input:expr, $result:expr) => {{
        let mut lexer = lexer::Lexer::new($input.chars());
        let tokens: Vec<Token> = lexer.lex_all().ok().expect("Failed to lex input");
        let mut parser = Parser::new(tokens.into_iter());
        let data = parser.parse_all();
        assert!(data == $result);
    }}
}

#[test]
fn parse_empty_list() {
    check_parse!("()", Ok(vec![Datum::EmptyList]));
}

#[test]
fn parse_basic_list() {
    check_parse!("(+ 2 2)", Ok(vec![
        Datum::pair(
            Datum::Symbol("+".to_string()),
            Datum::pair(
                Datum::Number(2),
                Datum::pair(
                    Datum::Number(2),
                    Datum::EmptyList)))]));
}

#[test]
fn parse_nested_list() {
    check_parse!("(+ 2 (* 3 4))", Ok(vec![
        Datum::pair(
            Datum::Symbol("+".to_string()),
            Datum::pair(
                Datum::Number(2),
                Datum::pair(
                    Datum::pair(
                        Datum::Symbol("*".to_string()),
                        Datum::pair(
                            Datum::Number(3),
                            Datum::pair(
                                Datum::Number(4),
                                Datum::EmptyList))),
                    Datum::EmptyList)))]));
}
