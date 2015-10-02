use std::iter::Peekable;

enum Token {
    OpenParen,
    OpenVectorParen,
    CloseParen,
    Identifier(String),
    String(String),
    Character(char),
    Number(i64),
    Boolean(bool),
    Dot,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteList,
    EOF
}

struct SyntaxError {
    msg: String,
    line: u64,
    column: u64
}

macro_rules! syntax_error {
    ($lexer:ident, $($arg:tt)*) => (
        return Err(SyntaxError{msg: format!($($arg)*), line: $lexer.line, column: $lexer.column})
    )
}

struct Lexer<I: Iterator<Item=char>> {
    input: Peekable<I>,
    line: u64,
    column: u64
}

impl<I: Iterator<Item=char>> Lexer<I> {
    pub fn new(input: I) -> Self {
        Lexer {input: input.peekable(), line: 1, column: 1}
    }

    pub fn advance(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    pub fn lex_all(&mut self) -> Result<Vec<Token>, SyntaxError> {
        let mut tokens = Vec::new();
        loop {
            match try!(self.lex_token()) {
                Token::EOF => return Ok(tokens),
                t @ _ => tokens.push(t)
            }
        }
    }

    pub fn lex_token(&mut self) -> Result<Token, SyntaxError> {
        try!(self.read_while(|c| c.is_whitespace()));

        let ch = match self.input.next() {
            Some(c) => c,
            None => return Ok(Token::EOF)
        };

        match ch {
            '(' => return Ok(Token::OpenParen),
            _ => syntax_error!(self, "test error")
        }
    }

    fn read_while<P: Fn(char) -> bool>(&mut self, pred: P) ->
        Result<String, SyntaxError>
    {
        let mut s = String::new();
        loop {
            let next = match self.input.peek() {
                Some(c) if pred(*c) => *c,
                _ => return Ok(s)
            };

            s.push(self.input.next().unwrap());
            self.advance(next);
        }
    }
}

#[test]
fn read_while_with_nothing_to_read() {
    let s = String::from("hello world");
    let mut lexer = Lexer::new(s.chars());
    assert!(lexer.read_while(|c| c == 'x').ok().unwrap() ==
        String::from(""));
    assert!(*lexer.input.peek().unwrap() == 'h');
}

#[test]
fn read_while_with_something_to_read() {
    let s = String::from(" \t \n   hello world");
    let mut lexer = Lexer::new(s.chars());
    assert!(lexer.read_while(|c| c.is_whitespace()).ok().unwrap() ==
        String::from(" \t \n   "));
}
