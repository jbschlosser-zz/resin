use std::fmt;
use std::iter::Peekable;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
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
    UnquoteList
}

pub struct SyntaxError {
    pub msg: String,
    pub line: u64,
    pub column: u64
}

impl fmt::Debug for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Syntax error @ (line {}, column {}): {}",
            self.line, self.column, &self.msg)
    }
}

macro_rules! syntax_error {
    ($lexer:ident, $($arg:tt)*) => (
        return Err(SyntaxError{msg: format!($($arg)*), line: $lexer.line,
            column: $lexer.column})
    )
}

pub struct Lexer<I: Iterator<Item=char>> {
    input: Peekable<I>,
    line: u64,
    column: u64
}

impl<I: Iterator<Item=char>> Lexer<I> {
    pub fn new(input: I) -> Self {
        Lexer {input: input.peekable(), line: 1, column: 1}
    }

    pub fn lex_all(&mut self) -> Result<Vec<Token>, SyntaxError> {
        let mut tokens = Vec::new();
        loop {
            match try!(self.lex_token()) {
                Some(t) => tokens.push(t),
                None => return Ok(tokens)
            }
        }
    }

    pub fn lex_token(&mut self) -> Result<Option<Token>, SyntaxError> {
        self.read_while(|c| c.is_whitespace());

        let ch = match self.next_char() {
            Some(c) => c,
            None => return Ok(None)
        };

        match ch {
            '(' => Ok(Some(Token::OpenParen)),
            ')' => Ok(Some(Token::CloseParen)),
            '\'' => Ok(Some(Token::Quote)),
            '`' => Ok(Some(Token::Quasiquote)),
            ',' => {
                match self.input.peek() {
                    Some(&'@') => {
                        self.next_char().unwrap();
                        Ok(Some(Token::UnquoteList))
                    },
                    _ => Ok(Some(Token::Unquote))
                }
            },
            // Skip these characters; they're reserved for future use.
            '[' | ']' | '{' | '}' | '|' => self.lex_token(),
            // Deal with the various hash lexes.
            '#' => {
                match self.next_char() {
                    Some('t') => Ok(Some(Token::Boolean(true))),
                    Some('f') => Ok(Some(Token::Boolean(false))),
                    Some('(') => Ok(Some(Token::OpenVectorParen)),
                    Some('\\') => {
                        match self.next_char() {
                            Some(c) => Ok(Some(Token::Character(c))),
                            None => syntax_error!(self,
                                "Expected character after #\\")
                        }
                    },
                    Some('e') | Some('i') | Some('b') | Some('o') | Some('d') |
                        Some('x') => unimplemented!(),
                    None => syntax_error!(self, "Expected character after #"),
                    _ => syntax_error!(self, "Unexpected character after #")
                }
            },
            // Skip the rest of the line for the comment.
            ';' => {
                self.read_while(|c| c != '\n');
                self.lex_token()
            },
            // Handle dots and ellipses.
            '.' => {
                match self.input.peek() {
                    Some(&'.') => {
                        // Check for ellipses ...
                        self.next_char().unwrap();
                        match self.input.next() {
                            Some('.') => Ok(Some(Token::Identifier(
                                String::from("...")))),
                            _ => syntax_error!(self,
                                "Identifiers cannot begin with .")
                        }
                    },
                    Some(&d) if is_identifier_char(d) =>
                        syntax_error!(self, "Identifiers cannot begin with ."),
                    _ => Ok(Some(Token::Dot))
                }
            },
            // Lex string.
            '"' => Ok(Some(try!(self.lex_string()))),
            // Lex number.
            c if c.is_digit(10) => Ok(Some(try!(self.lex_number(c)))),
            // +/- can indicate the beginning of a number or be an identifier.
            c if c == '+' || c == '-' => {
                match self.input.peek() {
                    Some(&d) if d.is_digit(10) =>
                        Ok(Some(try!(self.lex_number(c)))),
                    Some(&d) if is_identifier_char(d) => syntax_error!(self,
                        "Identifiers cannot begin with +/-"),
                    _ => {
                        let mut s = String::new();
                        s.push(c);
                        Ok(Some(Token::Identifier(s)))
                    }
                }
            },
            // Lex identifier. Note that identifiers cannot start with a digit,
            // a plus sign, a minus sign, or a dot. Those are lexed higher up.
            c if is_identifier_char(c) => Ok(Some(self.lex_identifier(c))),
            _ => unimplemented!()
        }
    }

    // Moves the iterator and the line/column markers.
    fn next_char(&mut self) -> Option<char> {
        match self.input.next() {
            Some(c) => {
                if c == '\n' {
                    self.line += 1;
                    self.column = 1;
                } else {
                    self.column += 1;
                }

                Some(c)
            },
            None => None
        }
    }

    fn read_while<P: Fn(char) -> bool>(&mut self, pred: P) -> String
    {
        let mut s = String::new();
        loop {
            let next = match self.input.peek() {
                Some(c) if pred(*c) => *c,
                _ => return s
            };

            s.push(self.next_char().unwrap());
        }
    }

    fn lex_string(&mut self) -> Result<Token, SyntaxError> {
        let mut s = String::new();
        loop {
            let part = self.read_while(|c| c != '\"' && c != '\n' && c != '\\');
            s.push_str(&part);
            match self.next_char() {
                Some('\"') => return Ok(Token::String(s)),
                Some('\n') => syntax_error!(self, "Unterminated string"),
                Some('\\') => {
                    // Handle escape characters.
                    // TODO: Handle the rest.
                    match self.next_char() {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('"') => s.push('"'),
                        Some(_) => unimplemented!(),
                        None => syntax_error!(self,
                            "Unterminated escape sequence")
                    }
                },
                _ => syntax_error!(self, "Unterminated string")
            }
        }
    }

    fn lex_identifier(&mut self, first: char) -> Token {
        let mut identifier = String::new();
        identifier.push(first);
        identifier.push_str(&self.read_while(|c| is_identifier_char(c)));
        Token::Identifier(identifier.to_lowercase())
    }

    fn lex_number(&mut self, first: char) -> Result<Token, SyntaxError> {
        // TODO: Handle decimal points and arbitrary precision numbers.
        let mut s = String::new();
        // Leave out any plus sign so the number can be properly parsed.
        if first != '+' { s.push(first); }
        s.push_str(&self.read_while(|c| c.is_digit(10)));
        match i64::from_str_radix(&s, 10) {
            Ok(n) => Ok(Token::Number(n)),
            Err(e) => syntax_error!(self, "Integer not in valid form")
        }
    }
}

fn is_identifier_char(ch: char) -> bool {
    is_a_z(ch) || is_extended_alphabetic(ch) || ch.is_digit(10)
}

fn is_a_z(ch: char) -> bool {
    // TODO: There has to be a better way to determine this?
    match ch {
        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' |
        'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' |
        's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
        'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' |
        'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' |
        'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' => {
            true
        },
        _ => false
    }
}

fn is_extended_alphabetic(ch: char) -> bool {
    match ch {
        '!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' |
        ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~' => {
            true
        },
        _ => false
    }
}

#[test]
fn lex_math() {
    let s = String::from("(+ 2 (* 100 5))");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen, Token::Identifier(String::from("+")),
        Token::Number(2), Token::OpenParen,
        Token::Identifier(String::from("*")),
        Token::Number(100), Token::Number(5), Token::CloseParen,
        Token::CloseParen
    ]);
}

#[test]
fn lex_numbers() {
    let s = String::from("(1234567890 +1234567890 -1234567890)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::Number(1234567890),
        Token::Number(1234567890),
        Token::Number(-1234567890),
        Token::CloseParen
    ]);
}

#[test]
fn lex_identifiers() {
    let s = String::from("(abcdefghijklmnopqrstuvwxyz!@$%^&*.~ AbCdEfG)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::Identifier(String::from("abcdefghijklmnopqrstuvwxyz!@$%^&*.~")),
        Token::Identifier(String::from("abcdefg")),
        Token::CloseParen
    ]);
}

#[test]
fn lex_plus_minus_dot_identifiers() {
    let s = String::from("(+ - . a+-.b +");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::Identifier(String::from("+")),
        Token::Identifier(String::from("-")),
        Token::Dot,
        Token::Identifier(String::from("a+-.b")),
        Token::Identifier(String::from("+"))
    ]);
}

#[test]
fn lex_identifier_starting_with_plus() {
    let s = String::from("(+abcd 1 2)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    let error = tokens.err().unwrap();
    assert!(error.line == 1 && error.column == 3);
}

#[test]
fn lex_identifier_starting_with_minus() {
    let s = String::from("(-abcd 1 2)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    let error = tokens.err().unwrap();
    assert!(error.line == 1 && error.column == 3);
}

#[test]
fn lex_identifier_starting_with_dot() {
    let s = String::from("(.abcd 1 2)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    let error = tokens.err().unwrap();
    assert!(error.line == 1 && error.column == 3);
}

#[test]
fn lex_dot_and_ellipses() {
    let s = String::from("(. ... 1 2 .");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::Dot,
        Token::Identifier(String::from("...")),
        Token::Number(1),
        Token::Number(2),
        Token::Dot
    ]);
}

#[test]
fn lex_two_dots() {
    let s = String::from("(.. 1)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    let error = tokens.err().unwrap();
    assert!(error.line == 1 && error.column == 4);
}

#[test]
fn lex_booleans() {
    let s = String::from("(#t #f)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::Boolean(true),
        Token::Boolean(false),
        Token::CloseParen
    ]);
}

#[test]
fn lex_string() {
    let s = String::from("(\"string\")");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::String(String::from("string")),
        Token::CloseParen
    ]);
}

#[test]
fn lex_string_within_string() {
    let s = String::from("(\"string with \\\"quotes\\\"\")");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::String(String::from("string with \"quotes\"")),
        Token::CloseParen
    ]);
}

#[test]
fn lex_unterminated_string() {
    let s = String::from("(\"unterminated string)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    let error = tokens.err().unwrap();
    assert!(error.line == 1 && error.column == 23);
}

#[test]
fn lex_string_split_across_lines() {
    let s = String::from("(\"string split\nacross lines)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    let error = tokens.err().unwrap();
    assert!(error.line == 2 && error.column == 1);
}

#[test]
fn lex_quotes() {
    let s = String::from("('() `(,1 ,@(2)))");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen, Token::Quote, Token::OpenParen, Token::CloseParen,
        Token::Quasiquote, Token::OpenParen, Token::Unquote, Token::Number(1),
        Token::UnquoteList, Token::OpenParen, Token::Number(2),
        Token::CloseParen, Token::CloseParen, Token::CloseParen
    ]);
}

#[test]
fn lex_characters() {
    let s = String::from("(#\\a #\\2 #\\# #\\) #\\')");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen, Token::Character('a'), Token::Character('2'),
        Token::Character('#'), Token::Character(')'), Token::Character('\''),
        Token::CloseParen
    ]);
}

#[test]
fn lex_comment() {
    let s = String::from("(); (cons 4 '(+ 12 3 (* 5 6)))\n(car l)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenParen,
        Token::CloseParen,
        Token::OpenParen,
        Token::Identifier(String::from("car")),
        Token::Identifier(String::from("l")),
        Token::CloseParen
    ]);
}

#[test]
fn lex_vector() {
    let s = String::from("#(1 2 3)");
    let mut lexer = Lexer::new(s.chars());
    let tokens = lexer.lex_all();
    assert!(tokens.ok().unwrap() == vec![
        Token::OpenVectorParen, Token::Number(1), Token::Number(2),
        Token::Number(3), Token::CloseParen
    ]);
}
