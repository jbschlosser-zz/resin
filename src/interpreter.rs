#[macro_use]
use datum::{Datum, Environment, Procedure};
#[macro_use]
use builtin;
use error::RuntimeError;
use lexer::Lexer;
use parser::Parser;
use repl;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Interpreter {
    root: Rc<RefCell<Environment>>
}

impl Interpreter {
    pub fn new() -> Self {
        // Define the built-in procedures.
        let mut root = Environment::new();
        for (name, datum) in builtin::get_builtins() {
            root.define(name, datum);
        }
        Interpreter {root: Rc::new(RefCell::new(root))}
    }
    pub fn with_root<F: Fn(&mut Environment)>(&mut self, func: F) {
        use std::ops::DerefMut;
        let mut env = self.root.borrow_mut();
        let mut deref = env.deref_mut();
        func(deref);
    }
    pub fn run_repl(&self) {
        repl::run("> ", |s| {
            // Lex.
            let mut lexer = Lexer::new(s.chars());
            let tokens = match lexer.lex_all() {
                Ok(t) => t,
                Err(e) => return Err(e.msg)
            };

            // Parse.
            let mut parser = Parser::new(tokens.into_iter());
            let data = match parser.parse_all() {
                Ok(d) => d,
                Err(e) => return Err(e.msg)
            };

            if data.len() == 0 {return Err("".to_string());}

            // Evaluate.
            let mut res = Ok(Datum::EmptyList);
            for datum in data {
                res = self.evaluate(&datum);
            }

            match res {
                Ok(d) => Ok(format!("{:?}", d)),
                Err(e) => return Err(e.msg)
            }
        });
    }
    pub fn evaluate(&self, datum: &Datum) -> Result<Datum, RuntimeError> {
        Environment::evaluate(self.root.clone(), datum)
    }
}

#[test]
fn string_evaluates_to_self() {
    let interp = Interpreter::new();
    assert!(interp.evaluate(&Datum::string("test")) ==
        Ok(Datum::string("test")));
}
#[test]
fn character_evaluates_to_self() {
    let interp = Interpreter::new();
    assert!(interp.evaluate(&Datum::Character('a')) ==
        Ok(Datum::Character('a')));
}
#[test]
fn number_evaluates_to_self() {
    let interp = Interpreter::new();
    assert!(interp.evaluate(&Datum::Number(2)) ==
        Ok(Datum::Number(2)));
}
#[test]
fn boolean_evaluates_to_self() {
    let interp = Interpreter::new();
    assert!(interp.evaluate(&Datum::Boolean(false)) ==
        Ok(Datum::Boolean(false)));
    assert!(interp.evaluate(&Datum::Boolean(true)) ==
        Ok(Datum::Boolean(true)));
}
