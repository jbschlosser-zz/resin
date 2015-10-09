use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum Datum {
    Symbol(String),
    String(String),
    Character(char),
    Number(i64),
    Boolean(bool),
    Vector(Rc<RefCell<Vec<Datum>>>),
    Procedure(Procedure),
    Pair(Box<Datum>, Box<Datum>),
    EmptyList
}

impl Datum {
    pub fn pair(d1: Datum, d2: Datum) -> Datum {
        Datum::Pair(Box::new(d1), Box::new(d2))
    }
}

#[derive(Clone)]
pub enum Procedure {
    Native(Rc<Box<Fn(Rc<RefCell<Environment>>, &[Datum]) ->
        Result<Datum, RuntimeError>>>),
    Scheme(Vec<String>, Vec<Datum>, Rc<RefCell<Environment>>)
}

pub struct RuntimeError {
    msg: String
}

pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<String, Datum>
}
