use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq)]
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

use std::fmt;
impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Procedure::Native(..) => write!(f, "#<procedure-native>"),
            Procedure::Scheme(..) => write!(f, "#<procedure-scheme>")
        }
    }
}

impl PartialEq for Procedure {
    fn eq(&self, other: &Procedure) -> bool {
        true
    }
}
impl Eq for Procedure {}

pub struct RuntimeError {
    msg: String
}

#[derive(Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<String, Datum>
}
