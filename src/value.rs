use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Symbol(String),
    String(String),
    Character(char),
    Number(i64),
    Boolean(bool),
    Vector(Rc<RefCell<Vec<Value>>>),
    Procedure(Procedure),
    Pair(Box<Value>, Box<Value>),
    Nil
}

#[derive(Clone)]
pub enum Procedure {
    Native(Rc<Box<Fn(Rc<RefCell<Environment>>, &[Value]) ->
        Result<Value, RuntimeError>>>),
    Scheme(Vec<String>, Vec<Value>, Rc<RefCell<Environment>>)
}

pub struct RuntimeError {
    msg: String
}

pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<String, Value>
}
