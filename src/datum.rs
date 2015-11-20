use error::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use vm::Instruction;

#[macro_export]
macro_rules! list {
    () => {{ Datum::EmptyList }};

    ($datum:expr) => ( Datum::pair($datum, Datum::EmptyList) );

    ($first:expr, $($rest:expr),+) => {{
        Datum::pair($first, list!($($rest),+))
    }};
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Datum {
    Symbol(String),
    String(String),
    Character(char),
    Number(i64),
    Boolean(bool),
    Vector(Rc<RefCell<Vec<Datum>>>),
    Procedure(Procedure),
    SyntaxRule(Procedure),
    Pair(Box<Datum>, Box<Datum>),
    EmptyList
}

impl fmt::Display for Datum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Datum::Symbol(ref s) => write!(f, "{}", &s),
            &Datum::String(ref s) => write!(f, "\"{}\"", &s),
            &Datum::Character(ref c) => write!(f, "\\#{}", c),
            &Datum::Number(ref n) => write!(f, "{}", n),
            &Datum::Boolean(ref b) => write!(f, "#{}", if *b {'t'} else {'f'}),
            &Datum::Vector(ref v) => {
                use std::ops::Deref;
                try!(write!(f, "#("));
                let borrowed = v.borrow();
                let vec = borrowed.deref();
                for (index, d) in vec.iter().enumerate() {
                    try!(d.fmt(f));
                    if index < vec.len() - 1 {
                        try!(write!(f, " "));
                    }
                }
                write!(f, ")")
            },
            &Datum::Procedure(_) => write!(f, "#<procedure>"),
            &Datum::SyntaxRule(_) => write!(f, "#<syntax-rule>"),
            &Datum::Pair(ref car, ref cdr) => {
                let car_str = format!("{}", car);
                let cdr_str = format!("{}", cdr);

                try!(write!(f, "("));
                if cdr_str.chars().next() == Some('(') {
                    // If the cdr is a list, leave out the . and parentheses.
                    try!(write!(f, "{}", &car_str));
                    match **cdr {
                        Datum::EmptyList => (),
                        _ => try!(write!(f, " "))
                    }
                    try!(write!(f, "{}", &cdr_str[1..cdr_str.len() - 1]));
                } else {
                    try!(write!(f, "{}", &car_str));
                    try!(write!(f, " . "));
                    try!(write!(f, "{}", &cdr_str));
                }
                write!(f, ")")
            },
            &Datum::EmptyList => write!(f, "()"),
        }
    }
}

impl Datum {
    pub fn pair(d1: Datum, d2: Datum) -> Datum {
        Datum::Pair(Box::new(d1), Box::new(d2))
    }
    pub fn symbol(s: &str) -> Datum {
        Datum::Symbol(s.to_string())
    }
    pub fn string(s: &str) -> Datum {
        Datum::String(s.to_string())
    }
    pub fn special<T: Fn(Rc<RefCell<Environment>>, &[Datum]) ->
        Result<Vec<Instruction>, RuntimeError> + 'static>(t: T) -> Datum
    {
        Datum::Procedure(Procedure::SpecialForm(SpecialForm(
            Rc::new(Box::new(t)))))
    }
    pub fn native<T: Fn(&[Datum]) ->
        Result<Datum, RuntimeError> + 'static>(t: T) -> Datum
    {
        Datum::Procedure(Procedure::Native(NativeProcedure(
            Rc::new(Box::new(t)))))
    }
    pub fn scheme(arg_names: Vec<String>, body_data: Vec<Datum>,
        saved_env: Rc<RefCell<Environment>>) -> Datum
    {
        Datum::Procedure(Procedure::Scheme(SchemeProcedure {
            arg_names: arg_names,
            body_data: body_data,
            saved_env: saved_env
        }))
    }
    pub fn list(elements: Vec<Datum>) -> Datum {
        let mut list = Datum::EmptyList;
        for element in elements.into_iter().rev() {
            list = Datum::Pair(Box::new(element), Box::new(list));
        }
        list
    }
    pub fn to_vec(&self) ->
        Result<Vec<Datum>, RuntimeError>
    {
        let mut vec: Vec<Datum> = Vec::new();
        let mut curr = self;
        loop {
            match curr {
                &Datum::Pair(ref car, ref cdr) => {
                    vec.push(*car.clone());
                    curr = cdr;
                },
                &Datum::EmptyList => return Ok(vec),
                _ => runtime_error!("Expected proper list")
            }
        }
    }
}

#[derive(Clone)]
pub enum Procedure {
    SpecialForm(SpecialForm),
    Native(NativeProcedure),
    Scheme(SchemeProcedure)
}

pub struct SpecialForm(Rc<Box<Fn(Rc<RefCell<Environment>>, &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>>>);
pub struct NativeProcedure(Rc<Box<Fn(&[Datum]) ->
    Result<Datum, RuntimeError>>>);
pub struct SchemeProcedure {
    pub arg_names: Vec<String>,
    pub body_data: Vec<Datum>,
    pub saved_env: Rc<RefCell<Environment>>
}

impl fmt::Debug for SpecialForm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<special-form>")
    }
}

impl Clone for SpecialForm {
    fn clone(&self) -> Self {
        SpecialForm(self.0.clone())
    }
}

impl SpecialForm {
    pub fn call(&self, env: Rc<RefCell<Environment>>, args: &[Datum]) ->
        Result<Vec<Instruction>, RuntimeError>
    {
        self.0(env, args)
    }
}

impl fmt::Debug for NativeProcedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<procedure-native>")
    }
}

impl Clone for NativeProcedure {
    fn clone(&self) -> Self {
        NativeProcedure(self.0.clone())
    }
}

impl NativeProcedure {
    pub fn call(&self, args: &[Datum]) ->
        Result<Datum, RuntimeError>
    {
        self.0(args)
    }
}

impl fmt::Debug for SchemeProcedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<procedure-scheme>")
    }
}

impl Clone for SchemeProcedure {
    fn clone(&self) -> Self {
        SchemeProcedure {
            arg_names: self.arg_names.clone(),
            body_data: self.body_data.clone(),
            saved_env: self.saved_env.clone()
        }
    }
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // TODO: Implement this properly.
            Procedure::Native(ref n) => n.fmt(f),
            Procedure::Scheme(..) => write!(f, "#<procedure-scheme>"),
            Procedure::SpecialForm(ref s) => s.fmt(f)
        }
    }
}

impl PartialEq for Procedure {
    fn eq(&self, _: &Procedure) -> bool {
        // TODO: Fix this.
        true
    }
}

impl Eq for Procedure {}

#[derive(Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<String, Datum>
}

impl Environment {
    pub fn new() -> Self {
        Environment {parent: None, bindings: HashMap::new()}
    }
    pub fn new_with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {parent: Some(parent), bindings: HashMap::new()}
    }
    pub fn define(&mut self, name: &str, datum: Datum) {
        self.bindings.insert(name.to_string(), datum);
    }
    pub fn set(&mut self, name: &str, datum: Datum) ->
        Result<(), RuntimeError>
    {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), datum);
            Ok(())
        } else {
            match self.parent {
                Some(ref p) => p.borrow_mut().set(name, datum),
                None => runtime_error!(
                    "Attempted to set! an undefined variable: {}", datum)
            }
        }
    }
    pub fn get(&self, name: &str) -> Option<Datum> {
        match self.bindings.get(name) {
            Some(d) => Some(d.clone()),
            None => {
                match self.parent {
                    Some(ref p) => p.borrow().get(name),
                    None => None
                }
            }
        }
    }
}
