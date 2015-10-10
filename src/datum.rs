use error::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

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
}

#[derive(Clone)]
pub enum Procedure {
    Native(Rc<Box<Fn(Rc<RefCell<Environment>>, &[Datum]) ->
        Result<Datum, RuntimeError>>>),
    Scheme(Vec<String>, Vec<Datum>, Rc<RefCell<Environment>>)
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // TODO: Implement this properly.
            Procedure::Native(..) => write!(f, "#<procedure-native>"),
            Procedure::Scheme(..) => write!(f, "#<procedure-scheme>")
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
    pub fn evaluate(env: Rc<RefCell<Environment>>, datum: &Datum) ->
        Result<Datum, RuntimeError>
    {
        match datum {
            &Datum::Symbol(ref s) => {
                match env.borrow().get(s) {
                    Some(d) => Ok(d),
                    None => runtime_error!("Undefined identifier: {}", datum)
                }
            },
            d @ &Datum::String(_) | d @ &Datum::Character(_) |
            d @ &Datum::Number(_) | d @ &Datum::Boolean(_) |
            d @ &Datum::Procedure(_) | d @ &Datum::Vector(_) => Ok(d.clone()),
            &Datum::Pair(ref car, ref cdr) =>
                Self::evaluate_expression(env.clone(), car, cdr),
            &Datum::EmptyList =>
                runtime_error!("Cannot evaluate empty list ()"),
        }
    }
    fn evaluate_expression(env: Rc<RefCell<Environment>>, car: &Datum,
        cdr: &Datum) -> Result<Datum, RuntimeError>
    {
        let first = try!(Self::evaluate(env.clone(), car));
        match first {
            Datum::Procedure(p) => Self::run_procedure(env.clone(), &p,
                &try!(Self::convert_list_to_vec(cdr))),
            _ => runtime_error!("First element in an expression must be a procedure: {}", first)
        }
    }
    fn run_procedure(env: Rc<RefCell<Environment>>, p: &Procedure,
        args: &[Datum]) -> Result<Datum, RuntimeError>
    {
        match p {
            &Procedure::Native(ref native) => native(env, args),
            &Procedure::Scheme(..) => unimplemented!()
        }
    }
    fn convert_list_to_vec(list: &Datum) -> Result<Vec<Datum>, RuntimeError> {
        let mut vec: Vec<Datum> = Vec::new();
        let mut curr = list;
        loop {
            match curr {
                &Datum::Pair(ref car, ref cdr) => {
                    vec.push(*car.clone());
                    curr = cdr;
                },
                &Datum::EmptyList => return Ok(vec),
                _ => runtime_error!("Cannot evaluate improper list")
            }
        }
    }
}
