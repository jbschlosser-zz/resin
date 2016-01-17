use environment::Environment;
use error::RuntimeError;
use std::any::Any;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use super::mopa;
use vm::Instruction;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Datum {
    Symbol(String),
    String(String),
    Character(char),
    Number(i64),
    Boolean(bool),
    Vector(Rc<RefCell<Vec<Datum>>>),
    Procedure(Procedure),
    SyntaxRule(Procedure, String),
    Pair(Box<Datum>, Box<Datum>),
    Ext(Ext),
    EmptyList
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
        Datum::Procedure(Procedure::SpecialForm(Rc::new(SpecialForm(
            Box::new(t)))))
    }
    pub fn native<T: Fn(&[Datum]) ->
        Result<Datum, RuntimeError> + 'static>(t: T) -> Datum
    {
        Datum::Procedure(Procedure::Native(Rc::new(NativeProcedure(
            Box::new(t)))))
    }
    pub fn scheme(arg_names: Vec<String>, body_data: Vec<Datum>,
        saved_env: Rc<RefCell<Environment>>) -> Datum
    {
        Datum::Procedure(Procedure::Scheme(Rc::new(SchemeProcedure {
            arg_names: arg_names,
            body_data: body_data,
            saved_env: saved_env
        })))
    }
    pub fn ext<E: AnyClone + Eq>(e: E, tag: &str) -> Datum {
        Datum::Ext(Ext::new(e, tag.to_string()))
    }
    pub fn list(elements: Vec<Datum>) -> Datum {
        let mut list = Datum::EmptyList;
        for element in elements.into_iter().rev() {
            list = Datum::Pair(Box::new(element), Box::new(list));
        }
        list
    }
    pub fn reverse(&self) -> Datum {
        // Reverse the list.
        let mut reversed = match self {
            &Datum::EmptyList | &Datum::Pair(..) => Datum::EmptyList,
            _ => return self.clone()
        };
        let mut current = self;
        loop {
            match current {
                &Datum::EmptyList => break,
                &Datum::Pair(ref a, ref b) => {
                    reversed = Datum::pair(*a.clone(), reversed);
                    current = &*b;
                },
                a @ _ => {
                    reversed = Datum::pair(a.clone(), reversed);
                    break;
                }
            }
        }

        reversed
    }
    // Returns the vector and a flag indicating whether the datum
    // was a proper list or not.
    pub fn as_vec(&self) -> (Vec<Datum>, bool) {
        let mut vec: Vec<Datum> = Vec::new();
        let mut curr = self;
        loop {
            match curr {
                &Datum::Pair(ref car, ref cdr) => {
                    vec.push(*car.clone());
                    curr = cdr;
                },
                &Datum::EmptyList => return (vec, true),
                d @ _ => {
                    vec.push(d.clone());
                    return (vec, false);
                }
            }
        }
    }
    // Returns a vector only for the case of a proper list. Errors otherwise.
    pub fn to_vec(&self) -> Result<Vec<Datum>, RuntimeError> {
        let (vec_form, proper) = self.as_vec();
        if !proper { runtime_error!("Expected proper list") }
        Ok(vec_form)
    }
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
            &Datum::SyntaxRule(_, ref name) =>
                write!(f, "#<syntax-rule:{}>", name),
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
            &Datum::Ext(ref e) => write!(f, "#<ext:{}>", &e.tag),
            &Datum::EmptyList => write!(f, "()"),
        }
    }
}

pub enum Procedure {
    SpecialForm(Rc<SpecialForm>),
    Native(Rc<NativeProcedure>),
    Scheme(Rc<SchemeProcedure>)
}

pub struct SpecialForm(Box<Fn(Rc<RefCell<Environment>>, &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>>);
pub struct NativeProcedure(Box<Fn(&[Datum]) ->
    Result<Datum, RuntimeError>>);
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

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            // TODO: Implement this properly.
            Procedure::Native(ref n) => n.fmt(f),
            Procedure::Scheme(ref s) => s.fmt(f),
            Procedure::SpecialForm(ref s) => s.fmt(f)
        }
    }
}

impl Clone for Procedure {
    fn clone(&self) -> Self {
        match self {
            &Procedure::SpecialForm(ref r) => Procedure::SpecialForm(r.clone()),
            &Procedure::Native(ref r) => Procedure::Native(r.clone()),
            &Procedure::Scheme(ref r) => Procedure::Scheme(r.clone())
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

// Used to store trait objects that are cloneable and can be
// downcast later on.
pub trait AnyClone: mopa::Any {
    fn any_clone(&self) -> Box<AnyClone>;
}

mopafy!(AnyClone);

impl<T> AnyClone for T where T: 'static + Any + Clone {
    fn any_clone(&self) -> Box<AnyClone> {
        Box::new(self.clone())
    }
}

// The extension type.
pub struct Ext {
    pub data: Box<AnyClone>,
    tag: String,
    comparer: Rc<Box<Fn(&Box<AnyClone>, &Box<AnyClone>) -> bool>>
}

impl Ext {
    pub fn new<E: AnyClone + Eq>(e: E, tag: String) -> Self {
        Ext {
            data: Box::new(e),
            tag: tag,
            comparer: Rc::new(Box::new(|a: &Box<AnyClone>, b: &Box<AnyClone>| {
                match (a.downcast_ref::<E>(), b.downcast_ref::<E>()) {
                    (Some(a_real), Some(b_real)) => a_real == b_real,
                    _ => false
                }
            }))
        }
    }
}

impl fmt::Debug for Ext {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<ext:{}>", &self.tag)
    }
}

impl PartialEq for Ext {
    fn eq(&self, other: &Ext) -> bool {
        (*self.comparer)(&self.data, &other.data)
    }
}

impl Eq for Ext {}

impl Clone for Ext {
    fn clone(&self) -> Ext {
        Ext {
            data: self.data.any_clone(),
            tag: self.tag.clone(),
            comparer: self.comparer.clone()
        }
    }
}

#[test]
fn test_reverse() {
    assert_eq!(list!(Datum::Number(1), Datum::Number(2), Datum::Number(3))
        .reverse(),
        list!(Datum::Number(3), Datum::Number(2), Datum::Number(1)));
    assert_eq!(Datum::EmptyList.reverse(), Datum::EmptyList);
    assert_eq!(Datum::Number(1).reverse(), Datum::Number(1));
    assert_eq!(list!(Datum::Number(1)).reverse(), list!(Datum::Number(1)));
}
