use datum::Datum;
use error::RuntimeError;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<String, Datum>
}

impl Environment {
    pub fn new() -> Self {
        Environment {parent: None, bindings: HashMap::new()}
    }
    pub fn with_parent(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {parent: Some(parent), bindings: HashMap::new()}
    }
    pub fn define(&mut self, name: &str, datum: Datum) {
        self.bindings.insert(name.to_string(), datum);
    }
    pub fn define_fn<F: Fn(&[Datum]) -> Result<Datum, RuntimeError> + 'static>(
        &mut self, name: &str, func: F)
    {
        self.bindings.insert(name.to_string(), Datum::native(func));
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
                    "Attempted to set! an undefined variable: {}", &name)
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
    pub fn contains(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }
    pub fn iter(&self) -> ::std::collections::hash_map::Iter<String, Datum> {
        self.bindings.iter()
    }
}
