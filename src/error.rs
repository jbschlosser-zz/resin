use std::fmt;

#[derive(PartialEq, Eq)]
pub struct RuntimeError {
    pub msg: String
}

impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Runtime error: {}", &self.msg)
    }
}

#[macro_export]
macro_rules! runtime_error{
    ($($arg:tt)*) => (
        return Err(RuntimeError{msg: format!($($arg)*)})
    )
}
