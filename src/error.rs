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

#[macro_export]
macro_rules! try_or_runtime_error {
    ($inp:expr, $($arg:tt)*) => (
        match $inp {
            Ok(v) => v,
            Err(_) => return Err(RuntimeError{msg: format!($($arg)*)})
        }
    )
}
