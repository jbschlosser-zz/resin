// Macro for creating a list.
#[macro_export]
macro_rules! list {
    () => {{ Datum::EmptyList }};

    ($datum:expr) => ( Datum::pair($datum, Datum::EmptyList) );

    ($first:expr, $($rest:expr),+) => {{
        Datum::pair($first, list!($($rest),+))
    }};
}

#[macro_export]
macro_rules! expect_args {
    ($args:ident == $num:expr) => {{
        if $args.len() != $num {
            let err_string = format!("Expected {} arguments; got {}", $num,
                                     $args.len());
            return Err(RuntimeError {msg: err_string});
        }
    }};

    ($args:ident > $num:expr) => {{
        if $args.len() <= $num {
            let err_string = format!("Expected more than {} arguments; got {}",
                $num, $args.len());
            return Err(RuntimeError {msg: err_string});
        }
    }};

    ($args:ident >= $num:expr) => {{
        if $args.len() < $num {
            let err_string = format!("Expected at least {} arguments; got {}",
                $num, $args.len());
            return Err(RuntimeError {msg: err_string});
        }
    }};

    ($args:ident < $num:expr) => {{
        if $args.len() >= $num {
            let err_string = format!("Expected less than {} arguments; got {}",
                $num, $args.len());
            return Err(RuntimeError {msg: err_string});
        }
    }};

    ($args:ident <= $num:expr) => {{
        if $args.len() > $num {
            let err_string = format!("Expected at most {} arguments; got {}",
                $num, $args.len());
            return Err(RuntimeError {msg: err_string});
        }
    }};
}

#[macro_export]
macro_rules! try_unwrap_arg {
    ($val:expr => i64) => (
        match $val {
            Datum::Number(ref v) => v.clone(),
            _ => runtime_error!("Expected number")
        }
    );
    ($val:expr => String) => (
        match $val {
            Datum::String(ref v) => v,
            _ => runtime_error!("Expected string")
        }
    );
    ($val:expr => Symbol) => (
        match $val {
            Datum::Symbol(ref v) => v,
            _ => runtime_error!("Expected symbol")
        }
    );
    ($val:expr => char) => (
        match $val {
            Datum::Character(ref v) => v.clone(),
            _ => runtime_error!("Expected character")
        }
    );
    ($val:expr => bool) => (
        match $val {
            Datum::Boolean(ref v) => v.clone(),
            _ => runtime_error!("Expected boolean")
        }
    );
    ($val:expr => Vec) => (
        match $val {
            Datum::Vector(ref v) => v.clone(),
            _ => runtime_error!("Expected vector")
        }
    );
    ($val:expr => $t:ty) => (
        match $val {
            Datum::Ext(ref e) => {
                match e.data.downcast_ref::<$t>() {
                    Some(v) => v,
                    None => runtime_error!("Expected {}", stringify!($t))
                }
            },
            _ => runtime_error!("Expected {}", stringify!($t))
        }
    )
}

#[macro_export]
macro_rules! unwrap_arg {
    ($val:expr => i64) => (
        match $val {
            Datum::Number(ref v) => Ok(v.clone()),
            _ => Err(RuntimeError{msg: "Expected number".to_string()})
        }
    );
    ($val:expr => String) => (
        match $val {
            Datum::String(ref v) => Ok(v),
            _ => Err(RuntimeError{msg: "Expected string".to_string()})
        }
    );
    ($val:expr => Symbol) => (
        match $val {
            Datum::Symbol(ref v) => Ok(v),
            _ => Err(RuntimeError{msg: "Expected symbol".to_string()})
        }
    );
    ($val:expr => char) => (
        match $val {
            Datum::Character(ref v) => Ok(v.clone()),
            _ => Err(RuntimeError{msg: "Expected character".to_string()})
        }
    );
    ($val:expr => bool) => (
        match $val {
            Datum::Boolean(ref v) => Ok(v.clone()),
            _ => Err(RuntimeError{msg: "Expected boolean".to_string()})
        }
    );
    ($val:expr => Vec) => (
        match $val {
            Datum::Vector(ref v) => Ok(v.clone()),
            _ => Err(RuntimeError{msg: "Expected vector".to_string()})
        }
    );
    ($val:expr => $t:ty) => (
        match $val {
            Datum::Ext(ref e) => {
                match e.data.downcast_ref::<$t>() {
                    Some(v) => Ok(v.clone()),
                    None => {
                        let err_string = format!("Expected {}", stringify!($t));
                        Err(RuntimeError{msg: err_string})
                    }
                }
            },
            _ => {
                let err_string = format!("Expected {}", stringify!($t));
                Err(RuntimeError{msg: err_string})
            }
        }
    )
}
