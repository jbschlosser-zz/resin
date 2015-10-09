use datum::{Datum, Environment, Procedure};
use error::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

pub fn get_builtins() -> Vec<(&'static str, Datum)>
{
    vec![
        ("define", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_define))))),
        ("+", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_add)))))]
}

fn native_add(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    let arg1 = match try!(Environment::evaluate(env.clone(), &args[0])) {
        Datum::Number(n) => n,
        _ => runtime_error!("Expected number")
    };
    let arg2 = match try!(Environment::evaluate(env.clone(), &args[1])) {
        Datum::Number(n) => n,
        _ => runtime_error!("Expected number")
    };
    Ok(Datum::Number(arg1 + arg2))
}

fn native_define(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    let name = match &args[0] {
        &Datum::Symbol(ref s) => s,
        _ => runtime_error!("Expected symbol")
    };
    let datum = try!(Environment::evaluate(env.clone(), &args[1]));
    env.borrow_mut().define(name, datum.clone());
    Ok(datum)
}
