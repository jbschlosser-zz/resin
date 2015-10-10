use datum::{Datum, Environment, Procedure};
use error::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

pub fn get_builtins() -> Vec<(&'static str, Datum)>
{
    vec![
        ("+", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_add))))),
        ("*", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_multiply))))),
        ("car", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_car))))),
        ("cdr", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_cdr))))),
        ("cons", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_cons))))),
        ("define", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_define))))),
        ("lambda", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_lambda))))),
        ("quote", Datum::Procedure(Procedure::Native(
            Rc::new(Box::new(native_quote)))))
    ]
}

fn native_add(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    let mut sum = 0;
    for a in args {
        sum += match try!(Environment::evaluate(env.clone(), a)) {
            Datum::Number(n) => n,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Number(sum))
}

fn native_car(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }
    match try!(Environment::evaluate(env.clone(), &args[0])) {
        Datum::Pair(car, _) => Ok(*car.clone()),
        _ => runtime_error!("Expected pair")
    }
}

fn native_cdr(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }
    match try!(Environment::evaluate(env.clone(), &args[0])) {
        Datum::Pair(_, cdr) => Ok(*cdr.clone()),
        _ => runtime_error!("Expected pair")
    }
}

fn native_cons(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    let car = try!(Environment::evaluate(env.clone(), &args[0]));
    let cdr = try!(Environment::evaluate(env.clone(), &args[1]));
    Ok(Datum::Pair(Box::new(car), Box::new(cdr)))
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

fn native_lambda(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() < 2 { runtime_error!("Expected at least 2 args"); }
    // TODO: Handle additional formal possibilities (e.g. ellipses).
    let mut arg_names = Vec::new();
    for formal in try!(Environment::convert_list_to_vec(&args[0])) {
        match formal {
            Datum::Symbol(s) => arg_names.push(s.clone()),
            _ => runtime_error!("Expected symbol in lambda formals")
        }
    }
    let body = Vec::from(&args[1..]);

    Ok(Datum::Procedure(Procedure::Scheme(arg_names, body, env.clone())))
}

fn native_multiply(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    let mut product = 1;
    for a in args {
        product *= match try!(Environment::evaluate(env.clone(), a)) {
            Datum::Number(n) => n,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Number(product))
}

fn native_quote(_: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }

    Ok(args[0].clone())
}
