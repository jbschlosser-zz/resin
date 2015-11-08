use datum::{Datum, Environment, Procedure};
use error::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;
use vm::Instruction;

pub fn get_builtins() -> Vec<(&'static str, Datum)>
{
    // TODO: Convert these to special forms.
    vec![
        ("define", Datum::special(special_form_define)),
        ("if", Datum::special(special_form_if)),
        ("lambda", Datum::special(special_form_lambda)),
        ("quote", Datum::special(special_form_quote)),
        ("set!", Datum::special(special_form_define)),

        ("+", Datum::native(native_add)),
        ("*", Datum::native(native_multiply)),
        ("=", Datum::native(native_equals)),
        ("car", Datum::native(native_car)),
        ("cdr", Datum::native(native_cdr)),
        ("cons", Datum::native(native_cons)),
    ]
}

fn special_form_define(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    let name = match args[0] {
        Datum::Symbol(ref s) => s.clone(),
        _ => runtime_error!("Expected symbol")
    };
    let instructions = vec![
        Instruction::Evaluate(env.clone(), args[1].clone(), false),
        Instruction::Define(env.clone(), name),
        // Return value is unspecified in the spec.
        Instruction::PushValue(Datum::EmptyList)
    ];
    Ok(instructions)
}

fn special_form_if(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() != 2 && args.len() != 3 {
        runtime_error!("Expected 2 or 3 args");
    }

    let instructions = vec![
        Instruction::Evaluate(env.clone(), args[0].clone(), false),
        Instruction::JumpIfFalse(3),
        Instruction::Evaluate(env.clone(), args[1].clone(), true),
        Instruction::Return,
        if args.len() == 3 {
            Instruction::Evaluate(env.clone(), args[2].clone(), true)
        } else {
            // Unspecified in the spec.
            Instruction::Evaluate(env.clone(), Datum::Boolean(false), true)
        }
    ];

    Ok(instructions)
}

fn special_form_lambda(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() < 2 { runtime_error!("Expected at least 2 args"); }
    // TODO: Handle additional formal possibilities (e.g. ellipses).
    let mut arg_names = Vec::new();
    for formal in try!(args[0].to_vec()) {
        match formal {
            Datum::Symbol(s) => arg_names.push(s.clone()),
            _ => runtime_error!("Expected symbol in lambda formals")
        }
    }
    let body = Vec::from(&args[1..]);
    let lambda = Datum::Procedure(Procedure::Scheme(
        arg_names, body, env.clone()));
    Ok(vec![Instruction::PushValue(lambda)])
}

fn special_form_quote(_: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }

    Ok(vec![Instruction::PushValue(args[0].clone())])
}

fn native_add(args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    let mut sum = 0;
    for a in args {
        sum += match a {
            &Datum::Number(n) => n,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Number(sum))
}

fn native_car(args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }
    match args[0] {
        Datum::Pair(ref car, _) => Ok(*car.clone()),
        _ => runtime_error!("Expected pair")
    }
}

fn native_cdr(args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }
    match args[0] {
        Datum::Pair(_, ref cdr) => Ok(*cdr.clone()),
        _ => runtime_error!("Expected pair")
    }
}

fn native_cons(args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    Ok(Datum::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
}

fn native_equals(args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    if args.len() == 0 {
        return Ok(Datum::Boolean(true));
    }

    let first = match args[0] {
        Datum::Number(n) => n,
        _ => runtime_error!("Expected number")
    };

    let mut res = true;
    for a in &args[1..] {
        res = res && match a {
            &Datum::Number(n) => n == first,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Boolean(res))
}

fn native_multiply(args: &[Datum]) ->
    Result<Datum, RuntimeError>
{
    let mut product = 1;
    for a in args {
        product *= match a {
            &Datum::Number(n) => n,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Number(product))
}
