use datum::{Datum, Environment};
use error::RuntimeError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use vm::Instruction;

pub fn get_builtins() -> Vec<(&'static str, Datum)>
{
    vec![
        ("begin", Datum::special(special_form_begin)),
        ("define", Datum::special(special_form_define)),
        ("define-syntax", Datum::special(special_form_define_syntax)),
        ("if", Datum::special(special_form_if)),
        ("lambda", Datum::special(special_form_lambda)),
        ("quote", Datum::special(special_form_quote)),
        ("set!", Datum::special(special_form_define)),
        ("syntax-rules", Datum::special(special_form_syntax_rules)),

        ("+", Datum::native(native_add)),
        ("-", Datum::native(native_subtract)),
        ("*", Datum::native(native_multiply)),
        ("=", Datum::native(native_equals)),
        ("car", Datum::native(native_car)),
        ("cdr", Datum::native(native_cdr)),
        ("cons", Datum::native(native_cons)),

        ("boolean?", Datum::native(native_boolean_p)),
        ("char?", Datum::native(native_char_p)),
        ("number?", Datum::native(native_number_p)),
        ("pair?", Datum::native(native_pair_p)),
        ("procedure?", Datum::native(native_procedure_p)),
        ("string?", Datum::native(native_string_p)),
        ("symbol?", Datum::native(native_symbol_p)),
        ("vector?", Datum::native(native_vector_p)),
    ]
}

fn special_form_begin(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() == 0 { runtime_error!("Expected at least 1 arg"); }

    let mut instructions = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        let last = i == args.len() - 1;
        instructions.push(Instruction::Evaluate(env.clone(),arg.clone(),last));
        if !last {
            instructions.push(Instruction::PopValue);
        }
    }

    Ok(instructions)
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
        Instruction::Define(env.clone(), name, false),
        // Return value is unspecified in the spec.
        Instruction::PushValue(Datum::EmptyList)
    ];
    Ok(instructions)
}

fn special_form_define_syntax(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    let name = match args[0] {
        Datum::Symbol(ref s) => s.clone(),
        _ => runtime_error!("Expected symbol")
    };
    let instructions = vec![
        Instruction::Evaluate(env.clone(), args[1].clone(), false),
        Instruction::Define(env.clone(), name, true),
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
    let lambda = Datum::scheme(arg_names, body, env.clone());
    Ok(vec![Instruction::PushValue(lambda)])
}

fn special_form_quote(_: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }

    Ok(vec![Instruction::PushValue(args[0].clone())])
}

fn special_form_syntax_rules(_: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    let usage_str =
        format!("Usage: (syntax-rules (keywords) ((pattern) template) ...)");
    if args.len() < 2 { runtime_error!("{}", &usage_str); }

    // Parse the keywords list.
    let mut keywords = Vec::new();
    for keyword in try!(args[0].to_vec()).iter() {
        keywords.push(match keyword {
            &Datum::Symbol(ref s) => s.clone(),
            _ => runtime_error!("{}", &usage_str)
        });
    }
    println!("keywords: {:?}", keywords);

    // Parse the pattern/template entries.
    let mut pattern_templates = Vec::new();
    for pt in args.iter().skip(1) {
        let mut parts = try!(pt.to_vec());
        if parts.len() != 2 { runtime_error!("{}", &usage_str); }
        let template = parts.remove(1);
        let pattern = parts.remove(0);
        pattern_templates.push((pattern, template));
    }
    println!("pattern/templates: {:?}", pattern_templates);

    // Create a function that takes in a raw form and attempts to match
    // it against the patterns. If one matches, it applies the associated
    // template.
    let func = Datum::native(move |args: &[Datum]| {
        if args.len() != 1 { runtime_error!("Expected 1 arg"); }
        for &(ref pattern, ref template) in pattern_templates.iter() {
            match match_pattern(&pattern, &args[0], &keywords) {
                Some(m) => {
                    println!("mappings: {:?}", m);

                    // Apply the template.
                    return Ok(apply_template(&template, &m));
                },
                None => ()
            }
        }
        runtime_error!("Failed to match any syntax patterns");
    });

    Ok(vec![Instruction::PushValue(func)])
}

fn match_pattern(pattern: &Datum, input: &Datum, keywords: &[String]) ->
    Option<HashMap<String, Datum>>
{
    let mut mappings = HashMap::new();
    match (pattern, input) {
        // Keyword literal.
        (&Datum::Symbol(ref s), inp @ _) if keywords.contains(s) => {
            match inp {
                &Datum::Symbol(ref t) => {
                    if s == t { return Some(mappings); }
                    None
                },
                _ => None
            }
        },
        // Non-keyword symbol.
        (&Datum::Symbol(ref s), inp @ _) => {
            mappings.insert(s.clone(), inp.clone());
            Some(mappings)
        },
        // TODO: Implement this.
        (&Datum::Vector(..), _) => unimplemented!(),
        (&Datum::Procedure(..), _) => None,
        (&Datum::SyntaxRule(..), _) => None,
        (&Datum::Pair(ref pcar, ref pcdr),&Datum::Pair(ref icar, ref icdr)) => {
            let mappings1 = match match_pattern(pcar, icar, keywords) {
                Some(m) => m,
                None => return None
            };
            let mappings2 = match match_pattern(pcdr, icdr, keywords) {
                Some(m) => m,
                None => return None
            };
            mappings1.into_iter().map(|m| mappings.insert(m.0, m.1)).last();
            mappings2.into_iter().map(|m| mappings.insert(m.0, m.1)).last();
            Some(mappings)
        },
        (p @ _, inp @ _) if p == inp => Some(mappings),
        (_, _) => None
    }
}

fn apply_template(template: &Datum, mappings: &HashMap<String, Datum>) ->
    Datum
{
    match template {
        &Datum::Symbol(ref s) if mappings.contains_key(s) => {
            mappings[s].clone()
        },
        &Datum::Pair(ref car, ref cdr) => {
            Datum::pair(
                apply_template(&car, mappings),
                apply_template(&cdr, mappings))
        },
        t @ _ => t.clone()
    }
}

fn native_add(args: &[Datum]) -> Result<Datum, RuntimeError> {
    let mut sum = 0;
    for a in args {
        sum += match a {
            &Datum::Number(n) => n,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Number(sum))
}

fn native_subtract(args: &[Datum]) -> Result<Datum, RuntimeError> {
    if args.len() < 1 { runtime_error!("Expected 1 arg"); }

    let mut difference = 0;
    for (i, a) in args.iter().enumerate() {
        match a {
            &Datum::Number(n) => {
                if i == 0 {
                    difference = n;
                } else {
                    difference -= n;
                }
            },
            _ => runtime_error!("Expected number")
        };
    }

    // Handle unary case.
    if args.len() == 1 { Ok(Datum::Number(-difference)) }
    else { Ok(Datum::Number(difference)) }
}

fn native_car(args: &[Datum]) -> Result<Datum, RuntimeError> {
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }
    match args[0] {
        Datum::Pair(ref car, _) => Ok(*car.clone()),
        _ => runtime_error!("Expected pair")
    }
}

fn native_cdr(args: &[Datum]) -> Result<Datum, RuntimeError> {
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }
    match args[0] {
        Datum::Pair(_, ref cdr) => Ok(*cdr.clone()),
        _ => runtime_error!("Expected pair")
    }
}

fn native_cons(args: &[Datum]) -> Result<Datum, RuntimeError> {
    if args.len() != 2 { runtime_error!("Expected 2 args"); }
    Ok(Datum::Pair(Box::new(args[0].clone()), Box::new(args[1].clone())))
}

fn native_equals(args: &[Datum]) -> Result<Datum, RuntimeError> {
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

fn native_multiply(args: &[Datum]) -> Result<Datum, RuntimeError> {
    let mut product = 1;
    for a in args {
        product *= match a {
            &Datum::Number(n) => n,
            _ => runtime_error!("Expected number")
        };
    }

    Ok(Datum::Number(product))
}

macro_rules! datum_predicate{
    ($dtype:path, $func:ident) => (
        fn $func(args: &[Datum]) -> Result<Datum, RuntimeError> {
            if args.len() != 1 { runtime_error!("Expected 1 arg"); }

            match args[0] {
                $dtype(..) => Ok(Datum::Boolean(true)),
                _ => Ok(Datum::Boolean(false))
            }
        }
    )
}

datum_predicate!(Datum::Boolean, native_boolean_p);
datum_predicate!(Datum::Character, native_char_p);
datum_predicate!(Datum::Number, native_number_p);
datum_predicate!(Datum::Pair, native_pair_p);
datum_predicate!(Datum::Procedure, native_procedure_p);
datum_predicate!(Datum::String, native_string_p);
datum_predicate!(Datum::Symbol, native_symbol_p);
datum_predicate!(Datum::Vector, native_vector_p);
