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
    if keywords.contains(&String::from("...")) {
        runtime_error!("Ellipses (...) cannot be in the keywords list");
    }
    println!("keywords: {:?}", keywords);

    // Parse the pattern/template entries.
    let mut pattern_templates = Vec::new();
    for pt in args.iter().skip(1) {
        let mut parts = try!(pt.to_vec());
        if parts.len() != 2 { runtime_error!("{}", &usage_str); }
        let template = parts.remove(1);
        let pattern_datum = parts.remove(0);

        let (macro_name, pattern) = match pattern_datum {
            Datum::Pair(ref car, ref cdr) => {
                match **car {
                    Datum::Symbol(ref s) => (s.clone(), *cdr.clone()),
                    _ => runtime_error!("First element in a pattern must be the macro identifier")
                }
            }
            _ => runtime_error!("{}", &usage_str)
        };

        // Parse out the pattern.
        //let pattern = try!(Pattern::parse(&pattern_datum, &keywords));
        println!("pattern: {:?}", pattern);

        // Verify the pattern.
        let variables = try!(verify_pattern(&pattern, &keywords));
        println!("variables: {:?}", variables);
        let template_syms = try!(verify_template(&template, &keywords));
        println!("template symbols: {:?}", template_syms);

        pattern_templates.push((pattern, template));
    }
    println!("pattern/templates: {:?}", pattern_templates);

    // Create a function that takes in a raw form and attempts to match
    // it against the patterns. If one matches, it applies the associated
    // template.
    let func = Datum::native(move |args: &[Datum]| {
        if args.len() != 1 { runtime_error!("Expected 1 arg"); }
        let (macro_name, input) = match args[0] {
            Datum::Pair(ref car, ref cdr) => {
                match **car {
                    Datum::Symbol(ref s) => (s.clone(), *cdr.clone()),
                    _ => runtime_error!("First element in a pattern must be the macro identifier")
                }
            },
            _ => runtime_error!("Cannot apply syntax-rules to non-list")
        };
        for &(ref pattern, ref template) in pattern_templates.iter() {
            //match pattern.match_with(&args[0], &macro_name) {
            match match_pattern(pattern, &input, &keywords) {
                Some(m) => {
                    println!("mappings: {:?}, macro_name: {}", m, &macro_name);

                    runtime_error!("early return");

                    // Apply the template.
                    match apply_template(&template, &vec![], &m) {
                        Some(d) => return Ok(d),
                        None => panic!("Bug in special form syntax-rules")
                    };
                },
                None => ()
            }
        }
        runtime_error!("Failed to match any syntax patterns");
    });

    Ok(vec![Instruction::PushValue(func)])
}

// Returns the names of all pattern variables if successful.
// Duplicates are not allowed.
fn verify_pattern(pattern: &Datum, keywords: &[String]) ->
    Result<Vec<String>, RuntimeError>
{
    let mut variables = Vec::new();
    try!(verify_pattern_helper(pattern, keywords, true, &mut variables));
    Ok(variables)
}

fn verify_pattern_helper(pattern: &Datum, keywords: &[String],
    list_begin: bool, variables: &mut Vec<String>) -> Result<(), RuntimeError>
{
    match pattern {
        &Datum::Symbol(ref s) if !keywords.contains(s) && s != "..." => {
            if variables.contains(s) {
                runtime_error!("Duplicate pattern variables are not allowed");
            }
            variables.push(s.clone());
            Ok(())
        },
        &Datum::Pair(ref car, ref cdr) => {
            // Check for ellipses. They should only be found at the
            // end of a list following a pattern.
            match **car {
                Datum::Symbol(ref s) if s == "..." => {
                    let list_end = match **cdr {
                        Datum::EmptyList => true,
                        _ => false
                    };
                    let follows_pattern = !list_begin;
                    if !list_end || !follows_pattern {
                        runtime_error!("Ellipses can only occur at the end of a list and must follow a pattern");
                    }
                },
                _ => ()
            }

            // Recursively verify the elements of the pair.
            try!(verify_pattern_helper(car, keywords, true, variables));
            try!(verify_pattern_helper(cdr, keywords, false, variables));
            Ok(())
        },
        _ => Ok(())
    }
}

// Returns the non-keyword symbols in the template if succesful.
fn verify_template(template: &Datum, keywords: &[String]) ->
    Result<Vec<String>, RuntimeError>
{
    let mut symbols = Vec::new();
    try!(verify_template_helper(template, keywords, true, &mut symbols));
    Ok(symbols)
}

fn verify_template_helper(template: &Datum, keywords: &[String],
    list_begin: bool, symbols: &mut Vec<String>) -> Result<(), RuntimeError>
{
    match template {
        &Datum::Symbol(ref s) if !keywords.contains(s) && s != "..." => {
            if !symbols.contains(s) {
                symbols.push(s.clone());
            }
            Ok(())
        },
        &Datum::Pair(ref car, ref cdr) => {
            // Check for ellipses- they should only be following a pattern.
            match **car {
                Datum::Symbol(ref s) if s == "..." => {
                    let follows_pattern = !list_begin;
                    if !follows_pattern {
                        runtime_error!("Ellipses must follow a pattern");
                    }
                },
                _ => ()
            }

            // Recursively verify the elements of the pair.
            try!(verify_template_helper(car, keywords, true, symbols));
            try!(verify_template_helper(cdr, keywords, false, symbols));
            Ok(())
        },
        _ => Ok(())
    }
}

fn match_pattern(pattern: &Datum, input: &Datum, keywords: &[String]) ->
    Option<HashMap<String, HashMap<Vec<usize>, Datum>>>
{
    let mut matchings = HashMap::new();
    if match_pattern_helper(pattern, input, keywords, &vec![], &mut matchings) {
        Some(matchings)
    } else {
        None
    }
}

fn match_pattern_helper(pattern: &Datum, input: &Datum, keywords: &[String],
    ellipsis_index: &Vec<usize>,
    matchings: &mut HashMap<String, HashMap<Vec<usize>, Datum>>) -> bool
{
    println!("matching {:?} to {:?}", pattern, input);
    match (pattern, input) {
        // Keyword literal.
        (&Datum::Symbol(ref s), inp @ _) if keywords.contains(s) => {
            match inp {
                &Datum::Symbol(ref t) => {
                    if s == t { return true; }
                    false
                },
                _ => false
            }
        },
        // Pattern variable.
        (&Datum::Symbol(ref s), inp @ _) => {
            if !matchings.contains_key(s) {
                matchings.insert(s.clone(), HashMap::new());
            }
            matchings.get_mut(s).unwrap().insert(
                ellipsis_index.clone(), inp.clone());
            true
        },
        // TODO: Implement this.
        (&Datum::Vector(..), _) => unimplemented!(),
        (&Datum::Procedure(..), _) => false,
        (&Datum::SyntaxRule(..), _) => false,
        (&Datum::Pair(ref pcar, ref pcdr), inp @ _) => {
            let zero_or_more = match **pcdr {
                Datum::Pair(ref next, _) => {
                    match **next {
                        Datum::Symbol(ref s) if s == "..." => true,
                        _ => false
                    }
                },
                _ => false
            };
            println!("zero or more: {:?}", zero_or_more);
            if zero_or_more {
                // Match as long as possible.
                let mut current = inp;
                let mut eindex = 0;
                loop {
                    // Set up ellipsis index.
                    let mut new_ei = ellipsis_index.clone();
                    new_ei.push(eindex);

                    // Make sure the current is part of a list.
                    let (element, next) = match current {
                        &Datum::Pair(ref ccar, ref ccdr) => (ccar, ccdr),
                        &Datum::EmptyList => break,
                        // Not a list so doesn't match.
                        _ => return false
                    };

                    // Check if the list element matches the pattern.
                    if !match_pattern_helper(pcar, &element, keywords,
                        &new_ei, matchings)
                    {
                        return false;
                    }

                    // Move to the next element.
                    current = &**next;
                    eindex += 1;
                }
                true
            } else {
                // Continue matching one at a time.
                println!("one at a time...");
                match inp {
                    &Datum::Pair(ref icar, ref icdr) => {
                        match_pattern_helper(pcar, icar, keywords,
                            ellipsis_index, matchings) &&
                            match_pattern_helper(pcdr, icdr, keywords,
                                ellipsis_index, matchings)
                    },
                    _ => false
                }
            }
        },
        (p @ _, inp @ _) if p == inp => true,
        (_, _) => false
    }
}

fn apply_template(template: &Datum, ellipsis_index: &Vec<usize>,
    matchings: &HashMap<String, HashMap<Vec<usize>, Datum>>) -> Option<Datum>
{
    println!("template: {:?}, ellipsis_index: {:?}", template, ellipsis_index);
    match template {
        &Datum::Symbol(ref s) if matchings.contains_key(s) => {
            match matchings[s].get(ellipsis_index) {
                Some(d) => Some(d.clone()),
                None => {println!("returning none from symbol"); None }
            }
        },
        &Datum::Pair(ref car, ref cdr) => {
            let (zero_or_more, after) = match **cdr {
                Datum::Pair(ref next, ref after) => {
                    match **next {
                        Datum::Symbol(ref s) if s == "..." =>
                            (true, Some(after)),
                        _ => (false, None)
                    }
                },
                _ => (false, None)
            };
            println!("template zero or more: {}", zero_or_more);
            if zero_or_more {
                // Build up the list of matchings (backwards).
                let mut match_list = Datum::EmptyList; 
                let mut eindex = 0;
                loop {
                    let mut new_ei = ellipsis_index.clone();
                    new_ei.push(eindex);
                    let test = apply_template(&car, &new_ei, matchings);
                    println!("test: {:?}", test);
                    match test {
                        Some(Datum::EmptyList) => break,
                        Some(d) => match_list = Datum::pair(d, match_list),
                        None => break
                    }
                    eindex += 1;
                }
                println!("match_list: {:?}", match_list);
                
                // Reverse the list.
                let mut reversed = match after {
                    Some(a) => *a.clone(),
                    None => Datum::EmptyList
                };
                println!("reversed at first: {:?}", reversed);
                let mut current = match_list;
                loop {
                    match current {
                        Datum::EmptyList => break,
                        Datum::Pair(a, b) => {
                            reversed = Datum::pair(*a, reversed);
                            current = *b;
                        },
                        a @ _ => {
                            reversed = Datum::pair(a.clone(), reversed);
                            break;
                        }
                    }
                }
                println!("reversed at end: {:?}", reversed);
                
                Some(reversed)
            } else {
                match (apply_template(&car, ellipsis_index, matchings),
                    apply_template(&cdr, ellipsis_index, matchings))
                {
                    (Some(a), Some(b)) => Some(Datum::pair(a, b)),
                    _ => None
                }
            }
        },
        t @ _ => Some(t.clone())
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
