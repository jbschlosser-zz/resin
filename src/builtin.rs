use datum::Datum;
use environment::Environment;
use error::RuntimeError;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
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
        ("let", Datum::special(special_form_let)),
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
        instructions.push(
            Instruction::PushValue(arg.clone()));
        instructions.push(
            Instruction::Evaluate(env.clone(), last));
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
        Instruction::PushValue(args[1].clone()),
        Instruction::Evaluate(env.clone(), false),
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
        Instruction::PushValue(args[1].clone()),
        Instruction::Evaluate(env.clone(), false),
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

    let mut instructions = vec![
        Instruction::PushValue(args[0].clone()),
        Instruction::Evaluate(env.clone(), false),
        Instruction::JumpIfFalse(4),
        Instruction::PushValue(args[1].clone()),
        Instruction::Evaluate(env.clone(), true),
        Instruction::Return
    ];
    if args.len() == 3 {
        instructions.push(Instruction::PushValue(args[2].clone()));
        instructions.push(Instruction::Evaluate(env.clone(), true));
    } else {
        // Unspecified in the spec.
        instructions.push(Instruction::PushValue(Datum::Boolean(false)));
    }

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

fn special_form_let(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    let usage_str =
        format!("Usage: (let ((variable init) ...) body ...)");
    if args.len() < 2 { runtime_error!("{}", &usage_str); }

    // Parse the bindings and add instructions for evaluating the initial
    // values to define within a sub-environment.
    let mut instructions = Vec::new();
    let let_env = Rc::new(RefCell::new(Environment::with_parent(env.clone())));
    let bindings = try_or_runtime_error!(args[0].to_vec(), "{}", &usage_str);
    for binding in bindings {
        let mut parts =
            try_or_runtime_error!(binding.to_vec(), "{}", &usage_str);
        if parts.len() != 2 { runtime_error!("{}", &usage_str); }
        let init = parts.remove(1);
        let variable = parts.remove(0);
        let var_name = match variable {
            Datum::Symbol(ref s) => s.to_string(),
            _ => runtime_error!("{}", &usage_str)
        };
        instructions.push(Instruction::PushValue(init));
        instructions.push(Instruction::Evaluate(env.clone(), false));
        instructions.push(
            Instruction::Define(let_env.clone(), var_name, false));
    }

    // Add the instructions for evaluating the body within the sub-environment.
    for (i, arg) in args.iter().skip(1).enumerate() {
        let last = i == args.len() - 2;
        instructions.push(Instruction::PushValue(arg.clone()));
        instructions.push(Instruction::Evaluate(let_env.clone(), last));
        if !last {
            instructions.push(Instruction::PopValue);
        }
    }
    Ok(instructions)
}

fn special_form_quote(_: Rc<RefCell<Environment>>, args: &[Datum]) ->
    Result<Vec<Instruction>, RuntimeError>
{
    if args.len() != 1 { runtime_error!("Expected 1 arg"); }

    Ok(vec![Instruction::PushValue(args[0].clone())])
}
fn special_form_syntax_rules(env: Rc<RefCell<Environment>>, args: &[Datum]) ->
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

    // Parse the pattern/template entries.
    let mut pattern_templates = Vec::new();
    for pt in args.iter().skip(1) {
        let mut parts = try!(pt.to_vec());
        if parts.len() != 2 { runtime_error!("{}", &usage_str); }
        let template = parts.remove(1);
        let pattern_datum = parts.remove(0);
        let pattern = match pattern_datum {
            Datum::Pair(ref car, ref cdr) => {
                match **car {
                    Datum::Symbol(_) => *cdr.clone(),
                    _ => runtime_error!("First element in a pattern must be the macro identifier")
                }
            }
            _ => runtime_error!("{}", &usage_str)
        };

        // Verify the pattern and template.
        let variables = try!(verify_pattern(&pattern, &keywords));
        let template_symbols = try!(verify_template(&template));

        // Environment to hold any free variables in the template.
        let mut free_env = Environment::new();
        for sym in template_symbols.iter() {
            if !variables.contains(sym) {
                if let Some(val) = env.borrow().get(sym) {
                    free_env.define(sym, val.clone());
                }
            }
        }

        pattern_templates.push((pattern, template, template_symbols, free_env));
    }

    // Create a function that takes in a raw form and attempts to match
    // it against the patterns. If one matches, it applies the associated
    // template and evaluates the result.
    let func = Datum::special(move |env: Rc<RefCell<Environment>>,
        args: &[Datum]|
    {
        // Verify that a raw un-expanded macro call has been passed.
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
        //println!("macro name: {}", macro_name);

        // Try to match against each pattern in order.
        for &(ref pattern, ref template, ref template_syms, ref free_env) in
            pattern_templates.iter()
        {
            // Try to match against this pattern.
            match match_pattern(pattern, &input, &keywords) {
                Some(var_env) => {
                    // === MACRO HYGIENE ===
                    // Rename symbols in the template for hygiene.
                    let mut name_mappings = HashMap::new();
                    for template_sym in template_syms.iter() {
                        // Rename the symbol if it exists in the current
                        // environment so as not to conflict.
                        let mut new_name = template_sym.clone();
                        let mut temp_index = 1;
                        while let Some(_) = env.borrow().get(&new_name) {
                            new_name = format!("{}_hygienic_{}",
                                template_sym, temp_index);
                            temp_index += 1;
                        }
                        name_mappings.insert(template_sym.clone(), new_name);
                    }
                    name_mappings.insert(macro_name.clone(),macro_name.clone());
                    let renamed_template = rename_template(&template,
                        &name_mappings);
                    //println!("name mappings: {:?}", name_mappings);
                    
                    // The evaluation environment for the template
                    // is the current environment plus the values of
                    // free variables stored when the macro was defined.
                    let eval_env = Rc::new(RefCell::new(
                        Environment::with_parent(env.clone())));
                    for (old_name, new_name) in name_mappings {
                        match free_env.get(&old_name) {
                            Some(d) => eval_env.borrow_mut().
                                define(&new_name, d),
                            None => (),
                        }
                    }

                    // Apply the template.
                    //println!(">>> matched pattern: {}", pattern);
                    //println!(">>> applying template: {}", renamed_template);
                    let result = try!(apply_template(&renamed_template,
                        &var_env));
                    //println!(">>> result: {}", result);
                    return Ok(vec![
                        Instruction::PushValue(result),
                        Instruction::Evaluate(eval_env.clone(), false)
                    ]);
                },
                None => ()
            }
        }
        runtime_error!("Failed to match any syntax patterns");
    });

    Ok(vec![Instruction::PushValue(func)])
}

// Renames symbols in the template according to the given mappings.
fn rename_template(template: &Datum, mappings: &HashMap<String, String>) ->
    Datum
{
    match template {
        &Datum::Symbol(ref s) => {
            match mappings.get(s) {
                Some(m) => Datum::Symbol(m.clone()),
                None => template.clone()
            }
        },
        &Datum::Pair(ref car, ref cdr) =>
            Datum::pair(rename_template(&car, mappings),
                rename_template(&cdr, mappings)),
        _ => template.clone()
    }
}

// Returns the names of all pattern variables if successful.
// Duplicates are not allowed.
fn verify_pattern(pattern: &Datum, keywords: &[String]) ->
    Result<HashSet<String>, RuntimeError>
{
    let mut variables = HashSet::new();
    try!(verify_pattern_helper(pattern, keywords, true, &mut variables));
    Ok(variables)
}

fn verify_pattern_helper(pattern: &Datum, keywords: &[String], list_begin: bool,
    variables: &mut HashSet<String>) -> Result<(), RuntimeError>
{
    match pattern {
        &Datum::Symbol(ref s) if !keywords.contains(s) && s != "..." => {
            if variables.contains(s) {
                runtime_error!("Duplicate pattern variables are not allowed");
            }
            variables.insert(s.clone());
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

// Returns the symbols in the template if successful.
fn verify_template(template: &Datum) -> Result<HashSet<String>, RuntimeError> {
    let mut symbols = HashSet::new();
    try!(verify_template_helper(template, true, &mut symbols));
    Ok(symbols)
}

fn verify_template_helper(template: &Datum, list_begin: bool,
    symbols: &mut HashSet<String>) -> Result<(), RuntimeError>
{
    match template {
        &Datum::Symbol(ref s) if s != "..." => {
            if !symbols.contains(s) {
                symbols.insert(s.clone());
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
            try!(verify_template_helper(car, true, symbols));
            try!(verify_template_helper(cdr, false, symbols));
            Ok(())
        },
        _ => Ok(())
    }
}

// Attempts to match the input to the given pattern. If successful,
// an environment of the pattern variables is returned.
fn match_pattern(pattern: &Datum, input: &Datum, keywords: &[String]) ->
    Option<Environment>
{
    let mut env = Environment::new();
    if match_pattern_helper(pattern, input, keywords, &mut env) {
        Some(env)
    } else {
        None
    }
}

fn match_pattern_helper(pattern: &Datum, input: &Datum, keywords: &[String],
    env: &mut Environment) -> bool
{
    match (pattern, input) {
        // Keyword literal.
        (&Datum::Symbol(ref s), inp @ _) if keywords.contains(s) => {
            match inp {
                &Datum::Symbol(ref t) => s == t,
                _ => false
            }
        },
        // Pattern variable.
        (&Datum::Symbol(ref s), inp @ _) => {
            env.define(s, inp.clone());
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
            if zero_or_more {
                // Match as long as possible.
                let mut current = inp;
                let mut at_least_one_found = false;
                let mut to_reverse = HashSet::new();
                loop {
                    // Make sure the current is part of a list.
                    let (element, next) = match current {
                        &Datum::Pair(ref ccar, ref ccdr) => (ccar, ccdr),
                        &Datum::EmptyList => break,
                        // Not a list so doesn't match.
                        _ => return false
                    };

                    // Check if the list element matches the pattern.
                    let mut sub_env = Environment::new();
                    if !match_pattern_helper(pcar, &element, keywords,
                        &mut sub_env)
                    {
                        return false;
                    }

                    // Merge in the sub environment.
                    for (var, value) in sub_env.iter() {
                        let mut curr = if let Some(d) = env.get(var) {
                            d
                        } else {
                            Datum::EmptyList
                        };
                        curr = Datum::pair(value.clone(), curr);
                        env.define(var, curr);
                        to_reverse.insert(var.clone());
                    }

                    // Move to the next element.
                    current = &**next;
                    at_least_one_found = true;
                }

                // If no matches were found, add an empty list for each
                // variable in the pattern.
                if !at_least_one_found {
                    add_empty_matching(pcar, keywords, env);
                }

                // Reverse any lists that were built up.
                for var in to_reverse {
                    let value = env.get(&var).unwrap();
                    env.define(&var, value.reverse());
                }
                true
            } else {
                // Continue matching one at a time.
                match inp {
                    &Datum::Pair(ref icar, ref icdr) => {
                        match_pattern_helper(pcar, icar, keywords, env) &&
                            match_pattern_helper(pcdr, icdr, keywords, env)
                    },
                    _ => false
                }
            }
        },
        (p @ _, inp @ _) => p == inp
    }
}

fn add_empty_matching(pattern: &Datum, keywords: &[String],
    env: &mut Environment)
{
    match pattern {
        &Datum::Symbol(ref s) if !keywords.contains(s) => {
            env.define(s, Datum::EmptyList);
        },
        &Datum::Pair(ref car, ref cdr) => {
            add_empty_matching(car, keywords, env);
            add_empty_matching(cdr, keywords, env);
        },
        _ => ()
    }
}

fn get_variables(template: &Datum, var_env: &Environment) -> HashSet<String> {
    let mut variables = HashSet::new();
    get_variables_helper(template, var_env, &mut variables);
    variables
}

fn get_variables_helper(template: &Datum, var_env: &Environment,
    variables: &mut HashSet<String>)
{
    match template {
        &Datum::Symbol(ref s) if var_env.contains(s) && s != "..." => {
            variables.insert(s.clone());
        },
        &Datum::Pair(ref car, ref cdr) => {
            get_variables_helper(car, var_env, variables);
            get_variables_helper(cdr, var_env, variables);
        },
        _ => ()
    }
}

fn apply_template(template: &Datum, var_env: &Environment) ->
    Result<Datum, RuntimeError>
{
    match template {
        // Handle variable substitution.
        &Datum::Symbol(ref s) if var_env.contains(s) =>
            Ok(var_env.get(s).unwrap()),
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
            if zero_or_more {
                // Determine which variables need to be iterated over for
                // the ellipses.
                let variables = get_variables(car, var_env);
                if variables.len() == 0 {
                    runtime_error!("Expected variables before ellipses");
                }
                let vectors: Vec<(String, Vec<Datum>)> = variables.iter()
                    .map(|v| (v.clone(), var_env.get(v).unwrap().as_vec().0))
                    .collect();
                let iterations = vectors.iter()
                    .map(|v| v.1.len()).min().unwrap();

                // Iterate over variables and build up a list (backwards).
                let mut reversed = Datum::EmptyList;
                for i in 0..iterations {
                    let mut sub_env = Environment::new();
                    for &(ref var, ref values) in vectors.iter() {
                        sub_env.define(&var, values[i].clone());
                    }
                    let result = try!(apply_template(car, &sub_env));
                    reversed = Datum::pair(result, reversed);
                }

                // Recursively apply the template to the rest.
                let mut result = try!(apply_template(after.unwrap(), var_env));

                // Unreverse the list as it is attached to the rest.
                let mut current = &reversed;
                loop {
                    current = match current {
                        &Datum::EmptyList => break,
                        &Datum::Pair(ref a, ref b) => {
                            result = Datum::pair(*a.clone(), result);
                            &*b
                        },
                        _ => panic!("bug in apply_template")
                    };
                }

                Ok(result)
            } else {
                Ok(Datum::pair(try!(apply_template(car, var_env)),
                    try!(apply_template(cdr, var_env))))
            }
        },
        t @ _ => Ok(t.clone())
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
