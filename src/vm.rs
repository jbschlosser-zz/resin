use datum::{Datum, NativeProcedure, Procedure};
use environment::Environment;
use error::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Instruction {
    // Evaluates the Datum at the top of the val_stack in the Environment with
    // optional tail-call optimization.
    Evaluate(Rc<RefCell<Environment>>, bool),
    // Calls procedure at the top of the val_stack with the specified number of
    // args from the val_stack. Note that this replaces the current stack frame
    // with the procedure instructions.
    CallProcedure(Rc<RefCell<Environment>>, usize),
    // Calls the native procedure with the specified number of args from the
    // val_stack.
    CallNative(Rc<NativeProcedure>, usize),
    // Defines the symbol corresponding with the String to the Datum at the
    // top of the val_stack. The first flag indicates whether a syntax is
    // being defined. The second flag indicates whether a set! should be done
    // instead of a define.
    Define(Rc<RefCell<Environment>>, String, DefineType),
    // Pops the top value of val_stack and checks if it is #f - skips the
    // program counter forward the specified amount if it is
    JumpIfFalse(usize),
    // Pushes the stack frame onto the call stack.
    PushStackFrame(StackFrame),
    // Pushes the Datum to the top of the val_stack.
    PushValue(Datum),
    // Pops and discards the top value of the val_stack.
    PopValue,
    // Returns to the previous stack frame by popping the current one.
    Return
}

#[derive(Debug, Clone)]
pub enum DefineType {
    Define,
    DefineSyntax,
    Set
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    instructions: Vec<Instruction>,
    pc: usize,
    expr: Datum
}

impl StackFrame {
    pub fn new(instructions: Vec<Instruction>, expr: Datum) -> Self {
        StackFrame {instructions: instructions, pc: 0, expr: expr}
    }
}

pub struct VirtualMachine {
    call_stack: Vec<StackFrame>,
    val_stack: Vec<Datum>
}

impl VirtualMachine {
    pub fn new() -> Self {
        //println!("creating new VM");
        VirtualMachine {call_stack: Vec::new(), val_stack: Vec::new()}
    }
    pub fn run(&mut self, env: Rc<RefCell<Environment>>, datum: &Datum) ->
        Result<Datum, (RuntimeError, String)>
    {
        let initial_frame = StackFrame::new(vec![
            Instruction::PushValue(datum.clone()),
            Instruction::Evaluate(env.clone(), true)
        ], datum.clone());
        self.call_stack.push(initial_frame);
        while self.call_stack.len() > 0 {
            // Run next instruction.
            match self.step() {
                Ok(more) => if !more { self.call_stack.pop(); },
                Err(e) => {
                    // Print stack trace. TODO: Make this a return value somehow.
                    let trace_elems: Vec<_> = self.call_stack.iter()
                        .enumerate()
                        .map(|(i, frame)| format!("[{}] Evaluating {}", i, frame.expr))
                        .collect();
                    return Err((e, trace_elems.join("\n")));
                }
            }
        }
        // TODO: Return last from val_stack.
        Ok(self.val_stack.last().
           expect("val_stack should contain result after evaluation").clone())
    }
    fn step(&mut self) -> Result<bool, RuntimeError> {
        // Frame pointer.
        let fp = self.call_stack.len() - 1;
        let curr_pc = self.call_stack[fp].pc;
        let inst = match self.call_stack[fp].instructions.get(curr_pc) {
            Some(i) => i.clone(),
            None => return Ok(false)
        };
        //println!("=== Running instruction. Stack: {} PC: {} ===", self.call_stack.len(),
        //    curr_pc);
        match inst {
            Instruction::Evaluate(ref env, tco) => {
                let datum = self.val_stack.pop()
                    .expect("Expected expression to evaluate");
                //println!("evaluating {}", datum);
                match datum {
                    Datum::Symbol(ref s) => {
                        match env.borrow().get(s) {
                            Some(d) => self.val_stack.push(d),
                            None => runtime_error!("Undefined identifier: {}",
                                datum)
                        }
                    },
                    d @ Datum::String(_) | d @ Datum::Character(_) |
                    d @ Datum::Number(_) | d @ Datum::Boolean(_) |
                    d @ Datum::Procedure(_) | d @ Datum::Vector(_) |
                    d @ Datum::SyntaxRule(..) | d @ Datum::Ext(_) => {
                        self.val_stack.push(d);
                    },
                    Datum::Pair(car, cdr) => {
                        let args = try!(cdr.to_vec());
                        let arg_len = args.len();
                        for arg in args {
                            self.val_stack.push(arg);
                        }
                        let instructions = vec![
                            Instruction::PushValue(*car.clone()),
                            Instruction::Evaluate(env.clone(), false),
                            Instruction::CallProcedure(env.clone(), arg_len)
                        ];
                        let pair = Datum::Pair(car, cdr);
                        if tco {
                            // Replace the current stack frame.
                            //println!("Performing tail-call optimization");
                            self.call_stack[fp].instructions = instructions;
                            self.call_stack[fp].pc = 0;
                            self.call_stack[fp].expr = pair;
                            return Ok(true);
                        } else {
                            self.call_stack.push(
                                StackFrame::new(instructions, pair));
                        }
                    },
                    Datum::EmptyList =>
                        runtime_error!("Cannot evaluate empty list ()"),
                }
            },
            Instruction::CallProcedure(env, n) => {
                //println!("calling procedure with {} args", n);
                let proc_datum = self.val_stack.pop().unwrap();
                let top = self.val_stack.len();
                let mut args = self.val_stack.split_off(top - n);
                let procedure = match proc_datum {
                    Datum::Procedure(p) => p,
                    Datum::SyntaxRule(p, name) => {
                        // Pass the whole form as input to the syntax rule.
                        let mut full_form = vec![Datum::Symbol(name)];
                        full_form.append(&mut args);
                        args = vec![Datum::list(full_form)];
                        //println!("macro args: {:?}", args);
                        p
                    },
                    _ => runtime_error!("First element in an expression must be a procedure or macro: {}", proc_datum)
                };
                let instructions = match procedure {
                    Procedure::SpecialForm(ref special) => {
                        try!(special.call(env, &args))
                    },
                    Procedure::Native(ref native) => {
                        let mut instructions = Vec::new();
                        for arg in args.iter() {
                            instructions.push(
                                Instruction::PushValue(arg.clone()));
                            instructions.push(
                                Instruction::Evaluate(env.clone(), false));
                        }
                        instructions.push(Instruction::CallNative(
                            native.clone(), args.len()));
                        instructions
                    },
                    Procedure::Scheme(ref s) => {
                        let ref arg_names = s.arg_names;
                        let ref rest_name = s.rest_name;
                        let ref body_data = s.body_data;
                        let ref saved_env = s.saved_env;
                        if let &Some(_) = rest_name {
                            if args.len() < arg_names.len() {
                                runtime_error!("Expected at least {} argument(s) to function",
                                    arg_names.len());
                            }
                        } else {
                            if args.len() != arg_names.len() {
                                runtime_error!("Expected {} argument(s) to function",
                                    arg_names.len());
                            }
                        }

                        // Set up the procedure's environment- start with the
                        // environment saved when the function was defined and
                        // add argument bindings. Arguments are evaluated within
                        // the context of the outer environment.
                        let proc_env = Rc::new(RefCell::new(
                            Environment::with_parent(saved_env.clone())));
                        let mut body_instructions = Vec::new();
                        for(name, arg) in arg_names.iter().zip(args.iter()) {
                            body_instructions.push(
                                Instruction::PushValue(arg.clone()));
                            body_instructions.push(
                                Instruction::Evaluate(env.clone(), false));
                            body_instructions.push(Instruction::Define(
                                proc_env.clone(), name.clone(), DefineType::Define));
                        }

                        // Wrap the rest of the args up into a list.
                        if let &Some(ref rn) = rest_name {
                            for arg in args.iter().skip(arg_names.len()) {
                                body_instructions.push(
                                    Instruction::PushValue(arg.clone()));
                            }
                            body_instructions.push(
                                Instruction::PushValue(
                                    Datum::native(|args: &[Datum]| {
                                        let elements: Vec<_> = args.iter()
                                            .map(|e| e.clone())
                                            .collect();
                                        Ok(Datum::list(elements))
                                    })));
                            body_instructions.push(Instruction::PushStackFrame(
                                StackFrame::new(vec![
                                    Instruction::CallProcedure(env.clone(),
                                        args.len() - arg_names.len())
                                ], list!(Datum::symbol("list"),
                                Datum::Number((args.len() - arg_names.len()) as
                                              i64)))));
                            body_instructions.push(Instruction::Define(
                                proc_env.clone(), rn.clone(), DefineType::Define));
                        }

                        // Evaluate the procedure body in the new environment.
                        for (i, expr) in body_data.iter().enumerate() {
                            let last = i == body_data.len() - 1;
                            body_instructions.push(
                                Instruction::PushValue(expr.clone()));
                            body_instructions.push(
                                Instruction::Evaluate(proc_env.clone(), last));
                            if !last {
                                // Throw away the result of every expr but the
                                // last.
                                body_instructions.push(Instruction::PopValue);
                            }
                        }

                        body_instructions
                    }
                };

                // Replace the current stack frame with the procedure call.
                //println!("Calling {:?} with {:?}", procedure, args);
                self.call_stack[fp].instructions = instructions;
                self.call_stack[fp].pc = 0;
                return Ok(true);
            },
            Instruction::CallNative(native, n) => {
                let top = self.val_stack.len();
                //println!("Calling native with {} args. top: {}", n, top);
                let args = self.val_stack.split_off(top - n);
                let result = try!(native.call(&args));
                self.val_stack.push(result);
            },
            Instruction::Define(env, name, dtype) => {
                //println!("Define value in environment");
                match dtype {
                    DefineType::Define => env.borrow_mut().define(&name,
                        self.val_stack.pop().unwrap()),
                    DefineType::DefineSyntax => {
                        let datum = self.val_stack.pop().unwrap();
                        match datum {
                            Datum::Procedure(p) => {
                                env.borrow_mut().define(&name,
                                    Datum::SyntaxRule(p, name.clone()));
                            }
                            _ => runtime_error!("Expected procedure for syntax")
                        }
                    },
                    DefineType::Set => try!(env.borrow_mut().set(&name,
                        self.val_stack.pop().unwrap()))
                }
            },
            Instruction::JumpIfFalse(n) => {
                //println!("Jumping forward {} if datum is false", n);
                let test = self.val_stack.pop().unwrap();
                match test {
                    // Only #f counts as false.
                    Datum::Boolean(false) => {
                        //println!("Jumping forward {}", n);
                        self.call_stack[fp].pc += n;
                        return Ok(true);
                    },
                    _ => () //println!("Not jumping forward")
                }
            },
            Instruction::PushStackFrame(frame) => self.call_stack.push(frame),
            Instruction::PushValue(d) => {
                //println!("Pushing top value: {}", d);
                self.val_stack.push(d);
            },
            Instruction::PopValue => {
                //println!("Popping top value");
                self.val_stack.pop();
            },
            Instruction::Return => {
                //println!("Returning to previous stack frame");
                self.call_stack.pop();
                return Ok(true);
            }
        };

        self.call_stack[fp].pc += 1;
        Ok(true)
    }
}
