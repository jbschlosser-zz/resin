use datum::{Datum, Environment, Procedure};
use error::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Instruction {
    // Evaluates Datum in the Environment.
    Evaluate(Rc<RefCell<Environment>>, Datum),
    // Calls procedure at the top of the val_stack with specified number of
    // args from the val_stack.
    CallProcedure(Rc<RefCell<Environment>>, usize),
    // Defines the symbol corresponding with the String to the Datum at the
    // top of the val_stack.
    Define(Rc<RefCell<Environment>>, String),
    // Pops and discards the top value of the val_stack.
    PopValue
}

#[derive(Debug)]
struct StackFrame {
    instructions: Vec<Instruction>,
    pc: usize
}

impl StackFrame {
    pub fn new(instructions: Vec<Instruction>) -> Self {
        StackFrame {instructions: instructions, pc: 0}
    }
}

pub struct VirtualMachine {
    call_stack: Vec<StackFrame>,
    val_stack: Vec<Datum>
}

impl VirtualMachine {
    pub fn new() -> Self {
        println!("creating new VM");
        VirtualMachine {call_stack: Vec::new(), val_stack: Vec::new()}
    }
    pub fn run(&mut self, env: Rc<RefCell<Environment>>, datum: &Datum) ->
        Result<Datum, RuntimeError>
    {
        let initial_frame = StackFrame::new(
            vec![Instruction::Evaluate(env.clone(), datum.clone())]);
        self.call_stack.push(initial_frame);
        while self.call_stack.len() > 0 {
            // Run next instruction.
            if !try!(self.run_next_instruction()) {
                // No more instructions in this frame.
                self.call_stack.pop();
            }
        }
        // TODO: Return last from val_stack.
        Ok(self.val_stack.last().
           expect("val_stack should contain result after evaluation").clone())
    }
    fn run_next_instruction(&mut self) -> Result<bool, RuntimeError> {
        // Frame pointer.
        let fp = self.call_stack.len() - 1;
        let curr_pc = self.call_stack[fp].pc;
        let inst = match self.call_stack[fp].instructions.get(curr_pc) {
            Some(i) => i.clone(),
            None => return Ok(false)
        };
        match inst {
            Instruction::Evaluate(ref env, ref datum) => {
                println!("evaluating {:?}", datum);
                match datum {
                    &Datum::Symbol(ref s) => {
                        match env.borrow().get(s) {
                            Some(d) => self.val_stack.push(d.clone()),
                            None => runtime_error!("Undefined identifier: {}",
                                datum)
                        }
                    },
                    d @ &Datum::String(_) | d @ &Datum::Character(_) |
                    d @ &Datum::Number(_) | d @ &Datum::Boolean(_) |
                    d @ &Datum::Procedure(_) | d @ &Datum::Vector(_) => {
                        self.val_stack.push(d.clone());
                    },
                    &Datum::Pair(ref car, ref cdr) => {
                        let args = try!(cdr.to_vec());
                        let arg_len = args.len();
                        for arg in args {
                            self.val_stack.push(arg);
                        }
                        self.call_stack.push(StackFrame::new(vec![
                            Instruction::Evaluate(env.clone(), *car.clone()),
                            Instruction::CallProcedure(env.clone(), arg_len)
                        ]));
                    },
                    &Datum::EmptyList =>
                        runtime_error!("Cannot evaluate empty list ()"),
                }
            },
            Instruction::CallProcedure(env, n) => {
                let proc_datum = self.val_stack.pop().unwrap();
                let top = self.val_stack.len();
                let args = self.val_stack.split_off(top - n);
                let procedure = match proc_datum {
                    Datum::Procedure(p) => p,
                    _ => runtime_error!("First element in an expression must be a procedure: {}", proc_datum)
                };
                match procedure {
                    Procedure::SpecialForm(ref special) => {
                        let instructions = try!(special(env, &args));
                        self.call_stack.push(StackFrame::new(instructions));
                    },
                    Procedure::Native(ref native) => {
                        // TODO: Evaluate args beforehand.
                        let result = try!(native(env, &args));
                        self.val_stack.push(result);
                    },
                    Procedure::Scheme(ref arg_names, ref body_data,
                        ref saved_env) =>
                    {
                        if arg_names.len() != args.len() {
                            runtime_error!("Expected {} argument(s) to function",
                                arg_names.len());
                        }

                        // Set up the procedure's environment- start with the
                        // environment saved when the function was defined and
                        // add argument bindings. Arguments are evaluated within
                        // the context of the outer environment.
                        let proc_env = Rc::new(RefCell::new(
                            Environment::new_with_parent(saved_env.clone())));
                        let mut body_instructions = Vec::new();
                        for(name, arg) in arg_names.iter().zip(args.iter()) {
                            body_instructions.push(Instruction::Evaluate(
                                env.clone(), arg.clone()));
                            body_instructions.push(Instruction::Define(
                                proc_env.clone(), name.clone()));
                        }

                        // Evaluate the procedure body in the new environment.
                        for (i, expr) in body_data.iter().enumerate() {
                            body_instructions.push(Instruction::Evaluate(
                                proc_env.clone(), expr.clone()));
                            if i < body_data.len() - 1 {
                                // Throw away the result of every expr but the
                                // last.
                                body_instructions.push(Instruction::PopValue);
                            }
                        }

                        // Add the frame for the procedure to the call stack.
                        self.call_stack.push(
                            StackFrame::new(body_instructions));
                    }
                }
                println!("Calling {:?} with {:?}", procedure, args);
            },
            Instruction::Define(env, name) => {
                println!("Define value in environment");
                env.borrow_mut().define(&name, self.val_stack.pop().unwrap());
            },
            Instruction::PopValue => {
                println!("Popping top value");
                self.val_stack.pop();
            }
        };

        self.call_stack[fp].pc += 1;
        Ok(true)
    }
}
