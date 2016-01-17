use super::*;

#[macro_export]
macro_rules! systest {
    ($input:expr => Error) => {{
        let interp = Interpreter::new();
        match interp.evaluate($input) {
            Ok(d) => panic!("Expected error, got {}", d),
            Err(_) => ()
        }
    }};
    ($input:expr => $output:expr) => {{
        let interp = Interpreter::new();
        match interp.evaluate($input) {
            Ok(d) => assert_eq!($output, format!("{}", d)),
            Err(e) => panic!("Error occurred: {}", e)
        }
    }};
}

#[test]
fn test_basic_evaluation() {
    systest!("5" => "5");
    systest!("#t" => "#t");
    systest!("#f" => "#f");
    systest!("#\\a" => "#\\a");
    systest!("\"hello world\"" => "\"hello world\"");
    systest!("#(1 2 3)" => "#(1 2 3)");
    systest!("()" => Error);
    systest!("'()" => "()");
    systest!("'(1 2 3)" => "(1 2 3)");
    systest!("(+ 2 2)" => "4");
}

#[test]
fn test_if_form() {
    systest!("(if)" => Error);
    systest!("(if (= 2 2))" => Error);
    systest!("(if (= 2 2) 1 3)" => "1");
    systest!("(if (= 2 4) 1 3)" => "3");
    systest!("(if (= 2 4) 1 3 5)" => Error);
}

#[test]
fn test_let_form() {
    systest!("(let)" => Error);
    systest!("(let ())" => Error);
    systest!("(let ((x 1)))" => Error);
    systest!("(let () 5)" => "5");
    systest!("(let ((x 1)) x)" => "1");
    systest!("(let ((x 1)) 2 3 4 x)" => "1");
    systest!("(let ((a 1) (b 2)) (+ a b))" => "3");
    systest!("(let ((a 1) (b a)) (+ a b))" => Error);
    systest!("(letrec ((a 1) (b a)) (+ a b))" => "2");
}

#[test]
fn test_begin_form() {
    systest!("(begin)" => Error);
    systest!("(begin 1)" => "1");
    systest!("(begin 1 2 3)" => "3");
}
