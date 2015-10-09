extern crate libc;
use std::ffi::{CStr, CString};

#[link(name = "readline")]
extern {
    fn readline(prompt: *const libc::c_char) -> *const libc::c_char;
    fn add_history(entry: *const libc::c_char);
}

// Get input from the user using readline.
fn get_input(prompt: &str) -> Option<String> {
    let prompt_c_str = CString::new(prompt).unwrap();

    unsafe {
        let raw = readline(prompt_c_str.as_ptr());
        if raw.is_null() {
            return None;
        }

        let buffer = CStr::from_ptr(raw).to_bytes();
        let input = String::from_utf8(buffer.to_vec()).unwrap();

        if input.len() > 0 {
            add_history(raw);
        }

        Some(input)
    }
}

// Continually prompt the user for input and run the specified function
// on the resultant input.
pub fn run<F: Fn(String) -> Result<String, String>>(prompt: &str, func: F) {
    loop {
        match get_input(prompt) {
            Some(input) => {
                if input.len() > 0 {
                    let result = func(input);
                    println!("{}", result.unwrap_or_else(|e| e));
                }
            },
            None => return
        };
    };
}
