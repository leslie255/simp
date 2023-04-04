#![feature(string_leak)]

use crate::token::TokenStream;

mod ast;
mod token;

fn main() {
    test_parse("a = (1 + 2 * 5); 2 * -3;");
    test_parse(r#"
x = 1 + {
    y = (5 + 2) * 3;
    y
};
    "#
    );
    test_parse(r#"
fn sum(a, b) = a + b;
fn succ(x) = x + 1;
fn identity(x) = x;
    "#
    );
}

fn test_parse(s: &'static str) {
    println!("{s}");
    let tokens = TokenStream::from_string(s.to_string()).peekable();
    let expr = ast::parse(tokens);
    println!("\n\n{expr:?}\n-----------------------------");
}
