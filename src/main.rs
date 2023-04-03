#![feature(string_leak)]

use crate::token::TokenStream;

mod token;
mod ast;

fn main() {
    let mut tokens = TokenStream::from_string("a = 1 + 2 * -3".to_string()).peekable();
    let expr = ast::parse_expr(15, &mut tokens).unwrap();
    println!("{expr:?}");
}
