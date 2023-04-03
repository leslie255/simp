#![feature(string_leak)]

mod token;

use token::TokenStream;

fn main() {
    TokenStream::from_string("fn let <<=56h45".to_string()).for_each(|token| println!("{token:?}"))
}
