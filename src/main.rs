#![feature(string_leak)]

use crate::token::TokenStream;

mod ast;
mod gen;
mod token;
mod scan;

fn main() {
    gen::compile_func(
        "main".to_string(),
        vec!["argc".to_string(), "argv".to_string()],
        ast::Expr::Num(0),
    );
}

#[allow(dead_code)]
fn test_parse(s: &'static str) {
    println!("{s}");
    let tokens = TokenStream::from_str(s).peekable();
    let expr = ast::parse(tokens);
    println!("\n\n{expr:?}\n-----------------------------");
}

pub(self) trait ExpectTrue {
    fn expect_true(self, msg: &str) -> ();
}

impl ExpectTrue for bool {
    #[inline]
    #[cold]
    #[track_caller]
    fn expect_true(self, msg: &str) -> () {
        if !self {
            panic!("{msg}");
        }
    }
}
