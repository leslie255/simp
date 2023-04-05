#![feature(string_leak)]

use crate::token::TokenStream;

mod ast;
mod token;

fn main() {
    test_parse("a = (1 + 2) * 5; 2 * -3;");
    test_parse(
        r#"
x = 1 + {
    y = (5 + 2) * 3;
    y
};
    "#,
    );
    test_parse(
        r#"
fn sum(a, b) = a + b;
fn succ(x) = x + 1;
fn identity(x) = x;
    "#,
    );
    test_parse(
        r#"
func();
print(255);
sum(255, 1);
    "#,
    );
}

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
    fn expect_true(self,msg:&str) -> () {
        if !self {
            panic!("{msg}");
        }
    }
}
