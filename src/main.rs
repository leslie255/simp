#![feature(string_leak)]

use std::fs;

use ast::Expr;
use cranelift_codegen::{settings, verify_function, Context};

use crate::token::TokenStream;

mod ast;
mod gen;
mod scan;
mod token;

fn main() {
    let ast = parse_ast("fn main() = { a = 255 + 3 * 4; b = 256 + a; 0 };");
    let func = ast.into_iter().next().unwrap().into_fn().unwrap();
    let func = gen::compile_func(func);
    let flags = settings::Flags::new(settings::builder());
    println!("{}", func.display());
    verify_function(&func, &flags).expect("Error verifying the function");
    let isa = cranelift_native::builder()
        .expect("Error getting the native ISA")
        .finish(flags)
        .unwrap();
    let mut ctx = Context::for_function(func);
    let mut buf = Vec::<u8>::new();
    ctx.compile_and_emit(isa.as_ref(), &mut buf).unwrap();
    write_bytes_to_file("output", buf.as_ref()).unwrap();
}

fn write_bytes_to_file(path: &str, buf: &[u8]) -> std::io::Result<()> {
    let mut file = fs::File::create(path)?;
    std::io::prelude::Write::write_all(&mut file, buf)
}

fn parse_ast(s: &'static str) -> Vec<Expr> {
    let tokens = TokenStream::from_str(s).peekable();
    ast::parse(tokens)
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
