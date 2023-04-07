#![feature(string_leak)]

use std::{fs, sync::Arc};

use ast::Expr;
use cranelift_codegen::settings;
use cranelift_object::{ObjectBuilder, ObjectModule};
use gen::SymbolTable;

use crate::token::TokenStream;

mod ast;
mod gen;
mod scan;
mod token;

fn main() {
    let ast = parse_stuff(r#"
fn identity(a) = {
    a
};
fn main() = {
    identity(255);
    x = identity(identity(128));
    0
};
"#);
    let isa = cranelift_native::builder()
        .expect("Error getting the native ISA")
        .finish(settings::Flags::new(settings::builder()))
        .unwrap();
    let mut module = {
        let obj_builder = ObjectBuilder::new(
            Arc::clone(&isa),
            "output",
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        ObjectModule::new(obj_builder)
    };
    let mut symbols = SymbolTable::default();
    ast.into_iter().for_each(|e| {
        let f = e.into_fn().unwrap();
        gen::compile_func(&mut module, &mut symbols, f).unwrap();
    });
    let obj = module.finish();
    let bytes = obj.emit().unwrap();
    write_bytes_to_file("output.o", bytes.as_ref()).unwrap();
}

fn write_bytes_to_file(path: &str, buf: &[u8]) -> std::io::Result<()> {
    let mut file = fs::File::create(path)?;
    std::io::prelude::Write::write_all(&mut file, buf)
}

fn parse_stuff(s: &'static str) -> Vec<Expr> {
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
