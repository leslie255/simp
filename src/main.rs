#![feature(string_leak)]

use std::{fs, sync::Arc};

use ast::AstParser;
use cranelift_codegen::settings;
use cranelift_frontend::FunctionBuilderContext;
use cranelift_object::{ObjectBuilder, ObjectModule};
use symbols::GlobalSymbols;

use crate::token::TokenStream;

mod ast;
mod gen;
mod symbols;
mod token;

fn main() {
    let ast = make_parser(
        r#"
fn identity(x) = x;
fn main() = {
    identity(0);
    (a, b) = if 1 {
        c = 255;
        (c, 256)
    } else {
        (0, 1)
    };
    a
};
"#,
    );
    let mut obj_module = {
        let isa = cranelift_native::builder()
            .expect("Error getting the native ISA")
            .finish(settings::Flags::new(settings::builder()))
            .unwrap();
        let obj_builder = ObjectBuilder::new(
            Arc::clone(&isa),
            "output",
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        ObjectModule::new(obj_builder)
    };
    let mut symbols = GlobalSymbols::default();
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    ast.into_iter().for_each(|e| {
        let f = e.into_fn().unwrap();
        gen::compile_func(&mut obj_module, &mut symbols, &mut fn_builder_ctx, f).unwrap();
    });
    let bytes = {
        let obj = obj_module.finish();
        obj.emit().unwrap()
    };
    write_bytes_to_file("output.o", bytes.as_ref()).unwrap();
}

fn write_bytes_to_file(path: &str, buf: &[u8]) -> std::io::Result<()> {
    let mut file = fs::File::create(path)?;
    std::io::prelude::Write::write_all(&mut file, buf)
}

fn make_parser(s: &'static str) -> AstParser {
    let tokens = TokenStream::from_str(s).peekable();
    AstParser::new(tokens)
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
