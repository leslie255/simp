#![feature(string_leak)]
#![feature(try_trait_v2)]

use std::{env, fs, path::PathBuf};

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
mod value;

fn main() {
    let (input, output) = {
        let mut args = env::args();
        let mut input = Option::<String>::None;
        let mut output = Option::<String>::None;
        args.next().unwrap();
        loop {
            match args.next() {
                Some(s) if &s == "-o" => {
                    output = Some(args.next().expect("Expects an output path after `-o`"));
                }
                Some(path) => {
                    if input.is_some() {
                        panic!("multiple input paths are not allowed");
                    }
                    input = Some(path);
                }
                None => break,
            }
        }
        let input = input.expect("Expects an input path");
        let output = output.unwrap_or(derive_out_path(input.to_string()));
        (input, output)
    };
    let ast = make_parser(String::leak(fs::read_to_string(input).unwrap()));
    let mut obj_module = {
        let isa = cranelift_native::builder()
            .expect("Error getting the native ISA")
            .finish(settings::Flags::new(settings::builder()))
            .unwrap();
        let obj_builder =
            ObjectBuilder::new(isa, "output", cranelift_module::default_libcall_names()).unwrap();
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
    write_bytes_to_file(&output, bytes.as_ref()).unwrap();
}

fn derive_out_path(in_path: String) -> String {
    let mut out_path = PathBuf::from(in_path);
    out_path.set_extension("o");
    out_path.to_string_lossy().to_string()
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
