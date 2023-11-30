#![feature(try_trait_v2)]

use core::panic;
use std::{
    env,
    fs::{self, File},
    path::PathBuf,
};

use ast::AstParser;
use cranelift_codegen::settings::{Configurable, Flags};
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
    let (input, output, enable_log) = {
        let mut args = env::args();
        let mut input = Option::<String>::None;
        let mut output = Option::<String>::None;
        let mut enable_log = false;
        args.next().unwrap();
        loop {
            match args.next() {
                Some(s) if &s == "-o" => {
                    output = Some(args.next().expect("Expects an output path after `-o`"));
                }
                Some(s) if &s == "--log" => {
                    enable_log = true;
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
        let output = output.unwrap_or_else(|| derive_out_path(&input));
        (input, output, enable_log)
    };
    let ast_parser = {
        let source: &'static str = fs::read_to_string(input).unwrap().leak();
        let tokens = TokenStream::from_str(&source).peekable();
        AstParser::new(tokens)
    };
    let clif_flags = {
        let mut settings = cranelift_codegen::settings::builder();
        settings.set("opt_level", "speed").unwrap();
        settings.enable("is_pic").unwrap();
        let flags = Flags::new(settings);
        flags
    };
    let mut obj_module = {
        let isa = cranelift_native::builder()
            .expect("Error getting the native ISA")
            .finish(clif_flags)
            .unwrap();
        let obj_builder =
            ObjectBuilder::new(isa, "program", cranelift_module::default_libcall_names()).unwrap();
        ObjectModule::new(obj_builder)
    };
    let mut symbols = GlobalSymbols::default();
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    ast_parser.into_iter().for_each(|e| {
        let f = e.into_fn().unwrap_or_else(|| {
            panic!("Non-function statements/expressions aren't allowed at top level")
        });
        gen::compile_func(
            &mut obj_module,
            &mut symbols,
            &mut fn_builder_ctx,
            f,
            enable_log,
        )
        .unwrap_or_else(|e| panic!("Unable to compile function, error: {:?}", e));
    });
    let bytes = {
        let obj = obj_module.finish();
        obj.emit().unwrap()
    };
    write_bytes_to_file(&output, bytes.as_ref()).unwrap();
}

fn derive_out_path(src_path: &str) -> String {
    let mut out_path: PathBuf = src_path.into();
    if out_path
        .extension()
        .is_some_and(|s| s.eq_ignore_ascii_case("o"))
    {
        panic!("Please specify an output path using -o");
    } else {
        out_path.set_extension("o");
    }
    out_path.to_string_lossy().to_string()
}

fn write_bytes_to_file(path: &str, buf: &[u8]) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    std::io::Write::write_all(&mut file, buf)
}
