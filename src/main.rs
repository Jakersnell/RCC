#![allow(unused)]

use std::fmt::Display;
use std::path::PathBuf;
use std::process::Command;

use clap::Parser as ArgParser;
use inkwell::context::Context;
use thiserror::__private::AsDisplay;

use crate::analysis::Analyzer;
use crate::codegen::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod analysis;
mod codegen;
mod data;
mod lexer;
mod parser;
mod util;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI frontend and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't have a compiler.

static mut ARGS: Option<Args> = None;

fn file_path() -> &'static str {
    unsafe { &ARGS.as_ref().unwrap().file_path }
}

macro_rules! build_access_flag {
    ($($flag:ident),+) => {
        $(
            pub(crate) fn $flag() -> bool {
                unsafe { ARGS.as_ref().unwrap().$flag }
            }
        )+
    };
}

build_access_flag!(
    display_ast,
    display_mlir,
    display_llvm_graph,
    display_internal_graphs,
    output_lexer,
    output_parser,
    output_analyzer,
    stop_at_lexer,
    stop_at_parser,
    stop_at_analyzer
);

#[derive(ArgParser, Debug, Default)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "The file path for the source file to compile.")]
    file_path: String,

    #[arg(short, long, help = "Display abstract syntax tree.")]
    display_ast: bool,

    #[arg(short, long, help = "Display the validated mid level ir.")]
    display_mlir: bool,

    #[arg(short, long, help = "Output LLVM graphs as '.dot' files.")]
    display_llvm_graph: bool,

    #[arg(short, long, help = "Output internal CFG graphs as '.dot' files.")]
    display_internal_graphs: bool,

    #[arg(short, long, help = "Display raw data output from the Lexer.")]
    output_lexer: bool,

    #[arg(short, long, help = "Display raw data output from the Parser.")]
    output_parser: bool,

    #[arg(short, long, help = "Display raw data output from the Analyzer.")]
    output_analyzer: bool,

    #[arg(short, long, help = "Stop operation after completing the Lexer phase.")]
    stop_at_lexer: bool,

    #[arg(
        short,
        long,
        help = "Stop operation after completing the Parser phase."
    )]
    stop_at_parser: bool,

    #[arg(
        short,
        long,
        help = "Stop operation after completing the Analyzer phase."
    )]
    stop_at_analyzer: bool,
}

fn main() {
    unsafe {
        let mut args = Args::parse();
        args.output_analyzer = true;
        args.stop_at_analyzer = true;
        ARGS = Some(args);
    }

    if let Err(errors) = run() {
        for error in errors {
            eprintln!("{}", error);
        }
    }
}

fn run() -> Result<(), Vec<String>> {
    let file_path = file_path();
    let source = load_src(file_path.into())?;
    let llir = compile(source)?;
    output_file(file_path.into(), llir)?;
    link(file_path.into())?;
    Ok(())
}

/// the parser outputs errors as a vec and so this function
/// is to homogenize the error reporting on various steps
#[inline]
fn display_to_vec<T: Display>(item: T) -> Vec<String> {
    vec![format!("{}", item)]
}
fn load_src(path: PathBuf) -> Result<String, Vec<String>> {
    if !path.exists() {
        Err(display_to_vec(format!(
            "'{:?}' does not exist.",
            path.as_display()
        )))?;
    }
    let file_name = path.file_name();
    if file_name.is_none() {
        Err(display_to_vec(format!(
            "'{:?}' is not a file.",
            path.as_display()
        )))?;
    }
    let source = std::fs::read_to_string(path);
    if let Err(io_error) = source {
        return Err(display_to_vec(format!("{}", io_error)));
    }
    let source = source.unwrap();
    Ok(source)
}

fn compile(source: String) -> Result<String, Vec<String>> {
    macro_rules! abort {
        () => {
            return Err(vec![]);
        };
    }

    let lexer = Lexer::new(source.into());
    let lexemes = lexer.lex_all().map_err(|errors| {
        errors
            .into_iter()
            .map(|err| err.to_string())
            .collect::<Vec<_>>()
    })?;

    if output_lexer() {
        println!("{:#?}", lexemes);
    }

    if stop_at_lexer() {
        abort!();
    }

    let ast = Parser::new(lexemes.into_iter())
        .parse_all()
        .map_err(|vec| {
            vec.into_iter()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
        })?;

    if display_ast() {
        println!("{}", ast); // pretty print
    }

    if output_parser() {
        println!("{:#?}", ast); // disgusting print
    }

    if stop_at_parser() {
        abort!();
    }

    let analyzer = Analyzer::new(ast);
    let mlir = analyzer.validate().map_err(|rep| {
        rep.borrow()
            .errors
            .iter()
            .map(|err| err.to_string())
            .collect::<Vec<_>>()
    })?;

    if display_mlir() {
        println!("{}", mlir); // pretty print
    }

    if output_analyzer() {
        println!("{:#?}", mlir); // disgusting print
    }

    let context = Context::create();
    let module = context.create_module("main");
    let compiler = Compiler::new(&mlir, &context, &module);
    let llir = compiler.compile().map_err(display_to_vec)?;

    Ok(llir.to_string())
}

fn output_file(path: PathBuf, llir: String) -> Result<(), Vec<String>> {
    assert!(path.exists());
    let ll_path = path.with_extension("ll");
    std::fs::write(ll_path, llir).map_err(display_to_vec)
}

fn link(path: PathBuf) -> Result<(), Vec<String>> {
    let base_path = path.parent();
    let base_path = if let Some(base_path) = base_path {
        Ok(base_path.to_path_buf())
    } else {
        std::env::current_dir()
    };
    let base_path = base_path.map_err(display_to_vec)?;

    let file_name = path
        .file_name()
        .expect("File should have prior verification.")
        .to_string_lossy()
        .to_string();

    let mut reported = vec![];

    macro_rules! cmd {
        ($cmd:expr, $($args:expr),*) => {
            let mut command = Command::new($cmd);
            command.args(&[
                $(
                    $args,
                )*
            ]);
            let result = command.output();
            if let Err(err) = result {
                reported.push(format!("{}", err));
            }
        };
    }

    macro_rules! filename {
        ($ext:literal) => {
            &format!("{file_name}.{}", $ext)
        };
    }

    cmd!("llc", filename!("ll"), "-o", filename!("s"));
    cmd!("as", filename!("s"), "-o", filename!("o"));
    cmd!(
        "ld",
        "-o",
        &file_name,
        filename!("o"),
        "-e",
        "_main",
        "-arch",
        "arm64",
        "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
        "-lSystem"
    );

    if !reported.is_empty() {
        Err(reported)
    } else {
        Ok(())
    }
}

/// Integration tests
#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{Args, lexer, parser};
    use crate::data::ast::{Expression, InitDeclaration};

    fn init_args() {
        let mut args = Args {
            file_path: "".to_string(),
            display_ast: false,
            display_mlir: false,
            display_llvm_graph: false,
            display_internal_graphs: false,
            output_lexer: false,
            output_parser: false,
            output_analyzer: false,
            stop_at_lexer: false,
            stop_at_parser: false,
            stop_at_analyzer: false,
        };
        unsafe {
            super::ARGS = Some(args);
        }
    }

    pub(crate) fn get_file_paths(path: &PathBuf) -> std::io::Result<Vec<PathBuf>> {
        let mut paths = Vec::new();
        let res_paths = std::fs::read_dir(path)?.map(|res| res.map(|entry| entry.path()));
        for path in res_paths {
            paths.push(path?);
        }
        Ok(paths)
    }

    mod should_succeed {
        use std::panic::catch_unwind;

        use crate::compile;
        use crate::util::display_utils::indent_string;

        fn test_should_succeed_file(filename: &str) {
            crate::tests::init_args();
            let file_path = format!("_c_test_files/should_succeed/{}.c", filename);
            let test = std::fs::read_to_string(file_path).expect("Could not read file.");
            match catch_unwind(|| compile(test)) {
                Ok(result) => {
                    if let Err(errors) = result {
                        let mut error_display = String::new();
                        error_display.push_str("ERRORS: {");
                        for mut err in errors {
                            err = indent_string(err, 0, 2);
                            error_display.push_str(&err);
                        }
                        error_display.push_str("}}");
                        panic!("Test '{filename}' failed!");
                    }
                }
                Err(err) => {
                    panic!("Panic in thread from file '{:#?}'.", filename);
                }
            };
        }

        #[test]
        fn pointer_member() {
            test_should_succeed_file("pointer_member")
        }

        #[test]
        fn basic_blocks() {
            test_should_succeed_file("basic_blocks")
        }

        #[test]
        fn comments_and_functions() {
            test_should_succeed_file("comments_and_functions")
        }

        #[test]
        fn const_ptr() {
            test_should_succeed_file("const_ptr")
        }

        #[test]
        fn control_flow_analysis() {
            test_should_succeed_file("control_flow_analysis")
        }

        #[test]
        fn fuzz() {
            test_should_succeed_file("fuzz")
        }

        #[test]
        fn member_access() {
            test_should_succeed_file("member_access")
        }

        #[test]
        fn sizeof() {
            test_should_succeed_file("sizeof")
        }

        #[test]
        fn structs() {
            test_should_succeed_file("structs")
        }
    }

    mod should_fail {
        use std::panic::catch_unwind;

        use crate::compile;

        fn test_should_fail_file(name: &str) {
            crate::tests::init_args();
            let file_path = format!("_c_test_files/should_succeed/{}.c", name);
            let test = std::fs::read_to_string(file_path).expect("Could not read file.");
            match catch_unwind(|| compile(test)) {
                Ok(result) => {
                    assert!(result.is_err());
                }
                Err(err) => {
                    panic!("Panic in thread from file '{:#?}'.", name);
                }
            };
        }

        #[test]
        fn control_flow_analysis() {
            test_should_fail_file("control_flow_analysis")
        }

        #[test]
        fn declaration_in_expression() {
            test_should_fail_file("declaration_in_expression")
        }

        #[test]
        fn function_in_struct() {
            test_should_fail_file("function_in_struct")
        }

        #[test]
        fn if_in_condition() {
            test_should_fail_file("if_in_condition")
        }
    }

    #[test]
    fn test_subscripting_order() {
        let src = "int y = &x[3];";
        let lexer = lexer::Lexer::new(src.into()).lex_all().unwrap();
        let parser = parser::Parser::new(lexer.into_iter());
        let result = parser.parse_all().unwrap();
        let var_dec = result.first().expect("Expected non-empty result.");

        let variable = match var_dec {
            InitDeclaration::Declaration(variable) => variable,
            _ => panic!("First element should be a variable declaration!"),
        };

        let init = &**variable
            .initializer
            .as_ref()
            .expect("Expected initializer in variable declaration.");

        match init {
            Expression::Unary(crate::data::ast::UnaryOp::AddressOf, _) => (),
            _ => panic!("Incorrect Parse Tree: Expected unary operator AddressOf '&'"),
        };
    }
}
