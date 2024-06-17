#![allow(unused)]

use std::ffi::OsStr;
use std::fmt::Display;
use std::path::{Path, PathBuf};
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
    stop_at_analyzer,
    keep_temp_files
);

#[derive(ArgParser, Debug, Default)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(help = "The file path for the source file to compile.")]
    file_path: String,

    #[arg(long, help = "Display abstract syntax tree.", action)]
    display_ast: bool,

    #[arg(long, help = "Display the validated mid level ir.", action)]
    display_mlir: bool,

    #[arg(long, help = "Output LLVM graphs as '.dot' files.", action)]
    display_llvm_graph: bool,

    #[arg(long, help = "Output internal CFG graphs as '.dot' files.", action)]
    display_internal_graphs: bool,

    #[arg(long, help = "Display raw data output from the Lexer.", action)]
    output_lexer: bool,

    #[arg(long, help = "Display raw data output from the Parser.", action)]
    output_parser: bool,

    #[arg(long, help = "Display raw data output from the Analyzer.", action)]
    output_analyzer: bool,

    #[arg(
        long,
        help = "Stop operation after completing the Lexer phase.",
        action
    )]
    stop_at_lexer: bool,

    #[arg(
        long,
        help = "Stop operation after completing the Parser phase.",
        action
    )]
    stop_at_parser: bool,

    #[arg(
        long,
        help = "Stop operation after completing the Analyzer phase.",
        action
    )]
    stop_at_analyzer: bool,

    #[arg(long, help = "Keep the .ll file produced during compilation.", action)]
    keep_llir: bool,

    #[arg(long, help = "Keep temp files produced during compilation.", action)]
    keep_temp_files: bool,
}

fn main() {
    unsafe {
        // This is safe because this is a single process program
        let mut args = Args::parse();
        ARGS = Some(args);
    }

    if let Err(errors) = run() {
        for error in errors {
            eprintln!("{}", error);
        }
    }
}

fn run() -> Result<(), Vec<String>> {
    let file_path: PathBuf = file_path().into();
    let file_stem = file_path
        .file_stem()
        .map_or_else(
            || {
                Err(vec![format!(
                    "Cannot read file '{}'",
                    file_path.to_str().unwrap()
                )])
            },
            Ok,
        )?
        .to_str()
        .unwrap();
    let current_dir: PathBuf = ".".into();
    let base_path = file_path.parent().unwrap_or_else(|| &current_dir);
    let source = load_src(file_path.clone())?;
    let llir = compile(source)?;
    output_program(base_path, file_stem, llir)?;
    Ok(())
}

fn parse_file_stem_from_path(path: &Path) -> Result<String, Vec<String>> {
    let create_error_message = || {
        Err(vec![format!(
            "Cannot read file '{}'",
            path.to_str().unwrap()
        )])
    };
    let map_os_str = |os_str: &OsStr| Ok(os_str.to_str().unwrap().to_string());

    let file_stem_opt = path.file_stem();
    file_stem_opt.map_or_else(create_error_message, map_os_str)
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

fn output_program(dir_path: &Path, file_stem: &str, llir: String) -> Result<(), Vec<String>> {
    let filepath = dir_path.join(file_stem);
    let ll_filepath = filepath.with_extension("ll");
    let s_filepath = filepath.with_extension("s");
    let o_filepath = filepath.with_extension("o");

    let filepath = filepath.to_str().unwrap().to_string();
    let ll_filepath = ll_filepath.to_str().unwrap().to_string();
    let s_filepath = s_filepath.to_str().unwrap().to_string();
    let o_filepath = o_filepath.to_str().unwrap().to_string();

    let mut reported = vec![];

    std::fs::write(ll_filepath.clone(), llir).map_or_else(
        |_| {
            reported.push(format!("Could not write to '{ll_filepath}'"));
            Err(reported)
        },
        Ok,
    )?;

    Command::new("llc")
        .args([&ll_filepath, "-o", &s_filepath])
        .output()
        .map_or_else(|error| Err(vec![error.to_string()]), Ok)?;

    if !keep_temp_files() {
        std::fs::remove_file(ll_filepath).unwrap();
    }

    Command::new("as")
        .args([&s_filepath, "-o", &o_filepath])
        .output()
        .map_or_else(|error| Err(vec![error.to_string()]), Ok)?;

    if !keep_temp_files() {
        std::fs::remove_file(s_filepath).unwrap();
    }

    Command::new("ld")
        .args([
            "-o",
            &filepath,
            &o_filepath,
            "-e",
            "_main",
            "-arch",
            "arm64",
            "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
            "-lSystem",
        ])
        .output()
        .map_or_else(|error| Err(vec![error.to_string()]), Ok)?;

    if !keep_temp_files() {
        std::fs::remove_file(o_filepath).unwrap();
    }
    Ok(())
}

/// Integration tests
#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{Args, lexer, parser};
    use crate::data::ast::{Expression, InitDeclaration};

    static DISPLAY_ERRORS_DURING_TESTS: bool = true;

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
            keep_llir: false,
            keep_temp_files: false,
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

    macro_rules! unexpected_error_outcome {
        ($filename:expr, $errors:expr) => {
            use crate::tests::DISPLAY_ERRORS_DURING_TESTS;
            if DISPLAY_ERRORS_DURING_TESTS {
                println!("ERRORS: {{");
                for mut err in $errors {
                    println!();
                    err = indent_string(err, 0, 4);
                    println!("{}", &err);
                }
                println!("}}");
            }
            panic!("Test '{}' failed!", $filename);
        };
    }

    mod output_tests {
        use std::panic::catch_unwind;
        use std::path::PathBuf;
        use std::process::Command;

        use crate::{compile, keep_temp_files, output_program};
        use crate::util::display_utils::indent_string;

        fn run_capture_output_test(filename: &str) {
            crate::tests::init_args();
            static BASE: &str = "_c_test_files/output_tests/";

            let src_filepath = format!("{BASE}{filename}.c");
            let expected_output_filepath = format!("{BASE}{filename}.expected_output");
            let src =
                std::fs::read_to_string(src_filepath.clone()).expect("Could not read source file.");
            let expected_output = std::fs::read_to_string(expected_output_filepath)
                .expect("Could not read expected output file.");

            match catch_unwind(|| compile(src)) {
                Ok(result) => match result {
                    Ok(llir) => {
                        let given_output =
                            run_program_capture_output(BASE.into(), filename.into(), llir);
                        assert_eq!(expected_output, given_output);
                    }
                    Err(errors) => {
                        unexpected_error_outcome!(src_filepath, errors);
                    }
                },
                Err(err) => {
                    panic!("Panic in thread from file '{:#?}'.", filename);
                }
            };
        }

        fn run_program_capture_output(
            src_filepath: PathBuf,
            src_file_stem: &str,
            llir: String,
        ) -> String {
            let temp_dir_filepath = src_filepath.join(PathBuf::from("temp"));
            std::fs::create_dir_all(temp_dir_filepath.clone()).unwrap();

            output_program(&temp_dir_filepath, src_file_stem, llir).unwrap();

            let given_output = Command::new(format!("./{src_file_stem}"))
                .current_dir(&temp_dir_filepath)
                .output()
                .unwrap();
            let std_out = given_output.stdout;

            if !keep_temp_files() {
                std::fs::remove_dir_all(&temp_dir_filepath).unwrap();
            }

            String::from_utf8(std_out).expect("Could not convert program output to utf8.")
        }

        macro_rules! test {
            ($name:ident) => {
                #[test]
                fn $name() {
                    run_capture_output_test(stringify!($name))
                }
            };
        }

        #[test]
        fn struct_member() {
            run_capture_output_test("struct_member")
        }

        #[test]
        fn fizz_buzz() {
            run_capture_output_test("fizz_buzz")
        }
    }

    mod should_succeed {
        use std::panic::catch_unwind;

        use crate::compile;
        use crate::util::display_utils::indent_string;

        fn test_should_succeed_file(filename: &str) {
            crate::tests::init_args();
            let file_path = format!("_c_test_files/should_succeed/{}.c", filename);
            let test = std::fs::read_to_string(file_path.clone()).expect("Could not read file.");
            match catch_unwind(|| compile(test)) {
                Ok(result) => {
                    if let Err(errors) = result {
                        unexpected_error_outcome!(file_path.clone(), errors);
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
            let file_path = format!("_c_test_files/should_fail/{}.c", name);
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
