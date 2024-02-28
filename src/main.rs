#![allow(unused)]

mod ast;
mod ast_pretty_print;
mod codegen;
mod error;
mod hlir;
mod lex;
mod lowering;
mod mlir;
mod parse;
mod str_intern;
mod tokens;
mod util;
mod validation;

/// The main entry point for the program.
/// This will be last to be completed because it's just a CLI and
/// requires the API to be complete in order to function.
/// Cant compile a program if you don't have a compiler.

const PRETTY: bool = true;

fn main() {
    let src = "
int main() {
    x.y.z = y = 3;
    return 0;
}
";
    let lexer = lex::Lexer::new(src.to_string());
    let program = util::Program::new("test.c".to_string());
    let parser = parse::Parser::from_lexer(program, lexer);
    let program = parser.parse();
    let body = program.body.unwrap().unwrap();

    if PRETTY {
        for decl in body {
            println!("{}", decl);
        }
    } else {
        println!("{:#?}", body);
    }
}

#[test]
fn check_test_files_are_ok() {
    let tests = [
        "structs.c",
        "comments_and_functions.c",
        "sizeof.c",
        "const_ptr.c",
        "member_access.c",
    ];

    for test in tests {
        let source = std::fs::read_to_string(format!("_c_test_files/{}", test))
            .expect("Could not read file.");
        let lexer = lex::Lexer::new(source);
        let program = util::Program::new(test.to_string());
        let parser = parse::Parser::from_lexer(program, lexer);
        let program = parser.parse();
        let body = program.body.unwrap();
        match body {
            Ok(body) => {
                assert!(!body.is_empty(), "No body in file: {}", test);
            }
            Err(errors) => panic!("File {} errors:\n{:#?}", test, errors),
        }
    }
}
