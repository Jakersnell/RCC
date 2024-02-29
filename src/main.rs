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

const PRETTY: bool = false;

fn main() {
    let src = r#"

struct test {
    int a;
    int b;
};

struct test* create_struct() {
    struct test *t = (struct test*)malloc(sizeof(struct test));
    t->a = 1;
    t->b = 2;
    return t;
}

int main() {
    struct test *t = create_struct();
    printf("t.a = %d, t.b = %d\n", t->a, t->b);
    free(t);
    return 0;
}
    "#;
    let lexer = lex::Lexer::new(src.to_string());
    // println!("{:#?}", lexer.collect::<Vec<_>>());
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::InitDeclaration;
    use crate::util::CompilerResult;
    use crate::util::Program;

    fn run_test_on_file(path: &str) -> CompilerResult<Vec<InitDeclaration>> {
        let source = std::fs::read_to_string(path).expect("Could not read file.");
        let lexer = lex::Lexer::new(source);
        let program = util::Program::new(path.to_string());
        let parser = parse::Parser::from_lexer(program, lexer);
        parser.parse().body.unwrap()
    }

    #[test]
    fn test_should_succeed_files() {
        let tests = [
            "structs.c",
            "comments_and_functions.c",
            "sizeof.c",
            "const_ptr.c",
            "member_access.c",
            "fuzz.c",
            "control_flow_analysis.c",
        ];

        for test in tests {
            let result = run_test_on_file(&format!("_c_test_files/should_succeed/{}", test));
            match result {
                Ok(body) => {
                    assert!(!body.is_empty(), "No body in file: {}", test);
                }
                Err(errors) => panic!("File {} errors:\n{:#?}", test, errors),
            }
        }
    }

    #[test]
    fn test_should_fail_files() {
        let tests = [
            "declaration_in_expression.c",
            "function_in_struct.c",
            "if_in_condition.c",
        ];

        for test in tests {
            let result = run_test_on_file(&format!("_c_test_files/should_fail/{}", test));
            match result {
                Err(errors) => {
                    assert!(!errors.is_empty(), "No errors in file: {}", test);
                }
                Ok(body) => panic!("File {} succeeded:\n{:#?}", test, body),
            }
        }
    }
}
