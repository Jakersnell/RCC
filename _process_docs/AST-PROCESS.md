# Parser - Phase 2

## [Parser source code](../src/parser/mod.rs)

## [Back to README](../README.md)

The parser takes a stream of tokens as given by the lexer and converts the tokens into an AST, so
long as the given tokens can produce such, meaning that they present valid syntax.

### What is an AST?

An Abstract Syntax Tree (AST) is a representation of the logical structure of a program
as a tree data structure. Each node in the tree denotes a construct occurring in the source code.
A tree is very convenient for representing the logical structure of a program, as it is easy to traverse
and evaluate, while preserving the intended logic. The tree is evaluated bottom to top, left to right.

#### The AST source code can be found [here](../src/data/ast.rs)

##### Example:

```
This expression 
d * 2 + 1
would be represented as

        +
       / \
      *   1
     / \
    d   2 
    
This is correct because it preserves our rules for operator precedence.
As the tree is evaluated bottom to top, left to right, the expression is evaluated correctly.
d * 2 is evaluated first, then the result is added to 1.

The same tree output from my compiler in debug mode would look like this:
    +
    ├─ 1
    └─ *
       ├─ 2
       └─ d
```

A more real-world example.
We are now able to parse the following Micro-C/C program:

### Input Program

```
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
```

We can then parse this into the following AST, which is how the compiler represents the data internally.
The AST is an intermediary data-structure used for the compiler to correctly process the source program.
The compiler will then use this information to generate the necessary assembly code to run the program.

### Stylized AST (using "ast_pretty_print.rs")

```
<struct> (struct test) {
    (int) a;
    (int) b;
};

<fn> (struct test *) create_struct() {
    <stmt> <var-dec> (struct test *) t = <init-expr> (
        <cast> (struct test *) (
            <fn-call> malloc(
                └─ sizeof (
                    (struct test)
                )
            )

        )
    );
    <stmt> <expr> (
        =
        ├─ 1
        └─ ->
           ├─ a
           └─ t
    );
    <stmt> <expr> (
        =
        ├─ 2
        └─ ->
           ├─ b
           └─ t
    );
    <stmt> return <expr> (
        t
    );
}

<fn> (int) main() {
    <stmt> <var-dec> (struct test *) t = <init-expr> (
        <fn-call> create_struct(
        )
    );
    <stmt> <expr> (
        <fn-call> printf(
            ├─ "t.a = %d, t.b = %d\n"
            ├─ ->
            │  ├─ a
            │  └─ t
            └─ ->
               ├─ b
               └─ t
        )
    );
    <stmt> <expr> (
        <fn-call> free(
            └─ t
        )
    );
    <stmt> return <expr> (
        0
    );
}
```

The same AST is actually represented in a more raw format, which contains all the data necessary for the compiler
to continue to the next steps.

#### Raw AST Output in RON format

```
[
    Struct(
        StructDeclaration {
            declaration: Declaration {
                specifier: DeclarationSpecifier {
                    specifiers: [],
                    qualifiers: [],
                    ty: [
                        Struct(
                            "test",
                        ),
                    ],
                },
                declarator: Base,
                ident: None,
            },
            members: [
                Declaration {
                    specifier: DeclarationSpecifier {
                        specifiers: [],
                        qualifiers: [],
                        ty: [
                            Int,
                        ],
                    },
                    declarator: Base,
                    ident: Some(
                        "a",
                    ),
                },
                Declaration {
                    specifier: DeclarationSpecifier {
                        specifiers: [],
                        qualifiers: [],
                        ty: [
                            Int,
                        ],
                    },
                    declarator: Base,
                    ident: Some(
                        "b",
                    ),
                },
            ],
        },
    ),
    Function(
        FunctionDeclaration {
            declaration: Declaration {
                specifier: DeclarationSpecifier {
                    specifiers: [],
                    qualifiers: [],
                    ty: [
                        Struct(
                            "test",
                        ),
                    ],
                },
                declarator: Pointer {
                    to: Base,
                },
                ident: Some(
                    "create_struct",
                ),
            },
            parameters: [],
            varargs: false,
            body: Some(
                Block(
                    [
                        Declaration(
                            VariableDeclaration {
                                declaration: Declaration {
                                    specifier: DeclarationSpecifier {
                                        specifiers: [],
                                        qualifiers: [],
                                        ty: [
                                            Struct(
                                                "test",
                                            ),
                                        ],
                                    },
                                    declarator: Pointer {
                                        to: Base,
                                    },
                                    ident: Some(
                                        "t",
                                    ),
                                },
                                initializer: Some(
                                    Cast(
                                        Declaration {
                                            specifier: DeclarationSpecifier {
                                                specifiers: [],
                                                qualifiers: [],
                                                ty: [
                                                    Struct(
                                                        "test",
                                                    ),
                                                ],
                                            },
                                            declarator: Pointer {
                                                to: Base,
                                            },
                                            ident: None,
                                        },
                                        FunctionCall(
                                            "malloc",
                                            [
                                                Sizeof(
                                                    Type(
                                                        Declaration {
                                                            specifier: DeclarationSpecifier {
                                                                specifiers: [],
                                                                qualifiers: [],
                                                                ty: [
                                                                    Struct(
                                                                        "test",
                                                                    ),
                                                                ],
                                                            },
                                                            declarator: Base,
                                                            ident: None,
                                                        },
                                                    ),
                                                ),
                                            ],
                                        ),
                                    ),
                                ),
                            },
                        ),
                        Expression(
                            Binary(
                                Assign(
                                    Assign,
                                ),
                                PointerMember(
                                    Variable(
                                        "t",
                                    ),
                                    "a",
                                ),
                                Literal(
                                    Integer {
                                        value: 1,
                                        suffix: None,
                                    },
                                ),
                            ),
                        ),
                        Expression(
                            Binary(
                                Assign(
                                    Assign,
                                ),
                                PointerMember(
                                    Variable(
                                        "t",
                                    ),
                                    "b",
                                ),
                                Literal(
                                    Integer {
                                        value: 2,
                                        suffix: None,
                                    },
                                ),
                            ),
                        ),
                        Return(
                            Some(
                                Variable(
                                    "t",
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        },
    ),
    Function(
        FunctionDeclaration {
            declaration: Declaration {
                specifier: DeclarationSpecifier {
                    specifiers: [],
                    qualifiers: [],
                    ty: [
                        Int,
                    ],
                },
                declarator: Base,
                ident: Some(
                    "main",
                ),
            },
            parameters: [],
            varargs: false,
            body: Some(
                Block(
                    [
                        Declaration(
                            VariableDeclaration {
                                declaration: Declaration {
                                    specifier: DeclarationSpecifier {
                                        specifiers: [],
                                        qualifiers: [],
                                        ty: [
                                            Struct(
                                                "test",
                                            ),
                                        ],
                                    },
                                    declarator: Pointer {
                                        to: Base,
                                    },
                                    ident: Some(
                                        "t",
                                    ),
                                },
                                initializer: Some(
                                    FunctionCall(
                                        "create_struct",
                                        [],
                                    ),
                                ),
                            },
                        ),
                        Expression(
                            FunctionCall(
                                "printf",
                                [
                                    Literal(
                                        String {
                                            value: "t.a = %d, t.b = %d\\n",
                                        },
                                    ),
                                    PointerMember(
                                        Variable(
                                            "t",
                                        ),
                                        "a",
                                    ),
                                    PointerMember(
                                        Variable(
                                            "t",
                                        ),
                                        "b",
                                    ),
                                ],
                            ),
                        ),
                        Expression(
                            FunctionCall(
                                "free",
                                [
                                    Variable(
                                        "t",
                                    ),
                                ],
                            ),
                        ),
                        Return(
                            Some(
                                Literal(
                                    Integer {
                                        value: 0,
                                        suffix: None,
                                    },
                                ),
                            ),
                        ),
                    ],
                ),
            ),
        },
    ),
]

```