# microc
This project is currently a work in progress.
A compiler to implement a subset of C language features. Thus dubbed "Micro C".
Since this does not follow any C specification, I wrote up a formal lexical grammar for the language, I plan on extending this to a small formal specification.
You can view the formal grammar [here](MICRO-C-GRAMMAR.md).

## Current Stage
Currently we are able to take in syntax such as this C code:
```
// int x = 1 + 4 / 2 + y * 20; this is in a comment
/*
double y = 2.3434; this is also in a comment, it will be ignored by the lexer
*/

double take_and_return_double(double d) {
    int new_double = d * 2 + 1;
    return d;
}

int x = 4;
double y;

int main() {
    y = y + x;
    return 0;
}
```
And parse it to the following Abstract Syntax Tree. Printed by the 'src/ast_pretty_print.rs' system to be more readable.
```
<init dec> <fn> double take_and_return_double(double d) {
    <stmt> <var> int new_double = <expr> (
        +
        ├─ 1
        └─ *
           ├─ 2
           └─ d
    )
    <stmt> return <expr> (
        d
    )
}

<init dec> <stmt> <var> int x = <expr> (
    4
)

<init dec> <stmt> <var> double y;

<init dec> <fn> int main() {
    <stmt> <expr> (
        y =
        └─ +
           ├─ x
           └─ y
    )

    <stmt> return <expr> (
        0
    )
}
```
This is really cool because the AST represents the logical structure of the program.
More to come soon!

---
You may notice some things look fairly similar to the
source code, this is because I designed the "pretty_print" function
to display certain elements like variable declarations as we are familiar
with seeing them. Other more important elements like the AST itself are
displayed similar to the output of the Unix "tree" command.

---
The non "pretty printed" actual AST data structure can be viewed [here](example_ast.ron). 
It is very verbose, but is a good illustration for how the data is parsed by the compiler.

### Roadmap
<pre>
[X] Integer arithmetic lexing / parsing 
[X] Basic types
[X] Variables
[X] Scope
[X] Function declarations
[X] "int main" entry point
[ ] Binding and HIR
[ ] Initial lowering and LLIR
[ ] Temp Buildin of printf function
[ ] First codegen with cranelift
[ ] If control flow
[ ] While loops
[ ] For loops
[ ] Arrays 
[ ] Pointers & buildin of malloc and free functions
[ ] Structs
[ ] Function pointers
</pre>

#### Why work on this?
I am doing this project to study program compilation. I want to write a fully fledged and complete C compiler some day and this is a good precursor to that. 

The phases I hope for this compiler to have are as follows.
1. Lexical analysis (Reading literals, keywords, operators, etc)
2. Parsing (Reading the logical structure of the code)
3. Semantic analysis (Type checking, variable validation, and the like)
4. Codegen (Creating the IR, linking the object files, creating a binary)

### Resources I have used to help me with this project
- [ANSI C specification pdf](https://web.archive.org/web/20200909074736if_/https://www.pdf-archive.com/2014/10/02/ansi-iso-9899-1990-1/ansi-iso-9899-1990-1.pdf)
- [Stanford CS143 semantic analysis](https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/handouts/180%20Semantic%20Analysis.pdf)
- [Developing a C compiler from scratch in C - Daniel McCarthy](https://www.udemy.com/course/creating-a-c-compiler-from-scratch-module-1)
- [Compiler Design - neso academy](https://www.youtube.com/playlist?list=PLBlnK6fEyqRjT3oJxFXRgjPNzeS-LFY-q)
- [Building a compiler - Immo Landwerth](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y)
- [The rust compiler - for reference](https://github.com/rust-lang/rust)
- [Salwater compiler - for reference](https://github.com/jyn514/saltwater)
