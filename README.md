# microc
This project is currently a work in progress.
A compiler to implement a subset of C language features. Thus dubbed "Micro C".

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
And parse it to the following Abstract Syntax Tree.

You may notice some things look fairly similar to the
source code, this is because I designed the "pretty_print" function
to display certain elements like variable declarations as we are familiar
with seeing them. Other more important elements like the AST itself are
displayed similar to the output of the Unix "tree" command.

---
The non "pretty printed" actual AST data structure can be viewed [here](example_ast.ron). 
It is very verbose, but is a good illustration for how the data is parsed by the compiler.
```
double take_and_return_double(double d) {
    int new_double = (
        +
        ├─ *
        │  ├─ d
        │  └─ 2
        └─ 1
    )
    return
        └─ d
}

int x = (
    4
)

double y;

int main() {
    y = (
        +
        ├─ y
        └─ x
    )
    return
        └─ 0
}

```
This is really cool because the AST represents the logical structure of the program.
More to come soon!

### Roadmap
<pre>
[X] Integer arithmetic lexing / parsing 
[X] Basic types
[X] Variables
[X] Scope
[X] Function declarations
[ ] "int main" entry point
[ ] First codegen with cranelift
[ ] Temp Buildin of printf function
[ ] If control flow
[ ] While loops
[ ] For loops
[ ] Arrays 
[ ] Pointers & buildin of malloc and free functions
[ ] Structs
[ ] Function pointers
</pre>

#### Why?
I am doing this project to study program compilation. I want to write a fully fledged C compiler and this is a good precursor to that. 

The phases I hope for this compiler to have are as follows.
1. Lexical analysis (Reading literals, keywords, operators, etc)
2. Parsing (Reading the logical structure of the code)
3. Binding (Type checking and the like)
4. Codegen (Creating the IR, linking the object files, creating a binary)
