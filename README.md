# microc
This project is currently a work in progress.
A compiler to implement a subset of C language features. Thus dubbed "Micro C".

## Current Stage
Currently we are able to take in syntax such as this C code:
```
int x = 1 + 4 / 2 + y * 20;
double y = 2.3434;

double take_and_return_double(double d) {
    int new_double = d * 2 + 1;
    return d;
}

int main() {
    y = y + x;
    return 0;
}
```
And parse it to the following Abstract Syntax Tree
```
int x
└── =
   ├── x
   └── +
      ├── +
      │  ├── 1
      │  └── /
      │     ├── 4
      │     └── 2
      └── *
         ├── y
         └── 20

double y
└── =
   ├── y
   └── 2.3434

double take_and_return_double(double d) {
    int new_double
    └── =
       ├── new_double
       └── +
          ├── *
          │  ├── d
          │  └── 2
          └── 1
    return
    └── d
}

int main() {
    └── =
       ├── y
       └── +
          ├── y
          └── x
    return
    └── 0
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
