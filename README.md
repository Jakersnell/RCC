# Micro-C

### A small handwritten compiler focusing on a subset of the C language. 

---

### These elements include but are not limited to

- ##### Primitive types (int, char, float, double, long)
- ##### Arrays, pointers, and structs
- ##### Functions (not function pointers, yet!)
- ##### Control flow (if, else, while, for)
- ##### Expressions (arithmetic, logical, bitwise, and relational)
- ##### Variable declarations and assignments
- ##### Comments (single-line and multi-line)
- ##### Built-in library functions (printf, malloc, free, etc.)
- ##### Casting (implicit and explicit)

---

#### Notice: This compiler only supports apple Arm64 CPUs

---
## About the Project

### Goal

The goal of this project is to create a simple, yet powerful, compiler that can take in a file written in Micro-C
and output an executable file. I chose to implement this fashion of C as to focus on core features such as loops,
functions, etc. While not focusing on implementing every C language feature so that the result was still achievable.

### Why

I have been programming for a few years now and compilers always felt like this elusive black box of magic. I was
extremely curious about how this thing may work as going from point A to B seemed so beyond me. I stand by the
belief that anyone can achieve nearly anything so long as they work hard at it, so I began doing research. I would like
to give a special thanks to [Immo Landwerth](https://www.youtube.com/@ImmoLandwerth) on YouTube as I watched his
livestream series where he writes [Minsk](https://github.com/terrajobst/minsk/tree/master)
and that gave me the inspiration to feel
that it was possible to write my own compiler.
I first wrote simple stuff like the .bf interpreter and .bf compiler on my GitHub. Then after doing more research I
decided
to start writing this compiler. I chose Rust as I wanted more experience in the language, and I really enjoy the
language.

### How - more detailed descriptions can be found via the directory below

Compilers can be broken up into two main phases, frontend and backend. The frontend of a compiler deals with high level
language concepts such as the rules for the language. The frontend is implemented from scratch as the functionality is
highly specific to this language. This deals with tokenizing the source text program. Then representing the program as a
tree and performing syntactic validation. The program will then be lowered into a more low level tree form, the
mid-level intermediary representation or MLIR.
This form is used to perform semantic analysis on the program, to both verify its integrity, and process it further.
After this the program is briefly represented as a graph to
perform control flow analysis on the program. If the program makes it to this point without rejection it is passed to
the backend.
The backend of the compiler works at a much lower level and deals
with optimizing binary output and such. There are a ton of frameworks that make the backend much easier on the
developer. The rust compiler uses [LLVM](https://llvm.org/). The one I am using is [Cranelift](https://cranelift.dev/),
a compiler
backend framework designed
for
the [WasmTime](https://github.com/bytecodealliance/wasmtime) runtime. I initially learned about this while studying
the [Saltwater](https://github.com/jyn514/saltwater) compiler project. I
chose this my backend framework as [LLVM](https://llvm.org/) is
extremely complex and very overkill for this project. [Cranelift](https://cranelift.dev/) is much easier to pick up and
still provides the necessary features for this project. After the backend is finished with its work it then emits a
binary executable that can be run by the computer.

### Testing

The compiler makes use of Rust testing with the annotation ```#[cfg(test)]``` for conditional test compilation. As per
rust conventions the tests are within the file of the items they are testing. There are an array of unit and integration
tests throughout the compiler to verify integrity of the system. The only code ever pushed to this repository is code
that passes all of these tests. In ```src/_c_test_files``` you will see integration tests that correspond to which type
of test they are based on the directory they are within.
___

#### All phases of the compiler are finished except for emission. Emission is being worked on currently.

---

## Directory

### [Semantic analysis process description - Phase 3](_process_docs/SEMANTIC-ANALYSIS.md)

### [Parsing process description - Phase 2](_process_docs/AST-PROCESS.md)

### [Lexing process description - Phase 1](_process_docs/LEXING.md)

### [Specification for casting operations](_process_docs/CASTING_SPEC.md)

### [Formal specification grammar ](_process_docs/MICRO-C-GRAMMAR.md)

### [Roadmap](_process_docs/ROADMAP.md)

---

### Phases of the compiler

1. Lexical analysis (Reading literals, keywords, operators, etc) {done}
2. Parsing (Reading the logical structure of the code) {done}
3. Semantic analysis (Type checking, variable validation, and the like) {in progress}
4. Codegen (Creating the IR, linking the object files, creating a binary)

---

### Resources I have found helpful

- [ANSI C specification pdf](https://web.archive.org/web/20200909074736if_/https://www.pdf-archive.com/2014/10/02/ansi-iso-9899-1990-1/ansi-iso-9899-1990-1.pdf)
- [Stanford CS143 semantic analysis](https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/handouts/180%20Semantic%20Analysis.pdf)
- [Developing a C compiler from scratch in C - Daniel McCarthy](https://www.udemy.com/course/creating-a-c-compiler-from-scratch-module-1)
- [Compiler Design - neso academy](https://www.youtube.com/playlist?list=PLBlnK6fEyqRjT3oJxFXRgjPNzeS-LFY-q)
- [Building a compiler - Immo Landwerth](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y)
- [The rust compiler - for reference](https://github.com/rust-lang/rust)
- [Salwater compiler - for reference](https://github.com/jyn514/saltwater)
