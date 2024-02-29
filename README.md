# Micro-C

### a small compiler focusing on a subset of the C language.

---

### These elements include but are not limited to

- ##### Primitive types (int, char, float, double)
- ##### Arrays, pointers, and structs
- ##### Functions (not function pointers, yet!)
- ##### Control flow (if, else, while, for)
- ##### Expressions (arithmetic, logical, bitwise, and relational)
- ##### Variable declarations and assignments
- ##### Comments (single-line and multi-line)
- ##### Built in library functions (printf, scanf, etc.)

---

## About the Project

The goal of this project is to create a simple, yet powerful, compiler that can take in a file written in Micro-C
and output an executable file. This is a long-term project and I will be updating this file as I make progress.
The reason that I am doing this is to learn more about how compilers work and to improve my programming skills.
I am also interested in learning more about the C language and how it is compiled. I chose to write this in rust because
I absolutely love rust, and the idea seemed funny to me, as rust is built to stop the things that make C so "dangerous".
___

## Directory

### [AST process analysis](AST-PROCESS.md)

### [Formal specification of the language parsing grammar. ](MICRO-C-GRAMMAR.md)

### [Current roadmap](ROADMAP.md)

---

### Phases of the compiler

1. Lexical analysis (Reading literals, keywords, operators, etc)
2. Parsing (Reading the logical structure of the code)
3. Semantic analysis (Type checking, variable validation, and the like)
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
