# micro c
This project is currently a work in progress.
A compiler to implement a subset of C language features. Thus dubbed "Micro C".
Since this does not follow any C specification, I wrote up a formal lexical grammar for the language, I plan on extending this to a small formal specification.
___
## Directory

### [Formal specification of the language parsing grammar. ](MICRO-C-GRAMMAR.md)

###  [Current roadmap](ROADMAP.md)

### [Current stage analysis](CURRENT.md)

---

### Why work on this?
I am doing this project to study program compilation. I want to write a fully fledged and complete C compiler some day and this is a good precursor to that. 

The phases I hope for this compiler to have are as follows.
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
