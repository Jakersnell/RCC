# Semantic Analysis

## [Analyzer source code](../src/analysis/mod.rs)

## [Back to README](../README.md)

Semantic analysis is the phase in which the nuanced or more logically complex rules of the language are analyzed and
enforced.
These rules involve elements that may be syntactically correct, but are not correct within the language. This phase also
includes
processing the abstract syntax tree (AST) into a more complete intermediate representation,
the high level intermediate representation (HLIR). This structure holds more information that will be necessary in the
codegen phase,
removes certain high level elements and replaces them with structurally correct representation. For example, sizeof,
during analysis sizeof is replaced with a literal integer value of the type it was evaluating. Other examples include
converting for loops into while loops, converting casts into a proper cast structure, and more. These elements are the
guts of the language, and make it function properly.

# Phases of Semantic Analysis

### Type Checking

Type checking is the process in which the compiler verifies that operations performed in the program are on legal types.
This includes verifying that binary operators (*/+- and more) have correct left hand and right hand types, i.e. 3 * 5 is
an integer multiplied by an integer, and that things like 1.0 * "string" (double * char*) are invalid and will be
rejected. This extends to other things as well, such as assignment, function args, etc. Type checking is an essential
component in assuring the validity of a program before its compilation.

### Symbol Resolution

A symbol can be a variety of things, including variables, struct identifiers, function identifiers, and more. Loosely
speaking it is the name we give a piece of data along with some metadata we may find useful. Symbol resolution is
essential as it allows us to see whether a variable, function, or struct exists. If the symbol does exist we then can
use that symbol to get metadata of the symbol such as what type a variable is, the signature of a function, or what
members a struct possesses.

### Casting

Casting is process in which one data type is converted to another. Casting must be verified by the compiler to ensure
that the cast is correct, meaning that there is a defined procedure to convert the data to another form. There are two
types of casting, implicit and explicit. Implicit casting is done by the compiler, where cast actions are inserted into
the program. And explicit casting is indicated by the user. Both have their own rules but follow roughly the same
process.

### Constant folding

Constant folding is the act of evaluating expressions with constant operands. Constant operands are those that can be
determined at compile time. i.e. x * 3 is not a constant expression due to the usage of a runtime variable, but 3 * 3 is
a constant expression due to both its operands being constant. Constant folding evaluates all possible constant
expressions in the program so that time isn't wasted evaluating these expressions during runtime.

### Lowering

A compiler takes source code written in a programming language and converts it to an actionable form that operates at
the machine or hardware level. A lot of concepts in a high level programming language do not exist at the machine level.
Such concepts include but are not limited to, loops and if statements. The way that the effect of these constructs are
achieved is different at lower levels. Lowering is the process of converting some of these elements into a lower level
form. Thus removing some abstraction. This more primitive form can provide greater ease in certain operations for the
compiler, such as control flow analysis.

### Control Flow Analysis

Control flow analysis is the process of representing the program in a graph form and performing certain analysis on this
graph to process the program and determine qualities of it. This is very useful for a variety of tasks, such as
eliminating dead code, keeping track of unused items, or validating function returns.

## MLIR

The analysis phase of the compiler creates the MLIR, this mlir can be viewed in pretty print with the '--display-mlir'
flag or in 'disgusting print' with the '--output-analyzer' flag when compiling a program. This data structure represents
the logical structure of the program being compiled with a different format than the AST. Some elements that were
present in the AST will not be present in the MLIR, and some elements in the MLIR will not have been present in the AST.
The MLIR is designed to contain the necessary data for the binary codegen phase. The MLIR can be
viewed [here](../src/data/mlir.rs)