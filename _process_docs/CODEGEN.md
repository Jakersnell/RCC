# Code Generation

## [Codegen source code](../src/codegen/mod.rs)

## [Back to README](../README.md)

Code generation (codegen) is the process of producing lower-level code, either assembly or LLVM Intermediate
Representation (LLIR), to create a runnable binary. This compiler uses the LLVM compiler infrastructure through the Rust
Inkwell wrapper, which provides a Rust API around the LLVM API. LLVM is a framework that uses a low-level code called
LLIR, which is very close to assembly but is platform-independent (for the most part). It uses LLIR to produce native
assembly and binaries on various platforms.

The codegen phase is responsible for using MLIR to produce LLIR, which the LLVM backend then uses to produce a native
runnable binary. This phase is where higher-level concepts get converted into bit-level representations. For example,
string literals are converted into static strings embedded in the program's binary and are treated as pointers in all
other places in the program. At this level, most things are pointers.

### Struct Type Creation

A struct type is represented differently in LLIR than in the MLIR. Because of this we need to convert the representation
of structs from MLIR to LLIR. At a binary level structs do not actually exist. They are a compiler tool used to keep
track of where in memory certain items (fields) will be located. An LLIR struct is a lot more low level than an MLIR
struct.

    Ex: 
        Struct in MLIR:
            struct Test {
                int x;
                char * str;
                double f;
            };
    
        Same struct in LLIR:
            %Test = type { i32, ptr, float }

A few things are different from the MLIR to the LLIR. Firstly, the field names no longer exist. This is because the
only piece of data relevant at this level is how many bytes needed to offset from the first byte in the struct to
retrieve relevant data. If you wanted to retrieve the 'Test.str' member of a struct, you would see that the struct has
20 bytes of data (in this example, in the real world you would have padding and alignment changes). The int takes up 4
bytes, the char pointer 8 bytes, and the double 8 bytes. This means if the struct is located at 0x00, to access the
'str' field of the struct, you would need to access the bytes at 0x08 to 0x10, which is the size of the char pointer,
and its location in memory (On little endian systems).

Another good observation is that the 'char *' is turned into a 'ptr'. This is because fundamentally at a low level all
pointers are the same. The type distinction is just used for higher level compiler functions.

### Global Variable Compilation

### Builtin Function Declaration

### Function Compilation

### Statements: Basic Blocks and Branches

### Lval operations

### Rval operations


