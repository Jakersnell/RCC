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

### Function Compilation

During compilation functions have a variety of operations done. Firstly a function can have parameters, and these
parameters
need to be organized so that they can be passed in CPU registers or on the stack, and the codegen phase needs to
determine which method will be used and how it will be done. Second, those parameters, and all other variables in the
function, will need
stack allocation instructions to be generated in the LLIR so that they have a memory location to occupy.
Third, the instructions in the function need to be compiled, or broken down into their most primitive components that
can
be executed by the CPU.
A program is a set of steps that is usually executed
sequentially. During compilation each type of instruction like
add, sub, goto, and others, are broken into single commands. At an assembly level these are usually commands that are
directly executed by the CPU. The LLIR produced by the codegen phase is very similar to assembly, and thus the program
is compiled into its most primitive instructions.

### Statements: Basic Blocks and Branches

Statements in a program such as if, while, for, etc, generally do not exist at the assembly level. These constructs are
replaced with two things. Branch instructions aka goto statements, tell the program to go to another part of the
program, and labels,
which mark locations in the program so that they can be accessed, or "jumped to" from branch instructions. These are
what
all types of control flow break down into at such a low level.

Basic blocks are blocks representing logical segments of an assembly or ssa like program. These blocks are meant to
represent the program as a control flow graph or CFG for short. A basic block is created like this. While stepping
through your program, at the start you begin a basic block. When you encounter a label statement, you end the current
basic block or complete it, and then start a new one and add the label into the new block, this is that blocks label
and is used to identify the block. Second, if you encounter a return, or branch statement, add that into the current
block and create a new block to be used. For all other types of statements you add them into the current block. At the
end of this process you have converted the program into a set of basic blocks that can be used to construct a control
flow graph. A control flow graph represents the structure of the program. See google for more on control flow graph.

    Ex program:
    
    int main() {
        int x = 3;
        if (x) {
            printf("test");
        }
        return 0;
    }

    Ex program as basic blocks (in pseudo code):
        
        static string arg1 = "test";
        fn main:
            label entry:
                stack_alloc i32 x;
                goto body;
            
            label body:                               
                assign x, i32 3;
                cond_goto x, if_then, end

            label if_then:
                load r0, @arg1;             // load string ptr into fn arg register
                fn_call printf;
                goto end;

            label end:
                return i32 0;

### Lval operations

Lval operations are operations that involve assigning a value into a memory location, or register in the case of
register
variables in C or other languages that allow for similar functionality. These values are on the left-hand side of
assignment expressions and thus are given the name Lval. During compilation these assignment operations are implemented
as storing a Rvalue or literal data value into the memory location of a Lval.

