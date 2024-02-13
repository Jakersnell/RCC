# rcc
A longterm project for a simple C compiler in rust. I will be working on this until it is done, and im very excited for where this goes.


### Roadmap
<pre>
[ ] Integer arithmetic lexing / parsing 
[ ] Boolean algebra lexing / parsing
[ ] Basic types
[ ] Variables
[ ] Scope
[ ] Function declarations
[ ] "int main" entry point
[ ] First codegen with cranelift
[ ] Temp Buildin of printf for testing
[ ] If control flow
[ ] While loops
[ ] For loops
[ ] Arrays 
[ ] Pointers / buildin of malloc and free for testing
[ ] Structs
[ ] #include directive
[ ] Function pointers
</pre>

#### Why?
I am doing this project to study program compilation. I want to write a fully fledged C compiler and hopeful this someday becomes that. 

The phases I hope for this compiler to have are as follows.
1. Preprocessor (#include, #define, etc)
2. Lexical analysis (Reading literals, keywords, operators, etc)
3. Parsing (Reading the logical structure of the code)
4. Binding (Type checking and the like)
5. Codegen (Creating the IR, linking the object files, creating a binary)
