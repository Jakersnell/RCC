# How to use

## [Back to README](../README.md)

### How to install

#### You must have LLVM and LLC version 18.1.7 installed to procede

To use the compiler by cloning and building the repository follow these steps

1. clone this repository with git, by using the following commands in your terminal
   ```cd <path-to-your-preferred-containing-directory-here>```
   ```git clone <this-repository-url-here>```
2. in the repository on your local machine run this command
   ```cargo build --release```
3. run the following command
   ```cp ./target/release/microc /usr/bin```
4. call the microc compiler on a micro c file, the file will be output as a runnable binary file
   ```microc test.c```

### Micro C syntax

Micro C is almost exactly the same as C except for a few things.

1. Strings are no longer char[] or char *, they are always unsigned char *. This is due to how strings are stored in the
   binary.
2. Fn pointers do not exist.
3. Nested pointers do not exist, this means pointers to pointers do not exist. I chose this for simplicity purposes.
4. long longs do not exist.
5. shorts do not exist.
6. anonymous structs do not exist.
7. const does not exist.
8. storage specifiers do not exist.
9. declaring a function with varargs is invalid. only builtin functions can have varargs.
10. the entire preprocessor system does not exist.
11. printf's formatter string is unchecked for validity.
12. the compiler only supports single file programs, as there is currently no way to import from other files.

### Micro C built in functions

Micro C supports a number of builtin functions.

      int printf(unsigned char * formatter, ...)
      
      void * malloc(unsigned long size)
      
      void free(void * ptr)
