# String Literals Codegen

String literals are interesting because there are a lot of ways to implement them, each with their
own positives and negatives. The different ways I am considering for this project are as follows.

### Implementations

#### 1. Global static strings

    A global static string is a string that is allocated and initialized before the execution of the main() entry-point. 
    This string is never re-initialized as long as the program runs. This means that if a function has a string literal 
    within its body and makes changes to this literal during execution of the function, the string literal will maintain
    the changes during the next call of the function.
    
    Pros:
        - simple
        - low execution overhead

    Cons:
        - potential for data change to be unexpected by the user

#### 2. Functional local static strings

    The difference in this type of static string is that the memory has been allocated at startup of the program, but 
    the string literal itself is initialized after the function which the literal is located within is called. This 
    initialization is done before execution of the function body.

    Pros:
        - More scope oriented string lifetime
        - More intuitive behavior
    
    Cons:
        - Higher runtime overhead due to runtime initialization per each call of the given function

#### 3. Local scope specific static strings

    Strings in this scenario are still allocated at program startup, but are initialized at the relative location to the 
    string literal within the program. This means that string literals are consistant accross function calls and loop
    iterations.

    Pros:
        - Consistent string literal accross program constructs
        - More intuitive behavior

    Cons:
        - More runtime overhead

### Selection: Option 1

    I am deciding on choosing option 1 due to its simplicity, which seems like a fit for a C compiler. The added benefit
    to this is the lack of runtime overhead during operation.