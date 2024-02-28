
## Current Stage
Currently we are able to take in syntax such as this C code:
```
// int x = 1 + 4 / 2 + y * 20; this is in a comment
/*
double y = 2.3434; this is also in a comment, it will be ignored by the lexer
*/

double take_and_return_double(double d) {
    int new_double = d * 2 + 1;
    return d;
}

int x = 4;
double y;

int main() {
    y = y + x;
    return 0;
}
```
And parse it to the following Abstract Syntax Tree. Printed by the 'src/ast_pretty_print.rs' system to be more readable.
```
<fn> double take_and_return_double(double d) {
    <stmt: dec> int new_double = <init-expr> (
        +
        ├─ 1
        └─ *
           ├─ 2
           └─ d
    )
    <stmt: return> (
        d
    )
}

<init-dec> int x = <init-expr> (
    4
)

<init-dec> double y;

<fn> int main() {
    <stmt: expr> (
        y =
        └─ +
           ├─ x
           └─ y
    )
    <stmt: return> (
        0
    )
}
```
This is really cool because the AST represents the logical structure of the program.
More to come soon!

---
You may notice some things look fairly similar to the
source code, this is because I designed the "pretty_print" function
to display certain elements like variable declarations as we are familiar
with seeing them. Other more important elements like the AST itself are
displayed similar to the output of the Unix "tree" command.

---
The non "pretty printed" actual AST data structure can be viewed [here](EX-AST.ron).
It is very verbose, but is a good illustration for how the data is parsed by the compiler.
---
Another great example is this monsterpiece of code. It really demonstrates how the program is represented as a tree.
```
int main() {
    int x = 3;
    int y = 4;
    int z = 1;
    int f = 9;
    x = y = z = (f = 3) + 2 * 3 + 4 * 5 + 6 * 7;
    return 0;
```
And then we turn it into this AST, remember, the ast is evaluated bottom up, left to right.
```
<fn> int main() {
    <stmt> <var-dec> int x = <init-expr> (
        3
    );
    <stmt> <var-dec> int y = <init-expr> (
        4
    );
    <stmt> <var-dec> int z = <init-expr> (
        1
    );
    <stmt> <var-dec> int f = <init-expr> (
        9
    );
    <stmt> <expr> (
        =
        ├─ =
        │  ├─ =
        │  │  ├─ +
        │  │  │  ├─ *
        │  │  │  │  ├─ 7
        │  │  │  │  └─ 6
        │  │  │  └─ +
        │  │  │     ├─ *
        │  │  │     │  ├─ 5
        │  │  │     │  └─ 4
        │  │  │     └─ +
        │  │  │        ├─ *
        │  │  │        │  ├─ 3
        │  │  │        │  └─ 2
        │  │  │        └─ (
        │  │  │           =
        │  │  │           ├─ 3
        │  │  │           └─ f
        │  │  │           )
        │  │  └─ z
        │  └─ y
        └─ x
    );
    <stmt> return <expr> (
        0
    );
}
```