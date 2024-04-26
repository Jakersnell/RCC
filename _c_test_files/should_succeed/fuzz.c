// fuzz.c
/*
This file is a fuzzing test for the compiler, fuzzing is essentially throwing random input
at the compiler to see if it crashes. This is a good way to find bugs and other issues.
all the code in this file is semantically correct, and should compile. if it doesnt then there
is a bug in the compiler.
*/

const double PI = 3.14159;

struct test {
    int a;
    int b;
    double f;
};

void foo(struct test *a, int *b) {
    a->a = *b;
}

const int x = {1};

int main() {
    int y;
    int x = sizeof(y);
    char c = 'a';
    char *p = &c;
    double d = c++ * *p + 4 + 4;
    struct test t;
    t.a = 4;
    t.b = 5;
    t.b++;
    ;;;;;;;
    t.a = sizeof(struct test);
    struct test *pt = &t;
    pt->a = 4;
    pt->b = 5;
    if (pt->a == 4) {
        pt->b = 6;
    }
    if (pt->b == 5) {
        pt->a = 5;
    } else {
        pt->a = 6;
    }
    while (pt->a < 10) {
        pt->a++;
    }
    for (int i = 0; i < 10; i++) {
        pt->a++;
    }
// todo: support initialization without declaration
//    int i;
//    for (i = 0; i < 10; i++) {
//        pt->a++;
//    }
    for (;;) {
        pt->a++;
    }
    int i;
    int array[] = {1, 2, 3, 4, 5};
    foo(pt, &i);
    return 0;
}