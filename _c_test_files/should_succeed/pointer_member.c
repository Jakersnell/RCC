struct test {
    int a;
    int b;
    double f;
};

void foo(struct test *_struct, int *b) {
    _struct->a = *b;
}

void main() {
    struct test t;
    int x = 0;
    foo(&t, &x);
}