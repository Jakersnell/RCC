struct test {
    int x;
    unsigned char * str;
    double f;
};

unsigned char * hello = "hello world!";

void print_struct_test(struct test * ptr) {
    printf("struct test {\n\tx = %d;\n\tstr = \"%s\";\n\tf = %f;\n};\n", ptr->x, ptr->str, ptr->f);
}

int main() {
    struct test * ptr = (struct test *) malloc(sizeof(struct test));

    ptr->x = 3;
    ptr->str = hello;
    ptr->f = 3.14;

    print_struct_test(ptr);

    return 0;
}