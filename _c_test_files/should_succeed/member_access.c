struct test {
    int a;
    int b;
    int c;
};

int main() {
    struct test t;
    t.a = 1;
    t.b = 2;
    t.c = 3;
    return t.a + t.b + t.c;
}