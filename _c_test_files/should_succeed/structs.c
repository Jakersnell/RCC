struct test1 {
    int x;
    int y;
};

struct test2 {
    struct test1 t1;
    int z;
};

int main() {
    struct test2 t2;
}