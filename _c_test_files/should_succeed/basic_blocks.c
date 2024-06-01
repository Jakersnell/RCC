/*
    This test is used to debug and test basic blocks for codegen
*/
void fizz_buzz(int x) {
    int is_mod_three = 2;
    int is_mod_five = 1;
    if (is_mod_three & is_mod_five) {
        printf("fizzbuzz\n");
    } else if (is_mod_three) {
        printf("fizz\n");
    } else {
        printf("buzz\n");
    }
}

int main() {
    for (int i = 0; i < 100; i++) {
        if (i % 9 == 0) {
            printf("i is evenly divisible by 9\n");
        }
        fizz_buzz(i);
    }
    return 0;
}