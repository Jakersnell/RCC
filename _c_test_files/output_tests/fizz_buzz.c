void fizz_buzz(int x) {
    int is_mod_three = !(x % 3);
    int is_mod_five = !(x % 5);
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
        fizz_buzz(i);
    }
    return 0;
}