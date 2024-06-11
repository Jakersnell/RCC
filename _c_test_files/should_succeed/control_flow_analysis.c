int test(int a, int b) {
    if (a > b) {
        return 1;
    } else if (a == b) {
        return 0;
    } else {
        return -1;
    }
}

int main() {
    test(1, 2);
    return 0;
}