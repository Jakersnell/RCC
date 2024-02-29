int test(int x) {
    if (x > 0) {
        return 1;
    } else if (x == 0) {
        // This branch is missing
    } else {
        return -1;
    }
}

int main() {
    return 0;
}