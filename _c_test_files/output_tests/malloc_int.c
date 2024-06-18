int main() {
    int * i = (int *) malloc(sizeof(int));
    *i = 200;
    int deref = *i;
    printf("%d\n", deref);
    return 0;
}