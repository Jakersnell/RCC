struct car {
 unsigned char * brand_name;
 unsigned char * model_name;
 int year;
 double price;
};

int main() {
    struct car gc;
    gc.brand_name = "jeep";
    gc.model_name = "grand cherokee";
    gc.year = 2024;
    gc.price = 60000;

    printf(gc.brand_name);
    printf(gc.model_name);
    printf("%d\n", gc.year);
    printf("%f\n", gc.price);

    return 0;
}