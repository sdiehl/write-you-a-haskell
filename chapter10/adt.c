typedef union {
    int a;
    float b;
} Sum;

typedef struct {
    int a;
    float b;
} Prod;

int main()
{
    Prod x = { .a = 1, .b = 2.0 };
    Sum sum1 = { .a = 1 };
    Sum sum2 = { .b = 1 }; 
}
