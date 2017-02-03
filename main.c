#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int our_code_starts_here() asm("our_code_starts_here");
extern int print(int val) asm("print");
extern void error(int err) asm("error");

const int BOOL_TRUE  = 0xFFFFFFFF;
const int BOOL_FALSE = 0x7FFFFFFF;
const int BOOL_FLAG  = 0x1;

int print(int val)
{
    if ((val & BOOL_FLAG) == 0)
        printf("%d\n", val >> 1);
    else if (val == 0xFFFFFFFF)
        printf("true\n");
    else if (val == 0x7FFFFFFF)
        printf("false\n");
    else
        printf("Unknown value: %#010x\n", val);
    return val;
}

void error(int err)
{
    if (err == 0xA)
        fprintf(stderr, "Error: Arithmetic operation expects a number");
    else if (err == 0xB)
        fprintf(stderr, "Error: Logic operation expects a boolean");
    else if (err == 0xC)
        fprintf(stderr, "Error: Integer overflow detected");
    exit(-1);
}

int main(int argc, char** argv)
{
    int result = our_code_starts_here();
    print(result);
    return 0;
}
