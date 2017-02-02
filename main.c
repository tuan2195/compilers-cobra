#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int our_code_starts_here() asm("our_code_starts_here");
extern int print(int val) asm("print");
extern void error(int err) asm("error");

int print(int val)
{
    if (val == 0xFFFFFFFF)
        printf("true\n");
    else if (val == 0x7FFFFFFF)
        printf("false\n");
    else
        printf("Unknown value: %#010x\n", val);
    return val;
}

void error(int err)
{
}

int main(int argc, char** argv)
{
    int result = our_code_starts_here();
    print(result);
    return 0;
}
