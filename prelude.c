#include <stdio.h>
#include <stdbool.h>
int pint(int x)
{
    printf("%i\n", x);
    return x;
}

int pbool(bool x)
{
	if(x)
		puts("true");
	else
		puts("false");
	return 0;
}