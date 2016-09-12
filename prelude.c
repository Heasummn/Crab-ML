#include <stdio.h>
#include <stdbool.h>

/* TODO: Make an interface for creating C extensions. That way we can avoid the ugly i1 postfix. */

int printi1(int x)
{
    printf("%i\n", x);
    return x;
}

bool printb1(bool x)
{
	if(x)
		puts("true");
	else
		puts("false");
	return x;
}

int get_int0()
{
	int ret, n;
	char junk_chars[256];
	ret = scanf("%d", &n);
	if(ret == EOF || ret != 1)
	{
		// Clear the buffer if we couldn't match.
		scanf("%s", &junk_chars);
		return 0;
	}
	return n;
}