#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int
main(void)
{
	printf("#define SIZEOF_UINTPTR_T %lu\n", sizeof(uintptr_t));
	exit(EXIT_SUCCESS);
}
