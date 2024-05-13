#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>

void prunt_controller_helper_lock_memory(void)
{
	if (mlockall(MCL_CURRENT | MCL_FUTURE)) {
		fprintf(stderr, "mlockall failed\n");
		exit(1);
	}
}
