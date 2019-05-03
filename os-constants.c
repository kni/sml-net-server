#include <stdio.h>
#include <sys/socket.h>

int main () {
	printf("structure OS_Constants =\n");
	printf("struct\n");
	printf("  val SOL_SOCKET   = %i\n", SOL_SOCKET);
#if SO_REUSEPORT_LB
	printf("  val SO_REUSEPORT = %i\n", SO_REUSEPORT_LB);
#else
	printf("  val SO_REUSEPORT = %i\n", SO_REUSEPORT);
#endif
	printf("end\n");
	return 0;
}
