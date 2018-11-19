#include <stdio.h>
#include <sys/socket.h>

int main () {
	printf("structure OS_Constants =\n");
	printf("struct\n");
	printf("  val SOL_SOCKET   = %zi\n", SOL_SOCKET);
#if SO_REUSEPORT_LB
	printf("  val SO_REUSEPORT = %zi\n", SO_REUSEPORT_LB);
#else
	printf("  val SO_REUSEPORT = %zi\n", SO_REUSEPORT);
#endif
	printf("end\n");
	return 0;
}
