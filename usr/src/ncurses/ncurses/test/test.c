#include <term.h>

main()
{

	setupterm(NULL, 1, (int *)0);
	tputs(newline, 1, putchar);
}
