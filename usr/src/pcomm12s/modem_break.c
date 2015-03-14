/*
 * Send a modem break.  This is an external program to be used in shell
 * scripts.
 */

#include <stdio.h>
#ifdef XENIX_3
#include <sys/types.h>
#include <sys/ioctl.h>
#endif /* XENIX_3 */
#ifdef BSD
#include <sgtty.h>
#else /* BSD */
#include <termio.h>
#endif /* BSD */

main()
{
	unsigned int sleep();

#ifdef BSD
	ioctl(1, TIOCSBRK, (struct sgttyb *) 0);
	sleep(1);
	return(ioctl(1, TIOCCBRK, (struct sgttyb *) 0));
#else /* BSD */
	return(ioctl(1, TCSBRK, 0));
#endif /* BSD */
}
