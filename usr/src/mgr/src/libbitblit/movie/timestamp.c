#include <sys/time.h>

/* return time stamp (100th's of seconds since first call) */

int
timestamp()
	{
	static long offset = 0;
	struct timeval timeval;
	
	gettimeofday(&timeval,0l);
	
	if (offset == 0) {
		offset =  timeval.tv_sec;
		return(0);
		}
	else {
		return((timeval.tv_sec - offset) * 100 + timeval.tv_usec / 10000);
		}
	}
