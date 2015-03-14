/* gettimeofday.c by Harry Pulley, IV; 18DEC92.  Written for Coherent 4.0 to 
   emulate functions, etc. so that it may compile Linux MGR code. */

#include <sys/time.h>

gettimeofday(buf,tz)
struct timeval *buf;
int tz;
{
	long t=times(0);

	buf->tv_sec=t/100;
	buf->tv_usec=(t%100)*10;
}
