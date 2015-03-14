/* High resolution clock structure for non-BSD systems.  */

struct timeval
{
  long tv_sec;			/* Seconds.  */
  long tv_usec;			/* Microseconds.  */
};

struct timezone
{
  int tz_minuteswest;		/* Minutes west of Greenwich.  */
  int tz_dsttime;		/* Type of DST correction.  */
};
