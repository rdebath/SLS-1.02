/* Partial wait3 emulation for non-BSD systems.
   Written by Franc,ois Pinard.  */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/param.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include "timeval.h"
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#else
#include "rusage.h"
#endif

/* Must be used with the below gettimeofday emulation.  */

int
wait3 (status, options, rusage)
     int *status;
     int options;
     struct rusage *rusage;
{
  struct tms tms;
  int pid;

  pid = wait (status);
  memset (rusage, 0, sizeof (struct rusage));

  times (&tms);
  rusage->ru_utime.tv_sec = tms.tms_cutime / HZ;
  rusage->ru_utime.tv_usec = tms.tms_cutime % HZ * (1000000 / HZ);
  rusage->ru_stime.tv_sec = tms.tms_cstime / HZ;
  rusage->ru_stime.tv_usec = tms.tms_cstime % HZ * (1000000 / HZ);

  return pid;
}

/* This version is only useful with the above wait3.  */

int
gettimeofday (tp, tz)
     struct timeval *tp;
     struct timezone *tz;
{
  int value;
  struct tms tms;

  value = times (&tms);
  tp->tv_sec = value / HZ;
  tp->tv_usec = value % HZ * (1000000 / HZ);
}
