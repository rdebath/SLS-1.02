/* Process resource usage structure for non-BSD systems.  */

struct rusage
{
  struct timeval ru_utime;	/* User time used.  */
  struct timeval ru_stime;	/* System time used.  */
  int ru_maxrss, ru_ixrss, ru_idrss, ru_isrss,
  ru_minflt, ru_majflt, ru_nswap, ru_inblock, 
  ru_oublock, ru_msgsnd, ru_msgrcv, ru_nsignals,
  ru_nvcsw, ru_nivcsw;
};
