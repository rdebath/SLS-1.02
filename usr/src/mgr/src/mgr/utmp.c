#ifdef WHO
/*{{{}}}*/
/*{{{  #includes*/
#include <pwd.h>
#include <utmp.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
/*}}}  */

/*{{{  utmp_entry*/
static void utmp_entry(line,type) char *line; int type;
{
  struct utmp entry;

  setutent();
  memset((char *)&entry,0,sizeof(entry));
  strncpy(entry.ut_line,line+sizeof("/dev/")-1,sizeof(entry.ut_line));
  strncpy(entry.ut_id,line+sizeof("/dev/tty")-1,sizeof(entry.ut_id));
  entry.ut_type=type;
  if (type==USER_PROCESS)
  {
    entry.ut_pid=getpid();
    time(&entry.ut_time);
    strncpy(entry.ut_user,getpwuid(getuid())->pw_name,sizeof entry.ut_user);
  }
  pututline(&entry);
  endutent();
}
/*}}}  */

/*{{{  rm_utmp*/
void rm_utmp(line) char *line;
{
  utmp_entry(line,DEAD_PROCESS);
}
/*}}}  */
/*{{{  add_utmp*/
void add_utmp(line) char *line;
{
  utmp_entry(line,USER_PROCESS);
}
/*}}}  */
#endif
