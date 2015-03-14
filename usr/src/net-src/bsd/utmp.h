#include_next <utmp.h>

#ifndef _PATH_UTMP
#define	_PATH_UTMP	UTMP_FILE
#endif
#define	_PATH_WTMP	WTMP_FILE
#define _PATH_LASTLOG	"/etc/lastlog"

#define UT_LINESIZE	sizeof(((struct utmp *)NULL)->ut_line)
#ifndef UT_NAMESIZE
#define UT_NAMESIZE	sizeof(((struct utmp *)NULL)->ut_user)
#endif
#define UT_HOSTSIZE	sizeof(((struct utmp *)NULL)->ut_host)

struct lastlog {
	long ll_time;
	char ll_line[12];
	char ll_host[16];
};

