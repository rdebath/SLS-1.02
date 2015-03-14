#define I_IOCTL
#define I_SYS
#define	I_TTY
#include "includes.h"
#include "debug.h"

# ifdef USE_WINCHKILL
#  include <signal.h>
# endif


void update_time(void) {
  struct timeval t;
  extern int bytes_left;
  long old_time = current_time;
  
#ifdef SVR4
  gettimeofday(&t);
#else
  gettimeofday(&t, (struct timezone *) 0 );
#endif
  current_time = t.tv_sec * 20 + t.tv_usec / 50000;
  
  bytes_left += (((current_time - old_time) * baudrate) / 200);
  if (bytes_left > (baudrate/10)) bytes_left = (baudrate/10);
  if (bytes_left < 0) bytes_left = 0;/* sanity check */
}

void do_debug(int level, char *c) {
/*  fprintf(stderr, "%s\n", c); */
  return;
}

void do_noise(int a) {
  if (!write_noise) return;
  a ^= byte_shift;
  if ((a != 10 && a != 13 && a < 32) || a > 127)
    fprintf(stderr, "<%d>", a);
  else 
    fprintf(stderr, "%c", a);
}

void do_alert(char *s) {
  if (remote) return;
  fprintf(stderr, "%s", s);
}

/* rebuild the arg list
 * compliment to build_arg()
 *
 * by: croutons
 * 
 * mallocs the arry that is returned.
 * see build_arg() for other assumptions.
 */
char ** rebuild_arg(char * f)
{
	int i,s;
	char ** a;

	DEBUG_FP(stderr,"rebuild :%s:\n",f);
	if ( ! f || !f[0]) return NULL;
	for ( s = 0, i = 2; '\0' != f[s]; s++ ) { 
/*		if ( '\xff' == f[s] ) { Ultrix MIP compiler chokes on this */
		if ( '\377' == f[s] ) {
			i++;
		}
	}
	if ( NULL == ( a = (char**) malloc(i*sizeof(char*)) ) ) {
		return NULL;
	}
	a[0]=f;
	for ( s = i = 0; '\0' != f[s]; s++ ) {
/*		if ( '\xff' == f[s] ) { */
		if ( '\377' == f[s] ) {
			f[s] = '\0';
			a[++i] = &f[s+1];
		}
	}
	a[i] = NULL;
	return a;
}

#ifdef USE_SIGWINCH
void do_resize(int number, int rows, int cols, int ypixels, int xpixels)
{
  int i;
#ifdef USE_WINCHKILL
  int pg;
#endif

  for (i=0; i < MAX_CLIENTS; i++)
    {
      if (clients[i].fd >= 0 && clients[i].number == number)
	{
#ifdef TIOCSWINSZ
	  struct winsize ws;

	  ws.ws_row = rows;
	  ws.ws_col = cols;
	  ws.ws_ypixel = ypixels;
	  ws.ws_xpixel = xpixels;
	  ioctl(clients[i].fd, TIOCSWINSZ, &ws);
#else
#ifdef TIOCSSIZE
	  struct ttysize ts;

	  ts.ts_lines = rows;
	  ts.ts_cols = cols;
	  ts.ts_yyy = ypixels;
	  ts.ts_xxx = xpixels;
	  ioctl(clients[i].fd, TIOCSSIZE, &ts);
#endif
#endif

#ifdef USE_WINCHKILL
#ifdef SYSV
	  pg = getpgid(clients[i].pid);
	  if (pg > 0)
	      kill(pg, SIGWINCH);
#else
	  pg = getpgrp(clients[i].pid);
	  if (pg > 0)
	      killpg(pg, SIGWINCH);
#endif
#endif	/* USE_WINCHKILL */
	}
    }

    return;
}
#endif /* USE_SIGWINCH */

