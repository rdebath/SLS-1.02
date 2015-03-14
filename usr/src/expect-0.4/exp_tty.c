/* exp_tty.c - tty support routines */

#include <stdio.h>

#include <sys/ioctl.h>

#include "tcl.h"

#include "exp_global.h"
#include "exp_rename.h"
#include "exp_tty.h"
#include "exp_log.h"
#include "exp_command.h"
#include "exp_main.h"

static int is_raw = FALSE;
static int is_noecho = FALSE;

extern int dev_tty;

exp_tty tty_current, tty_cooked;
int ioctled_devtty = FALSE;

/* if set == 1, set it to raw, else unset it */
void
tty_raw(set)
int set;
{
	if (set == 1) {
		is_raw = TRUE;
#if defined(SYSV3) || defined(POSIX)
		tty_current.c_iflag = 0;
		tty_current.c_oflag = 0;
		tty_current.c_lflag &= ECHO; /* disable everything except echo */
		tty_current.c_cc[VMIN] = 1;
		tty_current.c_cc[VTIME] = 0;
	} else {
		tty_current.c_iflag = tty_cooked.c_iflag;
		tty_current.c_oflag = tty_cooked.c_oflag;
		tty_current.c_lflag = tty_cooked.c_lflag;
		tty_current.c_cc[VMIN] = tty_cooked.c_cc[VMIN];
		tty_current.c_cc[VTIME] = tty_cooked.c_cc[VTIME];
#else
		tty_current.sg_flags |= RAW;
	} else {
		tty_current.sg_flags = tty_cooked.sg_flags;
#endif
		is_raw = FALSE;
	}
}
	
void
tty_echo(set)
int set;
{
	if (set == 1) {
		is_noecho = FALSE;
#if defined(SYSV3) || defined(POSIX)
		tty_current.c_lflag |= ECHO;
	} else {
		tty_current.c_lflag &= ~ECHO;
#else
		
		tty_current.sg_flags |= ECHO;
	} else {
		tty_current.sg_flags &= ~ECHO;
#endif
		is_noecho = TRUE;
	}
}

/* returns 0 if nothing changed */
/* if something changed, the out parameters are changed as well */
int
tty_raw_noecho(interp,tty_old,was_raw,was_echo)
Tcl_Interp *interp;
exp_tty *tty_old;
int *was_raw, *was_echo;
{
	if (exp_disconnected) return(0);
	if (is_raw && is_noecho) return(0);
	if (dev_tty == -1) return(0);

	*tty_old = tty_current;		/* save old parameters */
	*was_raw = is_raw;
	*was_echo = !is_noecho;
	debuglog("tty_raw_noecho: was raw = %d  echo = %d\r\n",is_raw,!is_noecho);

	tty_raw(1);
	tty_echo(-1);

#ifdef POSIX
 	if (tcsetattr(dev_tty, TCSADRAIN, &tty_current) == -1) {
#else
#	ifdef SYSV3
        if (ioctl(dev_tty, TCSETAW, &tty_current) == -1) {
#	else
	if (ioctl(dev_tty, TIOCSETP, &tty_current) == -1) {
#	endif
#endif
		errorlog("ioctl(raw): %s\r\n",sys_errlist[errno]);
		exp_exit(interp,-1);
	}

	ioctled_devtty = TRUE;
	return(1);
}

/* returns 0 if nothing changed */
/* if something changed, the out parameters are changed as well */
int
tty_cooked_echo(interp,tty_old,was_raw,was_echo)
Tcl_Interp *interp;
exp_tty *tty_old;
int *was_raw, *was_echo;
{
	if (exp_disconnected) return(0);
	if (!is_raw && !is_noecho) return(0);
	if (dev_tty == -1) return(0);

	*tty_old = tty_current;		/* save old parameters */
	*was_raw = is_raw;
	*was_echo = !is_noecho;
	debuglog("tty_cooked_echo: was raw = %d  echo = %d\r\n",is_raw,!is_noecho);

	tty_raw(-1);
	tty_echo(1);

#ifdef POSIX
 	if (tcsetattr(dev_tty, TCSADRAIN, &tty_current) == -1) {
#else
#    ifdef SYSV3
        if (ioctl(dev_tty, TCSETAW, &tty_current) == -1) {
#    else
	if (ioctl(dev_tty, TIOCSETP, &tty_current) == -1) {
#    endif
#endif
		errorlog("ioctl(noraw): %s\r\n",sys_errlist[errno]);
		exp_exit(interp,-1);
	}
	ioctled_devtty = TRUE;

	return(1);
}

void
tty_set(interp,tty,raw,echo)
Tcl_Interp *interp;
exp_tty *tty;
int raw;
int echo;
{
#ifdef POSIX
 	if (tcsetattr(dev_tty, TCSADRAIN, tty) == -1) {
#else
#	ifdef SYSV3
        if (ioctl(dev_tty, TCSETAW, tty) == -1) {
#	else
	if (ioctl(dev_tty, TIOCSETP, tty) == -1) {
#	endif
#endif
		errorlog("ioctl(set): %s\r\n",sys_errlist[errno]);
		exp_exit(interp,-1);
	}
	is_raw = raw;
	is_noecho = !echo;
	tty_current = *tty;
	debuglog("tty_set: raw = %d, echo = %d\r\n",is_raw,!is_noecho);
	ioctled_devtty = TRUE;
}	

void
exp_init_tty()
{
	extern exp_tty exp_tty_original;

	/* save original user tty-setting in 'cooked', just in case user */
	/* asks for it without earlier telling us what cooked means to them */
	tty_cooked = exp_tty_original;

	/* save our current idea of the terminal settings */
	tty_current = exp_tty_original;

	setbuf(stdout,(char *)0);	/* unbuffer stdout */
}

/* take strings with newlines and insert carriage-returns.  This allows user */
/* to write send_user strings without always putting in \r. */
/* If len == 0, use strlen to compute it */
/* NB: if terminal is not in raw mode, nothing is done. */
char *
exp_cook(s,len)
char *s;
int *len;	/* current and new length of s */
{
	static int destlen = 0;
	static char *dest = 0;
	char *d;		/* ptr into dest */
	unsigned int need;

	if (s == 0) return("<null>");

	if (!is_raw) return(s);

	/* worst case is every character takes 2 to represent */
	need = 1 + 2*(len?*len:strlen(s));
	if (need > destlen) {
		if (dest) free(dest);
		if (!(dest = malloc(need))) {
			destlen = 0;
			return("malloc failed in cook");
		}
		destlen = need;
	}

	for (d = dest;*s;s++) {
		if (*s == '\n') {
			*d++ = '\r';
			*d++ = '\n';
		} else {
			*d++ = *s;
		}
	}
	*d = '\0';
	if (len) *len = d-dest;
	return(dest);
}
