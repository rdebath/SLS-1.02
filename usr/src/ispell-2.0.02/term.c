/* -*- Mode:Text -*- */

/*
 * term.c - deal with termcap, and unix terminal mode settings
 *
 * Pace Willisson, 1983
 */

#include <stdio.h>
#ifdef USG
#include <termio.h>
#else
#include <sgtty.h>
#endif
#include <signal.h>
#include "config.h"
#include "ispell.h"

int putch();

erase ()
{
	if (cl)
		tputs(cl, li, putch);
	else {
		if (ho)
			tputs(ho, 100, putch);
		else if (cm)
			tputs(tgoto(cm, 0, 0), 100, putch);
		tputs(cd, li, putch);
	}
}

move (row, col)
{
	tputs (tgoto (cm, col, row), 100, putch);
}

inverse ()
{
	tputs (so, 10, putch);
}

normal ()
{
	tputs (se, 10, putch);
}

backup ()
{
	if (BC)
		tputs (BC, 1, putch);
	else
		putchar ('\b');
}

putch (c)
{
	putchar (c);
}

#ifdef USG
struct termio sbuf, osbuf;
#else
struct sgttyb sbuf, osbuf;
struct ltchars ltc, oltc;
#endif
static termchanged = 0;
static int (*oldint) ();
static int (*oldterm) ();
#ifdef SIGTTIN
static int (*oldttin) ();
static int (*oldttou) ();
static int (*oldtstp) ();
#endif

terminit ()
{
	int done();

#ifdef USG
	if (!isatty(0)) {
		fprintf (stderr, "Can't deal with non interactive use yet.\n");
		exit (1);
	}
	ioctl (0, TCGETA, &osbuf);
	termchanged = 1;

	sbuf = osbuf;
	sbuf.c_lflag &= ~(ECHO | ECHOK | ECHONL | ICANON);
	sbuf.c_oflag &= ~(OPOST);
	sbuf.c_iflag &= ~(INLCR | IGNCR | ICRNL);
	sbuf.c_cc[VMIN] = 1;
	sbuf.c_cc[VTIME] = 1;
	ioctl (0, TCSETAW, &sbuf);

	erasechar = osbuf.c_cc[VERASE];
	killchar = osbuf.c_cc[VKILL];

#else
	int tpgrp;
	int onstop();
	extern short ospeed;

retry:
#ifdef SIGTSTP
	sigsetmask(1<<(SIGTSTP-1) | 1<<(SIGTTIN-1) | 1<<(SIGTTOU-1));
#endif
#ifdef TIOCGPGRP
	if (ioctl(0, TIOCGPGRP, &tpgrp) != 0) {
		fprintf (stderr, "Can't deal with non interactive use yet.\n");
		exit (1);
	}
#endif
#ifdef SIGTSTP
	if (tpgrp != getpgrp(0)) { /* not in foreground */
		sigsetmask(1<<(SIGTSTP-1) | 1<<(SIGTTIN-1));
		signal(SIGTTOU, SIG_DFL);
		kill(0, SIGTTOU);
		/* job stops here waiting for SIGCONT */
		goto retry;
	}
#endif

	ioctl (0, TIOCGETP, &osbuf);
	ioctl (0, TIOCGLTC, &oltc);
	termchanged = 1;

	sbuf = osbuf;
	sbuf.sg_flags &= ~ECHO;
	sbuf.sg_flags |= TERM_MODE;
	ioctl (0, TIOCSETP, &sbuf);

	erasechar = sbuf.sg_erase;
	killchar = sbuf.sg_kill;
	ospeed = sbuf.sg_ospeed;

	ltc = oltc;
	ltc.t_suspc = -1;
	ioctl (0, TIOCSLTC, &ltc);

	if ((oldint = signal (SIGINT, SIG_IGN)) != SIG_IGN)
		signal (SIGINT, done);
	if ((oldterm = signal (SIGTERM, SIG_IGN)) != SIG_IGN)
		signal (SIGTERM, done);

#ifdef SIGTTIN
	sigsetmask(0);
	if (signal (SIGTTIN, SIG_IGN) != SIG_IGN)
		signal(SIGTTIN, onstop);
	if (signal (SIGTTOU, SIG_IGN) != SIG_IGN)
		signal(SIGTTOU, onstop);
	if (signal (SIGTSTP, SIG_IGN) != SIG_IGN)
		signal(SIGTSTP, onstop);
#endif
#endif

	tgetent(termcap, getenv("TERM"));
	termptr = termstr;
	bs = tgetflag("bs");
	BC = tgetstr("bc", &termptr);
	UP = tgetstr("up", &termptr);
	cd = tgetstr("cd", &termptr);
	ce = tgetstr("ce", &termptr);	
	cl = tgetstr("cl", &termptr);
	cm = tgetstr("cm", &termptr);
	dc = tgetstr("dc", &termptr);
	dl = tgetstr("dl", &termptr);
	dm = tgetstr("dm", &termptr);
	ed = tgetstr("ed", &termptr);
	ei = tgetstr("ei", &termptr);
	ho = tgetstr("ho", &termptr);
	ic = tgetstr("ic", &termptr);
	il = tgetstr("al", &termptr);
	im = tgetstr("im", &termptr);
	ip = tgetstr("ip", &termptr);
	nd = tgetstr("nd", &termptr);
	vb = tgetstr("vb", &termptr);
	so = tgetstr("so", &termptr);	/* inverse video on */
	se = tgetstr("se", &termptr);	/* inverse video off */
	co = tgetnum("co");
	li = tgetnum("li");	

}

done ()
{
	unlink (tempfile);
	if (termchanged)
#ifdef USG
		ioctl (0, TCSETAW, &osbuf);
#else
		ioctl (0, TIOCSETP, &osbuf);
		ioctl (0, TIOCSLTC, &oltc);
#endif
	exit (0);
}

#ifndef USG
onstop(signo)
int signo;
{
	ioctl (0, TIOCSETP, &osbuf);
	ioctl (0, TIOCSLTC, &oltc);
	signal(signo, SIG_DFL);
	sigsetmask(sigblock(0) & ~(1 << (signo-1)));
	kill(0, signo);
	/* stop here until continued */
	signal(signo, onstop);
	ioctl (0, TIOCSETP, &sbuf);
	ioctl (0, TIOCSLTC, &ltc);
}

stop ()
{
#ifdef SIGTSTP
	onstop (SIGTSTP);
#endif
	;
}
#endif

shellescape (buf)
char *buf;
{

#ifdef USG
	ioctl (0, TCSETAW, &osbuf);
#else
	ioctl (0, TIOCSETP, &osbuf);
	ioctl (0, TIOCSLTC, &oltc);
#endif
	signal (SIGINT, oldint);
	signal (SIGTERM, oldterm);
#ifdef SIGTTIN
	oldttin = signal(SIGTTIN, SIG_DFL);
	oldttou = signal(SIGTTOU, SIG_DFL);
	oldtstp = signal(SIGTSTP, SIG_DFL);
#endif

	system (buf);

	if (signal (SIGINT, SIG_IGN) != SIG_IGN)
		signal (SIGINT, done);
	if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
		signal (SIGTERM, done);

#ifdef SIGTTIN
	signal(SIGTTIN, oldttin);
	signal(SIGTTOU, oldttou);
	signal(SIGTSTP, oldtstp);
#endif

#ifdef USG
	ioctl (0, TCSETAW, &sbuf);
#else
	ioctl (0, TIOCSETP, &sbuf);
	ioctl (0, TIOCSLTC, &ltc);
#endif
	printf ("\n-- Type space to continue --");
	fflush (stdout);
	getchar ();
}
