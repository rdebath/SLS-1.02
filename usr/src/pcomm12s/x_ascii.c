/*
 * Transfer a file using just XON/XOFF flow control.  Currently limited to
 * 7 bit ASCII codes.  (If this causes too much trouble, I'll change it).
 */

#include <stdio.h>
#include <curses.h>
#include <signal.h>
#include "config.h"
#include "misc.h"
#include "param.h"

#ifdef BSD
#include <setjmp.h>
jmp_buf bl_buf;
#endif /* BSD */

void
xfer_ascii(list, up)
char *list;
int up;
{
	int cr_lf;
	char *file, *strtok();
	void send_ascii(), rcv_ascii(), line_set(), st_line(), suspend();
	void load_vs(), ascii_mode(), input_off(), term_mode();
	unsigned int sleep();

	touchwin(stdscr);
	refresh();
					/* only one file from list */
	file = strtok(list, " \t");

	cr_lf = !strcmp(param->cr_out, "CR/LF");
	ascii_mode(up);
					/* out of curses mode */
	resetterm();
	term_mode();
	tty_noblock(0, TRUE);

	if (up) {
					/* un-suspend the input routine */
		suspend(FALSE);
		send_ascii(file, cr_lf);
		suspend(TRUE);
	}
	else {
		input_off();
		rcv_ascii(file, cr_lf);
	}

	/*
	 * Restoring the TTY modes is easier than setting them... The
	 * fixterm() and line_set() routines fix most of the damage.
	 */
	line_set();
	fixterm();
	tty_noblock(0, FALSE);

	/*
	 * On downloading, the contents of the virtual screen won't contain
	 * the characters shown during the transfer.  Too bad...
	 */
	load_vs();
	beep();
	st_line("xfer complete");

	sleep(2);
	return;
}

/*
 * Send a file.  The local echo option is independent of the duplex option,
 * and would very rarely be used since the characters are most likely
 * being echoed on the screen anyway.
 */

static void
send_ascii(file, cr_lf)
char *file;
int cr_lf;
{
	extern int fd;
	FILE *fp, *my_fopen();
	int i, j, strip_cr, strip_lf, add_cr, add_lf, expand, lecho, pace;
	char buf[80];
	unsigned char c, last;
	unsigned int sleep();
	void error_win();
					/* permission already checked */
	if (!(fp = my_fopen(file, "r"))) {
		sprintf(buf, "\"%s\"", file);
		error_win(0, "Can't open file for read", buf);
		return;
	}
					/* ASCII transfer options */
	strip_cr = !strcmp(param->cr_up, "STRIP");
	add_lf = !strcmp(param->cr_up, "ADD LF");
	strip_lf = !strcmp(param->lf_up, "STRIP");
	add_cr = !strcmp(param->lf_up, "ADD CR");
	expand = !strcmp(param->expand, "YES");
	lecho = !strcmp(param->lecho, "YES");
	pace = !strcmp(param->pace, "YES");

	last = 0;
	while ((i = fgetc(fp)) != EOF) {
					/* any keyboard activity? */
		switch (j = getchar()) {
			case -1:	/* no key was pressed */
				break;
			case ESC:	/* <ESC> key for abort */
				fclose(fp);
				sleep(2);
				tty_drain(fd);
				return;
			default:	/* send the char */
				c = j;
				putc_line(c);
				if (c == '\r' && cr_lf)
					putc_line('\n');
				break;
		}
		c = i & 0x7f;
					/* expand blank lines */
		if (expand && last == '\n' && c == '\n')
			putc_line(' ');
		last = c;

					/* CR translations */
		if (c == '\r' && strip_cr)
			continue;
		if (c == '\r' && add_lf) {
			putc_line(c);
			putc_line('\n');
			continue;
		}
					/* LF translations */
		if (c == '\n' && strip_lf)
			continue;
		if (c == '\n' && add_cr) {
			putc_line('\r');
			putc_line(c);
			continue;
		}
		putc_line(c);
		/*
		 * There's really no mechanism for delaying characters
		 * going to the output, so we fake it by waiting for
		 * each character to clear the I/O buffer.
		 */
		if (pace)
			tty_drain(fd);
		if (lecho) {
			putchar((char) c);
			fflush(stdout);
		}
	}
	fclose(fp);
	sleep(2);
	tty_drain(fd);
	return;
}

/*
 * Receive a file.  The timer is used to end the transfer.  This is not
 * that much different from the data logging option.  The use of bgetc_line()
 * and non-blocking input makes it seem like full duplex, but it's not.
 * Be aware that while the timer is active the keyboard is deaf.  Input is
 * NOT loaded into the virtual screen!!
 */

static void
rcv_ascii(file, cr_lf)
char *file;
int cr_lf;
{
	FILE *fp, *my_fopen();
	int i, strip_cr, strip_lf, add_cr, add_lf, got_first;
	unsigned int delay;
	char c, buf[80];
	void error_win();
					/* permission already checked */
	if (!(fp = my_fopen(file, "w"))) {
		sprintf(buf, "\"%s\"", file);
		error_win(0, "Can't open file for write", buf);
		return;
	}
					/* ASCII transfer options */
	strip_cr = !strcmp(param->cr_dn, "STRIP");
	add_lf = !strcmp(param->cr_dn, "ADD LF");
	strip_lf = !strcmp(param->lf_dn, "STRIP");
	add_cr = !strcmp(param->lf_dn, "ADD CR");

	got_first = 0;
	delay = 1;
	while (1) {
					/* keyboard activity */
		switch (i = getchar()) {
			case -1:	/* no key was pressed */
				break;
			case ESC:	/* <ESC> key */
				fclose(fp);
				return;
			default:	/* send it */
				c = i;
				putc_line((unsigned char) c);
				if (c == '\r' && cr_lf)
					putc_line('\n');
				break;
		}
					/* read a character */
		if ((i = bgetc_line(delay)) == -1) {
			/*
			 * The transfer timeout is not activated until the
			 * first character is received.  Until then, it polls
			 * the line for one second and loops backs for
			 * keyboard input.
			 */
			if (got_first) {
				fclose(fp);
				return;
			}
			continue;
		}
		got_first = 1;
		delay = param->timer;
		c = i & 0x7f;
					/* display it on the screen */
		putchar(c);
		fflush(stdout);
					/* CR translations */
		if (c == '\r' && strip_cr)
			continue;
		if (c == '\r' && add_lf) {
			fputc(c, fp);
			fputc('\n', fp);
			continue;
		}
					/* LF translations */
		if (c == '\n' && strip_lf)
			continue;
		if (c == '\n' && add_cr) {
			fputc('\r', fp);
			fputc(c, fp);
			continue;
		}
		fputc(c, fp);
	}
}

/*
 * Get a character from the line (using buffered I/O) with a specified
 * time-out period in seconds.  If the function times-out, it returns a -1.
 */

static int bl_flag;

static int
bgetc_line(sec)
unsigned int sec;
{
	int c, bl_force();
	unsigned int alarm();

	signal(SIGALRM, bl_force);
	bl_flag = 0;

	alarm(sec);

#ifdef BSD
	if (setjmp(bl_buf))
		return(-1);
#endif /* BSD */

	if ((c = buf_read()) < 0) {
		alarm(0);
		return(-1);
	}
	if (bl_flag)
		return(-1);
	alarm(0);
	return(c);
}

/* ARGSUSED */
static int
bl_force(dummy)
int dummy;
{
#ifdef BSD
	longjmp(bl_buf, 1);
#else /* BSD */
	signal(SIGALRM, bl_force);
	bl_flag = 1;
#endif /* BSD */
}

/*
 * Do a single character buffered read from the serial port.
 */

static int
buf_read()
{
	extern int fd;
	static char buf[CLIST_SIZ];
	static char *bufp = buf;
	static int n = 0;

	if (n <= 0) {
		n = read(fd, buf, CLIST_SIZ);
		bufp = buf;
	}
	if (--n >= 0)
		return(*bufp++ & 0xff);
	return(-1);
}
