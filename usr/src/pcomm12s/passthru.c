/*
 * A transparent "pass-thru" mode, designed to allow binary transfers
 * between 3 machines (with the middle machine in the pass-thru mode).
 * A non-zero return code means the input routine should be restarted.
 */

#include <stdio.h>
#include <signal.h>
#include <curses.h>
#include "config.h"
#include "misc.h"

#ifdef BSD
#include <setjmp.h>
jmp_buf cp_buf;
#endif /* BSD */

int
pass_thru()
{
	extern int fd;
	WINDOW *pt_win, *newwin();
	int num;
	void cpio(), error_win();

	pt_win = newwin(5, 70, 5, 5);

	mvwaddstr(pt_win, 2, 4, "Enter the expiration time (5-60 sec): ");
	box(pt_win, VERT, HORZ);

	mvwattrstr(pt_win, 0, 3, A_BOLD, " Pass Through Mode ");
	wmove(pt_win, 2, 43);
	wrefresh(pt_win);
					/* get the answer */
	while ((num = get_num(pt_win, 2)) != -1) {
					/* out of bounds */
		if (num < 5 || num > 60) {
			beep();
			clear_line(pt_win, 2, 43, TRUE);
			wmove(pt_win, 2, 43);
			wrefresh(pt_win);
		}
		else {
			werase(pt_win);
			wrefresh(pt_win);
			delwin(pt_win);

			if (fd == -1) {
				error_win(0, "Not currently connected to any host", "");
				return(0);
			}

			touchwin(stdscr);
			refresh();

			cpio((unsigned int) num);
			return(1);
		}
	}
	if (fd == -1) {
		werase(pt_win);
		wrefresh(pt_win);
	}
	delwin(pt_win);
	return(0);
}

/*
 * Copy the stdin to the TTYout and copy the TTYin to the stdout.  Uses
 * multi character reads.  I'm not too concerned about the excess baggage
 * caused by the entire image being forked... this feature won't be used
 * that often.
 */

static int cp_flag;

static void
cpio(num)
unsigned int num;
{
	extern int fd;
	int cpid, n, cp_force();
	char buf[CLIST_SIZ];
	unsigned int alarm(), sleep();
	void line_set(), xmodem_mode(), input_off();

					/* out of curses mode */
	resetterm();

	input_off();
	xmodem_mode(0);
	xmodem_mode(fd);

					/* copy the TTYin to stdout */
	if (!(cpid = fork())) {
		while (1) {
			n = read(fd, buf, CLIST_SIZ);
			write(1, buf, n);
		}
	}

	cp_flag = 0;
	signal(SIGALRM, cp_force);
					/* copy the stdin to TTYout */
	while (1) {
		alarm(num);
#ifdef BSD
		if (setjmp(cp_buf))
			break;
#endif /* BSD */
		n = read(0, buf, CLIST_SIZ);
		if (cp_flag)
			break;
		write(fd, buf, n);
	}
	kill(cpid, SIGKILL);
					/* back to curses mode */
	sleep(1);
	fixterm();
	beep();
	line_set();
	clearok(curscr, TRUE);
	return;
}

/* ARGSUSED */
static int
cp_force(dummy)
{
#ifdef BSD
	longjmp(cp_buf, 1);
#else /* BSD */
	signal(SIGALRM, cp_force);
	cp_flag = 1;
#endif /* BSD */
}
