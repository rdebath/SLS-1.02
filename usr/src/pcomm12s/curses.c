/*
 * Miscellaneous curses(3) routines.
 */

#define STR_WIDTH	256
#define NUM_WIDTH	16

#include <stdio.h>
#include <curses.h>
#include <signal.h>
#include "config.h"
#include "misc.h"

#ifdef BSD
#include <setjmp.h>
jmp_buf wk_buf;
#endif /* BSD */

#ifndef OLDCURSES
#include <term.h>
#else /* OLDCURSES */
#ifdef UNIXPC
#include <sgtty.h>
#endif /* UNIXPC */
#endif /* OLDCURSES */

/*
 * Get a string from a window.  Similar to wgetstr(), except we limit
 * the length, return a NULL (not pointer to NULL) on <ESC> key, beep
 * at any character in "disallow" string, and beep at any character not
 * in "allow". (It doesn't make sense to use both "allow" and "disallow"
 * at the same time).  Returns a pointer to a static area.
 */

char *
get_str(win, num, allow, disallow)
WINDOW *win;
int num;
char *allow, *disallow;
{
	int count, x, y;
	char ans, *strchr();
	static char buf[STR_WIDTH];

	count = 0;
	while ((ans = wgetch(win)) != '\r') {
					/* do our own backspace */
		if (ans == BS || ans == DEL) {
			if (!count) {
				beep();
				continue;
			}
			count--;
			buf[count] = '\0';
			getyx(win, y, x);
			x--;
			wmove(win, y, x);
			waddch(win, (chtype) ' ');
			wmove(win, y, x);
			wrefresh(win);
			continue;
		}
					/* an <ESC> anywhere in the string */
		if (ans == ESC)
			return(NULL);

					/* illegal character? */
		if (*disallow != '\0' && strchr(disallow, ans)) {
			beep();
			continue;
		}
		if (*allow != '\0' && !strchr(allow, ans)) {
			beep();
			continue;
		}
					/* exceeded the max? */
		if (count >= num || count >= STR_WIDTH) {
			beep();
			continue;
		}

		buf[count] = ans;
		waddch(win, (chtype) ans);
		wrefresh(win);
		count++;
	}
	buf[count] = '\0';
	return(buf);
}

/*
 * Get a number from a window.  We limit the length and return a -1
 * on <ESC> key.
 */

int
get_num(win, num)
WINDOW *win;
int num;
{
	int count, x, y, number;
	char ans, buf[NUM_WIDTH];

	count = 0;
	while ((ans = wgetch(win)) != '\r') {
					/* do our own backspace */
		if (ans == BS || ans == DEL) {
			if (!count) {
				beep();
				continue;
			}
			count--;
			buf[count] = '\0';
			getyx(win, y, x);
			x--;
			wmove(win, y, x);
			waddch(win, (chtype) ' ');
			wmove(win, y, x);
			wrefresh(win);
			continue;
		}
					/* an <ESC> anywhere in the string */
		if (ans == ESC)
			return(-1);
					/* only digits are allowed */
		if (ans < '0' || ans > '9') {
			beep();
			continue;
		}
					/* exceeded the max? */
		if (count >= num || count >= NUM_WIDTH) {
			beep();
			continue;
		}

		buf[count] = ans;
		waddch(win, (chtype) ans);
		wrefresh(win);
		count++;
	}
	buf[count] = '\0';
	number = atoi(buf);
	return(number);
}

/*
 * Change video attributes while printing a string.  The use of the
 * pre-processor definition NOPROMOTE (located in config.h) means that
 * strings will be printed without any special video attribute if the
 * requested capability doesn't exist.
 */

wattrstr(win, attr, str)
WINDOW *win;
chtype attr;
char *str;
{
	int do_it;
					/* if nothing, do nothing */
	if (str == NULL || *str == '\0')
		return(0);

#ifdef OLDCURSES
	if (attr)
		wstandout(win);
	waddstr(win, str);
	if (attr)
		wstandend(win);
#else /* OLDCURSES */

#ifdef NOPROMOTE
					/* does the capability exist? */
	do_it = 0;
	if ((attr & A_STANDOUT) && enter_standout_mode)
		do_it++;
	if ((attr & A_UNDERLINE) && enter_underline_mode)
		do_it++;
	if ((attr & A_REVERSE) && (enter_reverse_mode || enter_standout_mode))
		do_it++;
	if ((attr & A_BLINK) && enter_blink_mode)
		do_it++;
	if ((attr & A_BOLD) && enter_bold_mode)
		do_it++;
	if ((attr & A_DIM) && enter_dim_mode)
		do_it++;
#else /* NOPROMOTE */
	do_it = 1;
#endif /* NOPROMOTE */

	if (do_it)
		wattron(win, attr);
					/* print the string */
	waddstr(win, str);
	if (do_it)
		wattroff(win, attr);
#endif /* OLDCURSES */
	return(0);
}

/*
 * Change video attributes while printing a character.
 */

wattrch(win, attr, c)
WINDOW *win;
chtype attr;
char c;
{
	int do_it;

	if (c == '\0')
		return(0);
#ifdef OLDCURSES
	if (attr)
		wstandout(win);
	waddch(win, (chtype) c);
	if (attr)
		wstandend(win);
#else /* OLDCURSES */

#ifdef NOPROMOTE
					/* does the capability exist? */
	do_it = 0;
	if ((attr & A_STANDOUT) && enter_standout_mode)
		do_it++;
	if ((attr & A_UNDERLINE) && enter_underline_mode)
		do_it++;
	if ((attr & A_REVERSE) && (enter_reverse_mode || enter_standout_mode))
		do_it++;
	if ((attr & A_BLINK) && enter_blink_mode)
		do_it++;
	if ((attr & A_BOLD) && enter_bold_mode)
		do_it++;
	if ((attr & A_DIM) && enter_dim_mode)
		do_it++;
#else /* NOPROMOTE */
	do_it = 1;
#endif /* NOPROMOTE */

	if (do_it)
		wattron(win, attr);
					/* print the character */
	waddch(win, (chtype) c);
	if (do_it)
		wattroff(win, attr);
#endif /* OLDCURSES */
	return(0);
}


/*
 * Change video attributes while printing a number.
 */

wattrnum(win, attr, num)
WINDOW *win;
chtype attr;
int num;
{
	int do_it;
	char buf[40];

	sprintf(buf, "%d", num);

#ifdef OLDCURSES
	if (attr)
		wstandout(win);
	waddstr(win, buf);
	if (attr)
		wstandend(win);
#else /* OLDCURSES */

#ifdef NOPROMOTE
					/* does the capability exist? */
	do_it = 0;
	if ((attr & A_STANDOUT) && enter_standout_mode)
		do_it++;
	if ((attr & A_UNDERLINE) && enter_underline_mode)
		do_it++;
	if ((attr & A_REVERSE) && (enter_reverse_mode || enter_standout_mode))
		do_it++;
	if ((attr & A_BLINK) && enter_blink_mode)
		do_it++;
	if ((attr & A_BOLD) && enter_bold_mode)
		do_it++;
	if ((attr & A_DIM) && enter_dim_mode)
		do_it++;
#else /* NOPROMOTE */
	do_it = 1;
#endif /* NOPROMOTE */

	if (do_it)
		wattron(win, attr);
					/* print the character */
	waddstr(win, buf);
	if (do_it)
		wattroff(win, attr);
#endif /* OLDCURSES */
	return(0);
}

/*
 * Prompt for a Yes or No answer.  Echo the single key input as words.
 * Handle the funny cursor movement problems with magic cookie terminals.
 * Returns a 1 on yes.
 */

int
yes_prompt(win, y, x, attr, str)
WINDOW *win;
int y, x;
chtype attr;
char *str;
{
	int ret_code;
	char new_str[80], *strcpy(), *strcat();
					/* sanity checking */
	if (strlen(str) > 71)
		*(str+71) = '\0';
					/* build and display the prompt */
	strcpy(new_str, str);
	strcat(new_str, "? (y/n):");
	mvwattrstr(win, y, x, attr, new_str);
	wmove(win, y, strlen(new_str)+x+2);
	wrefresh(win);

	ret_code = -1;
	while (ret_code == -1) {
		switch (wgetch(win)) {
			case 'y':
			case 'Y':
				waddstr(win, "Yes");
				ret_code = 1;
				break;
			case 'n':
			case 'N':
			case ESC:
				waddstr(win, "No");
				ret_code = 0;
				break;
			default:
				beep();

		}
	}
	wrefresh(win);
	return(ret_code);
}

/*
 * Handy routine for clear-to-end-of-line.  Fixes up the box if requested.
 */

int
clear_line(win, y, x, re_box)
WINDOW *win;
int y, x, re_box;
{
	if (wmove(win, y, x) == ERR)
		return(ERR);

	wclrtoeol(win);

	if (re_box) {
		mvwaddch(win, y, win->_maxx-1, (chtype) ACS_VLINE);
		wmove(win, y, x);
	}
	return(0);
}

/*
 * Routine to make a horizontal line.  Does NOT do a wrefresh().
 */

int
horizontal(win, x, y, len)
WINDOW *win;
int x, y, len;
{
	wmove(win, x, y);

	while (len--)
		waddch(win, ACS_HLINE);

	return(0);
}

/*
 * Wait for a key or time out.  Returns a -1 on timeout.  This is similar
 * to the half-delay mode in the newer versions of curses(3).
 */

static int wk_flag;

/* ARGSUSED */
int
wait_key(win, sec)
WINDOW *win;
unsigned int sec;
{
	int key, wk_force();
	unsigned int alarm();
	char c;

	signal(SIGALRM, wk_force);
	wk_flag = 0;

	alarm(sec);

#ifdef BSD
	if (setjmp(wk_buf))
		return(-1);
#endif /* BSD */

#ifdef WGETCH_BROKE
	read(0, &c, 1);
	key = c & 0x7f;
#else /* WGETCH_BROKE */
	key = wgetch(win);
#endif /* WGETCH_BROKE */

	if (wk_flag)
		return(-1);
	alarm(0);
	return(key);
}

/* ARGSUSED */
static int
wk_force(dummy)
int dummy;
{
#ifdef BSD
	longjmp(wk_buf, 1);
#else /* BSD */
	signal(SIGALRM, wk_force);
	wk_flag = 1;
#endif /* BSD */
}

/*
 * Here are some routines that are probably missing from the older
 * flavors of curses(3).
 */

#ifdef OLDCURSES
/*
 * Make the terminal bell go off
 */

int
beep()
{
	fputc(BEL, stderr);
	return(0);
}

/*
 * Fix the stdin so that a read is satisfied immediately.  When read()
 * is called it returns the character in the queue, or an error if no
 * key was pressed.  The window argument is not used!
 */

/* ARGSUSED */
int
nodelay(win, on)
WINDOW *win;
int on;
{
	if (on)
		tty_noblock(0, TRUE);
	else
		tty_noblock(0, FALSE);
	return(0);
}

/*
 * Take the terminal out of the "curses mode".  The t_mode structure was 
 * captured before we initialized the curses mode.
 */

int
resetterm()
{
	extern char _putchar();
	extern struct sgttyb t_mode;

	ioctl(0, TIOCSETP, &t_mode);
	tputs(TE, 1, _putchar);
	tputs(VE, 1, _putchar);
	return(0);
}

/*
 * Put the terminal back into the "curses mode".  The c_mode structure was
 * captured after we initialized the curses mode.
 */

int
fixterm()
{
	extern char _putchar();
	extern struct sgttyb c_mode;

	ioctl(0, TIOCSETP, &c_mode);
	tputs(TI, 1, _putchar);
	tputs(VS, 1, _putchar);
	return(0);
}
#endif /* OLDCURSES */
