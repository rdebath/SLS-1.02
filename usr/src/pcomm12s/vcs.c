/*
 * Routines for VCS detection.
 */

#include <stdio.h>
#include "config.h"
#include "vcs.h"

#ifndef OLDCURSES
#include <curses.h>
#include <term.h>
#else /* OLDCURSES */
char tcbuf[1024];
#endif /* OLDCURSES */

static int putc_cnt;
static char putc_buf[VCS_SIZE];

/*
 * Test for possible VCS (video command sequence).  A character return
 * code means no match.  An return code greater than 255 means a VCS
 * was found.
 */

int
vcs_filter(c)
char c;
{
	extern int vcs_codes[NUM_VCS][VCS_SIZE], vcs_leadin[NUM_VCS];
	extern int num_leadin;
	static int vcs_buf[VCS_SIZE];
	static int ptr = 0;
	register int i;
	int maybe, possible;

					/* see if possible */
	possible = 0;
	if (ptr == 0) {
		/*
		 * This is kinda crude... I'm checking to see if the
		 * lead-in character is greater than the space character.
		 * If so, it most probably is NOT a VCS.
		 */
		if (c >= ' ')
			return(c & 0xff);
					/* check the list */
		for (i=0; i<num_leadin; i++) {
			if (c == vcs_leadin[i]) {
				possible++;
				break;
			}
		}
		if (!possible)
			return(c & 0xff);
	}

					/* build the string */
	vcs_buf[ptr++] = c;
	vcs_buf[ptr] = -1;
					/* test for match */
	maybe = 0;
	for (i=0; i<NUM_VCS; i++) {
		switch (match_codes(vcs_buf, vcs_codes[i], i)) {
			case YES:
				ptr = 0;
				return(i+256);
			case NO:
				break;
			case MAYBE:
				maybe++;
				break;
		}
	}
					/* abandon what you've got */
	if (maybe && ptr == VCS_SIZE-1) {
		ptr = 0;
		return(c & 0xff);
	}
					/* hang on, wait and see */
	if (maybe)
		return(MAYBE);
					/* a clean miss */
	ptr = 0;
	return(c & 0xff);
}

/*
 * See if the two integer arrays "match".  Character parameters are
 * designated by codes > 1000 and ASCII digit parameters are designated
 * by codes > 2000.  Uses a simple linear search, so if NUM_VCS grows
 * this routine will have to mature a bit.
 */

static int
match_codes(test, code, k)
int test[], code[], k;
{
	extern int vcs_param[NUM_VCS][5];
	register int i, j;
	int pos, done;
					/* doesn't exist */
	if (code[0] == -1)
		return(NO);

	i = 0;
	j = 0;
	while (i<VCS_SIZE && j<VCS_SIZE) {
					/* at the end (a match) */
		if (test[i] == -1 && code[j] == -1)
			return(YES);
					/* ran out of input */
		if (test[i] == -1)
			break;
		/*
		 * The char parameter (code 1000) always matches the
		 * next character.
		 */
		if (code[j] >= 1000 && code[j] < 2000) {
			pos = code[j] -1000;
			vcs_param[k][pos] = test[i];
			i++;
			j++;
			continue;
		}
		/*
		 * The digit parameter (code 2000) tries to match as many
		 * ASCII digits as it can.
		 */
		if (code[j] >= 2000) {
			pos = code[j] -2000;
					/* done with this number? */
			if (vcs_param[k][pos])
				done = 1;
			else
				done = 0;
					/* only digits */
			while (test[i] >= 48 && test[i] <= 57) {
				if (!done)
					vcs_param[k][pos] = (vcs_param[k][pos] * 10) + test[i] -48;
				i++;
			}
					/* ended in a digit */
			if (test[i] == -1 && code[j+1] != -1) {
				vcs_param[k][pos] = 0;
				break;
			}
			j++;
			continue;
		}
					/* a clean miss */
		if (test[i] != code[j]) {
			for (j=0; j<5; j++)
				vcs_param[k][j] = 0;
			return(NO);
		}
		i++;
		j++;
	}
					/* a maybe */
	return(MAYBE);
}

/*
 * Build the table of VCS codes.  Actually we cheat... We tell curses(3)
 * to build the strings to perform the function, and then we decipher
 * what it did.
 *
 * For example: On a vt100 the cursor motion string in terminfo is:
 *	cup=\E[%i%p1%d;%p2%dH$<5>
 *
 * This gets translated to the integer array vcs_code[] as:
 *	\E   [   %p1%d  ;   %p2%d  H
 *	27,  91, 2000,  59, 2001,  72
 * 
 * Notice that the "%p1" and "%p2" parameters get translated to 2000 and
 * 2001.  This is to signify that the parameters are multiple digit ASCII
 * encoded numbers.  The "%i" and "%d" codes are imbedded into the vcs_opt[]
 * array in somewhat of a loose manner.  In other words, there is no set
 * format for the vcs_opt[] array.  The padding info "$<5>" is ignored.
 */

void
vcs_table()
{
	extern int vcs_codes[NUM_VCS][VCS_SIZE], vcs_opt[NUM_VCS][10];
	extern int vcs_leadin[NUM_VCS], num_leadin, max_row, max_col;
	int i, j, k, match, temp[VCS_SIZE];
	char *p, *strcpy(), buf[VCS_SIZE], *getenv(), *tparm();
	void fake_it();

#ifdef OLDCURSES
	char tb[1024], *t, *cursor_home, *clr_eol, *clr_eos;
	char *clear_screen, *cursor_up, *cursor_down, *cursor_right;
	char *cursor_left, *cursor_address, *getenv(), *tgetstr(), *tgoto();

	tgetent(tb, getenv("TERM"));
	t = tcbuf;

	cursor_home = tgetstr("ho", &t);
	clr_eol = tgetstr("ce", &t);
	clr_eos = tgetstr("cd", &t);
	clear_screen = tgetstr("cl", &t);
	cursor_up = tgetstr("up", &t);
	cursor_down = tgetstr("do", &t);
	cursor_right = tgetstr("nd", &t);
	cursor_left = tgetstr("le", &t);
	cursor_address = tgetstr("cm", &t);
	max_row = tgetnum("li");
	max_col = tgetnum("co");
#else /* OLDCURSES */
	setupterm(getenv("TERM"), 1, &i);
	max_row = lines;
	max_col = columns;
#endif /* OLDCURSES */

	/*
	 * Do the easy ones first.  These don't take positional parameters,
	 * so all we have to do is strip the padding info.
	 */
	for (i=0; i<NUM_VCS; i++) {
		switch (i) {
			case HOME:
				p = cursor_home;
				break;
			case CLR_EOL:
				p = clr_eol;
				break;
			case CLR_EOS:
				p = clr_eos;
				break;
			case CLEAR:
				p = clear_screen;
				break;
			case MV_UP:
				p = cursor_up;
				break;
			case MV_DOWN:
				p = cursor_down;
				break;
			case MV_RIGHT:
				p = cursor_right;
				break;
			case MV_LEFT:
				p = cursor_left;
				break;
			default:
				p = "";
				break;
		}
		/*
		 * Either the capability doesn't exist, or we're gonna
		 * do this one by hand (i.e.: ones with positional parameters)
		 */
		if (!p) {
			vcs_codes[i][0] = -1;
			continue;
		}
					/* fake an "output" */
		fake_it(p);
					/* copy what it did */
		j = 0;
		while (putc_buf[j]) {
			vcs_codes[i][j] = putc_buf[j];
			j++;
			if (j == VCS_SIZE-1)
				break;
		}
		vcs_codes[i][j] = -1;
	}

	/*
	 * And now for the difficult ones.  The way it's done is: load the
	 * string with a few known parameters and then find where the
	 * parameters end up.  The vcs_opt[][] array is "free-flowing"
	 * and means something only to the routine being used.
	 */
					/* add one to the param */
	if (substr(cursor_address, "%i") > 0)
		vcs_opt[MV_DIRECT][0] = 1;
					/* decimal codes used */
	if (substr(cursor_address, "%d") > 0)
		vcs_opt[MV_DIRECT][1] = 1;
					/* character codes used */
	if (substr(cursor_address, "%c") > 0)
		vcs_opt[MV_DIRECT][2] = 1;
					/* add an offset */
	if (substr(cursor_address, "%+") > 0)
		vcs_opt[MV_DIRECT][3] = 1;
					/* subtract an offset */
	if (substr(cursor_address, "%-") > 0)
		vcs_opt[MV_DIRECT][4] = 1;
					/* load with parameters 12 & 34 */
#ifdef OLDCURSES
	fake_it(tgoto(cursor_address, 12, 34));
#else /* OLDCURSES */
	fake_it(tparm(cursor_address, 12, 34));
#endif /* OLDCURSES */

	j = 0;
	while (putc_buf[j]) {
		temp[j] = putc_buf[j];
		j++;
		if (j == VCS_SIZE-1)
			break;
	}
	temp[j] = -1;
					/* if decimal parameters */
	if (vcs_opt[MV_DIRECT][1]) {
					/* if add one */
		if (vcs_opt[MV_DIRECT][0])
			strcpy(buf, "13");
		else
			strcpy(buf, "12");
					/* where is the 12 (or 13)? */
		if ((i = substr(putc_buf, buf)) > 0) {
			temp[i] = 2000;
			temp[i+1] = -2;
		}
		else
			temp[0] = -1;
					/* if add one */
		if (vcs_opt[MV_DIRECT][0])
			strcpy(buf, "35");
		else
			strcpy(buf, "34");
					/* where is the 34 (or 35)? */
		if ((i = substr(putc_buf, buf)) > 0) {
			temp[i] = 2001;
			temp[i+1] = -2;
		}
		else
			temp[0] = -1;
	}
					/* if character parameters */
	if (vcs_opt[MV_DIRECT][2]) {
					/* original with 12 and 34 */
		strcpy(buf, putc_buf);
					/* change 12 to 13 */
#ifdef OLDCURSES
		fake_it(tgoto(cursor_address, 13, 34));
#else /* OLDCURSES */
		fake_it(tparm(cursor_address, 13, 34));
#endif /* OLDCURSES */
					/* where are they different */
		i = 0;
		while (buf[i] != '\0') {
			if (buf[i] != putc_buf[i])
				break;
			i++;
		}
					/* sanity checking */
		if (buf[i] == '\0')
			temp[0] = -1;
					/* if add, what is offset? */
		if (vcs_opt[MV_DIRECT][3])
			vcs_opt[MV_DIRECT][5] = temp[i] - 13;

					/* if subtract, what is offset? */
		if (vcs_opt[MV_DIRECT][4])
			vcs_opt[MV_DIRECT][5] = 13 - temp[i];

		temp[i] = 1000;
					/* change 34 to 35 */
#ifdef OLDCURSES
		fake_it(tgoto(cursor_address, 12, 35));
#else /* OLDCURSES */
		fake_it(tparm(cursor_address, 12, 35));
#endif /* OLDCURSES */
					/* where are they different */
		i = 0;
		while (buf[i] != '\0') {
			if (buf[i] != putc_buf[i])
				break;
			i++;
		}
		temp[i] = 1001;
		if (buf[i] == '\0')
			temp[0] = -1;
	}
					/* strip the -2's out, if any */
	i = 0;
	j = 0;
	while (temp[i] != -1) {
		if (temp[i] != -2)
			vcs_codes[MV_DIRECT][j++] = temp[i];
		i++;
	}
	vcs_codes[MV_DIRECT][j] = -1;

	/*
	 * Simplify the list.  Some codes are already handled by the
	 * virtual screen routines... no need to duplicate them.
	 */
	if (vcs_codes[MV_DOWN][0] == '\n')
		vcs_codes[MV_DOWN][0] = -1;

	if (vcs_codes[MV_LEFT][0] == 8)
		vcs_codes[MV_LEFT][0] = -1;

	/*
	 * Often the "clear screen" sequence will contain the "home"
	 * sequence... if so, don't duplicate the "home" portion.
	 */
	fake_it(cursor_home);
	strcpy(buf, putc_buf);

	fake_it(clear_screen);
					/* if "home" inside "clear screen" */
	if ((k = substr(putc_buf, buf)) >= 0) {
					/* if at the beginning */
		if (k == 0) {
			i = 0;
			for (j=strlen(buf); j<VCS_SIZE; j++)
				vcs_codes[CLEAR][i++] = putc_buf[j];
			vcs_codes[CLEAR][i] = -1;
		}
					/* if at the end */
		else if (strlen(buf)+k == strlen(putc_buf))
			vcs_codes[CLEAR][k] = -1;
	}
					/* is "clear screen" still unique */
	k = 0;
	for (i=0; i<NUM_VCS; i++) {
		if (vcs_codes[CLEAR][i] == -1 || vcs_codes[CLR_EOS][i] == -1)
			break;
		if (vcs_codes[CLEAR][i] != vcs_codes[CLR_EOS][i]) {
			k++;
			break;
		}
	}
	if (k == 0)
		vcs_codes[CLEAR][0] = -1;

	/*
	 * Make a list of unique lead-in characters to be used as a
	 * simple hash table.
	 */
	num_leadin = 0;
	for (i=0; i<NUM_VCS; i++) {
		if (vcs_codes[i][0] == -1)
			continue;
					/* add any new lead-in character */
		match = 0;
		for (j=0; j<num_leadin; j++) {
			if (vcs_leadin[j] == vcs_codes[i][0])
				match++;
		}
		if (!match)
			vcs_leadin[num_leadin++] = vcs_codes[i][0];
	}
	return;
}

/*
 * The routine that fakes curses(3) into outputting the string info with
 * the padding removed.
 */
static void
fake_it(s)
char *s;
{
	int fake_putc();

	putc_cnt = 0;
	putc_buf[0] = '\0';
	tputs(s, 1, fake_putc);
	putc_buf[putc_cnt] = '\0';
	return;
}
static int
fake_putc(c)
char c;
{
	if (c != '\0')
		putc_buf[putc_cnt++] = c;
	return(c);
}

/*
 * Is string2 contained in string1?  If so, return the offset, otherwise
 * return a -1.
 */

static int
substr(s1, s2)
char *s1, *s2;
{
	int i, len;

	len = strlen(s2);
					/* not possible */
	if (len > strlen(s1))
		return(-1);

	i = 0;
	while (*s1) {
		if (!strncmp(s1, s2, len))
			return(i);
		s1++;
		i++;
	}
	return(-1);
}
