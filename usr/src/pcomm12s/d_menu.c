/*
 * Routines for the dialing directory menu.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "param.h"

static int current = 1;
/*
 * Display the dialing directory and prompt for options.  A non-zero return
 * code means we're ready to dial.
 */

int
dial_menu()
{
	extern int xmc;
	WINDOW *dm_win, *newwin();
	char buf[5], ld_code;
	int ans, start, needs_repair, count, x, y, i, ret_code;
	void dir_scroll(), active_ld(), disp_ld(), print_dir(), st_line();

	touchwin(stdscr);
	refresh();
	st_line("");

	dm_win = newwin(22, 78, 1, 1);
	mvwattrstr(dm_win, 1, 20, A_BOLD, "D I A L I N G       D I R E C T O R Y");
	horizontal(dm_win, 2, 0, 78);
	mvwattrstr(dm_win, 3, 0, A_STANDOUT, "           Name                   Number        Baud P D S Dpx  Script/TTY    ");
					/* show 10 entries */
	dir_scroll(dm_win, current);

	mvwaddstr(dm_win, 15, 4, "==>");
	mvwattrch(dm_win, 15, 14, A_BOLD, 'R');
	waddstr(dm_win, " Revise");
	mvwattrch(dm_win, 15, 34, A_BOLD, 'M');
	waddstr(dm_win, " Manual Dialing");
	mvwaddstr(dm_win, 15, 55, "Entry to Dial");

	mvwattrch(dm_win, 16, 14, A_BOLD, 'P');
	waddstr(dm_win, " LD Codes");
	mvwattrch(dm_win, 16, 34, A_BOLD, 'D');
	waddstr(dm_win, " Delete Entry");
	mvwattrstr(dm_win, 16, 55, A_BOLD, "<CR>");
	waddstr(dm_win, " Scroll Down");

#ifdef OLDCURSES
	mvwattrstr(dm_win, 17, 14, A_BOLD, "U/N");
#else /* OLDCURSES */
	mvwattrstr(dm_win, 17, 14, A_BOLD, "<up>/<down>");
#endif /* OLDCURSES */
	waddstr(dm_win, " Page");
	mvwattrch(dm_win, 17, 34, A_BOLD, 'L');
	waddstr(dm_win, " Print Entries");
	mvwattrstr(dm_win, 17, 55, A_BOLD, "<ESC>");
	waddstr(dm_win, " Exit");

	mvwaddstr(dm_win, 19, 4, "LD Codes Active:");
					/* show which LD codes are active */
	active_ld(dm_win);

	box(dm_win, VERT, HORZ);
	y = 15;
	x = 8;
	wmove(dm_win, 15, 8);
	wrefresh(dm_win);

#ifndef OLDCURSES
	keypad(dm_win, TRUE);
#endif /* OLDCURSES */
					/* prompt for options */
	count = 0;
	ld_code = '\0';
	ret_code = 0;
	do {
		needs_repair = 0;
		ans = wgetch(dm_win);
					/* get an entry number */
		if (ans >= '0' && ans <= '9') {
			if (count > 2) {
				beep();
				continue;
			}
			buf[count] = ans;
			waddch(dm_win, (chtype) ans);
			wrefresh(dm_win);
			count++;
			continue;
		}
		switch (ans) {
			case DEL:
			case BS:	/* do our own backspace */
				if (!count) {
					beep();
					break;
				}
				count--;
				if (!count)
					ld_code = '\0';
				buf[count] = '\0';
				getyx(dm_win, y, x);
				x--;
				wmove(dm_win, y, x);
				waddch(dm_win, (chtype) ' ');
				wmove(dm_win, y, x);
				wrefresh(dm_win);
				break;
#ifndef OLDCURSES
			case KEY_UP:
#endif /* OLDCURSES */
			case 'u':
			case 'U':	/* up arrow key */
				if (current == 1) {
					beep();
					break;
				}
				start = current - 10;
				if (start < 1)
					start = 1;
				current = start;
				dir_scroll(dm_win, start);
				break;
#ifndef OLDCURSES
			case KEY_DOWN:
			case '\n':
#endif /* OLDCURSES */
			case 'n':
			case 'N':	/* down arrow key */
				if (current == NUM_DIR-9) {
					beep();
					break;
				}
				start = current + 10;
				if (start > NUM_DIR-9)
					start = NUM_DIR-9;
				current = start;
				dir_scroll(dm_win, start);
				break;
			case '\r':	/* <CR> key */
				if (!count) {
					if (current == NUM_DIR-9) {
						beep();
						break;
					}
					current++;
					if (current > NUM_DIR-9)
						current = NUM_DIR-9;
					dir_scroll(dm_win, current);
				}
				/*
				 * The <CR> is used for the scroll-down-one-line
				 * function, and to terminate numeric input.
				 */
				else {
					buf[count] = '\0';
					i = atoi(buf);
					if (!i || i > NUM_DIR) {
						beep();
						mvwaddstr(dm_win, 15, 8, "   ");
						x = 8;
						count = 0;
						break;
					}
					dir->q_ld[0] = ld_code;
					dir->q_num[0] = i;
					dir->d_cur = i;

					/* end of queue marker */
					dir->q_num[1] = -1;

					ret_code++;
					break;
				}
				break;
			case 'r':
			case 'R':	/* revise */
				if (revise()) {
					active_ld(dm_win);
					dir_scroll(dm_win, current);
				}
				touchwin(dm_win);
				break;
			case 'p':	/* display LD codes */
			case 'P':
				disp_ld();
				touchwin(dm_win);
				needs_repair++;
				break;
			case 'd':
			case 'D':	/* delete a range of entries */
				if (delete())
					dir_scroll(dm_win, current);
				touchwin(dm_win);
				break;
			case 'm':
			case 'M':	/* manual dial */
				if (manual()) {
					ret_code++;
					break;
				}
				touchwin(dm_win);
				needs_repair++;
				break;
			case 'l':
			case 'L':	/* print the entries */
				print_dir();
				touchwin(dm_win);
				needs_repair++;
				break;
			case '+':	/* LD codes */
			case '-':
			case '@':
			case '#':
				waddch(dm_win, (chtype) ans);
				wrefresh(dm_win);
				ld_code = ans;
				continue;
			case ESC:	/* <ESC> key (exit) */
				break;
			default:
				beep();
		}
		if (ret_code)
			break;
					/* magic cookie terminal? */
		if (xmc > 0 && needs_repair) {
			clear_line(dm_win, 1, 0, FALSE);
			clear_line(dm_win, 3, 0, FALSE);
			wrefresh(dm_win);
			mvwattrstr(dm_win, 1, 20, A_BOLD, "D I A L I N G       D I R E C T O R Y");
			mvwattrstr(dm_win, 3, 0, A_STANDOUT, "           Name                   Number        Baud P D S Dpx  Script/TTY    ");
			box(dm_win, VERT, HORZ);
		}
		wmove(dm_win, y, x);
		wrefresh(dm_win);
	} while (ans != ESC);

	werase(dm_win);
	wrefresh(dm_win);
	delwin(dm_win);
	if (ret_code) {
		touchwin(stdscr);
		refresh();
	}
	return(ret_code);
}

/*
 * Scroll the dialing directory.  Actually, we're not doing a real scroll
 * function on the screen, we're just repainting 10 lines.
 */

static void
dir_scroll(win, start)
WINDOW *win;
int start;
{
	int i;

	wmove(win, 4, 0);
	for (i=start; i<start+10; i++)
		wprintw(win,
		 "%4d- %-20.20s %18.18s  %5d-%c-%d-%d  %c  %-14.14s\n", i,
		 dir->name[i], dir->number[i], dir->baud[i], dir->parity[i],
		 dir->dbits[i], dir->sbits[i], dir->duplex[i], dir->script[i]);
	box(win, VERT, HORZ);
	return;
}

/*
 * Display the Long Distance codes.  Press any key to continue.
 */

static void
disp_ld()
{
	WINDOW *ld_win, *newwin();

	ld_win = newwin(12, 30, 0, 0);
	mvwaddstr(ld_win, 1, 5, "Long Distance Codes\n");
	horizontal(ld_win, 2, 0, 30);
	mvwprintw(ld_win, 3, 2, "+ %-20.20s", param->ld_plus);
	mvwprintw(ld_win, 5, 2, "- %-20.20s", param->ld_minus);
	mvwprintw(ld_win, 7, 2, "@ %-20.20s", param->ld_at);
	mvwprintw(ld_win, 9, 2, "# %-20.20s", param->ld_pound);
	box(ld_win, VERT, HORZ);

	mvwaddstr(ld_win, 11, 8, " Press any key ");
	wmove(ld_win, 11, 29);
	wrefresh(ld_win);
	wgetch(ld_win);
					/* it overlaps, so erase it */
	werase(ld_win);
	wrefresh(ld_win);
	delwin(ld_win);
	return;
}

/*
 * Display which of the Long Distance codes are active.
 */

static void
active_ld(win)
WINDOW *win;
{
	mvwaddstr(win, 19, 21, "        ");
	wmove(win, 19, 21);
					/* a NULL means it's not active */
	if (*param->ld_plus != '\0')
		waddstr(win, "+ ");
	if (*param->ld_minus != '\0')
		waddstr(win, "- ");
	if (*param->ld_at != '\0')
		waddstr(win, "@ ");
	if (*param->ld_pound != '\0')
		waddstr(win, "# ");
	return;
}
