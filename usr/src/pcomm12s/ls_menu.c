/*
 * Routines for displaying current line settings and prompting for changes.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "param.h"

/*
 * Display the current line settings and prompt for changes.  A non-zero
 * return code means settings were changed.
 */

int
ls_menu()
{
	extern int fd;
	WINDOW *l_win, *newwin();
	int num, ret_code;
	void disp_settings();

	l_win = newwin(20, 47, 0, 16);

	mvwattrstr(l_win, 1, 16, A_BOLD, "Line Settings");
	horizontal(l_win, 2, 0, 47);
	mvwaddstr(l_win, 6, 5, "1)     300,E,7,1     7)     300,N,8,1");
	mvwaddstr(l_win, 7, 5, "2)    1200,E,7,1     8)    1200,N,8,1");
	mvwaddstr(l_win, 8, 5, "3)    2400,E,7,1     9)    2400,N,8,1");
	mvwaddstr(l_win, 9, 5, "4)    4800,E,7,1    10)    4800,N,8,1");
	mvwaddstr(l_win, 10, 5, "5)    9600,E,7,1    11)    9600,N,8,1");
	mvwaddstr(l_win, 11, 5, "6)   19200,E,7,1    12)   19200,N,8,1");
	mvwaddstr(l_win, 13, 4, "Parity        Data Bits       Stop Bits");
	mvwaddstr(l_win, 14, 4, "13) Odd       14) 7 bits      16) 1 bit");
	mvwaddstr(l_win, 15, 18, "15) 8 bits      17) 2 bits");
	mvwaddstr(l_win, 17, 4, "18) Save Changes");
	mvwattrstr(l_win, 17, 28, A_BOLD, "YOUR CHOICE:");
	wmove(l_win, 17, 41);
	box(l_win, VERT, HORZ);

	mvwaddstr(l_win, 19, 13, " Press <ESC> to return ");
					/* display current settings */
	disp_settings(l_win);
	wmove(l_win, 17, 41);
	wrefresh(l_win);
					/* get the options */
	ret_code = 0;
	while ((num = get_num(l_win, 2)) != -1) {
		switch (num) {
			case 1:
				dir->baud[dir->d_cur] = 300;
				dir->parity[dir->d_cur] = 'E';
				dir->dbits[dir->d_cur] = 7;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 2:
				dir->baud[dir->d_cur] = 1200;
				dir->parity[dir->d_cur] = 'E';
				dir->dbits[dir->d_cur] = 7;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 3:
				dir->baud[dir->d_cur] = 2400;
				dir->parity[dir->d_cur] = 'E';
				dir->dbits[dir->d_cur] = 7;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 4:
				dir->baud[dir->d_cur] = 4800;
				dir->parity[dir->d_cur] = 'E';
				dir->dbits[dir->d_cur] = 7;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 5:
				dir->baud[dir->d_cur] = 9600;
				dir->parity[dir->d_cur] = 'E';
				dir->dbits[dir->d_cur] = 7;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 6:
				dir->baud[dir->d_cur] = 19200;
				dir->parity[dir->d_cur] = 'E';
				dir->dbits[dir->d_cur] = 7;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 7:
				dir->baud[dir->d_cur] = 300;
				dir->parity[dir->d_cur] = 'N';
				dir->dbits[dir->d_cur] = 8;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 8:
				dir->baud[dir->d_cur] = 1200;
				dir->parity[dir->d_cur] = 'N';
				dir->dbits[dir->d_cur] = 8;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 9:
				dir->baud[dir->d_cur] = 2400;
				dir->parity[dir->d_cur] = 'N';
				dir->dbits[dir->d_cur] = 8;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 10:
				dir->baud[dir->d_cur] = 4800;
				dir->parity[dir->d_cur] = 'N';
				dir->dbits[dir->d_cur] = 8;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 11:
				dir->baud[dir->d_cur] = 9600;
				dir->parity[dir->d_cur] = 'N';
				dir->dbits[dir->d_cur] = 8;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 12:
				dir->baud[dir->d_cur] = 19200;
				dir->parity[dir->d_cur] = 'N';
				dir->dbits[dir->d_cur] = 8;
				dir->sbits[dir->d_cur] = 1;
				break;
			case 13:
				dir->parity[dir->d_cur] = 'O';
				break;
			case 14:
				dir->dbits[dir->d_cur] = 7;
				break;
			case 15:
				dir->dbits[dir->d_cur] = 8;
				break;
			case 16:
				dir->sbits[dir->d_cur] = 1;
				break;
			case 17:
				dir->sbits[dir->d_cur] = 2;
				break;
			case 18:
					/* copy the current settings */
				param->d_baud = dir->baud[dir->d_cur];
				param->d_parity = dir->parity[dir->d_cur];
				param->d_dbits = dir->dbits[dir->d_cur];
				param->d_sbits = dir->sbits[dir->d_cur];
				/*
				 * We've changed the values in memory even
				 * if the update fails.
				 */
				if (up_param()) {
					touchwin(l_win);
					wrefresh(l_win);
				}
				break;
			default:
				beep();
		}
		ret_code++;
		disp_settings(l_win);
		mvwaddstr(l_win, 17, 41, "    ");
		wmove(l_win, 17, 41);
		wrefresh(l_win);
	}
	if (fd == -1) {
		werase(l_win);
		wrefresh(l_win);
	}
	delwin(l_win);
	return(ret_code);
}

/*
 * Display the current settings.  Formats the entire string at one
 * time, in case you've got a magic cookie terminal.
 */

static void
disp_settings(win)
WINDOW *win;
{
	extern int xmc;
	char buf[40];

	sprintf(buf, "Current Settings: %5d,%c,%d,%d", dir->baud[dir->d_cur],
	 dir->parity[dir->d_cur], dir->dbits[dir->d_cur],
	 dir->sbits[dir->d_cur]);

	if (xmc > 0) {
		touchwin(win);
		clear_line(win, 4, 8, TRUE);
		wrefresh(win);
	}
	mvwattrstr(win, 4, 8, A_BOLD, buf);
	return;
}
