/*
 * The redial option (actually a misnomer, it's really a queuing system).
 * We expect a space-separated list of dialing directory entries (although
 * new users always try to put in a phone number).  A non-zero return code
 * means we're ready to dial.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"

int
redial()
{
	extern int fd;
	WINDOW *rd_win, *newwin();
	char *ans, *entry, *get_str(), *strchr(), *strtok();
	int i, oops, number, ret_code;

	rd_win = newwin(6, 70, 5, 5);

	mvwaddstr(rd_win, 4, 23, "(<CR> for previous numbers)");
	mvwaddstr(rd_win, 2, 4, "Directory Entry Number(s): ");
	box(rd_win, VERT, HORZ);

	mvwattrstr(rd_win, 0, 3, A_BOLD, " Redial Queue ");
	wmove(rd_win, 2, 31);
	wrefresh(rd_win);
					/* get the string of numbers */
	ret_code = 0;
	while ((ans = get_str(rd_win, 35, "0123456789+-@# ", "")) != NULL) {
		oops = 0;
		if (*ans == '\0') {
					/* use previous queue */
			if (dir->q_num[0] != -1) {
				ret_code++;
				break;
			}
					/* there is no previous queue */
			beep();
			mvwattrstr(rd_win, 3, 4, A_BOLD, "No previous numbers");
			wrefresh(rd_win);
			wait_key(rd_win, 3);
			clear_line(rd_win, 3, 4, TRUE);
			wmove(rd_win, 2, 31);
			wrefresh(rd_win);
			continue;
		}
					/* parse the queue values */
		entry = strtok(ans, " \t");
		for (i=0; i<NUM_QUEUE; i++) {
			if (entry == NULL) {
				dir->q_num[i] = -1;
				continue;
			}
					/* is there a LD code? */
			dir->q_ld[i] = '\0';
			if (strchr("+-@#", *entry)) {
				dir->q_ld[i] = *entry;
				entry++;
			}

			/*
			 * Zero is valid here, because it means use
			 * the current entry information.
			 */
			number = atoi(entry);
			if (number < -1 || number > NUM_DIR) {
				beep();
				mvwattrstr(rd_win, 3, 4, A_BOLD, "Invalid directory entry number");
				wrefresh(rd_win);
				wait_key(rd_win, 3);
				clear_line(rd_win, 3, 4, TRUE);
				clear_line(rd_win, 2, 31, TRUE);
				wrefresh(rd_win);
				oops++;
				break;
			}
					/* store the number in the queue */
			dir->q_num[i] = number;
			entry = strtok((char *) NULL, " \t");
		}
		if (oops)
			continue;
		ret_code++;
		break;
	}
	if (fd == -1) {
		werase(rd_win);
		wrefresh(rd_win);
	}
	delwin(rd_win);
	return(ret_code);
}
