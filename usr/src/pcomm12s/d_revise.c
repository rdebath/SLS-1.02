/*
 * The revise option of the dialing directory.  A non-zero return code
 * means that something was updated.  Prompts for saving changes to disk.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"
#include "param.h"

int
revise()
{
	WINDOW *r_win, *newwin();
	int count, dir_flag, param_flag, num, x, y, save;
	char ans, buf[40], *ld, *ld_prompt(), *str_rep();

	r_win = newwin(7, 77, 7, 2);

	mvwaddstr(r_win, 3, 6, "Entry to revise?");
	mvwaddstr(r_win, 3, 35, "(Entry Number, +, -, @, #)");
	box(r_win, VERT, HORZ);
	wmove(r_win, 3, 23);
	wrefresh(r_win);

	dir_flag = 0;
	param_flag = 0;
	count = 0;

	/*
	 * Can't use my home-grown get_str() and get_num() functions
	 * here, because we are prompting for an entry number or a
	 * long distance code.  This routine echoes numbers only.
	 */
	while ((ans = wgetch(r_win)) != ESC) {
		if (ans >= '0' && ans <= '9') {
			if (count == 3) {
				beep();
				continue;
			}
			buf[count] = ans;
			waddch(r_win, (chtype) ans);
			wrefresh(r_win);
			count++;
			continue;
		}
					/* terminating CR */
		if (ans == '\r') {
			if (!count) {
				beep();
				continue;
			}
			buf[count] = '\0';
			num = atoi(buf);
					/* valid range of numbers? */
			if (num == 0 || num > NUM_DIR) {
				beep();
				mvwaddstr(r_win, 3, 23, "   ");
				wmove(r_win, 3, 23);
				wrefresh(r_win);
				count = 0;
				continue;
			}
					/* prompt for that entry */
			if (prompt_lib(r_win, num)) {
				dir_flag++;
				break;
			}
			delwin(r_win);
			return(0);
		}
					/* do our own backspace */
		if (ans == BS || ans == DEL) {
			if (!count) {
				beep();
				continue;
			}
			count--;
			buf[count] = '\0';
			getyx(r_win, y, x);
			x--;
			wmove(r_win, y, x);
			waddch(r_win, (chtype) ' ');
			wmove(r_win, y, x);
			wrefresh(r_win);
			continue;
		}
					/* non-number after number is error */
		if (count) {
			beep();
			continue;
		}
					/* prompt for LD codes */
		switch (ans) {
			case '+':
				if ((ld = ld_prompt(r_win, param->ld_plus, ans)) != NULL) {
					param->ld_plus = str_rep(param->ld_plus, ld);
					param_flag++;
				}
				break;
			case '-':
				if ((ld = ld_prompt(r_win, param->ld_minus, ans)) != NULL) {
					param->ld_minus = str_rep(param->ld_minus, ld);
					param_flag++;
				}
				break;
			case '@':
				if ((ld = ld_prompt(r_win, param->ld_at, ans)) != NULL) {
					param->ld_at = str_rep(param->ld_at, ld);
					param_flag++;
				}
				break;
			case '#':
				if ((ld = ld_prompt(r_win, param->ld_pound, ans)) != NULL) {
					param->ld_pound = str_rep(param->ld_pound, ld);
					param_flag++;
				}
				break;
			default:
				beep();
				continue;
		}
		break;
	}
					/* if nothing changed */
	if (!param_flag && !dir_flag) {
		delwin(r_win);
		return(0);
	}
					/* save to disk? */
	clear_line(r_win, 4, 4, TRUE);
	if (dir_flag) {
		sprintf(buf, "Save entry %d to disk", num);
		save = yes_prompt(r_win, 4, 4, A_BOLD, buf);
	}
	else
		save = yes_prompt(r_win, 4, 4, A_BOLD, "Save to disk");

					/* update the files */
	if (save && dir_flag) {
		if (up_dir(num)) {
			touchwin(r_win);
			wrefresh(r_win);
		}
	}
	if (save && param_flag) {
		if (up_param()) {
			touchwin(r_win);
			wrefresh(r_win);
		}
	}
	delwin(r_win);
	return(1);
}

/*
 * Prompt for long distance code changes.  If new string is a space,
 * change it to null_ptr.  Returns NULL on escape.  Since it uses
 * get_str(), the return value is a pointer to a static area.
 */

static char *
ld_prompt(win, current_ld, name)
WINDOW *win;
char *current_ld, name;
{
	extern char *null_ptr;
	char *ans, *get_str();

	werase(win);
	mvwprintw(win, 2, 4, "%-20.20s", current_ld);
	mvwprintw(win, 4, 4, "New LD code for %c: ", name);
	box(win, VERT, HORZ);
	wrefresh(win);

	if ((ans = get_str(win, 20, "", "\n")) == NULL)
		return(NULL);
					/* if space, change to null_ptr */
	if (!strcmp(ans, " "))
		ans = null_ptr;
					/* display new value */
	clear_line(win, 2, 4, TRUE);
	wattrstr(win, A_BOLD, ans);

	return(ans);
}
