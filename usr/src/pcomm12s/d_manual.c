/*
 * The manual dial option of the dialing directory.  A non-zero return code
 * means we're ready to dial.  Dialing directory entry 0 is reserved
 * for the manual dial option.  No sanity checking is done on the input.
 */

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "dial_dir.h"
#include "misc.h"

int
manual()
{
	extern int xmc;
	extern char *null_ptr;
	WINDOW *m_win, *newwin();
	char *number, *str_rep(), *get_str(), ld_code, *strchr();
	void fix_xmc(), free_ptr();

	m_win = newwin(5, 50, 0, 20);

	box(m_win, VERT, HORZ);
	mvwaddstr(m_win, 2, 3, "Phone Number: ");
	wrefresh(m_win);
					/* get a phone number */
	if ((number = get_str(m_win, 30, "", "\n")) == NULL) {
		werase(m_win);
		wrefresh(m_win);
		delwin(m_win);
		if (xmc > 0)
			fix_xmc();
		return(0);
	}
					/* is first char an LD code? */
	ld_code = '\0';
	if (strchr("+-@#", *number)) {
		ld_code = *number;
		number++;
	}
					/* put it in the queue */
	dir->q_ld[0] = ld_code;
	dir->q_num[0] = 0;
					/* end of queue marker */
	dir->q_num[1] = -1;
	dir->d_cur = 0;
					/* build the entry zero */
	dir->name[0] = str_rep(dir->name[0], number);
					/* if space, change to null_ptr */
	if (!strcmp(number, " ")) {
		free_ptr(dir->number[0]);
		dir->number[0] = null_ptr;
	}
	else
		dir->number[0] = str_rep(dir->number[0], number);
					/* it overlaps dm_win, so erase it */
	werase(m_win);
	wrefresh(m_win);
	delwin(m_win);
	if (xmc > 0)
		fix_xmc();
	return(1);
}

/*
 * Clear the end of the physical screen where the magic cookie got shifted
 * Geez, I hate magic cookie terminals...  Wyse, are you listening?
 */

static void
fix_xmc()
{
	WINDOW *cl_win, *newwin();

	/*
	 * Since the cookie got shifted off the window, we have to
	 * create a new window to cover where the cookie ended up.
	 */
	cl_win = newwin(1, 2, 4, 78);
					/* have to touch, otherwise nothing! */
	touchwin(cl_win);
	wrefresh(cl_win);
	delwin(cl_win);

	return;
}
