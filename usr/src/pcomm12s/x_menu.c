/*
 * Open a window to display the choices of file transfer protocols and
 * prompt for the file name(s).  A non-zero return code means turn the
 * input routine back on.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <curses.h>
#include "config.h"
#include "extrnl.h"
#include "misc.h"
#include "xmodem.h"

int
xfer_menu(up)
int up;
{
	extern int fd;
	extern char *null_ptr;
	WINDOW *xm_win, *newwin();
	char buf[2048], *list, *get_names(), *get_extrnl(), *strcat();
	char *strcpy();
	int type, is_batch, i, ans, num_extrnl, n, ret_code;
	void xfer_win(), xfer_ascii(), do_extrnl(), error_win();

	num_extrnl = (up) ? extrnl->up_entries : extrnl->dn_entries;
	xm_win = newwin(14+num_extrnl, 20, 2, 45);

	mvwaddstr(xm_win, 2, 3, "1) xmodem");
	mvwaddstr(xm_win, 3, 3, "2) xmodem-1k");
	mvwaddstr(xm_win, 4, 3, "3) modem7");
	mvwaddstr(xm_win, 5, 3, "4) ymodem");
	mvwaddstr(xm_win, 6, 3, "5) ymodem-g");
	mvwaddstr(xm_win, 7, 3, "6) ASCII");

	for (i=0; i<num_extrnl; i++)
		mvwprintw(xm_win, i+8, 3, "%d) %-12.12s", i+7, extrnl->name[up][i]);
	mvwaddstr(xm_win, i+8, 3, "E) (external)");
	mvwaddstr(xm_win, i+10, 3, "<ESC> to Abort");
	mvwaddstr(xm_win, i+11, 3, "Protocol:");
	box(xm_win, VERT, HORZ);
	if (up)
		mvwattrstr(xm_win, 0, 6, A_BOLD, " Upload ");
	else
		mvwattrstr(xm_win, 0, 5, A_BOLD, " Download ");

	wmove(xm_win, i+11, 13);
	wrefresh(xm_win);
					/* get the protocol */
	type = -1;
	while ((ans = wgetch(xm_win)) != ESC) {
		switch (ans) {
			case '1':
				type = XMODEM;
				break;
			case '2':
				type = XMODEM_1k;
				break;
			case '3':
				type = MODEM7;
				break;
			case '4':
				type = YMODEM;
				break;
			case '5':
				type = YMODEM_G;
				break;
			case '6':
				type = XASCII;
				break;
			case '7':
				if (num_extrnl >= 1)
					type = EXT_1;
				else
					beep();
				break;
			case '8':
				if (num_extrnl >= 2)
					type = EXT_2;
				else
					beep();
				break;
			case '9':
				if (num_extrnl >= 3)
					type = EXT_3;
				else
					beep();
				break;
			case 'e':
			case 'E':
				type = EXT_MANUAL;
				break;
			default:
				beep();
		}
		if (type != -1)
			break;
	}
	werase(xm_win);
	wrefresh(xm_win);
	delwin(xm_win);
					/* chicken'd out */
	if (type == -1)
		return(0);

	if (fd == -1) {
		error_win(0, "Not currently connected to any host", "");
		return(0);
	}
					/* which protocol? */
	ret_code = 0;
	is_batch = 0;
	switch(type) {
		case MODEM7:
		case YMODEM:
		case YMODEM_G:		/* built-in protocols */
			is_batch++;
			/* fall thru */
		case XMODEM:
		case XMODEM_1k:		/* non-batch built-ins */
			list = null_ptr;
			/*
			 * When receiving in a batch mode, don't prompt
			 * for file names.
			 */
			if (up || !is_batch) {
				if (!(list = get_names(up, type, is_batch)))
					break;
			}
			xfer_win(list, up, type);
			ret_code++;
			break;
		case XASCII:		/* ascii xfer, yuck! */
			if (list = get_names(up, type, FALSE)) {
				xfer_ascii(list, up);
				if (!up)
					ret_code++;
			}
			break;
		case EXT_1:
		case EXT_2:
		case EXT_3:		/* one of the externals */
			n = type -NUM_INTERNAL;
			strcpy(buf, extrnl->command[up][n]);
					/* see if we need to prompt for files */
			if (extrnl->prompt[up][n] == 'Y') {
				if (list = get_names(up, type, TRUE)) {
					strcat(buf, " ");
					strcat(buf, list);
				}
				else
					break;
			}
			do_extrnl(buf);
			ret_code++;
			break;
		case EXT_MANUAL:	/* the manual external protocol */
			if (list = get_extrnl(up)) {
				do_extrnl(list);
				ret_code++;
			}
			break;
	}
	return(ret_code);
}

char *protocol[NUM_INTERNAL] = {"xmodem", "xmodem-1k", "modem7", "ymodem",
	"ymodem-g", "ASCII"};

/*
 * Prompt for a list of files for the transfer programs.  Since expand()
 * is used, it returns a pointer to a static area.  Returns a NULL if 
 * you chicken'd out.
 */

static char *
get_names(up, type, is_batch)
int up, type, is_batch;
{
	int got_it;
	WINDOW *gn_win, *newwin();
	char *ans, *file, *list, buf[40], *expand(), *get_str(), *strtok();
	void st_line();
	struct stat stbuf;

	touchwin(stdscr);
	refresh();
	st_line("");

	gn_win = newwin(7, 70, 5, 5);
	mvwaddstr(gn_win, 3, 4, "Enter filename: ");
	box(gn_win, VERT, HORZ);
	if (up) {
		if (type < NUM_INTERNAL)
			sprintf(buf, " Send %s ", protocol[type]);
		else
			sprintf(buf, " Send %s ", extrnl->name[up][type-NUM_INTERNAL]);
	}
	else {
		if (type < NUM_INTERNAL)
			sprintf(buf, " Receive %s ", protocol[type]);
		else
			sprintf(buf, " Receive %s ", extrnl->name[up][type-NUM_INTERNAL]);
	}
	mvwattrstr(gn_win, 0, 3, A_BOLD, buf);

	while (1) {
		wmove(gn_win, 3, 20);
		wrefresh(gn_win);
					/* get the answers */
		if (is_batch)
			ans = get_str(gn_win, 60, "", "\n");
		else
			ans = get_str(gn_win, 60, "", " \t\n");

		if (ans == NULL || *ans == '\0') {
			list = NULL;
			break;
		}
		list = expand(ans);
					/* batches are checked on-the-fly */
		if (is_batch)
			break;
		/*
		 * Here we have the opportunity to determine the read and
		 * write permissions before things get started.  Much nicer
		 * than finding out later when there's no way to fix it.
		 * Only checks the first file.
		 */
		file = strtok(list, " \t");
					/* sanity checking */
		if (!stat(file, &stbuf)) {
			if ((stbuf.st_mode & S_IFREG) != S_IFREG) {
				beep();
				clear_line(gn_win, 4, 15, TRUE);
				mvwattrstr(gn_win, 4, 15, A_BOLD, "Not a regular file");
				wrefresh(gn_win);
				wait_key(gn_win, 3);
				clear_line(gn_win, 4, 15, TRUE);
				clear_line(gn_win, 3, 20, TRUE);
				continue;
			}
		}
					/* check read permission */
		if (up) {
			if (access(file, 0)) {
				beep();
				mvwattrstr(gn_win, 4, 15, A_BOLD, "Can't find file");
				wrefresh(gn_win);
				wait_key(gn_win, 3);
				clear_line(gn_win, 4, 15, TRUE);
				clear_line(gn_win, 3, 20, TRUE);
				continue;
			}
			if (access(file, 4)) {
				beep();
				mvwattrstr(gn_win, 4, 15, A_BOLD, "No read permission");
				wrefresh(gn_win);
				wait_key(gn_win, 3);
				clear_line(gn_win, 4, 15, TRUE);
				clear_line(gn_win, 3, 20, TRUE);
				continue;
			}
			break;
		}
					/* check write permission */
		got_it = 0;
		switch(can_write(file)) {
			case DENIED:
				beep();
				clear_line(gn_win, 4, 15, TRUE);
				mvwattrstr(gn_win, 4, 15, A_BOLD, "No write permission");
				wrefresh(gn_win);
				wait_key(gn_win, 3);
				clear_line(gn_win, 4, 15, TRUE);
				clear_line(gn_win, 3, 20, TRUE);
				break;
			case OK_BUT_EXISTS:
				if (!yes_prompt(gn_win, 4, 15, A_BOLD, "File exists, overwrite")) {
					clear_line(gn_win, 4, 15, TRUE);
					clear_line(gn_win, 3, 20, TRUE);
					break;
				}
				/* fall thru */
			case WRITE_OK:
				got_it++;
				break;
		}
		if (got_it)
			break;
	}
	werase(gn_win);
	wrefresh(gn_win);
	delwin(gn_win);

	return(list);
}

/*
 * Prompt for the Unix command line to be used as an external file
 * transfer program.  Since expand() is used, it returns a pointer to
 * a static area.
 */

static char *
get_extrnl(up)
int up;
{
	WINDOW *ge_win, *newwin();
	char *ans, *cmd, *get_str(), *expand();
	void st_line();

	touchwin(stdscr);
	refresh();
	st_line("");
					/* prompt for command line */
	ge_win = newwin(7, 70, 5, 5);
	mvwaddstr(ge_win, 3, 4, "Enter Unix command: ");
	box(ge_win, VERT, HORZ);

	if (up)
		mvwattrstr(ge_win, 0, 3, A_BOLD, " Send (external) ");
	else
		mvwattrstr(ge_win, 0, 3, A_BOLD, " Receive (external) ");

	wmove(ge_win, 3, 24);
	wrefresh(ge_win);
					/* get the line */
	ans = get_str(ge_win, 60, "", "\n");
	if (ans == NULL || *ans == '\0')
		cmd = NULL;
	else
		cmd = expand(ans);

	werase(ge_win);
	wrefresh(ge_win);
	delwin(ge_win);
	return(cmd);
}
