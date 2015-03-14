/*
 * Routines to support the batch protocols.
 */

#include <stdio.h>
#include <ctype.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "xmodem.h"

/*
 * Send the file name for the modem7 batch.  Only uses 11 characters
 * of the filename.  Returns zero on success or the standard error codes.
 */

int
send_modem7(win, name)
WINDOW *win;
char *name;
{
	char *new_name, *fix_name();
	unsigned char sum, calc_sum();

					/* convert to 11 character name */
	new_name = fix_name(name);
	sum = calc_sum((unsigned char *) new_name, 12);

	putc_line(ACK);
					/* for each character in the name */
	while (*new_name != CTRLZ) {
		putc_line((unsigned char) *new_name);

		switch (getc_line(3)) {
			case -1:	/* timed out */
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "NO RESPONSE");
				wrefresh(win);
				return(ERROR);
			case ACK:	/* got it! */
				break;
			case CAN:	/* cancel transmission */
				if (getc_line(2) == CAN) {
					beep();
					clear_line(win, 12, 24, TRUE);
					wattrstr(win, A_BOLD, "REMOTE ABORTED");
					wrefresh(win);
					return(CANCEL);
				}
				/* fall thru... */
			default:
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "NAME FAILED");
				wrefresh(win);
				return(ERROR);
		}
		new_name++;
	}
	putc_line(CTRLZ);
					/* verify the checksum */
	if (getc_line(10) != sum) {
		putc_line('u');
		clear_line(win, 12, 24, TRUE);
		waddstr(win, "CHECKSUM FAILED");
		wrefresh(win);
		return(ERROR);
	}
	putc_line(ACK);
	return(0);
}

/*
 * Receive a modem7 file name.  Returns zero on success, the standard error
 * codes, or a -1 on the end-of-batch.  (Oddly enough, the end-of-batch code
 * is the same as the code for a user abort)
 */

int
rcv_modem7(win, default_err)
WINDOW *win;
int default_err;
{
	extern char file_name[15];
	int i, j, err_method, err_count, got_it;
	unsigned char sum, calc_sum();
	char temp_name[13];
	void change_name(), unfix_name();

	err_method = default_err;
	if (default_err == CRC_CHECKSUM)
		err_method = CRC;

	err_count = 0;
	got_it = 0;
	while (err_count < MAX_ERRORS) {
					/* switch to checksum? */
		if (default_err == CRC_CHECKSUM && err_count > MAX_ERRORS/2)
			err_method = CHECKSUM;

		if (err_method == CRC)
			putc_line('C');
		else
			putc_line(NAK);
					/* what'd we get? */
		switch (getc_line(10)) {
			case -1:	/* timed out */
				clear_line(win, 12, 24, TRUE);
				wattrstr(win, A_BOLD, "NO RESPONSE");
				wrefresh(win);
				err_count++;
			case ACK:	/* ready to go... */
				got_it++;
				break;
			default:	/* huh? */
				clear_line(win, 12, 24, TRUE);
				wattrstr(win, A_BOLD, "BAD HEADER");
				wrefresh(win);
				err_count++;
		}
	}
	if (!got_it)
		return(ERROR);
					/* get the name */
	for (i=0; i<12; i++) {
		j = getc_line(3);

		switch (j) {
			case -1:	/* timed out */
				clear_line(win, 12, 24, TRUE);
				wattrstr(win, A_BOLD, "NO RESPONSE");
				wrefresh(win);
				return(ERROR);
			case EOT:	/* end of batch? */
				return(-1);
			case CAN:	/* cancel transmission */
				if (getc_line(2) == CAN) {
					beep();
					clear_line(win, 12, 24, TRUE);
					wattrstr(win, A_BOLD, "REMOTE ABORTED");
					wrefresh(win);
					return(CANCEL);
				}
				/* fall thru... */
			case 'u':	/* bad name character */
				beep();
				clear_line(win, 12, 24, TRUE);
				wattrstr(win, A_BOLD, "BAD NAME");
				wrefresh(win);
				return(ERROR);
			default:	/* the name... */
				temp_name[i] = j & 0xff;
				if (j != CTRLZ)
					putc_line(ACK);
				break;
		}
	}
	temp_name[12] = '\0';
					/* send our checksum */
	sum = calc_sum((unsigned char *) temp_name, 12);
	putc_line(sum);
					/* do they agree? */
	if (getc_line(10) != ACK) {
		beep();
		clear_line(win, 12, 24, TRUE);
		wattrstr(win, A_BOLD, "BAD NAME");
		wrefresh(win);
		return(ERROR);
	}
					/* load the file_name array */
	unfix_name(temp_name);
					/* any name collisions? */
	change_name(win, file_name);
	return(0);
}

/*
 * Send the block 0 information for a ymodem batch transfer.  Uses only
 * the name component of the path and the file size.
 */

int
send_ymodem(win, file, size)
WINDOW *win;
char *file;
long size;
{
	unsigned short crc, calc_crc();
	char *strcpy(), *memset();
	unsigned char buf[133];
					/* start with a clean block */
	memset(buf, '\0', 133);
					/* the header */
	buf[0] = SOH;
	buf[1] = 0;
	buf[2] = 255;

	/*
	 * The block zero consists of the file name (no path component),
	 * a NULL, and the file length (as a string).  The end of batch
	 * marker is an empty block.
	 */
	if (*file != '\0') {
		strcpy((char *) &buf[3], file);
		sprintf((char *) &buf[strlen(file)+4], "%ld", size);
	}
					/* the crc */
	crc = calc_crc(&buf[3], 128);
	buf[131] = crc >> 8;
	buf[132] = crc;
					/* the block count */
	mvwaddstr(win, 7, 24, "0   ");

	return(send_block(win, buf, 133));
}

/*
 * Receive the block 0 information for a ymodem batch transfer.  We
 * only use the file name and the size (if present).  Currently doesn't
 * support full path names.
 */

int
rcv_ymodem(win)
WINDOW *win;
{
	extern unsigned char buf[1029];
	extern long file_length;
	extern char file_name[15];
	int code, length_is_at;
	long atol();

	file_length = 0L;
	file_name[0] = '\0';
					/* read the zero block */
	if (code = rcv_block(win, 1, 1024, 0))
		return(code);
					/* at end of batch */
	if (buf[3] == '\0')
		return(0);
					/* get the file name */
	change_name(win, (char *) &buf[3]);
					/* any trouble? */
	if (file_name[0] == '\0') {
		putc_line(CAN);
		return(0);
	}
	/*
	 * The file length is placed after the NULL of the file name
	 * and is terminated by another NULL.  If the length is missing,
	 * atol() will see a NULL and return 0.
	 */
	length_is_at = strlen((char *) &buf[3]) + 4;
	file_length = atol((char *) &buf[length_is_at]);
	return(0);
}

/*
 * Handle file name collisions.  Prepend an "X" to the name until you find
 * a name that doesn't already exist.  Creates a NULL name on error.
 * Loads the global character array "file_name".
 */

void
change_name(win, str)
WINDOW *win;
char *str;
{
	extern char file_name[15];
	register int i;
	int modified;
	char temp[15], ans[15], *s, *strrchr(), *strcpy(), *strncat();
	char *strncpy();
	unsigned int sleep();
					/* dissect the name component */
	if ((s = strrchr(str, '/')))
		strncpy(temp, ++s, 15);
	else
		strncpy(temp, str, 15);
	temp[14] = '\0';

	strcpy(ans, temp);
	file_name[0] = '\0';
					/* write permission on directory? */
	if (access(".", 2)) {
		beep();
		clear_line(win, 12, 24, TRUE);
		wattrstr(win, A_BOLD, "NO WRITE ON DIRECTORY");
		wrefresh(win);
		return;
	}
					/* prepend up to 13 "X"s */
	modified = 0;
	for (i=1; i<14; i++) {
		if (access(ans, 0)) {
			if (modified) {
				beep();
				clear_line(win, 12, 24, TRUE);
				waddstr(win, "NAME COLLISION");
				wrefresh(win);
				sleep(1);
			}
			strcpy(file_name, ans);
			return;
		}

		modified++;
		strcpy(temp, "X");
		strncat(temp, ans, 13);
		temp[14] = '\0';
		strcpy(ans, temp);
	}
	beep();
	clear_line(win, 12, 24, TRUE);
	waddstr(win, "BAD NAME");
	wrefresh(win);
	return;
}

/*
 * Convert a perfectly good Unix file name to fit the CP/M file name
 * rules.  Used for the modem7 batch file transfer.  Returns a pointer
 * to a static area containing the new name.
 */

char *
fix_name(path)
char *path;
{
	int i, dot;
	char *s, *name, temp[15], *ext, *strncpy(), *strrchr();
	static char ans[13];
					/* ignore the path component */
	if (s = strrchr(path, '/'))
		strncpy(temp, ++s, 15);
	else
		strncpy(temp, path, 15);
	temp[14] = '\0';
	name = temp;

	ext = "";
	dot = 0;
	for (i=strlen(temp)-1; i>=0; i--) {
		if (temp[i] == '.' && !dot) {
			dot = 1;
			temp[i] = '\0';
			ext = &temp[i+1];
		}
		if (islower(temp[i]))
			temp[i] = toupper(temp[i]);
	}
					/* if null name component */
	if (*name == '\0')
		name = "X";
					/* if name too long */
	if (strlen(name) > 8)
		*(name+8) = '\0';
					/* if extension too long */
	if (strlen(ext) > 3)
		*(ext+3) = '\0';

	sprintf(ans, "%-8.8s%-3.3s%c", temp, ext, CTRLZ);
	return(ans);
}

/*
 * Convert a CP/M style filename into a legal Unix file name.  Loads the
 * global character array "file_name".
 */

void
unfix_name(cpm_name)
char *cpm_name;
{
	extern char file_name[15];
	register int i, n;
	int dot;
	char temp[15];

	file_name[0] = '\0';
	if (*cpm_name == '\0')
		return;

	strcpy(temp, cpm_name);
					/* 8 character of the name */
	n = 0;
	for (i=0; i<8; i++) {
		if (temp[i] != ' ') {
			if (isupper(temp[i]))
				file_name[n++] = tolower(temp[i]);
			else
				file_name[n++] = temp[i];
		}
	}
					/* 3 character extension */
	dot = 0;
	for (i=8; i<11; i++) {
		if (temp[i] != ' ') {
			if (!dot) {
				dot++;
				file_name[n++] = '.';
			}
			if (isupper(temp[i]))
				file_name[n++] = tolower(temp[i]);
			else
				file_name[n++] = temp[i];
		}
	}
	file_name[n] = '\0';
	return;
}
