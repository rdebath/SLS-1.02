/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : screen.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 18-04-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

extern int errno;

char msg[LEN];
struct screen_t *screen;


void info_message (str)
	char *str;
{
	clear_message ();				/* Clear any old messages hanging around */
	center_line (LINES, FALSE, str);	/* center the message at screen bottom */
	if (! cmd_line) {
		MoveCursor (LINES, 0);
	}
}


void wait_message (str)
	char *str;
{
	clear_message ();	  /* Clear any old messages hanging around */
	fputs (str, stdout);
	fflush (stdout);
}


void error_message (template, str)
	char *template;
	char *str;
{
	errno = 0;

	clear_message ();	  /* Clear any old messages hanging around */
	
	fprintf (stderr, template, str);
	fflush (stderr);

	if (cmd_line) {
		fputc ('\n', stderr);
		fflush (stderr);
	} else {
		MoveCursor (LINES, 0);
		sleep (3);
	}
}


void perror_message (template, str)
	char *template;
	char *str;
{
	extern char *sys_errlist[];
	char str2[512];
	int err = 0;
	
	err = errno;

	clear_message ();	  /* Clear any old messages hanging around */

	sprintf (str2, template, str);
/*	
	perror (str2);
*/
	err = errno;
	fprintf (stderr, "%s: %s", str2, sys_errlist[err]);
	errno = 0;

	if (cmd_line) {
		fputc ('\n', stderr);
		fflush (stderr);
	} else {
		MoveCursor (LINES, 0);
		sleep (3);
	}
}


void clear_message ()
{
	if (! cmd_line) {
		MoveCursor (LINES, 0);
		CleartoEOLN ();
	}
}


void center_line (line, inverse, str)
	int line;
	int inverse;
	char *str;
{
	int pos;

	if (! cmd_line) {
		pos = (COLS - (int) strlen (str)) / 2;
		MoveCursor (line, pos);
		if (inverse) {
			StartInverse ();
		}
	}

	fputs (str, stdout);
	fflush (stdout);

	if (! cmd_line) {
		if (inverse) {
			EndInverse ();
		}
	}
}


void draw_arrow (line)
	int line;
{
	MoveCursor (line, 0);

	if (draw_arrow_mark) {
		fputs ("->", stdout);
		fflush (stdout);
	} else {
		StartInverse ();
		fputs (screen[line-INDEX_TOP].col, stdout);
		fflush (stdout);
		EndInverse ();
	}
	MoveCursor (LINES, 0);
}


void erase_arrow (line)
	int line;
{
	MoveCursor (line, 0);

	if (draw_arrow_mark) {
		fputs ("  ", stdout);
	} else {
		EndInverse ();
		fputs (screen[line-INDEX_TOP].col, stdout);
	}
	fflush (stdout);
}


void show_title (title)
	char *title;
{	
	int col;
	
	col = (COLS - (int) strlen (txt_type_h_for_help))+1;
	if (col) {
		MoveCursor (0, col);
		if (mail_check ()) {		/* you have mail message in */
			fputs (txt_you_have_mail, stdout);
		} else {
			fputs (txt_type_h_for_help, stdout);
		}
	}
	center_line (0, TRUE, title);
}


void ring_bell ()
{
	fputc ('\007', stdout);
	fflush (stdout);
}
