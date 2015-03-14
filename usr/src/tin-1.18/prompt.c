/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : prompt.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 03-06-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

/*
 *  prompt_num
 *  get a number from the user
 *  Return -1 if missing or bad number typed
 */

int prompt_num (ch, prompt)
	int ch;
	char *prompt;
{
	char *p;
	int num;

	set_alarm_clock_off ();
	
	clear_message ();

	sprintf (msg, "%c", ch);

	if ((p = getline (prompt, TRUE, msg)) != (char *) 0) {
		strcpy (msg, p);
		num = atoi (msg);
	} else {
		num = -1;
	}

	clear_message ();

	set_alarm_clock_on ();
		
	return (num);
}

/*
 *  prompt_string
 *  get a string from the user
 *  Return TRUE if a valid string was typed, FALSE otherwise
 */

int prompt_string (prompt, buf)
	char *prompt;
	char *buf;
{
	char *p;

	set_alarm_clock_off ();
	
	clear_message ();

	if ((p = getline (prompt, FALSE, (char *) 0)) == (char *) 0) {
		buf[0] = '\0';
		clear_message ();
		set_alarm_clock_on ();
		return FALSE;
	}
	strcpy (buf, p);
	
	clear_message ();
	set_alarm_clock_on ();
	
	return TRUE;
}

/*
 *  prompt_menu_string
 *  get a string from the user
 *  Return TRUE if a valid string was typed, FALSE otherwise
 */

int prompt_menu_string (line, col, var)
	int line;
	int col;
	char *var;
{
	char *p;

	set_alarm_clock_off ();

	MoveCursor (line, col);

	if ((p = getline ("", FALSE, var)) == (char *) 0) {
		set_alarm_clock_on ();
		return FALSE;
	}
	strcpy (var, p);

	set_alarm_clock_on ();
	
	return TRUE;
}


int prompt_yn (line, prompt, prompt_ch)
	int line;
	char *prompt;
	int prompt_ch;
{
	char ch;

	set_alarm_clock_off ();

	MoveCursor (line, 0);
	CleartoEOLN ();
	printf ("%s%c", prompt, prompt_ch);
	fflush (stdout);
	MoveCursor (line, (int) strlen (prompt));

	if ((ch = (char) ReadCh()) == CR) {
		ch = prompt_ch;
	}	

	if (line == LINES) {
		clear_message();
	} else {
		MoveCursor (line, (int) strlen (prompt));
		if (ch == ESC) {
			fputc (prompt_ch, stdout);
		} else {
			fputc (ch, stdout);
		}
		fflush (stdout);
	}

	set_alarm_clock_on ();

	return (ch == 'y' || ch == 'Y' ? TRUE : FALSE);
}


void prompt_on_off (row, col, var, help_text, prompt_text)
	int row;
	int col;
	int *var;
	char *help_text;
	char *prompt_text;
{
	int ch, var_orig;

	set_alarm_clock_off ();

	var_orig = *var;

	show_menu_help (help_text);
	do {
		MoveCursor (row, col + (int) strlen (prompt_text));
		if ((ch = (char) ReadCh ()) == ' ') {
			*var = !*var;
			printf ("%s", (*var ? "ON " : "OFF"));
			fflush (stdout);
		}
	} while (ch != CR && ch != ESC);

	if (ch == ESC) {
		*var = var_orig;
		printf ("%s", (*var ? "ON " : "OFF"));
		fflush (stdout);
	}
	
	set_alarm_clock_on ();
}


void continue_prompt ()
{
	char ch;

	set_alarm_clock_off ();
	
	info_message (txt_hit_any_key);
	ch = (char) ReadCh ();

	set_alarm_clock_on ();
}
