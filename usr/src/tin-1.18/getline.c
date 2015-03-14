/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : getline.c
 *  Author    : Chris Thewalt & Iain Lea 
 *  Created   : 09-11-91
 *  Updated   : 25-06-92
 *  Notes     : emacs style line editing input package.  
 *  Copyright : (c) Copyright 1991-92 by Chris Thewalt & Iain Lea
 *              Permission to use, copy, modify, and distribute this
 *              software for any purpose and without fee is hereby
 *              granted, provided that the above copyright notices
 *              appear in all copies and that both the copyright
 *              notice and this permission notice appear in supporting
 *              documentation. This software is provided "as is" without
 *              express or implied warranty.
 */

#include "tin.h"

extern int      isatty ();	

#define BUF_SIZE	1024
#define SCROLL		30
#define TABSIZE		4
#ifndef HIST_SIZE
#define HIST_SIZE	100
#endif

#define CTRL_A	'\001'
#define CTRL_B	'\002'
#define CTRL_D	'\004'
#define CTRL_E	'\005'
#define CTRL_F	'\006'
#define CTRL_H	'\010'
#define CTRL_K	'\013'
#define CTRL_L	'\014'
#define CTRL_R	'\022'
#define CTRL_N	'\016'
#define CTRL_P	'\020'
#define TAB		'\t'
#define DEL		'\177'

char	*hist_buf[HIST_SIZE];
int		hist_pos, hist_last;
static char gl_buf[BUF_SIZE];       /* input buffer */
static char *gl_prompt;				/* to save the prompt string */
static int  gl_init_done = 0;		/* -1 is terminal, 1 is batch  */
static int  gl_width = 0;			/* net size available for input */
static int  gl_pos, gl_cnt = 0;     /* position and size of input */

#if __STDC__

static int      gl_tab (char *, int, int *);
static void		gl_redraw (void);
static void     gl_addchar (int);
static void     gl_newline (void);
static void     gl_fixup (int, int);
static void     gl_del (int);
static void     gl_kill (void);
static void     hist_add (void);
static void     hist_init (void);
static void     hist_next (void);
static void     hist_prev (void);

int 	(*gl_in_hook)(char *) = 0;
int 	(*gl_out_hook)(char *) = 0;
int 	(*gl_tab_hook)(char *, int, int *) = gl_tab;

#else

static int      gl_tab ();
static void		gl_redraw ();
static void     gl_addchar ();
static void     gl_newline ();
static void     gl_fixup ();
static void     gl_del ();
static void     gl_kill ();
static void     hist_add ();
static void     hist_init ();
static void     hist_next ();
static void     hist_prev ();

int 	(*gl_in_hook)() = 0;
int 	(*gl_out_hook)() = 0;
int 	(*gl_tab_hook)() = gl_tab;

#endif


#if __STDC__
char *getline (char *prompt, int number_only, char *str)
#else
char *getline (prompt, number_only, str)
	char *prompt;
	int number_only;
	char *str;
#endif
{
	int c, i, loc, tmp;

	if (! gl_init_done) {
		gl_init_done = 1;
		hist_init ();
	}

	if (prompt == (char *) 0) {	
		prompt = "";
	}
	gl_buf[0] = 0;		/* used as end of input indicator */
	gl_fixup (-1, 0);	/* this resets gl_fixup */
	gl_width = COLS - strlen (prompt);
	gl_prompt = prompt;
	gl_pos = gl_cnt = 0;

	fputs (prompt, stdout);
	fflush (stdout);
	
	if (gl_in_hook) {
		loc = gl_in_hook (gl_buf);
		if (loc >= 0)
		    gl_fixup (0, BUF_SIZE);
	}
	if (str != (char *) 0) {
		for (i=0 ; str[i] ; i++) 
			gl_addchar (str[i]);
	}
	while ((c = ReadCh ()) != EOF) {
		c &= 0xff;	
		if (isprint (c)) {
			if (number_only) {
				if (isdigit (c) && gl_cnt < 6) {	/* num < 100000 */
				    gl_addchar (c);
				} else {
					ring_bell ();
				}
			} else {
			    gl_addchar (c);
			}
		} else {
			switch (c) {
				case ESC: 			/* abort */
					return (char *) 0;
				case '\n': 			/* newline */
				case '\r':
					gl_newline ();
					return gl_buf;
				case CTRL_A:
					gl_fixup (-1, 0);
					break;
				case CTRL_B:
					gl_fixup (-1, gl_pos-1);
					break;
				case CTRL_D:
					if (gl_cnt == 0) {
					    gl_buf[0] = 0;
						fputc ('\n', stdout);
						return gl_buf;
					} else {
						gl_del (0);
					}
					break;
				case CTRL_E:
					gl_fixup (-1, gl_cnt);
					break;
				case CTRL_F:
					gl_fixup (-1, gl_pos+1);
					break;
				case CTRL_H:
				case DEL:
					gl_del (-1);
					break;
				case TAB:
					if (gl_tab_hook) {
						tmp = gl_pos;
						loc = gl_tab_hook (gl_buf, strlen (gl_prompt), &tmp);
						if (loc >= 0 || tmp != gl_pos)
							gl_fixup (loc, tmp);
					}
					break;
				case CTRL_K:
					gl_kill ();
					break;
				case CTRL_L:
				case CTRL_R:
					gl_redraw ();
					break;
				case CTRL_N:
					hist_next ();
					break;
				case CTRL_P:
					hist_prev ();
					break;
				default:
					ring_bell ();
					break;
		    }
		}
	}
	return gl_buf;
}

/*
 * adds the character c to the input buffer at current location if
 * the character is in the allowed template of characters
 */

#if __STDC__
static void gl_addchar (int c)
#else
static void gl_addchar (c)
	int c;
#endif
{
	int  i;

	if (gl_cnt >= BUF_SIZE - 1) {
		error_message ("getline: input buffer overflow", "");
		exit (1);
	}
	
	for (i=gl_cnt; i >= gl_pos; i--) {
		gl_buf[i+1] = gl_buf[i];
	}
	gl_buf[gl_pos] = c;
	gl_fixup (gl_pos, gl_pos+1);
}

/*
 * Cleans up entire line before returning to caller. A \n is appended.
 * If line longer than screen, we redraw starting at beginning
 */

static void gl_newline ()
{
	int change = gl_cnt;
	int len = gl_cnt;
	int loc = gl_width - 5;	/* shifts line back to start position */

	if (gl_cnt >= BUF_SIZE - 1) {
		error_message ("getline: input buffer overflow", "");
		exit (1);
	}
	hist_add ();			/* only adds if nonblank */
	if (gl_out_hook) {
		change = gl_out_hook (gl_buf);
		len = strlen (gl_buf);
	} 
	if (loc > len)
		loc = len;
	gl_fixup (change, loc);	/* must do this before appending \n */
	gl_buf[len] = '\0';
}

/*
 * Delete a character.  The loc variable can be:
 *    -1 : delete character to left of cursor
 *     0 : delete character under cursor
 */

#if __STDC__
static void gl_del (int loc)
#else
static void gl_del (loc)
	int loc;
#endif
{
	int i;

	if ((loc == -1 && gl_pos > 0) || (loc == 0 && gl_pos < gl_cnt)) {
		for (i=gl_pos+loc; i < gl_cnt; i++)
			gl_buf[i] = gl_buf[i+1];
		gl_fixup (gl_pos+loc, gl_pos+loc);
	} else {
		ring_bell ();
	}
}

/*
 * delete from current position to the end of line
 */
 
static void gl_kill ()
{
	if (gl_pos < gl_cnt) {
		gl_buf[gl_pos] = '\0';
		gl_fixup (gl_pos, gl_pos);
	} else {
		ring_bell ();
	}
}

/*
 * emit a newline, reset and redraw prompt and current input line
 */

static void gl_redraw ()
{
	if (gl_init_done == -1) {
		fputc ('\n', stdout);
		fputs (gl_prompt, stdout);
		gl_pos = 0;
		gl_fixup (0, BUF_SIZE);
    }
}

/*
 * This function is used both for redrawing when input changes or for
 * moving within the input line.  The parameters are:
 *   change : the index of the start of changes in the input buffer,
 *            with -1 indicating no changes.
 *   cursor : the desired location of the cursor after the call.
 *            A value of BUF_SIZE can be used to indicate the cursor
 *            should move just past the end of the input line.
 */

#if __STDC__
static void gl_fixup (int change, int cursor)
#else
static void gl_fixup (change, cursor)
	int change;
	int cursor;
#endif
{
	static int   gl_shift;	/* index of first on screen character */
	static int   off_right;	/* true if more text right of screen */
	static int   off_left;	/* true if more text left of screen */
	int          left = 0, right = -1;		/* bounds for redraw */
	int          pad;		/* how much to erase at end of line */
	int          backup;        /* how far to backup before fixing */
	int          new_shift;     /* value of shift based on cursor */
	int          extra;         /* adjusts when shift (scroll) happens */
	int          i;

	if (change == -1 && cursor == 0 && gl_buf[0] == 0) {   /* reset */
		gl_shift = off_right = off_left = 0;
		return;
	}
	pad = (off_right) ? gl_width - 1 : gl_cnt - gl_shift;   /* old length */
	backup = gl_pos - gl_shift;
	if (change >= 0) {
		gl_cnt = strlen (gl_buf);
		if (change > gl_cnt)
			change = gl_cnt;
	}
	if (cursor > gl_cnt) {
		if (cursor != BUF_SIZE)		/* BUF_SIZE means end of line */
			ring_bell ();
			cursor = gl_cnt;
	}
	if (cursor < 0) {
		ring_bell ();
		cursor = 0;
	}
	if (off_right || off_left && (cursor < gl_shift + gl_width - SCROLL / 2))
		extra = 2;			/* shift the scrolling boundary */
	else 
		extra = 0;
	new_shift = cursor + extra + SCROLL - gl_width;
    if (new_shift > 0) {
		new_shift /= SCROLL;
		new_shift *= SCROLL;
    } else
		new_shift = 0;
    if (new_shift != gl_shift) {	/* scroll occurs */
		gl_shift = new_shift;
		off_left = (gl_shift) ? 1 : 0;
		off_right = (gl_cnt > gl_shift + gl_width - 1)? 1 : 0;
        left = gl_shift;
		right = (off_right) ? gl_shift + gl_width - 2 : gl_cnt;
    } else if (change >= 0) {		/* no scroll, but text changed */
		if (change < gl_shift + off_left) {
		    left = gl_shift;
		} else {
		    left = change;
		    backup = gl_pos - change;
		}
		off_right = (gl_cnt > gl_shift + gl_width - 1)? 1 : 0;
		right = (off_right) ? gl_shift + gl_width - 2 : gl_cnt;
	}
	pad -= (off_right) ? gl_width - 1 : gl_cnt - gl_shift;
	pad = (pad < 0)? 0 : pad;
	if (left <= right) {		/* clean up screen */
		for (i=0; i < backup; i++)
			fputc ('\b', stdout);
		if (left == gl_shift && off_left) {
			fputc ('$', stdout);
			left++;
		}
		for (i=left; i < right; i++)
			fputc (gl_buf[i], stdout);
		if (off_right) {
			fputc ('$', stdout);
			gl_pos = right + 1;
		} else { 
			for (i=0; i < pad; i++)	/* erase remains of prev line */
				fputc (' ', stdout);
			gl_pos = right + pad;
		}
	}
	i = gl_pos - cursor;		/* move to final cursor location */
	if (i > 0) {
		while (i--)
			fputc ('\b', stdout);
	} else {
		for (i=gl_pos; i < cursor; i++)
			fputc (gl_buf[i], stdout);
	}
	fflush (stdout);
	gl_pos = cursor;
}
	
/*
 * default tab handler, acts like tabstops every TABSIZE cols
 */

#if __STDC__
static int gl_tab (char *buf, int offset, int *loc)
#else
static int gl_tab (buf, offset, loc)
	char *buf;
	int offset;
	int *loc;
#endif
{
	int i, count, len;
	
	len = strlen (buf);
	count = TABSIZE - (offset + *loc) % TABSIZE;
	for (i=len; i >= *loc; i--)
		buf[i+count] = buf[i];
	for (i=0; i < count; i++)
		buf[*loc+i] = ' ';
	i = *loc;
	*loc = i + count;
	return i;
}

/*
 * History functions
 */

static void hist_init ()
{
	int i;
	
	for (i=0; i < HIST_SIZE; i++)
		hist_buf[i] = (char *) 0;
}


static void hist_add ()
{
	char *p = gl_buf;

	while (*p == ' ' || *p == '\t')	/* only save nonblank line */
		p++;
	if (*p) {
		hist_buf[hist_last] = str_dup (gl_buf);
		hist_last = (hist_last + 1) % HIST_SIZE;
		if (hist_buf[hist_last]) {	/* erase next location */
			free(hist_buf[hist_last]);
			hist_buf[hist_last] = (char *) 0;
		}
	}
	hist_pos = hist_last;
}

/*
 * loads previous hist entry into input buffer, sticks on first
 */
 
static void hist_prev ()
{
	int   next;

	next = (hist_pos - 1 + HIST_SIZE) % HIST_SIZE;
	if (next != hist_last) {
		if (hist_buf[next]) {
			hist_pos = next;
			strcpy (gl_buf, hist_buf[hist_pos]);
		} else {
			ring_bell ();
		}
	} else {
		ring_bell ();
	}
	if (gl_in_hook)
		gl_in_hook (gl_buf);
	gl_fixup (0, BUF_SIZE);
}

/*
 * loads next hist entry into input buffer, clears on last
 */
 
static void hist_next ()
{
	if (hist_pos != hist_last) {
		hist_pos = (hist_pos + 1) % HIST_SIZE;
		if (hist_buf[hist_pos]) {
			strcpy (gl_buf, hist_buf[hist_pos]);
		} else {
			gl_buf[0] = 0;
		}
	} else {
		ring_bell ();
	}
	if (gl_in_hook) 
		gl_in_hook (gl_buf);
	gl_fixup (0, BUF_SIZE);
}
