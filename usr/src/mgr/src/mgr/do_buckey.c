/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*}}}  */
/*{{{  #includes*/
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "bitblit.h"
#include "font.h"

#include "defs.h"
#include "event.h"

#include "border.h"
#include "cut.h"
#include "do_button.h"
#include "do_event.h"
#include "font_subs.h"
/*}}}  */

#ifdef MOVIE
extern char *log_command;
extern FILE *log_file;
#endif

/*{{{  do_it -- Given a WINDOW pointer, move that window to top.*/
static void do_it(win) register WINDOW *win;
{
	MOUSE_OFF(screen,mousex,mousey);
	cursor_off();
	ACTIVE_OFF();
	expose(win);
	ACTIVE(flags) &= ~W_NOINPUT;
	ACTIVE_ON();
	cursor_on();
	MOUSE_ON(screen,mousex,mousey);
}
/*}}}  */

/*{{{  topwin*/
/*	Given a window set ID and window number, move the window with
	that ID and number to the top.
	If the window number is -1, it is not considered and bottom-most
	window from the set is moved to the top.
*/

void topwin( winsetid, winnum ) register int winsetid, winnum;
{
	register WINDOW	*win;

	if( !active  ||  !ACTIVE(next) ) return;
	/*	We look from the back of the list toward the front so that
		we can cycle through a set.
	*/
	for( win = ACTIVE(prev);  win != active;  win = win->prev ) {
		if( winsetid  == W(setid)  &&
		    ( winnum == -1  ||  winnum == W(num) ) ) {
			do_it(win);
			return;
		}
	}
}
/*}}}  */
/*{{{  do_buckey*/
/**
	Check if the character is a "buckey" character, i.e. has the eighth bit
	on.  If it does, it may be a function character which causes some
	action.
	Returns 0 if not a buckey characater or not an function character.
	Returns 1 if a function character.
*/
int do_buckey(c) register unsigned char	c;
{
	static int	*intp;
	static int	windowflag = 0;	/* 1 when collecting window ID */
	static int	windowsetid;	/* collected window set ID */
	static int	windownum;	/* collected window number (in set) */
	char		*print_flags(), *print_events(),
				*print_stack();
	char		*print_ps();

#ifdef DEBUG
	static char	debugflag = 0;
	extern char	*debug_flags[];

	dprintf(b)(stderr, "Buckey %c\n", c & 0177);

	if( debugflag ) {
		c &= 0177;
		switch( debugflag ) {
		case '+':	/* add debug value  */
			{
			char s[2];
			s[0] = c;
			s[1] = '\0';
			if (debug && !index(debug_level, s)) {
				strcat(debug_level, s);
				fprintf(stderr,
					"Adding %s to debug list (now %s)\n",
					s, debug_level);
			}
			}
			break;
		case '-':	/* remove debug value  */
			{
			char *sp;
			if (debug && (sp=index(debug_level, c))) {
				strcpy(sp, sp+1);
				fprintf(stderr,
					"Deleting %c to debug list (now %s)\n",
					c, debug_level);
			}
			}
			break;
		}
		debugflag = 0;
		return 1;
	}
#endif
	if( windowflag ) {
		int	i = c & 0177;

		/*	Collect digits and create windowsetid.
			If a \n arrives, place window on top and swallow the
			\n.
			If any other character, place window on top and act
			on the character.
		*/
		switch( i ) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			*intp = (*intp)*10 + (i - '0');
			return 1;
		case ',':
			windownum = 0;
			intp = &windownum;
			return 1;
		case '\r':
		case '\n':
			windowflag = 0;
			topwin( windowsetid, windownum );
			return 1;
		default:
			windowflag = 0;
			topwin( windowsetid, windownum );
		}
	}
	if( !(c & 0200) )
		return 0;	/* Not a buckey character. */

	c &= 0177;
	switch(c) {
	case 'w':	/* switch to the following window number */
		windowflag = 1;
		windowsetid = 0;
		intp = &windowsetid;
		windownum = -1;
		break;
			/* Single digit windowsetid numbers.  As a convenience,
			'0' is mapped to window set ID 10. */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		windowsetid = (int)(c - '0');
		if( windowsetid == 0 )
			windowsetid = 10;
		topwin( windowsetid, -1 );
		break;
	case 'h':	/* hide top window on bottom */
		if (active && ACTIVE(next)) {
			MOUSE_OFF(screen,mousex,mousey);
			cursor_off();
			ACTIVE_OFF();
			hide_win();
			ACTIVE_ON();
			cursor_on();
			MOUSE_ON(screen,mousex,mousey);
		}
		break;
	case ' ':				/* go to next window */
		if (active && ACTIVE(next))
			do_it(ACTIVE(next));
		break;
	case '\b':			/* move bottom window to top */
		if (active && ACTIVE(next))
			do_it(ACTIVE(prev));
		break;
#ifdef MOVIE
	case 'S':				/* start logging */
		if (log_command && !log_file) {
			log_file = (strcmp(log_command,"-")==0) ? stdout : popen(log_command,"w");
#ifdef DEBUG
		 	dprintf(L)(stderr,"Start log to fd 0x%x\n",fileno(log_file));
#endif
   		log_start(log_file);
			}
		break;
	case 's':				/* end logging */
		if (log_file) {
   		log_end();
#ifdef DEBUG
		 	dprintf(L)(stderr,"Ending log\n",fileno(log_file));
#endif
			if (log_file != stdout)
				pclose(log_file);
			log_file = NULL;
			}
		break;
#ifdef DEBUG
	case 'x':				/* display bitmap stuff */
		{
		extern BITMAP *bit_maps[];
		extern unsigned short next_id;
		register int i;
		register BITMAP *b;
		for(i=0;i<next_id;i++)
			if (b=bit_maps[i])
				fprintf(stderr,"%d-%d: %d,%d %dx%d (%x->%x)\n",
						i,b->id,b->x0,b->y0,b->wide,b->high,b,b->primary);
		}
		break;
#endif	/* debug */
#endif	/* movie */
	case 'l':				/* clear the window */
		MOUSE_OFF(screen,mousex,mousey);
		cursor_off();
		ACTIVE_OFF();
		CLEAR(ACTIVE(window),PUTOP(BIT_NOT(BIT_SRC),ACTIVE(style)));
		ACTIVE_ON();
		cursor_on();
		MOUSE_ON(screen,mousex,mousey);
		break;
	case 'R':	/* redraw the screen */
	case 'r':
		MOUSE_OFF(screen,mousex,mousey);
		if( active ) {
			cursor_off();
			ACTIVE_OFF();
		}
		redraw();
		if( active ) {
			ACTIVE_ON();
			cursor_on();
		}
		MOUSE_ON(screen,mousex,mousey);
		break;
	case 'n':	/* make a new window 80x24 characters */
	case 'N':	/* make a new window, sweept out by the mouse */
		MOUSE_OFF(screen,mousex,mousey);
		if( active ) {
			cursor_off();
			ACTIVE_OFF();
		}
		if( c=='n' )
			initwindow();	/* default window */
		else
			new_window();	/* swept out with mouse */
		if( active ) {
			ACTIVE_ON();
			cursor_on();
		}
		MOUSE_ON(screen,mousex,mousey);
		break;
	case 'z':	/* stop process */
		suspend();
		break;
	case 'm':	/* move text (cut and paste) */
		if( !active )
			break;
		MOUSE_OFF(screen,mousex,mousey);
		cursor_off();
		if (cut() > 0)
			paste();
		cursor_on();
		MOUSE_ON(screen,mousex,mousey);
		break;
	case 'c':	/* start a cut of text */
		if( !active )
			break;
		MOUSE_OFF(screen,mousex,mousey);
		cursor_off();
		cut();
		cursor_on();
		MOUSE_ON(screen,mousex,mousey);
		break;
	case 'p':	/* paste text at current cursor */
		if( !active )
			break;
		paste();
		break;
#ifdef DEBUG
	case '+':	/* add debug value  */
	case '-':	/* remove debug value  */
		debugflag = c;
		break;
	case '#':	/* temporary (die) abruptly */
		setreuid(getuid(),getuid());
		setregid(getgid(),getgid());	/* so we can get a core dump */
		kill(getpid(),SIGQUIT);
		sleep(1);
		break;
	case '?':				/* Print debugging flags */
		if( !debug )
		   break;
		{
			int	i;

			fprintf(stderr,"Debug flags:\r\n");
			for(i=0;debug_flags[i];i++)
				fprintf(stderr,"  %s\n",debug_flags[i]);
		}
		break;
#endif
	case 'Q':	/* Quit nicely */
		_quit();
		exit(0);
		break;
#ifdef DEBUG
	case 'I':	/* temporary (status + process info) */
		if( !debug )
		   break;
		fprintf(stderr,"please wait...\r\n");
		get_ps();
		/* no break */
	case 'i':	/* temporary (status info) */
		if( !debug )
		   break;
		for(win=active;win!=(WINDOW *) 0;win=win->next) {
			fprintf(stderr,"%s: %d,%d  %d,%d num(%d) {%s}\r\n",
				W(tty),W(x0),W(y0),
				BIT_WIDE(W(window)),BIT_HIGH(W(window)),W(num),
				c=='I' ? print_ps(W(tty)):"");
			fprintf(stderr,"  flags: %s\n",print_flags(W(flags)));
			fprintf(stderr,"  events: %s\n",print_events(W(event_mask)));
			fprintf(stderr,"  cursors: %d,%d  %d,%d  function %d\r\n",
				W(x),W(y),W(gx),W(gy),W(op));
			fprintf(stderr,"  fd's: %d,<->%d, pid(%d)",W(from_fd),
				W(to_fd),W(pid));
			if (W(main)&& strcmp(W(tty),W(main)->tty)!=0)
				fprintf(stderr," main(%s)",W(main)->tty);
			if W(alt)
				fprintf(stderr," alt(%s)",W(alt)->tty);
			fprintf(stderr,"\r\n");
			if (W(snarf))
				fprintf(stderr,"  snarf (%d) [%.30s...]\r\n",strlen(W(snarf)),
					W(snarf));
			if (W(stack)) {
				fprintf(stderr,"  stacks:\r\n");
				for(win2=W(stack);win2;win2=win2->stack)
					fprintf(stderr,"\t%0x%x, events: 0x%x\r\n",
						win2->code,win2->event_mask);
			}
		}
		if (c=='I')
			free_ps();
		break;
#endif
	case 'M':	/* temporary (malloc info) */
		fprintf(stderr,"Malloc stats: \r\n");
#ifdef MALLOC
		c=malloc_verify();	/* 1 is ok */
		fprintf(stderr,"   Verify returns %s\n",c==1 ? "OK":"CORRUPTED");
#endif
		break;
	default:
		return 0;	/* not a valid function character */
	}
	return 1;		/* we acted on a function character */
}
/*}}}  */
