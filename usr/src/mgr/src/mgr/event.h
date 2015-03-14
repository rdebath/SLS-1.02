/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* event codings for mgr */

#define EVENT_B1_DOWN		1		/* right button down */
#define EVENT_B2_DOWN		2		/* middle button down */
#define EVENT_BSYS_DOWN		4		/* left (system) button down */
#define EVENT_B1_UP		-1		/* right button going up */
#define EVENT_B2_UP		-2		/* middle button going up */
#define EVENT_BSYS_UP		-4		/* left (system) button up */
#define EVENT_SHAPE		5		/* window reshaped */
#define EVENT_REDRAW		6		/* screen redraw */
#define EVENT_ACTIVATED		7		/* window made active window */
#define EVENT_DEACTIVATED	8		/* window no longer active */
#define EVENT_COVERED		9		/* window is partly obscured */
#define EVENT_UNCOVERED		10		/* window is un-obscured */
#define EVENT_MOVE		11		/* window was moved */
#define EVENT_DESTROY		12		/* client window destroyed */
#define EVENT_ACCEPT		13		/* messages accepted */
#define EVENT_NOTIFY		14		/* set window name */
#define EVENT_TELLME		15		/* unused */
#define EVENT_SNARFED		16		/* a snarf happened */
#define EVENT_PASTE		17		/* a paste happened */

#define MIN_EVENT		-4
#define MAX_EVENT		17
/* NOTE, MAXEVENTS in defines.h must be kept consistent with these values.
*/

/* Check if the given event number is legal, return TRUE if it is.
*/
#define CHK_EVENT(event)	(MIN_EVENT <= (event)  &&  (event) <= MAX_EVENT)

/* event stack control flags */

#define EVENT_STACK		18		/* stack events */
#define EVENT_STFLAG		19		/* events stacked somewhere */

/* button event control chars */

#define E_ESC			'%'		/* escape char */
#define E_POS			'p'		/* return mouse position */
#define E_CPOS			'P'		/* mouse position (cols/rows)*/
#define E_SWRECT		'r'		/* sweep out a rectangle */
#define E_SWRECTA		'R'		/* " in screen coords */
#define E_SWBOX      'b'      /* sweep out a box */
#define E_SWBOXA     'B'      /* " in screen coords */
#define E_SWLINE		'l'		/* sweep out a line */
#define E_SWTEXT		't'		/* sweep out text */
#define E_SWTEXTT		'T'		/* " text region only */
#define E_NOTIFY		'n'		/* get applic. notification */
#define E_WHO			'w'		/* get applic. id */
#define E_WHOSIZE		'S'		/* get applic. size */
#define E_FROM			'f'		/* who is message from */
#define E_MESS			'm'		/* message sent */
#define E_MSGSIZE		's'		/* size of message sent */
#define E_SNARFSIZE		'c'		/* size of cut buffer */
#define E_SNARFBUF		'C'		/* content of cut buffer */
#define E_TIMESTAMP	'M'		/* timestamp (100ths seconds since MGR startup) */

#define E_LIST_BUTTON		"%pPrRbBlTtnwSM"	/* valid esc's for buttons */
#define E_LIST_UP				"%pPM"  /* valid esc's for button ups */
#define E_LIST_ACCEPT		"%pPfms"	/* valid esc's for notify */
#define E_LIST_SNARF		"%cCf"		/* valid esc's for snarf */
#define E_LIST_PASTE		"%c"		/* valid esc's for snarf */

#define E_MAIN		0			/* in main window */
#define E_STACK		1			/* on window stack */

/* other event macros */

/* convert event number to index into event array */
#define GET_EVENT(event)	((event)-(MIN_EVENT))

#define EVENT_SET_MASK(win,event)	\
				((win->event_mask) |= (1L<<GET_EVENT(event)))
#define EVENT_CLEAR_MASK(win,event)	\
				((win->event_mask) &= ~(1L<<GET_EVENT(event)))
#define IS_EVENT(win,event)	\
				((win->event_mask) & (1L<<GET_EVENT(event)))
