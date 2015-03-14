/*                        Copyright (c) 1988 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

#ifndef _MGR_WINDOW_H
#define _MGR_WINDOW_H

/* defines for terminal emulator */

/* text flags - for commands with text string arguments */

#define T_INVALID	0	/* invalid command */
#define T_FONT		1	/* down load a new font */
#define T_MENU		2	/* down load a menu */
#define T_EVENT		3	/* down load an event string */
#define T_YANK		4	/* fill the yank bufffer */
#define T_BITMAP	5	/* down load a bit map */
#define T_COMMAND	6	/* start a new window & command in it */
#define T_GIMME		7	/* send me stuff */
#define T_SMAP		8	/* save a bitmap on a file */
#define T_GMAP		9	/* read a bitmap from a file */
#define T_SEND		10	/* send a message to another application */
#define T_GRUNCH	11	/* fast-draw, short vector mode */
#define T_STRING	12	/* write a text sting into an offscreen bitmap */

/* option codes for GETMODE / SETMODE  */

#define M_STANDOUT	0	/* window is in standout mode  */
#define M_WOB		1	/* window is white on black  */
#define M_AUTOEXPOSE	2	/* expose window upon shell output  */
#define M_BACKGROUND	3	/* permit obscured window to update  */
#define M_NOINPUT	4	/* don't accept keyboard input  */
#define M_NOWRAP	5	/* don't auto wrap  */
#define M_OVERSTRIKE	6	/* overstrike mode  */
#define M_ABS		7	/* use absolute coordinates  */
#define M_ACTIVATE	8	/* activate / hide window; not a mode */
#define M_STACK		12	/* permit event stacking */
#define M_DUPKEY	13	/* set keyboard escape key */
#define M_NOBUCKEY	14	/* prevent mgr processing buckey keys,
				   pass them through to the application */

/* cut/paste options */

#define M_SNARFLINES	9	/* only snarf entire lines */
#define M_SNARFTABS	10	/* change spaces to tabs in snarf */
#define M_SNARFHARD	11	/* snarf even if errors */

/* option codes for GETINFO */

#define G_MOUSE	 	0	/* mouse coordinates  */
#define G_TERMCAP 	1	/* send back termcap entry  */
#define G_WINSIZE 	2	/* cols, lines  */
#define G_FONT	 	3	/* font wide, high, #  */
#define G_COORDS 	4	/* window coords  */
#define G_STATUS 	5	/* window status  */
#define G_ALL	 	6	/* complete window status  */
#define G_SYSTEM 	7	/* system status  */
#define G_ALLFONT 	8	/* font information  */
#define G_TEXT		9	/* text region size */
#define G_ALLMINE  	10	/* window status for client windows */
#define G_CURSOR	11	/* character/ graphics cursor position */
#define G_MOUSE2	12	/* cooked mouse coordinates  */
#define G_NOTIFY	13	/* gimme info re notify windows */
#define G_ID		14	/* my client window number */
#define G_FLAGS		15	/* current window flags */
#define G_MAX		15	/* maximum GETINFO value */

/* option codes for stacking window environment */

#define P_MENU		0x001	/* push menus */
#define P_EVENT		0x002	/* push events */
#define P_FONT		0x004	/* push current font */
#define P_CURSOR	0x008	/* push current cursor positions and styles  */
#define P_BITMAP	0x010	/* push saved bitmaps */
#define P_POSITION	0x020	/* push window location */
#define P_WINDOW	0x040	/* push window contents */
#define P_FLAGS		0x080	/* push window flags */
#define P_MOUSE		0x100	/* push mouse position */
#define P_TEXT		0x200	/* push text region */
#define P_COLOR		0x400	/* push colors */


#define P_ALL		0x7ff	/* push everything */
#define P_MAX		0x800	/* end of codes marker */
#define P_DEFAULT	(P_MENU | P_EVENT | P_FONT | P_FLAGS | P_TEXT)
#define P_CLEAR		0x8000	/* clear new environment */

/* menu_flags */

#define MF_SNIP		8	/* don't send action for parent of s/o menu */
#define MF_PAGE		4	/* auto-page for menus */
#define MF_AUTO		2	/* auto-right exit for menus */
#define MF_CLEAR	1	/* clear menu flags */

/* Escape codes */

#define ESC		'\033'	/* escape character */

#define E_MINUS		'-'	/* set the munus flag */
#define E_SEP1		','	/* primary field seperator */
#define E_SEP2		';'	/* secondary field seperator */
#define E_MOUSE		'?'	/* testing  -- move the mouse  */
#define E_ADDLINE	'a'	/* add a new line  */
#define E_ADDCHAR	'A'	/* add a character  */
#define E_BITBLT	'b'	/* do a bit blit  */
#define E_BITCRT	'B'	/* create a bit blit  */
#define E_CLEAREOL	'c'	/* clear  */
#define E_CLEAREOS	'C'	/* clear  */
#define E_DELETELINE	'd'	/* delete a line */
#define E_BITLOAD	'D'	/* download a bitmap  */
#define E_EVENT		'e'	/* download events  */
#define E_DELETECHAR	'E'	/* delete a char */
#define E_DOWN		'f'	/* down 1 line */
#define E_FONT		'F'	/* pick a new font  */
#define E_GO		'g'	/* Go; move graphics pointer  */
#define E_MOVE		'G'	/* move to x,y pixels  */
#define E_SETCURSOR	'h'	/* select cursor style */
#define E_BLEEP		'H'	/* blink a section of the screen */
#define E_GETINFO	'I'	/* get info from mgr */
#define E_STANDOUT	'i'	/* start standout mode */
#define E_FCOLOR	'j'	/* set forground color */
#define E_BCOLOR	'J'	/* set background color */
#define E_LINE		'l'	/* Plot a line  */
#define E_LINK		'L'	/* menu links */
#define E_MENU		'm'	/* download menus */
#define E_CUP		'M'	/* move to col, row (zero origin)  */
#define E_STANDEND	'n'	/* end standout mode */
#define E_CIRCLE	'o'	/* Plot a circle or an ellipse or an arc */
#define E_PUSH		'P'	/* push window environment */
#define E_POP		'p'	/* pop window environment */
#define E_RUBBER	'R'	/* rubber band a line/rect (obsolete) */
#define E_RIGHT		'r'	/* right 1 column  */
#define E_SETMODE	'S'	/* set a window mode */
#define E_CLEARMODE	's'	/* clear window mode */
#define E_TEXTREGION	't'	/* set the text region */
#define E_BITGET	'U'	/* upload a bitmap  */
#define E_UPLINE	'u'	/* up 1 line  */
#define E_VI		'V'	/* set vi mode */
#define E_NOVI		'v'	/* turn off vi mode */
#define E_SHAPE		'W'	/* reshape window, make it active  */
#define E_SIZE		'w'	/* reshape window: cols,rows  */
#define E_GIMME		'x'	/* send me data */
#define E_SNARF		'Y'	/* snarf text into the snarf buffer  */
#define E_PUTSNARF	'y'	/* put the snarf buffer  */
#define E_MAKEWIN	'Z'	/* make/goto a new window */
#define E_HALFWIN	'z'	/* make a 1/2 window */
#define E_NULL		'$'	/* do nothing, force exit */
#define E_SMAP		'>'	/* save a bitmap */
#define E_GMAP		'<'	/* get a bitmap */
#define E_SEND		'|'	/* send a message to another application */
#define E_CURSOR	'%'	/* set mouse cursor */
#define E_GRUNCH	':'	/* graphics crunch mode (experimental) */
#define E_STRING	'.'	/* write characters in offscreen bitmap */
#ifdef XMENU
#define E_XMENU		'X'	/* extended menu operations */
#endif


/* misc */

#define C_NOCHAR	'?'	/* for character not in font */
#define C_EXPOSED	'e'	/* window is not obscured */
#define C_ACTIVE	'a'	/* window has input focus */
#define C_NOTACTIVE	'n'	/* window is obscured */
#define C_OBSCURED	'o'	/* window is obscured */
#define C_NAME		"px|mgr|mgr teminal emulator"

#define C_NULL		'\000'	/* null */
#define C_BS		'\b'	/* back space */
#define C_FF		'\f'	/* form feed */
#define C_BELL		'\007'	/* bell */
#define C_TAB		'\t'	/* tab */
#define C_RETURN	'\r'	/* return */
#define C_NL		'\n'	/* line feed */

/* cursor styles */
#define CS_BLOCK		0		/* standard block cursor */
#define CS_LEFT		1		/* left vertical bar */
#define CS_RIGHT		2		/* right vertical bar */
#define CS_BOX			3		/* outline */
#define CS_UNDER		4		/* underline */
#define CS_INVIS		9		/* invisible */

#define WHITE 0
#define BLACK 255

/* mask for opcode bits */
#define NOCOLOR 0xf
/* extract fg color from op */
#define GETFCOLOR(op) (((op)>>4)&0xff)
/* extract bg color from op */
#define GETBCOLOR(op) (((op)>>12)&0xff)
/* insert fg color into op */
#define PUTFCOLOR(op,color) (((op)&0xff00f) | (((color)&0xff)<<4))
/* insert bg color into op */
#define PUTBCOLOR(op,color) (((op)&0x00fff) | (((color)&0xff)<<12))
/* insert new bitblit function into op */
#define PUTOP(func,op) (((func)&0xf) | ((op)&~0xf))
/* build op */
#define BUILDOP(op,fg,bg) (((op)&0xf) | (((fg)&0xff)<<4) | (((bg)&0xff)<<12))
/* switch colors */
#define SWAPCOLOR(op) (((op)&0xf) | ((op)>>8 & 0xff0) | ((op)<<8 & 0xff000))

#define	DST 	0xA	/* 1010 */	/* same as f_dest */
#define	SRC	0xC	/* 1100 */	/* same as f_source */
#define OPCODE(expr)	(0xF&(expr))

/* Names for common bitblit functions */

#define BIT_NOT(x) (0xf^(x))
#define BIT_SRC BUILDOP(SRC,BLACK,WHITE)
#define BIT_DST	BUILDOP(DST,BLACK,WHITE)
#define BIT_SET	BUILDOP(~0,BLACK,WHITE)
#define BIT_CLR	BUILDOP(0,BLACK,WHITE)
#define BIT_OR  BUILDOP(BIT_SRC|BIT_DST,BLACK,WHITE)
#define BIT_NOR BUILDOP(BIT_NOT(BIT_SRC)|BIT_DST,BLACK,WHITE)
#define BIT_XOR	BUILDOP(BIT_SRC^BIT_DST,BLACK,WHITE)
#define BIT_AND BUILDOP(BIT_SRC&BIT_DST,BLACK,WHITE)
#define BIT_NAND BUILDOP(BIT_NOT(BIT_SRC)&BIT_DST,BLACK,WHITE)
#define BIT_INV (BIT_NOT(DST))

#define BIT_INVERT (BIT_NOT(DST))

/* other macros */

#define Scalex(x) \
	(W(flags)&W_ABSCOORDS ?  (x) :  (x) * (int)BIT_WIDE(W(window))/GMAX)
#define Scaley(y) \
	(W(flags)&W_ABSCOORDS ?  (y) :  (y) * (int)BIT_HIGH(W(window))/GMAX)
#define Scalexy(y) \
	(W(flags)&W_ABSCOORDS ?  (y) : \
	 (y) * (int)(BIT_HIGH(W(window))+BIT_WIDE(W(window)))/(2*GMAX))

#define FSIZE(c)	((int) (W(font)->head.c))
#define WIDE	        BIT_WIDE(window)
#define HIGH	        BIT_HIGH(window)
#define T_WIDE	        BIT_WIDE(text)
#define T_HIGH	        BIT_HIGH(text)

#endif
