# include <stdio.h>
#    include <curses.h>
# ifdef TERM_INFO
#    include <term.h>
# endif TERM_INFO 

/*	PSC MENU COPYRIGHT NOTICE

	Part of PSCMenu

	This software is to be considered to be public domain, it
may be copied, modified and parts of it may be used in other programs
as long as this copyright notice remains intact.

	Copyright()   PSC - Plymouth State College
	Written by:   Ted Wisniewski 12-9-1990
 
*/

# define 		MAXL		80
# define  		THIS_TTY	0
# define		DEL		0x08
# define		BS		0x7f
# define		SPACE		0x20
# define		LF		0x0a
# define 		RETURN		0x0d
# define		LAST_LINE	23

# define		UP		252
# define		DOWN		253
# define		LEFT		254
# define		RIGHT		255

char *getenv(), *tgetstr(), *tgoto();

int outc();

char *KU, *KD, *KL, *KR;
