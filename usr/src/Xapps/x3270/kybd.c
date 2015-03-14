/*
 * Copyright 1989 by Georgia Tech Research Corporation, Atlanta, GA.
 * Copyright 1988, 1989 by Robert Viduya.
 * Copyright 1990 Jeff Sparkes.
 *
 *                         All Rights Reserved
 */

/*
 *	kybd.c
 *		This module handles the keyboard for the 3270 emulator.
 */
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <X11/Intrinsic.h>
#include "3270.h"
#include "3270_enc.h"

/*
 * The following table is used to translate ascii key codes to 3270
 * character generator symbol.  Note that this is not an ebcdic
 * translation.  See xlate.h for details.
 */
u_char	asc2cg[128] = {
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,	CG_CENT,
	CG_SOLIDBAR,	CG_NULLBLANK,	CG_NULLBLANK,	CG_NULLBLANK,
	CG_BLANK,	CG_EXCLAMATION,	CG_DQUOTE,	CG_NUMBER,
	CG_DOLLAR,	CG_PERCENT,	CG_AMPERSAND,	CG_SQUOTE,
	CG_LPAREN,	CG_RPAREN,	CG_ASTERISK,	CG_PLUS,
	CG_COMMA,	CG_MINUS,	CG_PERIOD,	CG_FSLASH,
	CG_ZERO,	CG_ONE,		CG_TWO,		CG_THREE,
	CG_FOUR,	CG_FIVE,	CG_SIX,		CG_SEVEN,
	CG_EIGHT,	CG_NINE,	CG_COLON,	CG_SEMICOLON,
	CG_LESS,	CG_EQUAL,	CG_GREATER,	CG_QUESTION,
	CG_AT,		CG_CA,		CG_CB,		CG_CC,
	CG_CD,		CG_CE,		CG_CF,		CG_CG,
	CG_CH,		CG_CI,		CG_CJ,		CG_CK,
	CG_CL,		CG_CM,		CG_CN,		CG_CO,
	CG_CP,		CG_CQ,		CG_CR,		CG_CS,
	CG_CT,		CG_CU,		CG_CV,		CG_CW,
	CG_CX,		CG_CY,		CG_CZ,		CG_LBRACKET,
	CG_BSLASH,	CG_RBRACKET,	CG_NOT,		CG_UNDERSCORE,
	CG_GRAVE,	CG_LA,		CG_LB,		CG_LC,
	CG_LD,		CG_LE,		CG_LF,		CG_LG,
	CG_LH,		CG_LI,		CG_LJ,		CG_LK,
	CG_LL,		CG_LM,		CG_LN,		CG_LO,
	CG_LP,		CG_LQ,		CG_LR,		CG_LS,
	CG_LT,		CG_LU,		CG_LV,		CG_LW,
	CG_LX,		CG_LY,		CG_LZ,		CG_LBRACE,
	CG_BROKENBAR,	CG_RBRACE,	CG_TILDE,	CG_NULLBLANK
};

u_char	cg2asc[256] = {
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	'>',	'<',	'[',	']',	')',	'(',	'}',	'{',
	' ',	'=',	'\'',	'"',	'/',	'\\',	'|',	'|',
	'?',	'!',	'$',	'c',	'L',	'Y',	'P',	'o',
	'0',	'1',	'2',	'3',	'4',	'5',	'6',	'7',
	'8',	'9',	'B',	'S',	'#',	'@',	'%',	'_',
	'&',	'-',	'.',	',',	':',	'+',	'^',	'~',
	'*',	'^',	'^',	'~',	'~',	'`',	'\'',	',',
	'a',	'e',	'i',	'o',	'u',	'a',	'o',	'y',
	'a',	'e',	'e',	'i',	'o',	'u',	'u',	'c',
	'a',	'e',	'i',	'o',	'u',	'a',	'e',	'i',
	'o',	'u',	'a',	'e',	'i',	'o',	'u',	'n',
	'A',	'E',	'I',	'O',	'U',	'A',	'O',	'Y',
	'A',	'E',	'E',	'I',	'O',	'U',	'Y',	'C',
	'A',	'E',	'I',	'O',	'U',	'A',	'E',	'I',
	'O',	'U',	'A',	'E',	'I',	'O',	'U',	'N',
	'a',	'b',	'c',	'd',	'e',	'f',	'g',	'h',
	'i',	'j',	'k',	'l',	'm',	'n',	'o',	'p',
	'q',	'r',	's',	't',	'u',	'v',	'w',	'x',
	'y',	'z',	'a',	'0',	'a',	'c',	';',	'*',
	'A',	'B',	'C',	'D',	'E',	'F',	'G',	'H',
	'I',	'J',	'K',	'L',	'M',	'N',	'O',	'P',
	'Q',	'R',	'S',	'T',	'U',	'V',	'W',	'X',
	'Y',	'Z',	'A',	'0',	'A',	'C',	';',	'*',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
	' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
};

Bool		kybdlock = FALSE,	/* kybd locked */
		insert = FALSE;		/* insert mode */
u_char		aid = AID_NO;		/* current attention ID */

extern u_char		*screen_buf;
extern Bool		screen_dirty;
extern u_char		screen_get();
extern u_char		*get_field_attribute();
extern int		cursor_addr, buffer_addr;
extern Bool		formatted, cursor_alt, mono_case;
extern int		char_width, char_height, char_base;
extern int		ROWS, COLS;
extern	redraw();

/*
 * Toggle insert mode.
 */
insert_mode (on)
	Bool on;
{
    if (on) {
	insert = TRUE;
	status_disp (51, CG_INSERT);
    }
    else {
	insert = FALSE;
	status_disp (51, CG_BLANK);
    }
}


shift_on()
{
    status_disp (41, CG_UPSHIFT);
}

shift_off()
{
    status_disp (41, CG_BLANK);
}


/*
 * Handle an AID (Attention IDentifier) key.  This is the common stuff that
 * gets executed for all AID keys (PFs, PAs, Clear and etc).
 */
key_AID (aid_code)
int	aid_code;
{
    status_disp (1, CG_BLANK);
    status_disp (8, CG_LOCK);
    status_disp (9, CG_BLANK);
    status_disp (10, CG_CLOCKLEFT);
    status_disp (11, CG_CLOCKRIGHT);
    insert_mode (FALSE);
    kybdlock = TRUE;
    aid = aid_code;
    do_read_modified ();
    status_disp (1, CG_UNDERA);
}


/*
~ * Handle an ordinary displayable character key.  Lots of stuff to handle
 * insert-mode, protected fields and etc.
 */
Bool
key_Character (cgcode)
int	cgcode;
{
    register int	baddr, end_baddr, t_baddr;
    register u_char	*fa;

    if (kybdlock)
	return (FALSE);
    baddr = cursor_addr;
    fa = get_field_attribute (baddr);
    if (IS_FA (screen_buf[baddr]) || FA_IS_PROTECTED (*fa)) {
	status_disp (8, CG_LOCK);
	status_disp (9, CG_BLANK);
	status_disp (10, CG_LEFTARROW);
	status_disp (11, CG_HUMAN);
	status_disp (12, CG_RIGHTARROW);
	kybdlock = TRUE;
	return (FALSE);
    }
    else {
	if (FA_IS_NUMERIC (*fa)
	&&  !((cgcode >= CG_ZERO && cgcode <= CG_NINE) || cgcode == CG_MINUS || cgcode == CG_PERIOD)) {
	    status_disp (8, CG_LOCK);
	    status_disp (9, CG_BLANK);
	    status_disp (10, CG_HUMAN);
	    status_disp (11, CG_CN);
	    status_disp (12, CG_CU);
	    status_disp (13, CG_CM);
	    kybdlock = TRUE;
	    return (FALSE);
	}
	else {
	    if (insert && screen_buf[baddr]) {
		/* find next null or next fa */
		end_baddr = baddr;
		do {
		    INC_BA (end_baddr);
		    if (screen_buf[end_baddr] == CG_NULLBLANK
		    ||  IS_FA (screen_buf[end_baddr]))
			break;
		} while (end_baddr != baddr);
		if (screen_buf[end_baddr] != CG_NULLBLANK) {
		    status_disp (8, CG_LOCK);
		    status_disp (9, CG_BLANK);
		    status_disp (10, CG_HUMAN);
		    status_disp (11, CG_GREATER);
		    kybdlock = TRUE;
		    return (FALSE);
		}
		else {
		    if (end_baddr > baddr) {
			bcopy ((char *) &screen_buf[baddr], (char *) &screen_buf[baddr+1], end_baddr - baddr);
		    } else {
			bcopy ((char *) &screen_buf[0], (char *) &screen_buf[1], end_baddr);
			screen_buf[0] = screen_buf[(ROWS * COLS) - 1];
			bcopy ((char *) &screen_buf[baddr], (char *) &screen_buf[baddr+1], ((ROWS * COLS) - 1) - baddr);
		    }
		    screen_add(baddr, cgcode);
		}
	    }
	    else {
	        screen_add(baddr, cgcode);
	    }
	    INC_BA (baddr);
	    if (IS_FA (screen_buf[baddr])) {
		if (FA_IS_NUMERIC (screen_buf[baddr])) {
		    if (FA_IS_PROTECTED (screen_buf[baddr])) {
			/* skip to next unprotected */
			while (TRUE) {
			    INC_BA (baddr);
			    if (IS_FA (screen_buf[baddr])
			    &&  !FA_IS_PROTECTED (screen_buf[baddr]))
				break;
			}
			INC_BA (baddr);
		    }
		    else
			INC_BA (baddr);
		}
		else
		    INC_BA (baddr);
	    }
	    cursor_move (baddr);
	    /* cursor_on (); */
	    *fa |= FA_MODIFY;
	}
    }
    return (TRUE);
}


/*
 * Toggle underline/block cursor.
 */
key_AltCr ()
{
    alt_cursor ((Bool) (!cursor_alt));
}


/*
 * Toggle blink/no-blink cursor.
 */
key_CursorBlink ()
{
#if 0
    blink_cursor ((Bool) (!cursor_blink));
#endif
}


/*
 * Toggle mono-/dual-case operation.
 */
key_MonoCase ()

{
    change_case ((Bool) (!mono_case));
    redraw(0, 0, 0, 0);
}


/*
 * Tab forward to next field.
 */
key_FTab ()
{
    register int	baddr, nbaddr, count;

    if (kybdlock)
	return;
    nbaddr = cursor_addr;
    if (!IS_FA(screen_buf[nbaddr]))
        INC_BA (nbaddr);
    while (TRUE) {
	baddr = nbaddr;
	INC_BA (nbaddr);
	if (IS_FA (screen_buf[baddr])
	&&  !FA_IS_PROTECTED (screen_buf[baddr])
	&&  !IS_FA (screen_buf[nbaddr]))
	    break;
	if (baddr == cursor_addr && count == 0) {
	    cursor_move (0);
	    return;
	}
	count++;
    }
    INC_BA (baddr);
    cursor_move (baddr);
}


/*
 * Tab backward to previous field.
 */
key_BTab ()
{
    register int	baddr, nbaddr;
    int			sbaddr;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    DEC_BA (baddr);
    if (IS_FA (screen_buf[baddr]))	/* at bof */
	DEC_BA (baddr);
    sbaddr = baddr;
    while (TRUE) {
	nbaddr = baddr;
	INC_BA (nbaddr);
	if (IS_FA (screen_buf[baddr])
	&&  !FA_IS_PROTECTED (screen_buf[baddr])
	&&  !IS_FA (screen_buf[nbaddr]))
	    break;
	DEC_BA (baddr);
	if (baddr == sbaddr) {
	    cursor_move (0);
	    return;
	}
    }
    INC_BA (baddr);
    cursor_move (baddr);
}


/*
 * Reset keyboard lock.
 */
key_Reset ()
{
    register int	i;

    kybdlock = FALSE;
    insert_mode (FALSE);
    for (i = 0; i < 9; i++)
	status_disp (i + 8, CG_BLANK);
}


/*
 * Move to first unprotected field on screen.
 */
key_Home ()
{
    register int	baddr, nbaddr;
    register u_char	*fa;

    if (kybdlock)
	return;
    fa = get_field_attribute ((ROWS*COLS)-1);
    if (!FA_IS_PROTECTED (*fa))
	cursor_move (0);
    else {
	nbaddr = 1;		/* start at 2nd col, 1st col is fa */
	while (TRUE) {
	    baddr = nbaddr;
	    INC_BA (nbaddr);
	    if (IS_FA (screen_buf[baddr])
	    &&  !FA_IS_PROTECTED (screen_buf[baddr])
	    &&  !IS_FA (screen_buf[nbaddr]))
		break;
	    if (baddr == 0) {
		cursor_move (0);
		return;
	    }
	}
	INC_BA (baddr);
	cursor_move (baddr);
    }
}


/*
 * Cursor left 1 position.
 */
key_Left ()
{
    register int	baddr;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    DEC_BA (baddr);
    cursor_move (baddr);
}


/*
 * Cursor right 1 position.
 */
key_Right ()
{
    register int	baddr;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    INC_BA (baddr);
    cursor_move (baddr);
}


/*
 * Cursor left 2 positions.
 */
key_Left2 ()
{
    register int	baddr;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    DEC_BA (baddr);
    DEC_BA (baddr);
    cursor_move (baddr);
}


/*
 * Cursor right 2 positions.
 */
key_Right2 ()
{
    register int	baddr;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    INC_BA (baddr);
    INC_BA (baddr);
    cursor_move (baddr);
}


/*
 * Cursor up 1 position.
 */
key_Up ()
{
    register int	baddr;

    if (kybdlock)
	return;
    baddr = cursor_addr - COLS;
    if (baddr < 0)
	baddr = (cursor_addr + (ROWS * COLS)) - COLS;
    cursor_move (baddr);
}


/*
 * Cursor down 1 position.
 */
key_Down ()
{
    register int	baddr;

    if (kybdlock)
	return;
    baddr = (cursor_addr + COLS) % (COLS * ROWS);
    cursor_move (baddr);
}


/*
 * Cursor to first field on next line or any lines after that.
 */
key_Newline ()
{
    register int	baddr;
    register u_char	*fa;

    if (kybdlock)
	return;
    baddr = (cursor_addr + COLS) % (COLS * ROWS);	/* down */
    baddr = (baddr / COLS) * COLS;			/* 1st col */
    fa = get_field_attribute (baddr);
    if (fa != (&screen_buf[baddr]) && !FA_IS_PROTECTED (*fa))
	cursor_move (baddr);
    else {	/* find next unprotected */
	if (fa == (&screen_buf[baddr]) && !FA_IS_PROTECTED (*fa)) {
	    INC_BA (baddr);
	}
	else {
	    while (TRUE) {
		INC_BA (baddr);
		if (IS_FA (screen_buf[baddr])
		&&  !FA_IS_PROTECTED (screen_buf[baddr]))
		    break;
		if (baddr == cursor_addr) {
		    cursor_move (0);
		    return;
		}
	    }
	    INC_BA (baddr);
	}
	cursor_move (baddr);
    }
}


/*
 * DUP key
 */
key_Dup ()
{
    register int	baddr, nbaddr;

    if (key_Character (CG_DUP)) {
	nbaddr = cursor_addr;
	INC_BA (nbaddr);
	while (TRUE) {
	    baddr = nbaddr;
	    INC_BA (nbaddr);
	    if (IS_FA (screen_buf[baddr])
	    &&  !FA_IS_PROTECTED (screen_buf[baddr])
	    &&  !IS_FA (screen_buf[nbaddr]))
		break;
	    if (baddr == cursor_addr) {
		cursor_move (0);
		return;
	    }
	}
	INC_BA (baddr);
	cursor_move (baddr);
    }
}


/*
 * FM key
 */
key_FieldMark ()
{
    (void) key_Character (CG_FM);
}


/*
 * Enter AID key.
 */
key_Enter ()
{
    if (kybdlock)
	return;
    key_AID (AID_ENTER);
}


/*
 * PA1 AID key
 */
key_PA1 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PA1);
}


/*
 * PA2 AID key
 */
key_PA2 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PA2);
}


/*
 * PA3 AID key
 */
key_PA3 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PA3);
}


/*
 * PF1 AID key
 */
key_PF1 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF1);
}


/*
 * PF2 AID key
 */
key_PF2 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF2);
}


/*
 * PF3 AID key
 */
key_PF3 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF3);
}


/*
 * PF4 AID key
 */
key_PF4 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF4);
}


/*
 * PF5 AID key
 */
key_PF5 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF5);
}


/*
 * PF6 AID key
 */
key_PF6 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF6);
}


/*
 * PF7 AID key
 */
key_PF7 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF7);
}


/*
 * PF8 AID key
 */
key_PF8 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF8);
}


/*
 * PF9 AID key
 */
key_PF9 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF9);
}


/*
 * PF10 AID key
 */
key_PF10 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF10);
}


/*
 * PF11 AID key
 */
key_PF11 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF11);
}


/*
 * PF12 AID key
 */
key_PF12 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF12);
}


/*
 * PF13 AID key
 */
key_PF13 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF13);
}


/*
 * PF14 AID key
 */
key_PF14 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF14);
}


/*
 * PF15 AID key
 */
key_PF15 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF15);
}


/*
 * PF16 AID key
 */
key_PF16 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF16);
}


/*
 * PF17 AID key
 */
key_PF17 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF17);
}


/*
 * PF18 AID key
 */
key_PF18 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF18);
}


/*
 * PF19 AID key
 */
key_PF19 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF19);
}


/*
 * PF20 AID key
 */
key_PF20 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF20);
}


/*
 * PF21 AID key
 */
key_PF21 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF21);
}


/*
 * PF22 AID key
 */
key_PF22 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF22);
}


/*
 * PF23 AID key
 */
key_PF23 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF23);
}


/*
 * PF24 AID key
 */
key_PF24 ()
{
    if (kybdlock)
	return;
    key_AID (AID_PF24);
}


/*
 * System Request AID key
 */
key_SysReq ()
{
    if (kybdlock)
	return;
    key_AID (AID_SYSREQ);
}


/*
 * Clear AID key
 */
key_Clear ()
{
    extern Display *display;
    extern Window w;

    if (kybdlock)
	return;
    buffer_addr = 0;
    /* cursor_off (); */
    clear_text();
    cursor_move (0);
    /* cursor_on (); */
    key_AID (AID_CLEAR);
}


/*
 * Cursor Select key (light pen simulator).
 */
key_CursorSelect ()
{
    register u_char	*fa, *sel;

    if (kybdlock)
	return;
    fa = get_field_attribute (cursor_addr);
    if (!FA_IS_SELECTABLE (*fa)) {
	status_disp (8, CG_LOCK);
	status_disp (9, CG_BLANK);
	status_disp (10, CG_LEFTARROW);
	status_disp (11, CG_HUMAN);
	status_disp (12, CG_RIGHTARROW);
	kybdlock = TRUE;
    }
    else {
	sel = fa + 1;
	switch (*sel) {
	    case CG_GREATER:		/* > */
		screen_add(sel, CG_QUESTION); /* change to ? */
		*fa &= ~FA_MODIFY;	/* clear mdt */
		break;
	    case CG_QUESTION:		/* ? */
		screen_add(sel,CG_GREATER);	/* change to > */
		*fa |= FA_MODIFY;	/* set mdt */
		break;
	    case CG_BLANK:		/* space */
	    case CG_NULLBLANK:		/* null */
		key_AID (AID_SELECT);
		break;
	    case CG_AMPERSAND:		/* & */
		key_AID (AID_ENTER);
		break;
	    default:
		status_disp (8, CG_LOCK);
		status_disp (9, CG_BLANK);
		status_disp (10, CG_LEFTARROW);
		status_disp (11, CG_HUMAN);
		status_disp (12, CG_RIGHTARROW);
		kybdlock = TRUE;
	}
    }
}


/*
 * Erase End Of Field Key.
 */
key_EraseEOF ()
{
    register int	baddr;
    register u_char	*fa;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    fa = get_field_attribute (baddr);
    if (FA_IS_PROTECTED (*fa) || IS_FA (screen_buf[baddr])) {
	status_disp (8, CG_LOCK);
	status_disp (9, CG_BLANK);
	status_disp (10, CG_LEFTARROW);
	status_disp (11, CG_HUMAN);
	status_disp (12, CG_RIGHTARROW);
	kybdlock = TRUE;
    }
    else {
	if (formatted) {	/* erase to next field attribute */
	    /* cursor_off (); */
	    do {
		screen_add(baddr, CG_NULLBLANK);
		INC_BA (baddr);
	    } while (!IS_FA (screen_buf[baddr]));
	    *fa |= FA_MODIFY;
	    /* cursor_on (); */
	}
	else {	/* erase to end of screen */
	    /* cursor_off (); */
	    do {
		screen_add(baddr, CG_NULLBLANK);
		INC_BA (baddr);
	    } while (baddr != 0);
	    /* cursor_on (); */
	}
    }
}


/*
 * Erase all Input Key.
 */
key_EraseInput ()
{
    register int	baddr, sbaddr;
    u_char		fa;
    Bool		f;
    extern 		Display *display;
    extern 		Window w;

    if (kybdlock)
	return;
    if (formatted) {
	/* find first field attribute */
	baddr = 0;
	do {
	    if (IS_FA (screen_buf[baddr]))
		break;
	    INC_BA (baddr);
	} while (baddr != 0);
	sbaddr = baddr;
	f = FALSE;
	do {
	    fa = screen_buf[baddr];
	    if (!FA_IS_PROTECTED (fa)) {
		screen_buf[baddr] &= ~FA_MODIFY;
		do {
		    INC_BA (baddr);
		    if (!f) {
			cursor_move (baddr);
			f = TRUE;
		    }
		    if (!IS_FA (screen_buf[baddr])) {
			screen_add(baddr, CG_NULLBLANK);
		    }
		}
		while (!IS_FA (screen_buf[baddr]));
	    }
	    else {	/* skip protected */
		do {
		    INC_BA (baddr);
		} while (!IS_FA (screen_buf[baddr]));
	    }
	} while (baddr != sbaddr);
	if (!f)
	    cursor_move (0);
    }
    else {
	clear_text();
	cursor_move (0);
    }
}


/*
 * Delete char key.
 */
key_Delete ()
{
    register int	baddr, end_baddr, t_baddr;
    register u_char	*fa;

    if (kybdlock)
	return;
    baddr = cursor_addr;
    fa = get_field_attribute (baddr);
    if (FA_IS_PROTECTED (*fa) || IS_FA (screen_buf[baddr])) {
	status_disp (8, CG_LOCK);
	status_disp (9, CG_BLANK);
	status_disp (10, CG_LEFTARROW);
	status_disp (11, CG_HUMAN);
	status_disp (12, CG_RIGHTARROW);
	kybdlock = TRUE;
    }
    else {
	/* find next fa */
	end_baddr = baddr;
	do {
	    INC_BA (end_baddr);
	    if (IS_FA (screen_buf[end_baddr]))
		break;
	} while (end_baddr != baddr);
	DEC_BA (end_baddr);
	if (end_baddr > baddr) {
	    bcopy ((char *) &screen_buf[baddr+1], (char *) &screen_buf[baddr], end_baddr - baddr);
	} else {
	    bcopy ((char *) &screen_buf[baddr+1], (char *) &screen_buf[baddr], ((ROWS * COLS) - 1) - baddr);
	    screen_add((ROWS * COLS) - 1, screen_buf[0]);
	    bcopy ((char *) &screen_buf[1], (char *) &screen_buf[0], end_baddr);
	}
	screen_add(end_baddr, CG_NULLBLANK);
	screen_dirty = TRUE;
    }
}


/*
 * Set insert mode key.
 */
key_Insert ()
{
    if (kybdlock)
	return;
    insert_mode (TRUE);
}


/*
 * Call by the toolkit for any key without special actions.
 */
Default (w, event, params, num_params)
	Widget w;
	XEvent *event;
	String *params;
	Cardinal num_params;
{
    register int	baddr;
    int			cgcode;
    char		buf[10];

    if (XLookupString(event, buf, 10, 0, 0) != 1) {
	/* fprintf(stderr, "unknown key seq %s\n", buf); */
	return;
    }
    key_Character(asc2cg[buf[0]]);
    return 0;
}

/*
 * Usually bound to left mouse button.
 */
movecursor(w, event, params, num_params)
	Widget w;
	XEvent *event;
	String *params;
	Cardinal num_params;
{
   register int baddr; 

#define event_x(event) event->xbutton.x
#define event_y(event) event->xbutton.y

    if (kybdlock)
        return 0;

    baddr = ROWCOL_TO_BA(Y_TO_ROW (event_y (event)), 
			X_TO_COL (event_x (event)));
    while (baddr >= (COLS * ROWS))
    baddr -= COLS;
    cursor_move (baddr);
}

#if 0
	case MS_MIDDLE:	/* middle mouse sets selections */
	    baddr = ROWCOL_TO_BA(Y_TO_ROW (event_y (event)), 
				 X_TO_COL (event_x (event)));
	    while (baddr >= (COLS * ROWS))
		baddr -= COLS;
	    set_seln (cursor_addr, baddr);
	    break;
	case MS_RIGHT:	/* right mouse pops up canvas menu */
	    if (event_is_down (event))
		menu_show (Key_menu, win, event, 0);
	    break;
	case LOC_RGNENTER:	/* mouse entered window */
	    if (seln_acquire (s_client, SELN_CARET) != SELN_CARET)
		fprintf (stderr, "can't acquire SELN_CARET!\n");
	    break;
#endif

void
string(w, event, params, nparams)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    int		i;
    char	c;
    char	*s;

    for (i=0; i<*nparams; i++) {
	for (s=params[i]; *s; s++) {
	    if ((s[0] == '0' || s[0] == '\\') && s[1] && s[1] == 'x') {
		/* Read hex value */
		if (s[2]) {
		    c = s[2];
		    if (s[3]) {
			c *= 16;
			c += s[3];
			s += 2;
		    } else {
			XtWarning("incomplete hex number in String action");
			s += 1;
		    }
		} else {
		    XtWarning("incomplete hex number in String action");
		    c = 0;
		}
		s += 2;
	    } else if (s[0] == '\\' && s[1]) {
		switch (s[1]) {
		case 'a':
		    XtWarning("bell unsupported in String action");
		    continue;
		case 'b':
		    key_Left();
		    continue;
		case 'f':
		    key_Clear();
		    continue;
		case 'n':
		    key_Enter();
		    continue;
		case 'r':
		    key_Newline();
		    continue;
		case 't':
		    key_FTab();
		    continue;
		case 'v':
		    XtWarning("vertical tab unsupported in String action");
		    continue;
		case '\\':
		    c = '\\';
		    break;
		case '?':
		    c = '\?';
		    break;
		case '\'':
		    c = '\'';
		    break;
		case '\"':
		    c = '\"';
		    break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		    XtWarning("octal escape not supported in String action");
		    c = 0;
		    break;
		default:
		    XtWarning("unknown escape in String action");
		    c = s[1];
		}
		s += 2;
	    } else {
	        c = asc2cg[*s];
	    }
            key_Character(c);
	}
    }
}

XtActionsRec actions[] = {
	{ "Redraw",	(XtActionProc)redraw },
	{ "Insert",	(XtActionProc)key_Insert },
	{ "Enter",	(XtActionProc)key_Enter },
	{ "AltCursor",  (XtActionProc)key_AltCr },
	{ "MonoCase",	(XtActionProc)key_MonoCase },
	{ "Tab",	(XtActionProc)key_FTab },
	{ "BackTab",	(XtActionProc)key_BTab },
	{ "Reset",	(XtActionProc)key_Reset },
	{ "Home",	(XtActionProc)key_Home },
	{ "Left",	(XtActionProc)key_Left },
	{ "Left2", 	(XtActionProc)key_Left2 },
	{ "Right",	(XtActionProc)key_Right },
	{ "Right2",	(XtActionProc)key_Right2 },
	{ "Up",		(XtActionProc)key_Up },
	{ "Down",	(XtActionProc)key_Down },
	{ "Newline",	(XtActionProc)key_Newline },
	{ "Dup",	(XtActionProc)key_Dup },
	{ "FieldMark",	(XtActionProc)key_FieldMark },
	{ "PA1",	(XtActionProc)key_PA1 },
	{ "PA2",	(XtActionProc)key_PA2 },
	{ "SysReq",	(XtActionProc)key_SysReq },
	{ "Clear",	(XtActionProc)key_Clear },
	{ "CursorSelect", (XtActionProc)key_CursorSelect },
	{ "EraseEOF",	(XtActionProc)key_EraseEOF },
	{ "EraseInput", (XtActionProc)key_EraseInput },
	{ "Delete", 	(XtActionProc)key_Delete },
	{ "Default",	(XtActionProc)Default },
	{ "MoveCursor", (XtActionProc)movecursor },
	{ "String",	(XtActionProc)string },
	{ "ShiftOn",	(XtActionProc)shift_on },
	{ "ShiftOff",	(XtActionProc)shift_off },
	{ "PF1",	(XtActionProc)key_PF1 },
	{ "PF2",	(XtActionProc)key_PF2 },
	{ "PF3",	(XtActionProc)key_PF3 },
	{ "PF4",	(XtActionProc)key_PF4 },
	{ "PF5",	(XtActionProc)key_PF5 },
	{ "PF6",	(XtActionProc)key_PF6 },
	{ "PF7",	(XtActionProc)key_PF7 },
	{ "PF8",	(XtActionProc)key_PF8 },
	{ "PF9",	(XtActionProc)key_PF9 },
	{ "PF10",	(XtActionProc)key_PF10 },
	{ "PF11",	(XtActionProc)key_PF11 },
	{ "PF12",	(XtActionProc)key_PF12 },
	{ "PF13",	(XtActionProc)key_PF13 },
	{ "PF14",	(XtActionProc)key_PF14 },
	{ "PF15",	(XtActionProc)key_PF15 },
	{ "PF16",	(XtActionProc)key_PF16 },
	{ "PF17",	(XtActionProc)key_PF17 },
	{ "PF18",	(XtActionProc)key_PF18 },
	{ "PF19",	(XtActionProc)key_PF19 },
	{ "PF20",	(XtActionProc)key_PF20 },
	{ "PF21",	(XtActionProc)key_PF21 },
	{ "PF22",	(XtActionProc)key_PF22 },
	{ "PF23",	(XtActionProc)key_PF23 },
	{ "PF24",	(XtActionProc)key_PF24 },
};

int actioncount = XtNumber(actions);
