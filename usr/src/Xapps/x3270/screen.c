/*;
 * Copyright 1989 by Georgia Tech Research Corporation, Atlanta, GA.
 * Copyright 1988, 1989 by Robert Viduya.
 * Copyright 1990 Jeff Sparkes.
 *
 *                         All Rights Reserved
 */

/*
 *	screen.c
 *		This module handles interpretation of the 3270 data stream
 *		and other screen management actions.
 */
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <stdio.h>
#include "3270.h"
#include "3270_enc.h"
#include "X.h"

/* ebcdic to 3270 character generator xlate table */

u_char	ebc2cg[256] = {
	CG_NULLBLANK,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_OVERBAR2,	CG_OVERBAR6,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_OVERBAR3,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_OVERBAR1,	CG_PERIOD,	CG_PERIOD,
	CG_DUP,		CG_PERIOD,	CG_FM,		CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,	CG_OVERBAR4,
	CG_BLANK,	CG_LBRACKET,	CG_RBRACKET,	CG_POUND,
	CG_YEN,		CG_PT,		CG_CURRENCY,	CG_SHARPS,
	CG_SECTION,	CG_OVERSCORE,	CG_CENT,	CG_PERIOD,
	CG_LESS,	CG_LPAREN,	CG_PLUS,	CG_SOLIDBAR,
	CG_AMPERSAND,	CG_DEGREE,	CG_BREVE,	CG_CIRCUMFLEX,
	CG_DIAERESIS,	CG_ACUTE,	CG_CEDILLA,	CG_LAACUTE1,
	CG_LEACUTE1,	CG_LIACUTE1,	CG_EXCLAMATION,	CG_DOLLAR,
	CG_ASTERISK,	CG_RPAREN,	CG_SEMICOLON,	CG_NOT,
	CG_MINUS,	CG_FSLASH,	CG_LOACUTE1,	CG_LUACUTE1,
	CG_LATILDE,	CG_LOTILDE,	CG_LYDIAERESIS,	CG_LAACUTE2,
	CG_LEACUTE2,	CG_LEGRAVE1,	CG_BROKENBAR,	CG_COMMA,
	CG_PERCENT,	CG_UNDERSCORE,	CG_GREATER,	CG_QUESTION,
	CG_LIACUTE2,	CG_LOACUTE2,	CG_LUACUTE2,	CG_LUDIAERESIS1,
	CG_LCCEDILLA1,	CG_LADIAERESIS,	CG_LEDIAERESIS,	CG_LIDIAERESIS,
	CG_LODIAERESIS,	CG_GRAVE,	CG_COLON,	CG_NUMBER,
	CG_AT,		CG_SQUOTE,	CG_EQUAL,	CG_DQUOTE,
	CG_LUDIAERESIS2,CG_LA,		CG_LB,		CG_LC,
	CG_LD,		CG_LE,		CG_LF,		CG_LG,
	CG_LH,		CG_LI,		CG_LACIRCUMFLEX,CG_LECIRCUMFLEX,
	CG_LICIRCUMFLEX,CG_LOCIRCUMFLEX,CG_LUCIRCUMFLEX,CG_LAGRAVE,
	CG_LEGRAVE2,	CG_LJ,		CG_LK,		CG_LL,
	CG_LM,		CG_LN,		CG_LO,		CG_LP,
	CG_LQ,		CG_LR,		CG_LIGRAVE,	CG_LOGRAVE,
	CG_LUGRAVE,	CG_LNTILDE,	CG_CAACUTE,	CG_CEACUTE,
	CG_CIACUTE,	CG_TILDE,	CG_LS,		CG_LT,
	CG_LU,		CG_LV,		CG_LW,		CG_LX,
	CG_LY,		CG_LZ,		CG_COACUTE,	CG_CUACUTE,
	CG_CATILDE,	CG_COTILDE,	CG_CY1,		CG_CA1,
	CG_CE1,		CG_CE2,		CG_CI1,		CG_CO1,
	CG_CU1,		CG_CY2,		CG_CC1,		CG_CADIAERESIS,
	CG_CEDIAERESIS,	CG_CIDIAERESIS,	CG_CODIAERESIS,	CG_CUDIAERESIS,
	CG_CACIRCUMFLEX,CG_CECIRCUMFLEX,CG_CICIRCUMFLEX,CG_COCIRCUMFLEX,
	CG_LBRACE,	CG_CA,		CG_CB,		CG_CC,
	CG_CD,		CG_CE,		CG_CF,		CG_CG,
	CG_CH,		CG_CI,		CG_CUCIRCUMFLEX,CG_CAGRAVE,
	CG_CEGRAVE,	CG_CIGRAVE,	CG_PERIOD,	CG_PERIOD,
	CG_RBRACE,	CG_CJ,		CG_CK,		CG_CL,
	CG_CM,		CG_CN,		CG_CO,		CG_CP,
	CG_CQ,		CG_CR,		CG_COGRAVE,	CG_CUGRAVE,
	CG_CNTILDE,	CG_PERIOD,	CG_PERIOD,	CG_PERIOD,
	CG_BSLASH,	CG_LAE,		CG_CS,		CG_CT,
	CG_CU,		CG_CV,		CG_CW,		CG_CX,
	CG_CY,		CG_CZ,		CG_SSLASH0,	CG_LADOT,
	CG_LCCEDILLA2,	CG_PERIOD,	CG_PERIOD,	CG_MINUS,
	CG_ZERO,	CG_ONE,		CG_TWO,		CG_THREE,
	CG_FOUR,	CG_FIVE,	CG_SIX,		CG_SEVEN,
	CG_EIGHT,	CG_NINE,	CG_CAE,		CG_BSLASH0,
	CG_CADOT,	CG_CCCEDILLA,	CG_MINUS,	CG_OVERBAR7
};

/* 3270 character generator to ebcdic xlate table */
/* generated programmatically from ebc2cg */

u_char	cg2ebc[256] = {
	0x00, 0x19, 0x0c, 0x15, 0x3f, 0x00, 0x0d, 0xff,	/* 0x00 */
	0x6e, 0x4c, 0x41, 0x42, 0x5d, 0x4d, 0xd0, 0xc0,
	0x40, 0x7e, 0x7d, 0x7f, 0x61, 0xe0, 0x4f, 0x6a,	/* 0x10 */
	0x6f, 0x5a, 0x5b, 0x4a, 0x43, 0x44, 0x45, 0x46,
	0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,	/* 0x20 */
	0xf8, 0xf9, 0x47, 0x48, 0x7b, 0x7c, 0x6c, 0x6d,
	0x50, 0x60, 0x4b, 0x6b, 0x7a, 0x4e, 0x5f, 0x49,	/* 0x30 */
	0x51, 0x52, 0x53, 0xa1, 0x54, 0x79, 0x55, 0x56,
	0x57, 0x58, 0x59, 0x62, 0x63, 0x64, 0x65, 0x66,	/* 0x40 */
	0x67, 0x68, 0x69, 0x70, 0x71, 0x72, 0x73, 0x74,
	0x75, 0x76, 0x77, 0x78, 0x80, 0x8a, 0x8b, 0x8c,	/* 0x50 */
	0x8d, 0x8e, 0x8f, 0x90, 0x9a, 0x9b, 0x9c, 0x9d,
	0x9e, 0x9f, 0xa0, 0xaa, 0xab, 0xac, 0xad, 0xae,	/* 0x60 */
	0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6,
	0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe,	/* 0x70 */
	0xbf, 0xca, 0xcb, 0xcc, 0xcd, 0xda, 0xdb, 0xdc,
	0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88,	/* 0x80 */
	0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
	0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,	/* 0x90 */
	0xa8, 0xa9, 0xe1, 0xea, 0xeb, 0xec, 0x1e, 0x1c,
	0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8,	/* 0xA0 */
	0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
	0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,	/* 0xB0 */
	0xe8, 0xe9, 0xfa, 0xfb, 0xfc, 0xfd, 0x5e, 0x5c,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,	/* 0xC0 */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,	/* 0xD0 */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,	/* 0xE0 */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,	/* 0xF0 */
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

/* code_table is used to translate buffer addresses to the 3270
 * datastream representation
 */
u_char	code_table[64] = {
    0x40, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
    0xC8, 0xC9, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
    0xD8, 0xD9, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
    0x60, 0x61, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
    0xE8, 0xE9, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
    0xF8, 0xF9, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
};

char defaultTranslations[] = "\
	<Expose>:		Redraw()\n\
	<Key>Return:		Enter()\n\
	<Key>Linefeed:		Newline()\n\
	<KeyPress>Caps_Lock:	ShiftOn()\n\
	<KeyRelease>Caps_Lock:	ShiftOff()\n\
	<KeyPress>Shift_L:	ShiftOn()\n\
	<KeyRelease>Shift_L:	ShiftOff()\n\
	<KeyPress>Shift_R:	ShiftOn()\n\
	<KeyRelease>Shift_R:	ShiftOff()\n\
	!Shift<Key>Tab:		BackTab()\n\
	<Key>Tab:		Tab()\n\
	<Key>Home:  		Home()\n\
	<Key>Left:		Left()\n\
	!Meta<Key>Left:		Left2()\n\
	<Key>Right: 		Right()\n\
	!Meta<Key>Right:	Right2()\n\
	<Key>Up:		Up()\n\
	<Key>Down:		Down()\n\
	<Key>Delete: 		Delete()\n\
	<Key>BackSpace: 	Left()\n\
	<Btn1Down>:		MoveCursor()\n\
	!Meta<Key>F1:		PF13()\n\
	!Meta<Key>F2:		PF14()\n\
	!Meta<Key>F3:		PF15()\n\
	!Meta<Key>F4:		PF16()\n\
	!Meta<Key>F5:		PF17()\n\
	!Meta<Key>F6:		PF18()\n\
	!Meta<Key>F7:		PF19()\n\
	!Meta<Key>F8:		PF20()\n\
	!Meta<Key>F9:		PF21()\n\
	!Meta<Key>F10:		PF22()\n\
	!Meta<Key>F11:		PF23()\n\
	!Meta<Key>F12:		PF24()\n\
	<Key>F1:		PF1()\n\
	<Key>F2:		PF2()\n\
	<Key>F3:		PF3()\n\
	<Key>F4:		PF4()\n\
	<Key>F5:		PF5()\n\
	<Key>F6:		PF6()\n\
	<Key>F7:		PF7()\n\
	<Key>F8:		PF8()\n\
	<Key>F9:		PF9()\n\
	<Key>F10:		PF10()\n\
	<Key>F11:		PF11()\n\
	<Key>F12:		PF12()\n\
	Meta<Key>c:		Clear()\n\
	Meta<Key>r:		Reset()\n\
	Meta<Key>1:		PA1()\n\
	Meta<Key>2:		PA2()\n\
	Meta<Key>i:		Insert()\n\
	Meta<Key>d:		Delete()\n\
	Meta<Key>h:		Home()\n\
	Meta<Key>l:		Redraw()\n\
	Ctrl<Key>h:		Left()\n\
	:<Key>:			Default()";

extern Display		*display;
extern Widget		toplevel;
extern XtAppContext	appcontext;
extern int		foreground, background;	
extern int		ROWS, COLS;
Widget 			win;
Window			w;
extern Font		ibmfont;
extern XFontStruct	*ibmfontinfo;
GC			gc[4], invgc[4];
int			cursor_addr, buffer_addr;
Bool			cursor_displayed = FALSE;
Bool			cursor_alt = FALSE;
Bool			mono_case = FALSE;

/* the following are set from values in the 3270 font */
int			char_width, char_height, char_base;

u_char			*screen_buf;
u_char			*status_buf;
u_char			*update_buf;
static u_char		*blanks;
Bool			formatted = FALSE;	/* set in screen_disp */
Bool			status_dirty = FALSE;
Bool			screen_dirty = FALSE;

extern u_char	obuf[], *obptr;
extern u_char	aid;
extern u_char	*get_field_attribute();


/*
 * Initialize the screen canvas.  Should only be called once.
 */
screen_init (kbdtrans, usertrans)
	char *kbdtrans, *usertrans;
{
    Arg	args[10];
    int i = 0;
    XtTranslations trans;

    screen_buf = (u_char *)XtCalloc(sizeof(u_char), ROWS * COLS);
    update_buf = (u_char *)XtCalloc(sizeof(u_char), ROWS * COLS);
    status_buf = (u_char *)XtCalloc(sizeof(u_char), COLS);
    blanks = (u_char *)XtCalloc(sizeof(u_char), COLS);

    char_width = CHAR_WIDTH;
    char_height = CHAR_HEIGHT;
    char_base = CHAR_BASE;

    i = 0;
    XtSetArg(args[i], XtNwidth, COL_TO_X(COLS)); i++;
    XtSetArg(args[i], XtNheight, ROW_TO_Y(ROWS)+8); i++;
    XtSetArg(args[i], XtNbaseWidth, COL_TO_X(COLS)); i++;
    XtSetArg(args[i], XtNbaseHeight, ROW_TO_Y(ROWS)+8); i++;
    XtSetArg(args[i], XtNminWidth, COL_TO_X(COLS)); i++;
    XtSetArg(args[i], XtNminHeight, ROW_TO_Y(ROWS)+8); i++;
    XtSetArg(args[i], XtNmaxWidth, COL_TO_X(COLS)); i++;
    XtSetArg(args[i], XtNmaxHeight, ROW_TO_Y(ROWS)+8); i++;
    XtSetValues(toplevel, args, i);

    i = 0;
    XtSetArg(args[i], XtNwidth, COL_TO_X(COLS)); i++;
    XtSetArg(args[i], XtNheight, ROW_TO_Y(ROWS)+8); i++;
    win = XtCreateManagedWidget("screen", widgetClass, toplevel, args, i);


    if (appres.mono == False && DefaultDepthOfScreen(XtScreen(toplevel)) > 1) {
	make_gc_pair(FA_INT_NORM_NSEL, appres.normal, appres.colorbg);
	make_gc_pair(FA_INT_NORM_SEL,  appres.select, appres.colorbg);
	make_gc_pair(FA_INT_HIGH_SEL,  appres.bold,   appres.colorbg);
	make_gc_pair(FA_INT_ZERO_NSEL, appres.colorbg, appres.colorbg);
        XtSetArg(args[i], XtNbackground, appres.colorbg); i++;
    } else {
	appres.mono = True;
	make_gc_pair(FA_INT_NORM_NSEL, appres.foreground, appres.background);
	make_gc_pair(FA_INT_NORM_SEL,  appres.foreground, appres.background);
	make_gc_pair(FA_INT_HIGH_SEL,  appres.foreground, appres.background);
	make_gc_pair(FA_INT_ZERO_NSEL, appres.background, appres.background);
        XtSetArg(args[i], XtNbackground, appres.background); i++;
    }
    XtSetValues(win, args, i);
    trans = XtParseTranslationTable(defaultTranslations);
    XtOverrideTranslations(win, trans);
    if (kbdtrans && *kbdtrans) {
	trans = XtParseTranslationTable(kbdtrans);
        XtOverrideTranslations(win, trans);
    }
    if (usertrans && *usertrans) {
	trans = XtParseTranslationTable(usertrans);
        XtOverrideTranslations(win, trans);
    }
    XtRealizeWidget(toplevel);
    w = XtWindow(win);

    cursor_addr = 0;
    buffer_addr = 0;
    status_disp (0, CG_BOX4);
    status_disp (1, CG_UNDERA);
    status_disp (2, CG_BOXSOLID);
    clear_text();
    cursor_on ();
}


/*
 * Update the status line by displaying "symbol" at column "col".
 */
status_disp (col, symbol)
int	col, symbol;
{
    status_buf[col] = symbol;
    status_dirty = TRUE;
    /*
    XDrawImageString(display,w,gc[0], COL_TO_X(col), ROW_TO_Y(ROWS)+5, 
	&(status_buf[col]), 1);
    */
}


/*
 * Make the cursor disappear.
 */
cursor_off ()
{
    int  color;
    u_char *fa = get_field_attribute(cursor_addr);

    color = fa_color(*fa);
    if (color == FA_INT_ZERO_NSEL)
	color = FA_INT_NORM_SEL;
    if (cursor_displayed) {
	cursor_displayed = FALSE;
	if (!cursor_alt)
	    put_cursor(cursor_addr, color, 0);
	else
	    put_altcursor(cursor_addr, gc[color], 0);
    }
}


/*
 * Make the cursor visible.
 */
cursor_on ()
{
    int  color;
    u_char *fa = get_field_attribute(cursor_addr);

    color = fa_color(*fa);
    if (color == FA_INT_ZERO_NSEL)
	color = FA_INT_NORM_SEL;
    if (!cursor_displayed) {
	cursor_displayed = TRUE;
	if (!cursor_alt)
	    put_cursor(cursor_addr, color, 1);
	else
	    put_altcursor(cursor_addr, gc[color], 1);
    }
}


/*
 * Toggle the cursor (block/underline).
 */
alt_cursor (alt)
Bool	alt;
{
    if (alt != cursor_alt) {
	cursor_off ();
	cursor_alt = alt;
	cursor_on ();
    }
}


/*
 * Move the cursor to the specified buffer address.
 */
cursor_move (baddr)
int	baddr;
{
    int	cflag = cursor_displayed;

    if (cflag) {
	cursor_off();
    }
    cursor_addr = baddr;
    if (cflag) {
	cursor_on();
    }
}


/*
 * Redraw the entire screen.
 */
redraw(wid, event, params, num_params)
	Widget	wid;
	XEvent	*event;
	String	*params;
	Cardinal num_params;
{
    int 	i;
    int		x, y, w, h;
    int		start, end;

    /* Only redraw as necessary for an expose event */
    if (event && event->type == Expose) {
	x = event->xexpose.x;
	y = event->xexpose.y;
	w = event->xexpose.width;
	h = event->xexpose.height;
	/*
	printf("expose %d: %d,%d,%dx%d\n", event->xexpose.count, x, y, w, h);
	*/
	start = Y_TO_ROW(y);
	start = start == 0 ? 0 : start - 1;
	end = Y_TO_ROW(y+h);
	end = end >= ROWS-1 ? ROWS-1 : end + 1;
	for (i=start; i<end; i++) {
	    int a, b;
	    a = ROWCOL_TO_BA(i, X_TO_COL(x));
	    b = X_TO_COL(w+char_width);
	    bzero(&(update_buf[a]), b);
	}
    } else {
	bzero(update_buf, ROWS*COLS);
    }
    screen_dirty = TRUE;
    status_dirty = TRUE;
    screen_disp ();
}

/*
 * Redraw the dirty parts of the screen.
 */
screen_disp ()
{
    int 	i;

    if (status_dirty) {
        XDrawImageString(display,w,gc[0],0,ROW_TO_Y(ROWS)+5, status_buf, COLS);
        status_dirty = FALSE;
    }
    if (screen_dirty == FALSE)
	return;
    cursor_off ();
    for (i=0; i< ROWS; i++) {
        do_line(i*COLS);
    }
    cursor_on ();
    /* XXXX */
    XDrawLine(display, w, gc[0], 0, ROW_TO_Y(ROWS-1)+3, COL_TO_X(COLS), 
	ROW_TO_Y(ROWS-1)+3);
    screen_dirty = FALSE;
}


/*
 * Set the formatted screen flag.  A formatted screen is a screen that
 * has at least one field somewhere on it.
 */
set_formatted ()
{
    register int	baddr;

    formatted = FALSE;
    baddr = 0;
    do {
	if (IS_FA (screen_buf[baddr])) {
	    formatted = TRUE;
	    break;
	}
	INC_BA (baddr);
    } while (baddr != 0);
}


/*
 * Find the field attribute for the given buffer address.
 */
u_char	*
get_field_attribute (baddr)
register int	baddr;
{
    static u_char	fake_fa;
    int			sbaddr;

    sbaddr = baddr;
    do {
	if (IS_FA (screen_buf[baddr]))
	    return (&(screen_buf[baddr]));
	DEC_BA (baddr);
    } while (baddr < sbaddr);
    fake_fa = 0xE0;
    return (&fake_fa);
}


/*
 * Toggle mono-/dual-case mode.
 */
change_case (mono)
Bool	mono;
{
    if (mono_case != mono) {
	mono_case = mono;
	screen_disp ();
    }
}


/*
 * Interpret an incoming 3270 datastream.
 */
net_process (buf, buflen)
u_char	*buf;
int	buflen;
{
    switch (buf[0]) {	/* 3270 command */
	case CMD_EAU:	/* erase all unprotected */
	    do_erase_all_unprotected ();
	    break;
	case CMD_EWA:	/* erase/write alternate */
	    /* on 3278-2, same as erase/write.  fallthrough */
	case CMD_EW:	/* erase/write */
	    buffer_addr = 0;
	    /* cursor_off (); */
	    clear_text();
	    cursor_move (0);
	    /* fallthrough into write */
	case CMD_W:	/* write */
	    do_write (buf, buflen);
	    break;
	case CMD_RB:	/* read buffer */
	    do_read_buffer ();
	    break;
	case CMD_RM:	/* read modifed */
	    do_read_modified ();
	    break;
	case CMD_NOP:	/* no-op */
	    break;
	default:
	    /* unknown 3270 command */
	    exit (1);
    }
}


/*
 * Process a 3270 Read-Modified command and transmit the data back to the
 * host.
 */
do_read_modified ()
{
    register int	baddr, sbaddr;

    obptr = &obuf[0];
    if (aid != AID_PA1 && aid != AID_PA2
    &&  aid != AID_PA3 && aid != AID_CLEAR) {
	if (aid == AID_SYSREQ) {
	    *obptr++ = 0x01;	/* soh */
	    *obptr++ = 0x5B;	/*  %  */
	    *obptr++ = 0x61;	/*  /  */
	    *obptr++ = 0x02;	/* stx */
	}
	else {
	    *obptr++ = aid;
	    *obptr++ = code_table[(cursor_addr >> 6) & 0x3F];
	    *obptr++ = code_table[cursor_addr & 0x3F];
	}
	baddr = 0;
	if (formatted) {
	    /* find first field attribute */
	    do {
		if (IS_FA (screen_buf[baddr]))
		    break;
		INC_BA (baddr);
	    } while (baddr != 0);
	    sbaddr = baddr;
	    do {
		if (FA_IS_MODIFIED (screen_buf[baddr])) {
		    INC_BA (baddr);
		    *obptr++ = ORDER_SBA;
		    *obptr++ = code_table[(baddr >> 6) & 0x3F];
		    *obptr++ = code_table[baddr & 0x3F];
		    do {
			if (screen_buf[baddr])
			    *obptr++ = cg2ebc[screen_buf[baddr]];
			INC_BA (baddr);
		    } while (!IS_FA (screen_buf[baddr]));
		}
		else {	/* not modified - skip */
		    do {
			INC_BA (baddr);
		    } while (!IS_FA (screen_buf[baddr]));
		}
	    } while (baddr != sbaddr);
	}
	else {
	    do {
		if (screen_buf[baddr])
		    *obptr++ = cg2ebc[screen_buf[baddr]];
		INC_BA (baddr);
	    } while (baddr != 0);
	}
    }
    else
	*obptr++ = aid;
    net_output (obuf, obptr - obuf);
}


/*
 * Process a 3270 Read-Buffer command and transmit the data back to the
 * host.
 */
do_read_buffer ()
{
    register int	baddr;
    u_char		fa;

    obptr = &obuf[0];
    *obptr++ = aid;
    *obptr++ = code_table[(cursor_addr >> 6) & 0x3F];
    *obptr++ = code_table[cursor_addr & 0x3F];
    baddr = 0;
    do {
	if (IS_FA (screen_buf[baddr])) {
	    *obptr++ = ORDER_SF;
	    fa = 0x00;
	    if (FA_IS_PROTECTED (screen_buf[baddr]))
		fa |= 0x20;
	    if (FA_IS_NUMERIC (screen_buf[baddr]))
		fa |= 0x10;
	    if (FA_IS_MODIFIED (screen_buf[baddr]))
		fa |= 0x01;
	    fa |= ((screen_buf[baddr] | FA_INTENSITY) << 2);
	    *obptr++ = fa;
	}
	else
	    *obptr++ = cg2ebc[screen_buf[baddr]];
	INC_BA (baddr);
    } while (baddr != 0);
    net_output (obuf, obptr - obuf);
}


/*
 * Process a 3270 Erase All Unprotected command.
 */
do_erase_all_unprotected ()
{
    register int	baddr, sbaddr;
    u_char		fa;
    Bool		f;

    screen_dirty = TRUE;
    /* cursor_off (); */
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
		} while (!IS_FA (screen_buf[baddr]));
	    }
	    else {
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
	buffer_addr = 0;
	cursor_move (0);
    }
    /* cursor_on (); */
    aid = AID_NO;
    key_Reset ();
}


/*
 * Process a 3270 Write command.
 */
do_write (buf, buflen)
u_char	buf[];
int	buflen;
{
    register u_char	*cp;
    register int	baddr;
    u_char		*current_fa;
    Bool		last_cmd;
    Bool		wcc_keyboard_restore, wcc_sound_alarm;

    buffer_addr = cursor_addr;
    wcc_sound_alarm = WCC_SOUND_ALARM (buf[1]);
    wcc_keyboard_restore = WCC_KEYBOARD_RESTORE (buf[1]);
    status_disp (8, CG_LOCK);
    status_disp (9, CG_BLANK);
    status_disp (10, CG_CS);
    status_disp (11, CG_CY);
    status_disp (12, CG_CS);
    status_disp (13, CG_CT);
    status_disp (14, CG_CE);
    status_disp (15, CG_CM);
    /* This may take a while, so update status line. */
    XDrawImageString(display,w,gc[0],0,ROW_TO_Y(ROWS)+5, status_buf, COLS);
    XSync(display, 0);

    if (WCC_RESET_MDT (buf[1])) {
	baddr = 0;
        screen_dirty = TRUE;
	do {
	    if (IS_FA (screen_buf[baddr])) {
		screen_buf[baddr] &= ~FA_MODIFY;
	    }
	    INC_BA (baddr);
	} while (baddr != 0);
    }
    last_cmd = TRUE;
    /* cursor_off (); */
    current_fa = get_field_attribute (buffer_addr);
    for (cp = &buf[2]; cp < (buf + buflen); cp++) {
	switch (*cp) {
	    case ORDER_GE:	/* graphic escape - ignore */
		last_cmd = TRUE;
		break;
	    case ORDER_SF:	/* start field */
		cp++;		/* skip field attribute */
		screen_dirty = TRUE;
		screen_buf[buffer_addr] = FA_BASE;
		if (*cp & 0x20)
		    screen_buf[buffer_addr] |= FA_PROTECT;
		if (*cp & 0x10)
		    screen_buf[buffer_addr] |= FA_NUMERIC;
		if (*cp & 0x01)
		    screen_buf[buffer_addr] |= FA_MODIFY;
		screen_buf[buffer_addr] |= (*cp >> 2) & FA_INTENSITY;
		current_fa = &(screen_buf[buffer_addr]);
		formatted = TRUE;
		INC_BA (buffer_addr);
		last_cmd = TRUE;
		break;
	    case ORDER_SBA:	/* set buffer address */
		cp += 2;	/* skip buffer address */
		if ((*(cp-1) & 0xC0) == 0x00) /* 14-bit binary */
		    buffer_addr = ((*(cp-1) & 0x3F) << 8) | *cp;
		else	/* 12-bit coded */
		    buffer_addr = ((*(cp-1) & 0x3F) << 6) | (*cp & 0x3F);
		buffer_addr %= (COLS * ROWS);
		current_fa = get_field_attribute (buffer_addr);
		last_cmd = TRUE;
		break;
	    case ORDER_IC:	/* insert cursor */
		cursor_move (buffer_addr);
		last_cmd = TRUE;
		break;
	    case ORDER_PT:	/* program tab */
		baddr = buffer_addr;
		screen_dirty = TRUE;
		while (TRUE) {
		    if (IS_FA (screen_buf[baddr])
		    &&  (!FA_IS_PROTECTED (screen_buf[baddr]))) {
			current_fa = &screen_buf[baddr];
			INC_BA (baddr);
			buffer_addr = baddr;
			if (!last_cmd) {
			    while (!IS_FA (screen_buf[baddr])) {
				screen_add(baddr, CG_NULLBLANK);
				INC_BA (baddr);
			    }
			}
			break;
		    }
		    else {
			INC_BA (baddr);
			if (baddr == 0) {
			    buffer_addr = baddr;
			    current_fa = get_field_attribute (baddr);
			    break;
			}
		    }
		}
		last_cmd = TRUE;
		break;
	    case ORDER_RA:	/* repeat to address */
		cp += 2;	/* skip buffer address */
		screen_dirty = TRUE;
		if ((*(cp-1) & 0xC0) == 0x00) /* 14-bit binary */
		    baddr = ((*(cp-1) & 0x3F) << 8) | *cp;
		else	/* 12-bit coded */
		    baddr = ((*(cp-1) & 0x3F) << 6) | (*cp & 0x3F);
		baddr %= (COLS * ROWS);
		cp++;		/* skip char to repeat */
		if (*cp == ORDER_GE)
		    cp++;
		if (buffer_addr == baddr) {
		    screen_add(buffer_addr, ebc2cg[*cp]);
		    INC_BA (buffer_addr);
		}
		while (buffer_addr != baddr) {
		    screen_add(buffer_addr, ebc2cg[*cp]);
		    INC_BA (buffer_addr);
		}
		current_fa = get_field_attribute (buffer_addr);
		last_cmd = TRUE;
		break;
	    case ORDER_EUA:	/* erase unprotected to address */
		cp += 2;	/* skip buffer address */
		screen_dirty = TRUE;
		if ((*(cp-1) & 0xC0) == 0x00) /* 14-bit binary */
		    baddr = ((*(cp-1) & 0x3F) << 8) | *cp;
		else	/* 12-bit coded */
		    baddr = ((*(cp-1) & 0x3F) << 6) | (*cp & 0x3F);
		baddr %= (COLS * ROWS);
		do {
		    if (IS_FA (screen_buf[buffer_addr]))
			current_fa = &(screen_buf[buffer_addr]);
		    else if (!FA_IS_PROTECTED (*current_fa)) {
			screen_add(buffer_addr, CG_NULLBLANK);
		    }
		    INC_BA (buffer_addr);
		} while (buffer_addr != baddr);
		current_fa = get_field_attribute (buffer_addr);
		last_cmd = TRUE;
		break;
	    case ORDER_MF:	/* modify field */
	    case ORDER_SFE:	/* start field extended */
	    case ORDER_SA:	/* set attribute */
		/* unsupported 3270 order */
		break;
	    default:	/* enter character */
		if (IS_FA(ebc2cg[*cp])) {
			/* debug hook */
		        last_cmd = FALSE;
		}
		screen_add(buffer_addr, ebc2cg[*cp]);
		INC_BA (buffer_addr);
		last_cmd = FALSE;
		break;
	}
    }
    /* cursor_on (); */
    set_formatted ();
    if (wcc_keyboard_restore) {
	key_Reset ();
	aid = AID_NO;
    }
    if (wcc_sound_alarm)
	XBell(display, 100);
}

put_cursor(baddr, color, on)
	int baddr;
	int color;
	int on;
{
    int		x, y;
    u_char 	*fa = get_field_attribute(baddr);
    int		invis;

    x = COL_TO_X(BA_TO_COL(baddr));
    y = ROW_TO_Y(BA_TO_ROW(baddr));
    invis = FA_IS_ZERO(*fa) || IS_FA(screen_buf[baddr]) || screen_buf[baddr] == 0 |
		screen_buf[baddr] == 0x10;
    if (on) {
	if (invis) {
	    XDrawImageString(display, w, invgc[color], x, y, blanks, 1);
	} else {
	    XDrawImageString(display, w, invgc[color], x, y, &(screen_buf[baddr]), 1);
	}
	if (appres.mono && FA_IS_HIGH(*fa) && !invis)
	    XDrawString(display, w, invgc[color], x+1, y, &(screen_buf[baddr]), 1);
    } else {
	put_string(baddr, 1);
    }
}

put_altcursor(baddr, gc, on)
	int baddr;
	GC gc;
	int on;
{
    char 	str[2];
    int		x, y;

    put_string(baddr, 1);
    if (on) {
        str[0] = CG_COMMLO; str[1] = 0;
        x = COL_TO_X(BA_TO_COL(baddr));
        y = ROW_TO_Y(BA_TO_ROW(baddr));
        XDrawString(display, w, gc, x, y+1, str, 1);
        XDrawString(display, w, gc, x, y+2, str, 1);
    }
}

put_string(baddr, len)
	int baddr, len;
{
    put_string_gc(screen_buf, baddr, len, gc[fa_color(*get_field_attribute(baddr))]);
}

put_string_gc(buf, baddr, len, gc)
	u_char *buf;
	int baddr, len;
	GC gc;
{
    u_char	*str, b[128];
    u_char 	*fa, ch;
    int		x, y, i;

#if 0
	extern u_char cg2asc[];

	putchar('\n');
	printf("address %d len %d", baddr, len);
	for (i=0; i<len; i++) {
	    ch = buf[i+baddr];
	    putchar(cg2asc[ch]);
	}
#endif
    if (mono_case) {
	for (i=0; i<len; i++) {
	    ch = buf[i+baddr];
	    /* this if xlates lowercase to uppercase */
	    if (mono_case && ((ch & 0xE0) == 0x40 || (ch & 0xE0) == 0x80))
	       ch |= 0x20;
	    b[i] = ch;
	}
	str = b;
    } else
	str = &(buf[baddr]);

    fa = get_field_attribute(baddr);
    x = COL_TO_X(BA_TO_COL(baddr));
    y = ROW_TO_Y(BA_TO_ROW(baddr));

    /* Clear the area first... kludge for R2 on HP's */
    /*
    XDrawImageString(display, w, gc, x, y, blanks, len);
    */
    XDrawImageString(display, w, gc, x, y, str, len);
    if (appres.mono && (FA_IS_HIGH(*fa))) {
       XDrawString(display, w, gc, x+1, y, str, len);
    }
}

/*
 * Clear the text (non-status) portion of the display.
 */
clear_text()
{
    int 	i;

    screen_dirty = TRUE;
    bzero(screen_buf, ROWS*COLS);
}

/* 
 * Do one of line display.
 */
do_line (baddr)
    register int baddr;
{
    register int	end, start, len;
    int			i;

    end = baddr + COLS;
    if (end >= ROWS*COLS)
	end = 0;

    start = baddr;
    len = 0;
    while(baddr != end) {
	if (marked(baddr)) {
	    if (len == 0) {
	        start = baddr;
	        len = 1;
	    } else {
                len++;
                if (IS_FA(screen_buf[baddr])) {
	            put_string(start, len);
		    unmark_chars(start, len);
		    INC_BA(baddr);
	            start = baddr;
	            len = 0;
		    continue;
	        }
	    }
	} else if (len > 0) {
            put_string(start, len);
	    unmark_chars(start, len);
	    len = 0;
	}
	INC_BA(baddr);
    }
    if (len > 0) {
        put_string(start, len);
	unmark_chars(start, len);
    }
}

screen_add(baddr, c)
	int	baddr;
	u_char	c;
{
    if (screen_buf[baddr] != c) {
	screen_buf[baddr] = c;
	screen_dirty = TRUE;
    }
}

make_gc_pair(i, fg, bg)
    int		i;
    Pixel 	fg, bg;
{
    static XGCValues xgcv;

    xgcv.foreground = fg;
    xgcv.background = bg;
    xgcv.font = ibmfont;
    gc[i] = XtGetGC(toplevel, GCForeground|GCBackground|GCFont, &xgcv);
    xgcv.foreground = bg;
    xgcv.background = fg;
    invgc[i] = XtGetGC(toplevel, GCForeground|GCBackground|GCFont, &xgcv);
}

fa_color(fa)
    u_char fa;
{
    int	rval = 0;

    switch (fa & FA_INTENSITY) {
    case FA_INT_NORM_NSEL:
	rval = 0;
        if (FA_IS_MODIFIED(fa))
	    rval = 1;
	break;
    case FA_INT_NORM_SEL:
	rval = 1;
        if (FA_IS_MODIFIED(fa))
	    rval = 1;
	break;
    case FA_INT_HIGH_SEL:
	rval = 2;
        if (FA_IS_MODIFIED(fa))
	    rval = 1;
	break;
    case FA_INT_ZERO_NSEL:
	rval = 3;
	break;
    }
    return rval;
}

unmark_chars(start, len)
	int start, len;
{
    memcpy(&(update_buf[start]), &(screen_buf[start]), len);
}

int
marked(addr)
	int addr;
{
    int	rval;

    rval = (update_buf[addr] != screen_buf[addr]);

    if (rval && IS_FA(screen_buf[addr]))
	mark_field(addr);
    return rval;
}

/* Mark the entire field as different from the screen_buf */
mark_field(addr)
{
    int s_addr = addr;

    update_buf[addr] = 1;
    INC_BA(addr);
    while (!IS_FA(screen_buf[addr]) && addr != s_addr) {
        update_buf[addr] = 1;
        INC_BA(addr);
    }
}
