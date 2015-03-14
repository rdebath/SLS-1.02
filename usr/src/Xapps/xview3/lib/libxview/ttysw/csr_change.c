#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)csr_change.c 20.38 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/*
 * Character screen operations (except size change and init).
 */

#include <xview_private/i18n_impl.h>
#include <xview_private/portable.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <X11/Xlib.h>
#include <pixrect/pixrect.h>
#include <pixrect/pixfont.h>

#ifdef __STDC__ 
#ifndef CAT
#define CAT(a,b)        a ## b 
#endif 
#endif
#include <pixrect/memvar.h>

#include <xview/rect.h>
#include <xview/rectlist.h>
#include <xview/pixwin.h>
#include <xview/ttysw.h>
#include <xview/sel_svc.h>

#include <xview_private/charimage.h>
#include <xview_private/charscreen.h>
#undef CTRL
#include <xview_private/ttyansi.h>
#include <xview/window.h>
#include <xview/pkg.h>
#include <xview/attrol.h>
#include <xview/server.h>
#include <xview/font.h>
Xv_private_data char *shell_prompt;

#ifdef OW_I18N
#define NULL_CHARP      (CHAR *) 0
#endif

#define TTYSW_HOME_CHAR	'A'

extern Xv_Window csr_pixwin;
extern int      cursor;		/* NOCURSOR, UNDERCURSOR, BLOCKCURSOR */
extern void     ttysw_pos();

static int      caretx, carety, lxhome;
static short    charcursx, charcursy;

#ifdef  OW_I18N
/*
 *      To save the width of cursor that has been most recently
 *      drawn , so that when deleting the cursor , we can construct
 *      the reversed image easily.
 */
static  int     curs_width;
#endif

static int      boldstyle, inverse_mode, underline_mode;

extern struct timeval ttysw_bell_tv;	/* initialized to 1/10 second */

static u_short  ttysw_gray17_data[16] = {	/* really 16-2/3	 */
    0x8208, 0x2082, 0x0410, 0x1041, 0x4104, 0x0820, 0x8208, 0x2082,
    0x0410, 0x1041, 0x4104, 0x0820, 0x8208, 0x2082, 0x0410, 0x1041
};

mpr_static(ttysw_gray17_pr, 12, 12, 1, ttysw_gray17_data);

ttysw_setboldstyle(new_boldstyle)
    int             new_boldstyle;
{
    if (new_boldstyle > TTYSW_BOLD_MAX
	|| new_boldstyle < TTYSW_BOLD_NONE)
	boldstyle = TTYSW_BOLD_NONE;
    else
	boldstyle = new_boldstyle;
    return boldstyle;
}

ttysw_set_inverse_mode(new_inverse_mode)
    int             new_inverse_mode;
{
    inverse_mode = new_inverse_mode;
}

ttysw_set_underline_mode(new_underline_mode)
    int             new_underline_mode;
{
    underline_mode = new_underline_mode;
}

ttysw_getboldstyle()
{
    return boldstyle;
}

ttysw_get_inverse_mode()
{
    return inverse_mode;
}

ttysw_get_underline_mode()
{
    return underline_mode;
}


ttysw_setleftmargin(left_margin)
{
    chrleftmargin = left_margin > 0 ? left_margin : 0;
}

ttysw_getleftmargin()
{
    return chrleftmargin;
}

ttysw_fixup_display_mode(mode)
    char           *mode;
{

    if ((*mode & MODE_INVERT) && (inverse_mode != TTYSW_ENABLE)) {
	*mode &= ~MODE_INVERT;
	if (inverse_mode == TTYSW_SAME_AS_BOLD)
	    *mode |= MODE_BOLD;
    }
    if ((*mode & MODE_UNDERSCORE) && (underline_mode != TTYSW_ENABLE)) {
	*mode &= ~MODE_UNDERSCORE;
	if (underline_mode == TTYSW_SAME_AS_BOLD)
	    *mode |= MODE_BOLD;
    }
    if ((*mode & MODE_BOLD) && (boldstyle & TTYSW_BOLD_INVERT)) {
	*mode &= ~MODE_BOLD;
	*mode |= MODE_INVERT;
    }
}

/* Note: whole string will be diplayed with mode. */
ttysw_pstring(s, mode, col, row, op)
    register CHAR  *s;          /* must be null-terminated */
    char            mode;
    register int    col, row;
    int             op;		/* PIX_SRC | PIX_DST (faster), or PIX_SRC
				 * (safer) */
{
#ifdef OW_I18N
    register CHAR *cp;
#define BUFSIZE 1024
    /*
     *  It is guranteed that ttysw_pstring() is called at most by
     *  amount of one line.
     */
    CHAR           buf[BUFSIZE];
#else
    register unsigned char *cp;
#endif
    register int    x_home;
    register int    y_home;
    XFontStruct	*x_font_info = (XFontStruct *)xv_get((Xv_opaque)pixfont, FONT_INFO);

#ifdef OW_I18N
    /*
    * This code is using the per_char of the ASCII font in the font set to 
    * figure out the metrics.
    * 
    if (x_font_info.per_char)  
    **/
    if (0)  {
#else
    if (x_font_info->per_char)  {
#endif
        x_home = x_font_info->per_char[TTYSW_HOME_CHAR - x_font_info->min_char_or_byte2].lbearing;
    }
    else  {
        x_home = x_font_info->min_bounds.lbearing;
    }
#ifdef  OW_I18N
    y_home = -chrbase;
#else
    y_home = -x_font_info->ascent;
#endif
    /* this is needed for correct caret rendering */
    lxhome = x_home;

    /* possibly use escape sequences ? */

    if (xv_get(XV_SERVER_FROM_WINDOW(csr_pixwin), SERVER_JOURNALLING))
#ifdef OW_I18N
        if (INDEX(s, shell_prompt[0]))
#else
	if (XV_INDEX(s, shell_prompt[0]))
#endif
	    xv_set(XV_SERVER_FROM_WINDOW(csr_pixwin), SERVER_JOURNAL_SYNC_EVENT, 1, 0);

    if (delaypainting) {
	if (row == ttysw_bottom)
	    /*
	     * Reached bottom of screen so end delaypainting.
	     */
	    (void) ttysw_pdisplayscreen(1);
	return;
    }
    if (s == 0)
	return;
    ttysw_fixup_display_mode(&mode);
#ifdef  OW_I18N
    ttysw_convert_string( buf , s );
    if (mode & MODE_BOLD) {
                /* Clean up first */
        (void) ttysw_pclearline(col, col + STRLEN(s), row);

        /* render the first one, the potential offset of the others */
        (void) tty_newtext(csr_pixwin,
                           col_to_x(col) - x_home,
                           row_to_y(row) - y_home,
                           (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) : op,
                           pixfont, buf, STRLEN(buf));

        if (boldstyle & TTYSW_BOLD_OFFSET_X)
            (void) tty_newtext(csr_pixwin,
                               col_to_x(col) - x_home + 1,
                               row_to_y(row) - y_home,
                         (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) & PIX_DST :
                               PIX_SRC | PIX_DST, pixfont, buf, STRLEN(buf));
        if (boldstyle & TTYSW_BOLD_OFFSET_Y)
            (void) tty_newtext(csr_pixwin,
                               col_to_x(col) - x_home,
                               row_to_y(row) - y_home + 1,
                         (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) & PIX_DST :
                               PIX_SRC | PIX_DST, pixfont, buf, STRLEN(buf));
        if (boldstyle & TTYSW_BOLD_OFFSET_XY)
            (void) tty_newtext(csr_pixwin,
                               col_to_x(col) - x_home + 1,
                               row_to_y(row) - y_home + 1,
                         (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) & PIX_DST :
                               PIX_SRC | PIX_DST, pixfont, buf, STRLEN(buf));
    } else {
        (void) tty_newtext(csr_pixwin,
                           col_to_x(col) - x_home,
                           row_to_y(row) - y_home,
        (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) : op, pixfont, buf, STRLEN(buf));
    }
    if (mode & MODE_UNDERSCORE) {
   /*
    * Bug in XVIEW "cannot draw under_score"
    * remove PIX_NOT
    */
   /* 
    *   Note.
    *   STRLEN(s) represents the column length regardless of
    *   codesets in s.
    */
        tty_background(csr_pixwin,
                       col_to_x(col), row_to_y(row) + chrheight - 1,
                       (STRLEN(s)) * chrwidth, 1,
                       (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) : PIX_SRC);
    }
#else
    if (mode & MODE_BOLD) {
	    	/* Clean up first */
	(void) ttysw_pclearline(col, col + strlen(s), row);

	/* render the first one, the potential offset of the others */
	(void) tty_newtext(csr_pixwin,
			   col_to_x(col) - x_home,
			   row_to_y(row) - y_home,
			   (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) : op, 
			   pixfont, s, strlen(s));

	if (boldstyle & TTYSW_BOLD_OFFSET_X)
	    (void) tty_newtext(csr_pixwin,
			       col_to_x(col) - x_home + 1,
			       row_to_y(row) - y_home,
			 (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) & PIX_DST :
			       PIX_SRC | PIX_DST, pixfont, s, strlen(s));
	if (boldstyle & TTYSW_BOLD_OFFSET_Y)
	    (void) tty_newtext(csr_pixwin,
			       col_to_x(col) - x_home,
			       row_to_y(row) - y_home + 1,
			 (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) & PIX_DST :
			       PIX_SRC | PIX_DST, pixfont, s, strlen(s));
	if (boldstyle & TTYSW_BOLD_OFFSET_XY)
	    (void) tty_newtext(csr_pixwin,
			       col_to_x(col) - x_home + 1,
			       row_to_y(row) - y_home + 1,
			 (mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) & PIX_DST :
			       PIX_SRC | PIX_DST, pixfont, s, strlen(s));
    } else {
	(void) tty_newtext(csr_pixwin,
			   col_to_x(col) - x_home,
			   row_to_y(row) - y_home,
	(mode & MODE_INVERT) ? PIX_NOT(PIX_SRC) : op, pixfont, s, strlen(s));
    }
    if (mode & MODE_UNDERSCORE) {
	tty_background(csr_pixwin,
		       col_to_x(col), row_to_y(row) + chrheight - 1,
		       strlen(s) * chrwidth, 1,
		       (mode & MODE_INVERT) ? PIX_SRC : PIX_NOT(PIX_SRC));
    }
#endif  OW_I18N

#ifdef  OW_I18N
#undef  BUFSIZE
#endif
}

ttysw_pclearline(fromcol, tocol, row)
    int             fromcol, tocol, row;
{
    int	klu1284	= (fromcol == 0 ? 1 : 0 ); 

    if (delaypainting)
	return;
    (void) tty_background(csr_pixwin, 
			  col_to_x(fromcol)-klu1284, row_to_y(row),
			  col_to_x(tocol) - col_to_x(fromcol)+klu1284,
			  chrheight, PIX_CLR);
}

ttysw_pcopyline(tocol, fromcol, count, row)
    int             fromcol, tocol, count, row;
{
    int             pix_width = (count * chrwidth);
    if (delaypainting)
	return;
    (void) tty_copyarea(csr_pixwin,
		     col_to_x(fromcol)-1, row_to_y(row), pix_width+1, chrheight,
			col_to_x(tocol)-1, row_to_y(row));
}

ttysw_pclearscreen(fromrow, torow)
    int             fromrow, torow;
{
    if (delaypainting)
	return;
    (void) tty_background(csr_pixwin, col_to_x(ttysw_left)-1,
			  row_to_y(fromrow),
			  winwidthp+1, row_to_y(torow - fromrow), PIX_CLR);
}

ttysw_pcopyscreen(fromrow, torow, count)
    int             fromrow, torow, count;
{
    if (delaypainting)
	return;
    (void) tty_copyarea(csr_pixwin,
			col_to_x(ttysw_left)-1, row_to_y(fromrow), winwidthp+1, row_to_y(count),
			col_to_x(ttysw_left)-1, row_to_y(torow));
}

ttysw_pdisplayscreen(dontrestorecursor)
    int             dontrestorecursor;
{
    Rectlist        rl;
    struct rect     rct;

    rl = rl_null;
    delaypainting = 0;
    /*
     * refresh the entire image.
     */
    rct = *((struct rect *) xv_get(csr_pixwin, WIN_RECT));
    /* Make rct sw relative, as ttysw_prepair operates in sw coords. */
    rct.r_top = 0;
    rct.r_left = 0;
    (void) rl_initwithrect(&rct, &rl);
    (void) ttysw_prepair(xv_get(csr_pixwin, WIN_FD), &rl);
    if (!dontrestorecursor)
	/*
	 * The following has effect of restoring cursor.
	 */
	(void) ttysw_removeCursor();
}

/* ARGSUSED */
Pkg_private int
ttysw_prepair(window, rlp)
    Xv_object       window;
    struct rectlist *rlp;
{
    struct rectnode *rnp;
    struct rect     rect;

    rl_rectoffset(rlp, &rlp->rl_bound, &rect);
    for (rnp = rlp->rl_head; rnp; rnp = rnp->rn_next) {
	register int    row, colstart, blanks, colfirst;
        register CHAR  *strstart, *strfirst;
        register char  modefirst;
	register char  *modestart;
	int             toprow, botrow, leftcol;
        CHAR            csave;

	colfirst = 0;		/* make LINT shut up */
	rl_rectoffset(rlp, &rnp->rn_rect, &rect);
	(void) tty_background(csr_pixwin, rect.r_left-1, rect.r_top,
			      rect.r_width+1, rect.r_height, PIX_CLR);
	toprow = y_to_row(rect.r_top);
	botrow = MIN(ttysw_bottom, y_to_row(rect_bottom(&rect)) + 1);
	leftcol = x_to_col(rect.r_left);
	for (row = toprow; row < botrow; row++) {
	    colstart = leftcol;
#ifdef OW_I18N
            if ((unsigned char)leftcol < LINE_LENGTH(image[row])) {
		strfirst = NULL_CHARP;
#else
	    if ((unsigned char)leftcol < length(image[row])) {
		strfirst = (caddr_t) 0;
#endif
		modefirst = MODE_CLEAR;
		blanks = 1;
		for (strstart = image[row] + leftcol,
		     modestart = screenmode[row] + leftcol; *strstart;
		     strstart++, modestart++, colstart++) {
		    /*
		     * Find beginning of bold string
		     */
		    if (*modestart != modefirst) {
			goto Flush;
			/*
			 * Find first non-blank char
			 */
		    } else if (blanks && (*strstart != ' '))
			goto Flush;
		    else
			continue;
	    Flush:
#ifdef OW_I18N
                    if (strfirst != NULL_CHARP) {
#else
		    if (strfirst != (caddr_t) 0) {
#endif
			csave = *strstart;
			*strstart = '\0';
			(void) ttysw_pstring(strfirst, modefirst,
				       colfirst, row,
				       PIX_SRC /* | PIX_DST - jcb */ );
			*strstart = csave;
		    }
		    colfirst = colstart;
		    strfirst = strstart;
		    modefirst = *modestart;
		    blanks = 0;
		}
#ifdef OW_I18N
                if (strfirst != NULL_CHARP)
#else
		if (strfirst != (caddr_t) 0)
#endif
		    (void) ttysw_pstring(strfirst, modefirst, colfirst,
				   row, PIX_SRC /* | PIX_DST -- jcb */ );
	    }
	}
    }
}

ttysw_drawCursor(yChar, xChar)
{
#ifdef  OW_I18N
    int         offset;

    /*
     *  OW_I18N needs to check whether the target character is
     *  ascii or not , so we cannot put cursor out of range.
     */

    if( xChar >= ttysw_right )
        xChar = ttysw_right-1;
    if( xChar < ttysw_left )
        xChar = ttysw_left;
    if( yChar >= ttysw_bottom )
        yChar = ttysw_bottom-1;
    if( yChar < ttysw_top )
        yChar = ttysw_top;
#endif
    charcursx = xChar;
    charcursy = yChar;
    caretx = col_to_x(xChar);
    carety = row_to_y(yChar);
    if (delaypainting || cursor == NOCURSOR)
	return;
#ifdef  OW_I18N
/*
 *    Setup appropriate Cursor-width and originated pixrect address
 *    according as the character size
 */
    tty_column_wchar_type( xChar , yChar , &curs_width , &offset );
    curs_width *= chrwidth;
    caretx     -= offset*chrwidth;
    (void) tty_background(csr_pixwin,
             caretx-lxhome, carety, curs_width, chrheight, PIX_NOT(PIX_DST));
#else
    (void) tty_background(csr_pixwin,
		     caretx-lxhome, carety, chrwidth, chrheight, PIX_NOT(PIX_DST));
#endif
    if (cursor & LIGHTCURSOR) {
#ifdef  OW_I18N
        (void) tty_background(csr_pixwin,
                              caretx - lxhome - 1, carety - 1, curs_width + 2,
                              chrheight + 2, PIX_NOT(PIX_DST));
#else
	(void) tty_background(csr_pixwin,
			      caretx - lxhome - 1, carety - 1, chrwidth + 2, 
			      chrheight + 2, PIX_NOT(PIX_DST));
#endif
	(void) ttysw_pos(xChar, yChar);
    }
}

ttysw_removeCursor()
{
    if (delaypainting || cursor == NOCURSOR)
	return;
#ifdef  OW_I18N
/*
 *      caretx and curs_width are stored in global and those values
 *      represent the location and width of the cursor.
 */
    (void) tty_background(csr_pixwin,
             caretx-lxhome, carety, curs_width, chrheight, PIX_NOT(PIX_DST));
#else
    (void) tty_background(csr_pixwin,
		     caretx-lxhome, carety, chrwidth, chrheight, PIX_NOT(PIX_DST));
#endif
    if (cursor & LIGHTCURSOR) {
#ifdef  OW_I18N
        (void) tty_background(csr_pixwin,
                              caretx - lxhome - 1, carety - 1, curs_width + 2,
                              chrheight + 2, PIX_NOT(PIX_DST));
#else
	(void) tty_background(csr_pixwin,
			      caretx - lxhome - 1, carety - 1, chrwidth + 2, 
			      chrheight + 2, PIX_NOT(PIX_DST));
#endif
    }
}

ttysw_saveCursor()
{
    (void) ttysw_removeCursor();
}

ttysw_restoreCursor()
{
    (void) ttysw_removeCursor();
}

ttysw_screencomp()
{
}

ttysw_blinkscreen()
{
    struct timeval  now;
    static struct timeval lastblink;

    (void) gettimeofday(&now, (struct timezone *) 0);
    if (now.tv_sec - lastblink.tv_sec > 1) {
	Xv_object       window = (Xv_object) csr_pixwin;
	(void) win_bell(window, ttysw_bell_tv, csr_pixwin);
	lastblink = now;
    }
}

ttysw_pselectionhilite(r, sel_rank)
    struct rect    *r;
    Seln_rank       sel_rank;
{
    struct rect     rectlock;

    rectlock = *r;
    rect_marginadjust(&rectlock, 1);
    if (sel_rank == SELN_PRIMARY)
	(void) tty_background(csr_pixwin, r->r_left, r->r_top,
			      r->r_width, r->r_height, PIX_NOT(PIX_DST));
    else
	(void) xv_replrop(csr_pixwin,
			  r->r_left, r->r_top,
			  r->r_width, r->r_height,
			  PIX_SRC | PIX_DST, &ttysw_gray17_pr, 0, 0);
}

#ifdef OW_I18N

/*
 *      Tty-subwindow stores screen image in a global CHAR **image.
 *      This array treats characters in a tricky way.
 *      For a character which has larger size than ascii characters,
 *      image data array is padded with TTY_NON_WCHAR so that
 *      image data array and the actual screen get coincident.
 *      This function converts image arrary to a normal wchar array
 *      by eliminating TTY_NON_WCHAR.
 */

ttysw_convert_string( str , ttystr )
    CHAR        *str;
    CHAR        *ttystr;
{
    register    CHAR *strtmp = str;
    register    CHAR *ttystrtmp  = ttystr;

    while( *ttystrtmp ) {
        if( *ttystrtmp != TTY_NON_WCHAR )
                *strtmp++ = *ttystrtmp++;
        else
                ttystrtmp++;
    }
    *strtmp = (CHAR)'\0';
}


/*
 *      Get the size of a character.
 */
tty_character_size(c)
   CHAR         c;
{

    /*
     *  Warning!!
     *  To get the charcter-width , this function calls wscol()
     *  which may cause a problem in portability.
     */
    static wchar_t      str[2] = {(wchar_t)'\0',(wchar_t)'\0'};

    if( c == (wchar_t)'\0' )
        return 1;

    str[0] = c;
    return( wscol(str) );

}
#endif
