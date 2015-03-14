#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)tty_cb.c %I 91/09/14";
#endif
#endif

/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL NOTICE
 *      file for terms of the license.
 */

#include <sys/types.h>
#include <pixrect/pixrect.h>

#include <xview/tty.h>
#include <xview/ttysw.h>
#include <xview/termsw.h>
#include <xview_private/tty_impl.h>
#include <xview_private/term_impl.h>
#include <xview_private/i18n_impl.h>
#include <xview_private/charimage.h>
#include <xview_private/charscreen.h>

#define ITERM_BUFSIZE   1024

#define	TTYSW_GET_COL(folio)		(curscol)
#define	TTYSW_GET_ROW(folio)		(cursrow)
#define	TTYSW_GET_MAX_COL(folio)	(ttysw_right)
#define	TTYSW_GET_MAX_ROW(folio)	(ttysw_bottom)

/*   
 *    committed_left takes care about the case of implicit commit.
 *   Preedit-callback suspend drawing preedit text until all committed 
 *   string is drawn.
 */
extern	int	committed_left;

Xv_private      void
tty_text_start(ic, client_data, callback_data)
XIC		ic;
XPointer	client_data;
XPointer 	callback_data;
{
    Tty		ttysw_public;
    Ttysw_folio	folio;

    ttysw_public = (Tty)client_data;
    folio = TTY_PRIVATE_FROM_ANY_PUBLIC(ttysw_public);


    folio->im_first_col = TTYSW_GET_COL(folio);
    folio->im_first_row = TTYSW_GET_ROW(folio);
    folio->im_len = 0;

    if( !folio->im_store )
    	folio->im_store = (CHAR *)malloc( ITERM_BUFSIZE * sizeof(CHAR) );
    if( !folio->im_attr )
    	folio->im_attr  = (XIMFeedback *)malloc( ITERM_BUFSIZE * sizeof( XIMFeedback) );

    folio->im_store[0] = (CHAR)'\0';

    /*  
     *  preedit_state is used to check pre-editting is on or off.
     */

    folio->preedit_state = TRUE;

    return;
}

Xv_private      void
tty_text_draw(ic, client_data, callback_data)
XIC	ic;
XPointer	client_data;
XIMPreeditDrawCallbackStruct 	*callback_data;
{
    Tty			ttysw_public;
    CHAR		*wcs;
    char		*mbs;
    XIMFeedback		*attr;
    unsigned short	len = 0;
    Ttysw_folio		folio;
    static Bool		curs_set = TRUE;
    int			org_len, chg_start, chg_length;
    CHAR		*org_text;
    CHAR		*new_text;
    CHAR		*insert_text;
    XIMFeedback		*org_attr;
    XIMFeedback		*new_attr;
    XIMFeedback		*insert_attr;
    int                 delete_only = 0;

    ttysw_public = (Tty)client_data;
    folio = TTY_PRIVATE_FROM_ANY_PUBLIC(ttysw_public);

    if( !folio->preedit_state ) {
	tty_text_start(ic, client_data, callback_data);
    }

    if( !curs_set ) {
    	folio->im_first_col = TTYSW_GET_COL(folio);
    	folio->im_first_row = TTYSW_GET_ROW(folio);
	curs_set = TRUE;
    }

    org_len = wslen(folio->im_store);
    org_text = folio->im_store;
    org_attr = folio->im_attr;

    chg_start = callback_data->chg_first;
    chg_length = callback_data->chg_length;

    if( callback_data->text == NULL && org_len == chg_length ) {
                                                        /* erase preedit */
        ttysw_removeCursor();
        tty_preedit_erase(folio, folio->im_first_col, folio->im_first_row,
                                folio->im_len );
        ttysw_drawCursor(folio->im_first_row, folio->im_first_col);
        if ( chg_length != 0 )
            folio->im_store[0] = (CHAR)'\0';

        return;
    }

    if ( callback_data->text != NULL ) {		/* temporaray code */
	len = callback_data->text->length;
    }

    if( callback_data->text == NULL || ( len == 0 && chg_start == 0 ) ) {
							/* erase preedit */
     	ttysw_removeCursor();
	tty_preedit_erase( folio, folio->im_first_col,folio->im_first_row,
				folio->im_len );
        ttysw_drawCursor(folio->im_first_row, folio->im_first_col);

	if ( chg_length != 0 )
		folio->im_store[0] = (CHAR)'\0';

	return;
    }

    if ( callback_data->text != NULL ) {
        len = callback_data->text->length;
        wcs = (CHAR *)callback_data->text->string.wide_char;
        mbs = (char *)callback_data->text->string.multi_byte;
        attr = (XIMFeedback *)callback_data->text->feedback;
    } else {
        delete_only = 1;
        len = 0;
        wcs = (CHAR *)NULL;
        mbs = (char *)NULL;
        attr = (XIMFeedback *)NULL;
    }

    if( committed_left > 0  && wcs ) {
	if( !callback_data->text->encoding_is_wchar ) {
	    CHAR    *wcsbuf;

            wcsbuf = (CHAR *)calloc( len + 1 , sizeof(CHAR) );
            mbstowcs( wcsbuf , mbs , len + 1);
	    wscpy( org_text, wcsbuf);
            free(wcsbuf);
        } else
	    wscpy( org_text , wcs );

	XV_BCOPY(attr, org_attr, sizeof(XIMFeedback)*wslen(org_text));
	curs_set = FALSE;
	return;
    }

    if( ( !wcs ) && ( !mbs ) && (!delete_only) ) { /* updtate the feedback */
	insert_attr = org_attr + chg_start;
	XV_BCOPY(attr, insert_attr, sizeof(XIMFeedback)*chg_length);
    }
    else { 				/* draw intermediate text */
	new_text = (wchar_t *)calloc(org_len + len + 1, sizeof(wchar_t));
	new_attr = (XIMFeedback *)calloc(org_len + len + 1,
						sizeof(XIMFeedback));
 
	if ( chg_start > 0 ) {
		XV_BCOPY(org_text, new_text, sizeof(wchar_t)*chg_start);
		XV_BCOPY(org_attr, new_attr, sizeof(XIMFeedback)*chg_start);
	}

	insert_text = new_text + chg_start;
	insert_attr = new_attr + chg_start;
 
	if ( len != 0 ) {
	    if( !callback_data->text->encoding_is_wchar ) {
		CHAR    *wcsbuf;

		wcsbuf = (CHAR *)calloc( len + 1 , sizeof(CHAR) );
		mbstowcs( wcsbuf , mbs , len + 1);
		XV_BCOPY(wcsbuf, insert_text, sizeof(wchar_t)*len );
		free(wcsbuf);
	    } else
		XV_BCOPY(wcs, insert_text, sizeof(wchar_t)*len );

	    XV_BCOPY(attr, insert_attr, sizeof(XIMFeedback)*len );
	}

	if ( (chg_start + chg_length) < org_len ) {
		insert_text += len;
		insert_attr += len;
		XV_BCOPY(org_text + (chg_start + chg_length),
			insert_text,
			sizeof(wchar_t)*(org_len - chg_start - chg_length) );
		XV_BCOPY(org_attr + (chg_start + chg_length),
			insert_attr,
			sizeof(XIMFeedback)*(org_len - chg_start - chg_length));
	}
	new_text[org_len + len - chg_length] = (CHAR)'\0';
	wscpy(folio->im_store, new_text);
	XV_BCOPY(new_attr, folio->im_attr,
			sizeof(XIMFeedback)*(org_len + len - chg_length));

	free(new_text);
	free(new_attr);
    }

	new_text = folio->im_store;
	new_attr = folio->im_attr;

	if ( chg_start != 0 ) {
		int	i;
		int	max_col;
		int	d_len, imlen = 0;
		int	col_wchar, n_col = 0, d_col = 0, d_row = 0;
		int	end_gap;
		CHAR	* wcs;

		max_col = TTYSW_GET_MAX_COL( folio );
		d_col = folio->im_first_col;
		wcs = new_text;

		for ( i=0; i < chg_start; i++) {
			col_wchar = tty_character_size(*wcs ++);
			n_col += col_wchar;
			if ( d_col + col_wchar < max_col ) {
				d_col += col_wchar;
			} else {
				if ( d_col + col_wchar == max_col ) {
					d_col = 0;
				} else {
					end_gap = max_col - d_col;
					n_col += end_gap;
					d_col = col_wchar;
				}
				d_row ++;
			}
		}
		if ( (d_len = folio->im_len - n_col) > 0 )
			tty_preedit_erase(folio, 
				d_col, folio->im_first_row + d_row, d_len);

		tty_preedit_put_wcs(folio,
			new_text + chg_start, new_attr + chg_start,
			d_col,folio->im_first_row + d_row , &imlen);

		folio->im_len = n_col + imlen;
	} else
		tty_preedit_replace_wcs(folio, new_text, new_attr,
			folio->im_first_col,folio->im_first_row );

    return;			
}

Xv_private      void
tty_text_done(ic, client_data, callback_data)
XIC		ic;
XPointer	client_data;
XPointer 	callback_data;
{
    Tty		ttysw_public;
    Ttysw_folio	folio;

    ttysw_public = (Tty)client_data;
    folio = TTY_PRIVATE_FROM_ANY_PUBLIC(ttysw_public);

    folio->preedit_state = FALSE;

    curscol = folio->im_first_col;
    cursrow = folio->im_first_row;
    XV_BZERO( folio->im_store, ITERM_BUFSIZE * sizeof( CHAR ));

    return;
}


tty_preedit_erase( folio , first_col , first_row , len )
Ttysw_folio	folio;
int		first_col;
int		first_row;
int		len;
{
    int		fromcol,tocol,row,num_row;
    int 	maxcol = TTYSW_GET_MAX_COL( folio );

    num_row = (first_col + len + 1)/maxcol + 1;

    fromcol = first_col;
    if( num_row == 1 )
	tocol = first_col + len;
    else
	tocol = maxcol;

    for( row = first_row ; row < first_row + num_row ; row++ ) {
	ttysw_pclearline( fromcol , tocol , row );
	len -= (tocol - fromcol - 1);
	fromcol = 0;
	if ( len > maxcol )
		tocol = maxcol;
	else
		tocol = len - 1;
    }
}
    
tty_preedit_replace_wcs( folio, wcs, attr, first_col, first_row )
Ttysw_folio     folio;  
CHAR		*wcs;
XIMFeedback	*attr;
int		first_col;
int		first_row;
{
    tty_preedit_erase( folio ,folio->im_first_col ,folio->im_first_row,
				folio->im_len );
    tty_preedit_put_wcs( folio, wcs , attr, first_col , first_row , &(folio->im_len));
}

tty_preedit_put_wcs( folio, wcs , attr, first_col ,first_row, len)
Ttysw_folio     folio;  
CHAR		*wcs;
XIMFeedback	*attr;
int		first_col;
int		first_row;
int		* len;
{
    int		col,row,maxcol,maxrow;
    int		i,j;
    char	mode;
    static CHAR	buf[4] = { (CHAR)'\0',(CHAR)'\0',
				   (CHAR)'\0',(CHAR)'\0'};
    int		colwidth;
    int		end_gap;

    maxcol = TTYSW_GET_MAX_COL( folio );
    maxrow = TTYSW_GET_MAX_ROW( folio );
    col = first_col;
    row = first_row;

    * len = 0;
    ttysw_removeCursor();

    while( *wcs ) {
    	mode = MODE_CLEAR;
	buf[0] = *wcs++;
	colwidth = tty_character_size( buf[0] );
	/*
	 *	BUG!!
	 *	This code is restricted by the length of buf(--4--)
	 *	, which means you cannot display a character that is bigger
	 *	than 3 times the size of ascii characters
	 */
	if( col+colwidth > maxcol ) {
		end_gap = colwidth - (maxcol-col);
		if( end_gap > 0 )
			* len += end_gap;
		col = 0;
		if( row >= maxrow-1 ) {
			ttysw_cim_scroll(1);
			folio->im_first_row --;
		}
		else {
			row++;
		}
	}
	if( *attr & XIMReverse )
		mode |= MODE_INVERT;
	if( *attr & XIMUnderline )
		mode |= MODE_UNDERSCORE;
	j = 1;
	while( j < colwidth )
		buf[j++] = TTY_NON_WCHAR;
    	ttysw_pstring(buf, mode, col , row, PIX_SRC);
	j = 1;
	while( j < colwidth )
		buf[j++] = (CHAR)'\0';
	col += colwidth;
	* len += colwidth;
        attr++;
    }

	if ( col >= maxcol )
		ttysw_drawCursor(++row, 0);
	else
		ttysw_drawCursor(row, col);

}


ttysw_preedit_resize_proc( folio )
Ttysw_folio	folio;
{
/*  
 *      This function takes care about the preedit text, when ttysw is
 *    resized when conversion is on. This function is dependent on
 *    the pre-edit callback code. And this function is never invoked
 *    if our pre-edit callback is overridden by another callback.  
 */
    if( folio->im_first_col >= TTYSW_GET_MAX_COL(folio) ) {
	folio->im_first_col = 0;
	folio->im_first_row += 1;
	if( folio->im_first_row >= TTYSW_GET_MAX_ROW(folio) )
		ttysw_cim_scroll(1);
    }

    if( folio->im_first_row >= TTYSW_GET_MAX_ROW(folio) )
	folio->im_first_row = TTYSW_GET_MAX_ROW(folio) - 1;


}



