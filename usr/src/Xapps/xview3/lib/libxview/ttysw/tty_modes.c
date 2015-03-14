#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)tty_modes.c 20.46 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/*
 * Manages mode changes between termsw and ttysw.
 * 
 * The modes are as follows:
 * 
 * termsw cooked, echo append_only_log caret must be at the end.  all input is
 * buffered until a command completion character or interrupt character is
 * inserted. !append_only_log if caret is at the end, interpret as in
 * append_only_log. otherwise, ignore edits.
 * 
 * direct (![cooked, echo]) append_only_log caret must be at pty mark. insertion
 * into the text subwindow is refused and characters that were supposed to be
 * inserted are instead copied into the pty input buffer. !append_only_log if
 * caret is at pty mark, interpret as in append_only_log. otherwise, ignore
 * edits. ctrl-Return should move to pty mark, not end.
 * 
 * ttysw split view append_only_log read only, no caret at all. !append_only_log
 * caret anywhere, no interpretation of input.
 */


#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/signal.h>

#include <xview_private/portable.h>	/* for XV* defines and termios */

#include <xview/icon.h>
#include <xview/window.h>
#include <xview/ttysw.h>
#include <xview/termsw.h>
#include <xview/textsw.h>
#include <xview/server.h>
#include <xview_private/i18n_impl.h>
#include <xview_private/tty_impl.h>
#include <xview_private/term_impl.h>
#ifdef OW_I18N
#include <xview_private/txt_impl.h>
#endif
#include <xview/defaults.h>

extern Notify_value ttysw_itimer_expired();
extern Notify_value ttysw_pty_input_pending();
int             ttysw_waiting_for_pty_input;
Xv_private Menu_item ttysw_get_scroll_cmd_from_menu_for_ttysw();
Xv_private Menu_item ttysw_get_scroll_cmd_from_menu_for_textsw();

extern Xv_Window csr_pixwin;
extern CHAR    **image;
extern char    **screenmode;
extern void     xv_tty_imagealloc();
#ifdef SVR4
extern int doremote;
#endif SVR4


/*
 * sw should currently be a termsw, but need not be if a shelltool has been
 * started in an environment that has termsw TERM&TERMCAP entries.
 */
int
ttysw_be_ttysw(ttysw_view)
/*
 * This might not be the current view handle, for current view look at
 * current_view_public in the ttysw folio
 */
    Ttysw_view_handle ttysw_view;
{
    Ttysw_folio     ttysw = TTY_FOLIO_FROM_TTY_VIEW_HANDLE(ttysw_view);
    Textsw          textsw;
    Menu_item       disable_scroll, enable_scroll;
    Termsw_folio    termsw;
    int             off = 0;
    int             fd;
#ifdef OW_I18N
    Termsw              termsw_public;
#endif

    if (!ttysw_getopt(ttysw, TTYOPT_TEXT)) {
	return (-1);
    }
    ttysw_view = TTY_VIEW_PRIVATE_FROM_ANY_VIEW(ttysw->current_view_public);

    textsw = ttysw->current_view_public;	/* Textsw really need the
						 * public view handle */
    termsw = TERMSW_PRIVATE(TTY_PUBLIC(ttysw));

    /* If this is invoked by vi, ttysw_ansi_escape() will reset it to false */
    termsw->ok_to_enable_scroll = TRUE;

    (void) xv_set(textsw,
		  TEXTSW_READ_ONLY, TRUE,
		  0);
    (void) xv_set(xv_get(textsw, WIN_VERTICAL_SCROLLBAR),
		  XV_SHOW, FALSE, 0);

    /* Update all of the tty-dependent shadow size state. */
    csr_pixwin = (Xv_Window) ttysw->current_view_public;
    csr_resize(ttysw_view);
    /* Cannot call cim_resize(ttysw), call xv_tty_image*() instead. */
    xv_tty_free_image_and_mode();
    xv_tty_imagealloc(ttysw, FALSE);	/* Damn globals! */

    if (ioctl(ttysw->ttysw_pty, TIOCREMOTE, &off) < 0)
	perror("ioctl: TIOCREMOTE");

#   ifdef XV_USE_SVR4_PTYS
    /*
     * Since we track all tty ioctls as they come up in packetized form
     * through the master side pty, there's no need to determine the slave's
     * state here.
     */
#   else /* XV_USE_SVR4_PTYS */
    fd = (int) xv_get(textsw, TTY_TTY_FD);
#   ifdef XV_USE_TERMIOS
    (void) tcgetattr(fd, &ttysw->termios);
#   else /* XV_USE_TERMIOS */
    /*
     * XXX: This code should be encapsulated into a routine.
     */
    (void) ioctl(fd, TIOCGETP, &ttysw->sgttyb);
    (void) ioctl(fd, TIOCGETC, &ttysw->tchars);
    (void) ioctl(fd, TIOCGLTC, &ttysw->ltchars);
#   endif /* XV_USE_TERMIOS */
#   endif /* XV_USE_SVR4_PTYS */
    ttysw_drawCursor(0, 0);		/* Ensure cursor at upper-left. */

    if (xv_get(TTY_PUBLIC(ttysw), WIN_KBD_FOCUS)) {
	ttysw_restore_cursor();
    } else {
	ttysw_lighten_cursor();
    }

    if (!ttysw_waiting_for_pty_input) {
	(void) notify_set_input_func((Notify_client) (TTY_PUBLIC(ttysw)),
				 ttysw_pty_input_pending, ttysw->ttysw_pty);
	/* Wait for child process to die */
	ttysw_waiting_for_pty_input = 1;
    }
    (void) ttysw_pdisplayscreen(FALSE);

    termsw->ttysw_resized = 0;
#ifdef OW_I18N
    termsw_public = TERMSW_PUBLIC(termsw);

    xv_set(termsw_public,
		WIN_IC_PREEDIT_START,
		(XIMProc)ttysw->start_pecb_struct.callback,
		(XPointer)ttysw->start_pecb_struct.client_data,
		NULL);

    xv_set(termsw_public,
		WIN_IC_PREEDIT_DRAW,
		(XIMProc)ttysw->draw_pecb_struct.callback,
		(XPointer)ttysw->draw_pecb_struct.client_data,
		NULL);

    xv_set(termsw_public,
		WIN_IC_PREEDIT_DONE,
		(XIMProc)ttysw->done_pecb_struct.callback,
		(XPointer)ttysw->done_pecb_struct.client_data,
		NULL);

    (void) xv_set(textsw,
                  WIN_IC, ttysw->ic,
                  0);
#endif
    if (xv_get(XV_SERVER_FROM_WINDOW(TTY_PUBLIC(ttysw)), SERVER_JOURNALLING))
	xv_set(XV_SERVER_FROM_WINDOW(TTY_PUBLIC(ttysw)),
			SERVER_JOURNAL_SYNC_EVENT, 1, NULL);

    return (0);
}

/*
 * sw should currently be a ttysw, but need not be if a shelltool has been
 * started in an environment that has termsw TERM&TERMCAP entries.
 */
ttysw_be_termsw(ttysw_view)
    Ttysw_view_handle ttysw_view;

{
    Ttysw_folio     ttysw = TTY_FOLIO_FROM_TTY_VIEW_HANDLE(ttysw_view);
    Textsw          textsw = TEXTSW_FROM_TTY(ttysw);
    Textsw_view     textsw_view;
    Termsw_folio    termsw;
    int		    on = 1;
#ifdef OW_I18N
    Termsw              termsw_public;
    Textsw_folio        text_folio;
#endif

    if ((!TTY_IS_TERMSW(ttysw)) || ttysw_getopt((caddr_t) ttysw, TTYOPT_TEXT))
	return (-1);

    textsw_view = ttysw->current_view_public;	/* Textsw really need the
						 * public view handle */
    (void) xv_set(textsw_view,
		  TEXTSW_READ_ONLY, FALSE,
		  0);

    termsw = TERMSW_PRIVATE(TTY_PUBLIC(ttysw));

    (void) notify_set_itimer_func((Notify_client) (ttysw),
		  ttysw_itimer_expired, ITIMER_REAL, (struct itimerval *) 0,
				  (struct itimerval *) 0);

    (void) ttysw_clear(ttysw);

    if (termsw->ttysw_resized > 0) {
	/* ttysw swallowed resize, but now we need to let textsw_view know */
	textsw_do_resize(textsw_view);
    }
    (void) xv_set(xv_get(textsw_view, WIN_VERTICAL_SCROLLBAR),
		  XV_SHOW, TRUE, 0);
#   ifdef XV_USE_SVR4_PTYS
    /*
     * Since we track all tty ioctls as they come up in packetized form
     * through the master side pty, there's no need to determine the slave's
     * state here.
     */
#   else /* XV_USE_SVR4_PTYS */
#   ifdef XV_USE_TERMIOS
    (void) tcgetattr(ttysw->ttysw_tty, &ttysw->termios);
#   else /* XV_USE_TERMIOS */
    (void) ioctl(ttysw->ttysw_tty, TIOCGETC, &ttysw->tchars);
    (void) ioctl(ttysw->ttysw_tty, TIOCGLTC, &ttysw->ltchars);
#   endif /* XV_USE_TERMIOS */
#   endif /* XV_USE_SVR4_PTYS */
    (void) ttysw_getp((Ttysw_view_handle) ttysw_view);
    /*
     * Rather than having ttysw_getp and dynamic descendants guess whether
     * remote mode is appropriate, do it explicitly here.
     */
    (void) ioctl(ttysw->ttysw_pty, TIOCREMOTE, &on);

    if (!ttysw_waiting_for_pty_input) {
	(void) notify_set_input_func((Notify_client) (TTY_PUBLIC(ttysw)),
				     ttysw_pty_input_pending,
				     ttysw->ttysw_pty);
	/* Wait for child process to die */
	ttysw_waiting_for_pty_input = 1;
    }
    textsw_display_view(textsw_view, (Rect *) 0);
    if (xv_get(textsw, WIN_KBD_FOCUS)) {
	win_post_id((Notify_client) textsw_view, KBD_USE, NOTIFY_IMMEDIATE);
    } else {
	win_post_id((Notify_client) textsw_view, KBD_DONE, NOTIFY_IMMEDIATE);
    }
#ifdef OW_I18N
    termsw_public = TERMSW_PUBLIC(termsw);
    text_folio = (Textsw_folio)TEXTSW_PRIVATE_FROM_TERMSW(termsw_public);

    xv_set(termsw_public,
		WIN_IC_PREEDIT_START,
		(XIMProc)text_folio->start_pecb_struct.callback,
		(XPointer)text_folio->start_pecb_struct.client_data,
                NULL);

    xv_set(termsw_public,
		WIN_IC_PREEDIT_DRAW,
		(XIMProc)text_folio->draw_pecb_struct.callback,
		(XPointer)text_folio->draw_pecb_struct.client_data,
                NULL);
 
    xv_set(termsw_public,
		WIN_IC_PREEDIT_DONE,
		(XIMProc)text_folio->done_pecb_struct.callback,
		(XPointer)text_folio->done_pecb_struct.client_data,
                NULL);
 
    (void) xv_set(textsw_view,
                  WIN_IC, text_folio->ic,
                  0);
#endif
    if (xv_get(XV_SERVER_FROM_WINDOW(TTY_PUBLIC(ttysw)), SERVER_JOURNALLING))
	xv_set(XV_SERVER_FROM_WINDOW(TTY_PUBLIC(ttysw)),
			SERVER_JOURNAL_SYNC_EVENT, 1, NULL);
    return (0);
}

/*
 * Inspect the tty's modes and set cooked_echo accordingly.  The ttysw is
 * known to be a termsw.
 */
int
ttysw_getp(ttysw_view)
    Ttysw_view_handle ttysw_view;
{
    int             cooked_echo;
    Ttysw_folio     ttysw = TTY_FOLIO_FROM_TTY_VIEW_HANDLE(ttysw_view);
    Termsw_folio    termsw = TERMSW_FOLIO_FOR_VIEW(TERMSW_VIEW_PRIVATE_FROM_TTY_PRIVATE(ttysw));

    cooked_echo = termsw->cooked_echo;

#   ifndef XV_USE_SVR4_PTYS
    /*
     * We can't rely on the stored settings, so grab them afresh.
     */
#   ifdef XV_USE_TERMIOS
    (void) tcgetattr(ttysw->ttysw_tty, &ttysw->termios);
#   else /* XV_USE_TERMIOS */
    (void) ioctl(ttysw->ttysw_tty, TIOCGETP, &ttysw->sgttyb);
#   endif /* XV_USE_TERMIOS */
#   endif /* XV_USE_SVR4_PTYS */
    termsw->cooked_echo = (tty_isecho(ttysw) && tty_iscanon(ttysw));

    ttysw_cooked_echo(ttysw_view, cooked_echo, termsw->cooked_echo);
    return (0);
}

/*
 * Change cooked_echo mode.
 */
ttysw_cooked_echo(ttysw_view, old_cooked_echo, new_cooked_echo)
    Ttysw_view_handle ttysw_view;
    int             old_cooked_echo, new_cooked_echo;
{
    Ttysw_folio     ttysw = TTY_FOLIO_FROM_TTY_VIEW_HANDLE(ttysw_view);
    Textsw          textsw = TEXTSW_FROM_TTY(ttysw);
    Termsw_folio    termsw =
    TERMSW_FOLIO_FOR_VIEW(TERMSW_VIEW_PRIVATE_FROM_TEXTSW(textsw));
    Textsw_index    length;

    if (old_cooked_echo && !new_cooked_echo) {
	termsw->history_limit =
	    (int) xv_get(textsw, TEXTSW_HISTORY_LIMIT);
	(void) xv_set(textsw, TEXTSW_HISTORY_LIMIT, 0, 0);
    } else if (!old_cooked_echo && new_cooked_echo) {
	(void) xv_set(textsw,
		      TEXTSW_HISTORY_LIMIT, termsw->history_limit,
		      0);
	/*
	 * if insertion point == pty insert point, move it to the end, doing
	 * whatever is necessary to the read_only_mark.
	 */
	if (textsw_find_mark(textsw, termsw->pty_mark) ==
		(int) xv_get(textsw, TEXTSW_INSERTION_POINT)) {
	    if (termsw->append_only_log) {
		/* Remove read_only_mark to allow insert */
		textsw_remove_mark(textsw, termsw->read_only_mark);
	    }
#ifdef OW_I18N
            length = (int) xv_get(textsw, TEXTSW_LENGTH_WC);
#else
	    length = (int) xv_get(textsw, TEXTSW_LENGTH);
#endif
	    (void) xv_set(textsw,
			  TEXTSW_INSERTION_POINT, length,
			  0);
	    if (termsw->append_only_log) {
		termsw->read_only_mark =
		    textsw_add_mark(textsw, length, TEXTSW_MARK_READ_ONLY);
	    }
	}
    }
    if (old_cooked_echo && !new_cooked_echo && termsw->cmd_started)
	ttysw_scan_for_completed_commands(ttysw_view, -1, 0);
}
