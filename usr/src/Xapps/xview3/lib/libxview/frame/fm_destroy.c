#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)fm_destroy.c 20.46 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/i18n_impl.h>
#include <xview_private/fm_impl.h>
#include <xview_private/draw_impl.h>
#include <xview/notice.h>

static int      frame_confirm_destroy();
static void     frame_free();

static int	frame_notice_key;

/*
 * Destroy the frame struct, its subwindows, and its subframes. If any
 * subwindow or subframe vetoes the destroy, return NOTIFY_IGNORED. If
 * confirmation is enabled, prompt the user.
 */
Pkg_private int
frame_destroy(frame_public, status)
    Frame             frame_public;
    Destroy_status    status;
{
    Frame_class_info *frame = FRAME_CLASS_PRIVATE(frame_public);
    Xv_Window         child;
    Xv_Drawable_info *info;
    int               is_subframe = (int)xv_get(xv_get(frame_public, WIN_OWNER),
					        XV_IS_SUBTYPE_OF, FRAME_CLASS);

    DRAWABLE_INFO_MACRO(frame_public, info);

    /* Unmapped the frame that is about to be destroyed. */
    if ((status == DESTROY_CLEANUP) || (status == DESTROY_PROCESS_DEATH))
	win_remove(frame_public);

    /* destroy the subframes */
    FRAME_EACH_SUBFRAME(frame, child)
	if (notify_post_destroy(child, status, NOTIFY_IMMEDIATE) != XV_OK)
	    return XV_ERROR;
    FRAME_END_EACH

    /* Since this frame is going away, set a flag to tell
     * window_destroy_win_struct not to destroy any subwindows...when the
     * frame window is destroyed the server will destroy all subwindows for us.
     */
    if ((status != DESTROY_CHECKING) && (status != DESTROY_SAVE_YOURSELF))
	window_set_parent_dying();

    /* destroy the subwindows */
    FRAME_EACH_SUBWINDOW(frame, child)
	if (notify_post_destroy(child, status, NOTIFY_IMMEDIATE) != XV_OK)
	    return XV_ERROR;
    FRAME_END_EACH

    if ((status != DESTROY_CHECKING) && (status != DESTROY_SAVE_YOURSELF)) {
	window_unset_parent_dying();
	/*
	 * Conditionally stop the notifier.  This is a special case so that
	 * single tool clients don't have to interpose in from of tool_death
	 * in order to notify_remove(...other clients...) so that
	 * notify_start will return.
	 */
        if (!is_subframe)
	    if (--frame_notify_count == 0)
	        (void) notify_stop();
    } else if (status != DESTROY_SAVE_YOURSELF) {
	/* subframe does not need confirmation */
	return ((is_subframe || frame_confirm_destroy(frame) == XV_OK)
		? XV_OK : XV_ERROR);
    }
    if (status == DESTROY_CLEANUP) {	/* waste of time if ...PROCESS_DEATH */
	if (frame->footer != NULL)
	  xv_destroy(frame->footer);
	if (frame->left_footer != NULL)
	  free(frame->left_footer);
	if (frame->right_footer != NULL)
	  free(frame->right_footer);
	if (frame->focus_window != NULL)  {
	    Server_image	image;
	    
	    image = xv_get(frame->focus_window, 
				XV_KEY_DATA, FRAME_FOCUS_UP_IMAGE);
	    if (image)  {
		xv_destroy(image);
	    }

	    image = xv_get(frame->focus_window, 
				XV_KEY_DATA, FRAME_FOCUS_RIGHT_IMAGE);
	    if (image)  {
		xv_destroy(image);
	    }

	    xv_destroy(frame->focus_window);
	    frame->focus_window = (Xv_window)NULL;
	}
	frame_free(frame);
    }
    return XV_OK;
}

/*
 * Return XV_OK if confirmation is allowed, and the user says yes, or if
 * confirmation is not allowed. Reset confirmation to be allowed.
 */
static int
frame_confirm_destroy(frame)
    Frame_class_info *frame;
{
    Xv_object       window = FRAME_CLASS_PUBLIC(frame);
    Xv_Notice       frame_notice;
    int             result;

    if (status_get(frame, no_confirm)) {
	status_set(frame, no_confirm, FALSE);
	return XV_OK;
    }

    if (frame_is_iconic(frame))
	window = (Xv_object) frame->icon;

        if (!frame_notice_key)  {
	    frame_notice_key = xv_unique_key();
        }

        frame_notice = (Xv_Notice)xv_get((Frame)window, 
                                XV_KEY_DATA, frame_notice_key, 
				NULL);
        if (!frame_notice)  {
            frame_notice = xv_create((Frame)window, NOTICE,
                        NOTICE_LOCK_SCREEN, FALSE,
                        NOTICE_BLOCK_THREAD, TRUE,
                        NOTICE_MESSAGE_STRINGS,
		            XV_MSG("Are you sure you want to Quit?"),
                        0,
			NOTICE_BUTTON_YES, 
			    XV_MSG("Confirm"),
			NOTICE_BUTTON_NO, 
			    XV_MSG("Cancel"),
			NOTICE_NO_BEEPING, 1,
			NOTICE_STATUS, &result,
                        XV_SHOW, TRUE,
                        0);

            xv_set((Frame)window, 
                XV_KEY_DATA, frame_notice_key, frame_notice,
                NULL);
        }
        else  {
            xv_set(frame_notice, 
                NOTICE_LOCK_SCREEN, FALSE,
                NOTICE_BLOCK_THREAD, TRUE,
                NOTICE_MESSAGE_STRINGS,
		    XV_MSG("Are you sure you want to Quit?"),
                0,
                NOTICE_BUTTON_YES, XV_MSG("Confirm"),
                NOTICE_BUTTON_NO, XV_MSG("Cancel"),
                NOTICE_NO_BEEPING, 1,
		NOTICE_STATUS, &result,
                XV_SHOW, TRUE, 
                NULL);
        }

    /* BUG ALERT Should not abort if alerts failed */
    if (result == NOTICE_FAILED)
	xv_error((Xv_opaque)frame,
	         ERROR_STRING, XV_MSG("Notice failed on attempt to destroy frame."),
		 ERROR_PKG, FRAME,
		 0);
    return ((result == NOTICE_YES) ? XV_OK : XV_ERROR);
}


/*
 * free the frame struct and all its resources.
 */
static void
frame_free(frame)
    Frame_class_info *frame;
{
    Frame_accelerator *accel;
    Frame_accelerator *next_accel;

    /* Free frame struct */
    if (frame->label)
    	xv_free(frame->label);
    for (accel = frame->accelerators; accel; accel = next_accel) {
	next_accel = accel->next;
	xv_free(accel);
    }
    free((char *) frame);
}
