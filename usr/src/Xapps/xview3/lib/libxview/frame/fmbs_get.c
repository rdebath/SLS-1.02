#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)fmbs_get.c 1.26 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

#include <xview_private/fm_impl.h>
#include <xview_private/frame_base.h>

/* ARGSUSED */
Pkg_private     Xv_opaque
frame_base_get_attr(frame_public, status, attr, valist)
    Frame           frame_public;
    int            *status;
    Frame_attribute attr;
    va_list         valist;
{
    register Frame_base_info *frame = FRAME_BASE_PRIVATE(frame_public);

    switch (attr) {

      case FRAME_WM_COMMAND_ARGV:
	attr = (Frame_attribute) ATTR_NOP(attr);
	return (Xv_opaque) frame->cmd_line_strings;

      case FRAME_WM_COMMAND_ARGC:
	attr = (Frame_attribute) ATTR_NOP(attr);
	return (Xv_opaque) frame->cmd_line_strings_count;

      case FRAME_PROPERTIES_PROC:
	attr = (Frame_attribute) ATTR_NOP(attr);
	return (Xv_opaque) frame->props_proc;

      case FRAME_SHOW_LABEL:
	attr = (Frame_attribute) ATTR_NOP(attr);
	return (Xv_opaque) status_get(frame, show_label);

      case FRAME_SHOW_RESIZE_CORNER:
	attr = (Frame_attribute) ATTR_NOP(attr);
	return (Xv_opaque) status_get(frame, show_resize_corner);

      case FRAME_SCALE_STATE:
	attr = (Frame_attribute) ATTR_NOP(attr);
	/*
	 * WAIT FOR NAYEEM return (Xv_opaque)
	 * window_get_rescale_state(frame_public);
	 */
	return (Xv_opaque) 0;

#ifdef OW_I18N
      case FRAME_INPUT_WINDOW:
        {
            Frame_class_info *frame_class = FRAME_CLASS_FROM_BASE(frame);
 
           attr = (Frame_attribute) ATTR_NOP(attr);
            return (Xv_opaque) frame_class->input_window;
        }
#endif OW_I18N

      default:
	*status = XV_ERROR;
	return (Xv_opaque) 0;
    }
}
