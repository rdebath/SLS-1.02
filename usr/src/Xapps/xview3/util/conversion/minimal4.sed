#!/bin/sh
#
  cat ${1} | \
    sed -e 's%\<xv_get_value%panel_get_value%g' \
        -e 's%\<walkmenu\.h%openmenu\.h%g' \
        -e 's%\<WIN_BOTTOM_MARGIN\>%XV_BOTTOM_MARGIN%g' \
        -e 's%\<WIN_DEVICE_NAME\>%XV_XNAME%g' \
        -e 's%\<WIN_DEVICE_NUMER\>%XV_DEVICE_NUMER%g' \
        -e 's%\<win_environ\.h%win_env\.h%g' \
        -e 's%\<WIN_FD%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
WIN_FD%g' \
        -e 's%\<win_fdto%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.1\
#endif\
win_fdto%g' \
        -e 's%\<win_findintersect%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, use win_pointer_under instead\
#endif\
win_findintersect%g' \
        -e 's%etgfxwindow%etgfxwindow\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, GFX subwindows are no longer supported\
     See Sect 3\.2\
#endif\
%g' \
        -e 's%\<win_getcursor\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_getcursor%g' \
        -e 's%\<win_getowner\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_getowner%g' \
        -e 's%\<win_getparentwindow\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_getparentwindow%g' \
        -e 's%\<win_getsavedrect%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_getsavedrect%g' \
        -e 's%\<win_getscreenposition%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_getscreenposition%g' \
        -e 's%\<win_getuserflag%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_getuserflag%g' \
        -e 's%\<win_get_button_order%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_button_order%g' \
        -e 's%\<win_get_designee%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
win_get_designee%g' \
        -e 's%\<win_get_event_timeout%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_event_timeout%g' \
        -e 's%\<win_get_fd%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_fd%g' \
        -e 's%\<win_get_focus_event%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_focus_event%g' \
        -e 's%\<win_get_kbd_mask%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
win_get_kbd_mask%g' \
        -e 's%\<win_get_pick_mask%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
win_get_pick_mask%g' \
        -e 's%\<win_get_pixwin%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_pixwin%g' \
        -e 's%\<win_get_scaling%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_scaling%g' \
        -e 's%\<win_get_swallow_event%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_swallow_event%g' \
        -e 's%\<win_get_tree_layer%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_get_tree_layer%g' \
        -e 's%\<win_initscreenfromargv%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_initscreenfromargv%g' \
        -e 's%\<win_insertblanket%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_insertblanket%g' \
        -e 's%\<win_is_input_device%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_is_input_device%g' \
        -e 's%\<win_numbertoname%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
win_numbertoname%g' \
        -e 's%\<win_release_event_lock%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_release_event_lock%g' \
        -e 's%\<win_removeblanket%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_removeblanket%g' \
        -e 's%\<win_remove_input_device%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_remove_input_device%g' \
        -e 's%\<win_screendestroy%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_screendestroy%g' \
        -e 's%\<win_screenget%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_screenget%g' \
        -e 's%\<win_screennew%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_screennew%g' \
        -e 's%\<win_setcursor%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setcursor%g' \
        -e 's%\<win_setkbd%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setkbd%g' \
        -e 's%\<win_setms%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setms%g' \
        -e 's%\<win_setowner%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setowner%g' \
        -e 's%\<win_setparentwindow%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setparentwindow%g' \
        -e 's%\<win_setsavedrect%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setsavedrect%g' \
        -e 's%\<win_setscreenposition%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setscreenposition%g' \
        -e 's%\<win_setuserflag%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_setuserflag%g' \
        -e 's%\<win_set_button_order%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_set_button_order%g' \
        -e 's%\<win_set_designee%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_set_designee%g' \
        -e 's%\<win_set_event_timeout%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_set_event_timeout%g' \
        -e 's%\<win_set_focus_event%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_set_focus_event%g' \
        -e 's%\<win_set_input_device%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_set_input_device%g' \
        -e 's%\<win_set_kbd_mask%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
win_set_kbd_mask%g' \
        -e 's%\<in_set_pick_mask%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
win_set_pick_mask%g' \
        -e 's%\<win_set_swallow_event%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.14\
#endif\
win_set_swallow_event%g' \
	-e 's%\<Window\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Due to a name space clash with Xlib, the SunView\
     data-type Window is now Xv_Window in XView\
#endif\
Window%g' \
        -e 's%\<window_fit\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - If attempting to fit a frame around multiple subwindows\
     where at least one is a text, tty or term subwindow, you should explicitly\
     xv_set the size of the windows first, then call window_fit, see window_fit\
     in documentation\
#endif\
window_fit%g' \
        -e 's%\<window_default_event_proc%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, remove reference and write own event proc\
#endif\
window_default_event_proc%g' \
    	-e 's%\<window_create\>%xv_create%g' \
        -e 's%\<window_destroy\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - If this is being called from within an event/notify\
     call-back proc, call func xv_destroy_safe instead\
#endif\
xv_destroy%g' \
        -e 's%\<window_get\>%xv_get%g' \
        -e 's%\<window_set\>%xv_set%g' \
        -e 's%^\(.*\)\<wmgr_\(.*\)$%\1wmgr_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.14\
#endif\
%g' 

