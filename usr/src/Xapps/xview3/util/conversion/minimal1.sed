#!/bin/sh
#
  cat ${1} | \
    sed -e 's%\<alert\.h%notice\.h%g' \
	-e 's%\<ALERT_POSITION\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Alert-Positioning is now defunct\
     Use NOTICE_FOCUS_XY instead\. See XVPM Chap 12\
#endif\
NOTICE_FOCUS_XY%g' \
        -e 's%\<ALERT_%NOTICE_%g' \
        -e 's%\<alert_prompt%notice_prompt%g' \
	-e 's%\<ATTR_COL%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_col/xv_cols instead\. See XVPM Appendix B\
     and Sect 7\.3\.2\
#endif\
ATTR_COL%g' \
        -e 's%\<ATTR_ROW%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_row/xv_rows instead\. See XVPM Appendix B\
     and Sect 7\.3\.2\
#endif\
ATTR_ROW%g' \
        -e 's%\<cms_rainbow\.h%cmsrainbow\.h%g' \
	-e 's%\<Cursor\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Due to a name space clash with Xlib, the SunView\
     data-type Cursor is now Xv_Cursor in XView\
#endif\
Cursor%g' \
	-e 's%\<DEFINE_CURSOR_FROM_IMAGE\>%DEFINE_CURSOR_FROM_IMAGE\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Cursors are now XView objects that must be created,\
     read Sect 3\.5 on how to convert to the new API\
#endif\
%g' \
        -e 's%\<DEFINE_ICON_FROM_IMAGE\>%DEFINE_ICON_FROM_IMAGE\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Icons are now XView objects that must be created, read\
     Sect 3\.8 on how to convert to the new API\
#endif\
%g' \
        -e 's%\<CANVAS_MARGIN\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Look at XVPM Chapter 5 on canvases, this attr now\
     applies to the canvas view margin\
#endif\
CANVAS_MARGIN%g' \
        -e 's%\<CANVAS_PIXWIN\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Pixwins are now logically replaced with paint\
     windows\. Read XVPM Chapter 5\
#endif\
CANVAS_PIXWIN%g' \
        -e 's%\<CANVAS_RETAINED\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Now only a hint, must be prepared to repaint,\
     read Sect 3\.4\
#endif\
CANVAS_RETAINED%g' \
	-e 's%\<CMDSW\>%TERMSW%g' \
        -e 's%^\(.*\)\<CURSOR_\(.*\)$%\1CURSOR_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Many cursor attributes are no longer supported,\
     read Sect 3\.5 and 4\.2 to check if this one\
#endif\
%g' \
        -e 's%\<emptysubwindow%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer supported in XView\
#endif\
emptysubwindow%g' \
        -e 's%^\(.*\)\<esw_\(.*\)$%\1esw_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer supported in XView\
#endif\
%g' \
        -e 's%\<FRAME_ARGC_PTR_ARGV\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Make sure to use xv_init to process the attrs first,\
     XVPM 3\.2\
#endif\
FRAME_ARGC_PTR_ARGV%g' \
        -e 's%\<FRAME_ARGS\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Make sure to use xv_init tto process the attrs first,\
     XVPM 3\.2\
#endif\
FRAME_ARGS%g' \
        -e 's%\<FRAME_EMBOLDEN_LABEL\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, simply remove\
#endif\
FRAME_EMBOLDEN_LABEL%g' \
        -e 's%\<FRAME_NTH_WINDOW\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Iterate using exitsing attrs FRAME_NTH_SUBFRAME and\
     FRAME_NTH_SUBWINDOW instead, XVPM 4\.4\.4\
#endif\
FRAME_NTH_WINDOW%g' \
        -e 's%\<FRAME_PROPS_ACTIVE\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, property item applies to wmgr in client under\
     OPEN LOOK\
#endif\
FRAME_PROPS_ACTIVE%g' \
        -e 's%\<FRAME_PROPS_PROC\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Properties are now the full responsibility of the\
     application program, no notification from window menu\. Will want to add\
     props button or look for ACTION_PROPS to your app\
#endif\
FRAME_PROPERTIES_PROC%g' \
        -e 's%\<FRAME_SUBWINDOWS_ADJUSTABLE\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct\
#endif\
FRAME_SUBWINDOWS_ADJUSTABLE%g' \
        -e 's%^\(.*\)\<fs_\(.*\)$%\1fs_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use new FULLSCREEN package,\
     see XVPM 15\.5 for replacement\
#endif\
%g' \
        -e 's%^\(.*\)\<fullscreen_\(.*\)$%\1fullscreen_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use new FULLSCREEN package,\
     see XVPM 15\.5 for replacement\
#endif\
%g' \
        -e 's%^\(.*\)\<gfxsw\.h\(.*\)$%\1gfxsw\.h\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct\. GFX subwindows not supported\
#endif\
%g' \
        -e 's%^\(.*\)\<gfx_\(.*\)$%\1gfx_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct\. GFX subwindows not supported\
#endif\
%g' \
        -e 's%\<HELP_DATA\>%XV_HELP_DATA%g' \
	-e 's%\<ic_mpr%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Icon structs no longer valid,\
     use the ICON_IMAGE attr instead\
#endif\
ic_mpr%g' \
	-e 's%^\(.*\)\<im_\(.*\)$%\1im_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Handling of input masks have changed,\
     see Sect 3\.2 for any necessary changes\
#endif\
%g' \
        -e 's%\<input_read_event\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_input_readevent instead, see XVPM 6.3.2\
#endif\
input_read_event%g' \
	-e 's%\<KBD_REQUEST\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Handled by wmgr in X, simply remove it  Sect 3\.2\
#endif\
KBD_REQUEST%g' \
        -e 's%\<LINT_CAST\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Now defunct, simply remove it  Sect 3\.2\
#endif\
LINT_CAST%g' \
        -e 's%\<LOC_RGN\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer valid in X, simply remove it \
     Sect 3\.2 and 3\.4\
#endif\
LOC_RGN%g' \
        -e 's%\<LOC_STILL\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer valid in X, simply remove it \
     Sect 3\.2\
#endif\
LOC_STILL%g' \
        -e 's%\<LOC_TRAJECTORY\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer valid in X, simply remove it \
     Sect 3\.2\
#endif\
LOC_TRAJECTORY%g'

