#!/bin/sh
#
  cat ${1} | \
    sed -e 's%^\(.*\)\<SCROLL_\(.*\)$%\1SCROLL_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.12\
#endif\
%g' \
        -e 's%^\(.*\)\<SCROLLBAR_\(.*\)$%\1SCROLLBAR_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly Defunct, see Sect 3\.12\
#endif\
%g' \
        -e 's%\<scrollbar_create\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - This compatibility interface is VERY inefficient and\
     VERY slow\. Also, it will not be supported in later releases\. Change\
     this call to xv_create of SCROLLBAR with owner being the window the\
     scrollbar should manage, Sect 3\.12\
#endif\
scrollbar_create%g' \
        -e 's%\<selection_attributes\.h%sel_attrs\.h%g' \
        -e 's%\<selection_compat\.h%sel_compat\.h%g' \
        -e 's%\<selection_svc\.h%sel_svc\.h%g' \
	-e 's%\<struct cursor\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, change to Xv_Cursor  Sect 3\.2\
#endif\
struct cursor%g' \
	-e 's%\<struct icon\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, change to Icon  Sect 3\.2\
#endif\
struct icon%g' \
        -e 's%\<struct pixwin\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, change to Pixwin  Sect 3\.2\
#endif\
struct pixwin%g' \
        -e 's%\<struct prompt\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, use new NOTICE package, see Sect 3\.9\
#endif\
struct prompt%g' \
        -e 's%\<struct screen\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, change to Screen  Sect 3\.2\
#endif\
struct screen%g' \
        -e 's%\<struct tool\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.15\
#endif\
struct tool%g' \
	-e 's%\<sun\.h%base\.h%g' \
        -e 's%\<suntool/%xview/%g' \
        -e 's%\<sunview\.h%xview\.h%g' \
        -e 's%\<sunwindow/\>%xview/%g' \
        -e 's%^\(.*\)\<TERM\>\(.*\)$%\1TERM\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - If TERM is being used as a window type, it should be\
     changed to TERMSW for XView and you must now include termsw\.h,\
     otherwise review XVPM\
#endif\
%g' \
	-e 's%textsw_get%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Possibly defunct, see Sect 3\.13\
#endif\
textsw_get%g' \
        -e 's%\<TEXTSW_LEFT_MARGIN\>%XV_LEFT_MARGIN%g' \
        -e 's%\<TEXTSW_MENU\>%WIN_MENU%g' \
        -e 's%\<TEXTSW_RIGHT_MARGIN\>%XV_RIGHT_MARGIN%g' \
        -e 's%\<tool_find_attribute\>%attr_find%g' \
        -e 's%^\(.*\)\<tool_hs\.h\(.*\)$%\1tool_hs\.h\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, simply remove  Sect 3\.2\
#endif\
%g' \
        -e 's%\<tool_parse_one\>%xv_parse_one%g' \
        -e 's%\<tool_usage\>%xv_usage%g' 
