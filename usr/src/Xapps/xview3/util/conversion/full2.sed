#!/bin/sh
#
  cat ${1} | \
    sed -e 's%^\(.*\)\<RT_STANDARD\>\(.*\)$%\1RT_STANDARD\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - This is raw pixrect image in 68000 byte order so must\
     include rasterfile\.h and/or consider whether you really want to use\
     this still\
#endif\
%g' \
        -e 's%\<TEXTSW_FONT\>%XV_FONT%g' \
        -e 's%^\(.*\)\<WIN_CONSUME_KBD_\(.*\)$%\1WIN_CONSUME_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Events are no longer split between kbd and pick,\
     use WIN_IGNORE_EVENT or WIN_IGNORE_EVENTS instead and combine these if \
     both kbd and pick specified  Sect 3\.14\
#endif\
%g' \
        -e 's%^\(.*\)\<WIN_CONSUME_PICK_\(.*\)$%\1WIN_CONSUME_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Events are no longer split between kbd and pick, use\
     WIN_IGNORE_EVENT or WIN_IGNORE_EVENTS instead and combine these if both\
     kbd and pick specified  Sect 3\.14\
#endif\
%g' \
        -e 's%\<WIN_FONT\>%XV_FONT%g' \
        -e 's%\<WIN_HEIGHT\>%XV_HEIGHT%g' \
        -e 's%^\(.*\)\<WIN_IGNORE_KBD_\(.*\)$%\1WIN_IGNORE_KBD_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Events are no longer split between kbd and pick, use\
     WIN_IGNORE_EVENT or WIN_IGNORE_EVENTS instead and combine these if both \
     kbd and pick specified  Sect 3\.14\
#endif\
%g' \
        -e 's%^\(.*\)\<WIN_IGNORE_PICK_\(.*\)$%\1WIN_IGNORE_PICK_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Events are no longer split between kbd and pick, use\
     WIN_IGNORE_EVENT or WIN_IGNORE_EVENTS instead and combine these if both \
     kbd and pick specified  Sect 3\.14\
#endif\
%g' \
        -e 's%\<WIN_LEFT_MARGIN\>%XV_LEFT_MARGIN%g' \
        -e 's%\<WIN_NAME\>%XV_NAME%g' \
        -e 's%\<WIN_OWNER\>%XV_OWNER%g' \
        -e 's%^\(.*\)\<WIN_KBD_INPUT_\(.*\)$%\1WIN_INPUT_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Events are no longer split between kbd and pick, use\
     WIN_INPUT_ instead and combine these if both kbd and pick specified\
     Sect 4\.2\
#endif\
%g' \
        -e 's%^\(.*\)\<WIN_PICK_INPUT_\(.*\)$%\1WIN_INPUT_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Events are no longer split between kbd and pick, use\
     WIN_INPUT_ instead and combine these if both kbd and pick specified\
     Sect 4\.2\
#endif\
%g' \
        -e 's%\<WIN_RIGHT_MARGIN\>%XV_RIGHT_MARGIN%g' \
        -e 's%\<WIN_TOP_MARGIN\>%XV_TOP_MARGIN%g' \
        -e 's%\<WIN_WIDTH\>%XV_WIDTH%g' 
