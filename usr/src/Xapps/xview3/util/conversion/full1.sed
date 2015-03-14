#!/bin/sh
#
cat ${1} | \
    sed -e 's%\<CANVAS_AUTO_CLEAR\>%OPENWIN_AUTO_CLEAR%g' \
        -e 's%\<FRAME_LABEL\>%XV_LABEL%g' \
        -e 's%\<ICON_FONT\>%XV_FONT%g' \
        -e 's%\<ICON_HEIGHT\>%XV_HEIGHT%g' \
        -e 's%\<ICON_LABEL\>%XV_LABEL%g' \
        -e 's%\<ICON_WIDTH\>%XV_WIDTH%g' \
        -e 's%\<MENU_HEIGHT\>%XV_HEIGHT%g' \
        -e 's%\<MENU_LEFT_MARGIN\>%XV_LEFT_MARGIN%g' \
        -e 's%\<MENU_MARGIN\>%XV_MARGIN%g' \
        -e 's%\<MENU_PARENT\>%XV_OWNER%g' \
        -e 's%\<MENU_RIGHT_MARGIN\>%XV_RIGHT_MARGIN%g' \
        -e 's%\<MENU_WIDTH\>%XV_WIDTH%g' \
        -e 's%\<panel_create_item\>%xv_create%g' \
        -e 's%\<panel_create\>%xv_create%g' \
        -e 's%\<panel_destroy_item\>%xv_destroy%g' \
        -e 's%\<panel_destroy\>%xv_destroy%g' \
        -e 's%\<panel_each_item\>%PANEL_EACH_ITEM%g' \
        -e 's%\<panel_end_each\>%PANEL_END_EACH%g' \
        -e 's%\<panel_fit_height\>%window_fit_height%g' \
        -e 's%\<PANEL_FIT_HEIGHT\>%WIN_FIT_HEIGHT%g' \
        -e 's%\<panel_fit_width\>%window_fit_width%g' \
        -e 's%\<panel_get%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - You may wish to change this to xv_get format\
     Look at panel\.h and/or See Sect 3\.1\
#endif\
panel_get%g' \
        -e 's%\<PANEL_HEIGHT\>%WIN_HEIGHT%g' \
        -e 's%\<PANEL_HORIZONTAL_SCROLLBAR\>%WIN_HORIZONTAL_SCROLLBAR%g' \
        -e 's%\<panel_item_create\>%xv_create%g' \
        -e 's%\<panel_item_destroy\>%xv_destroy%g' \
        -e 's%\<panel_item_get\>%xv_get%g' \
        -e 's%\<panel_item_set\>%xv_set%g' \
        -e 's%\<PANEL_ITEM_X\>%XV_X%g' \
        -e 's%\<PANEL_ITEM_Y\>%XV_Y%g' \
        -e 's%\<PANEL_PARENT_PANEL\>%XV_OWNER%g' \
        -e 's%\<PANEL_PIXWIN\>%WIN_PIXWIN%g' \
        -e 's%\<PANEL_SHOW_ITEM\>%XV_SHOW%g' \
        -e 's%\<PANEL_VERTICAL_SCROLLBAR%WIN_VERTICAL_SCROLLBAR%g' \
        -e 's%\<PANEL_WIDTH\>%XV_WIDTH%g'

