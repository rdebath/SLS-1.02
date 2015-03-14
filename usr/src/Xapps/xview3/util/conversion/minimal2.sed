#!/bin/sh
#
  cat ${1} | \
    sed -e 's%\<MENU_BOXED\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, simply remove  Sect 4\.2\
#endif\
MENU_BOXED%g' \
        -e 's%\<menu_display\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer supported, see Sect 3\.9\
#endif\
menu_display%g' \
        -e 's%\<menu_show\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer blocks until menu taken down, should specify\
     a MENU_ACTON_PROC for each menu item or at least a MENU_DONE_PROC,\
     see Sect 3\.9 and XVPM Chap 11\
#endif\
menu_show%g' \
        -e 's%\<menu_prompt\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, use new NOTICE package  See XVPM Chap 12\
#endif\
menu_prompt%g' \
        -e 's%\<MENU_FONT\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Compatibility attr, use XV_FONT instead  Sect 3\.1\
#endif\
MENU_FONT%g' \
        -e 's%\<MENU_SELECTED\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Compatibilty, XVPM Appendix A\
#endif\
MENU_SELECTED%g' \
        -e 's%\<MENU_STAY_UP\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, simply remove  Sect 4\.2\
#endif\
MENU_STAY_UP%g' \
        -e 's%^\(.*\)\<msgsw_\(.*\)$%\1msgsw_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - No longer supported in XView, see Sect 3\.15\
#endif\
%g' \
	-e 's%\<panel_button_image\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Still supported, but for OPEN LOOK buttons,\
     should use PANEL_LABEL_STRING for PANEL_BUTTON items, XVPM 7\.7\
#endif\
panel_button_image%g' \
        -e 's%\<PANEL_CURSOR\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use XV_CURSOR instead  XVPM Chap 13\
#endif\
PANEL_CURSOR%g' \
        -e 's%\<PANEL_LABEL_IMAGE\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - If being used with panel_button_string,\
     use PANEL_LABEL_STRING instead, XVPM 7\.7\
#endif\
PANEL_LABEL_IMAGE%g' \
        -e 's%\<panel_make%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.10\
#endif\
panel_make%g' \
        -e 's%^\(.*\)\<PANEL_MENU_\(.*\)$%\1PANEL_MENU_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct, see Sect 3\.10\
#endif\
%g' \
        -e 's%\<panel_set%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - If this is panel_set, change to xv_set,\
     Sect 3\.10\
#endif\
panel_set%g' \
        -e 's%\<pick_mask%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Notion of pick and kbd masks are gone,\
     only one input mask, see XVPM Chap 6\
#endif\
pick_mask%g' \
        -e 's%\<pf_default%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - If function pf_default use xv_pf_default\.  If struct value ignore\. Remember to extern it\.\
#endif\
pf_default%g' \
	-e 's%\<pf_open\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_open instead Remember to extern it\
#endif\
pf_open%g' \
        -e 's%\<pf_open_private\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_open_private instead\. Remember to extern it\
#endif\
pf_open_private%g' \
        -e 's%\<pf_close\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_close instead, remember to extern it\
#endif\
pf_close%g' \
        -e 's%\<pf_text\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_text instead, remember to extern it\
#endif\
pf_text%g' \
        -e 's%\<pf_textbatch%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_textbatch instead, remember to extern it\
#endif\
pf_textbatch%g' \
        -e 's%\<pf_textbound%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_textbound instead, remember to extern it\
#endif\
pf_textbound%g' \
        -e 's%\<pf_textwidth\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_textwidth instead, remember to extern it\
#endif\
pf_textwidth%g' \
        -e 's%\<pf_ttext\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Use xv_pf_ttext instead, remember to extern it\
#endif\
pf_ttext%g' \
        -e 's%\<Pw_attribute_value\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
Pw_attribute_value%g' \
        -e 's%\<pw_close\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_close%g' \
        -e 's%\<pw_damaged%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_damaged%g' \
        -e 's%^\(.*\)\<PW_DBL_\(.*\)$%\1PW_DBL_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
%g' \
        -e 's%^\(.*\)\<pw_dbl_\(.*\)$%\1pw_dbl_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
%g' \
        -e 's%^\(.*\)\<Pw_dbl_\(.*\)$%\1Pw_dbl_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
%g' \
        -e 's%\<pw_donedamaged%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_donedamaged%g' \
        -e 's%\<pw_exposed%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_exposed%g' \
        -e 's%\<pw_open%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_open%g' \
        -e 's%\<pw_pfsysopen%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct Simply remove, see Sect 3\.6\
#endif\
pw_pfsysopen%g' \
        -e 's%\<pw_preparesurface%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_preparesurface%g' \
        -e 's%\<pw_putcolormap%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Need to set WIN_DYNAMIC_VISUAL on window,\
     see Sect 3\.11\
#endif\
pw_putcolormap%g' \
        -e 's%\<pw_region%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_region%g' \
        -e 's%\<pw_repairretained%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_repairretained%g' \
        -e 's%\<pw_restrict%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_restrict%g' \
        -e 's%^\(.*\)\<pw_set_\(.*\)$%\1pw_set_\2\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
%g' \
        -e 's%\<pw_traprop\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_traprop%g' \
        -e 's%\<pw_use_fast_mono\>%\
#ifdef XVIEW_COMMENT\
     XView CONVERSION - Defunct See Sect 3\.11\
#endif\
pw_use_fast_mono%g'

