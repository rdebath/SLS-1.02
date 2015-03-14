/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Unique user interface builder class identifiers
 */

#ifndef ibclasses_h
#define ibclasses_h

#include <Unidraw/classes.h>

#ifndef MAKEDEPEND

#define PRECISEMOVE_CMD          1011
#define PRECISEROTATE_CMD        1012
#define PRECISESCALE_CMD         1013

#define ABOUT_CMD                2999
#define NEWVIEW_CMD              3000
#define PLACE_CMD		 3001
#define STREDIT_COMP             3002
#define INTERACTOR_COMP          3003
#define SHAPE_VAR                3005
#define CANVAS_VAR               3006
#define BUTTON_COMP              3008
#define BUTTONSTATE_VAR          3009
#define GLUEVISIBILITY_CMD       3010
#define SCENE_COMP		 3011
#define BOX_COMP		 3012
#define HBOX_COMP		 3013
#define VBOX_COMP		 3014
#define GLUE_COMP                3015
#define SHAPER_COMP		 3016
#define INFO_CMD                 3017
#define PUSHBUTTON_GRAPHIC       3018
#define RADIOBUTTON_GRAPHIC      3019
#define CHECKBOX_GRAPHIC         3020
#define CODE_VIEW                3022
#define CODE_CMD                 3023
#define BOX_CODE                 3024
#define HVCOMP                   3025
#define BORDER_COMP              3026
#define SCROLLER_COMP            3027
#define MESSAGE_COMP             3028
#define MESSAGE_GRAPHIC          3029
#define STREDIT_GRAPHIC          3030
#define FBROWSER_COMP            3031
#define STRBROWSER_GRAPHIC       3032
#define INSTANCENAME_VAR         3033
#define ADJUSTER_GRAPHIC         3034
#define LMOVER_GRAPHIC           3035
#define RMOVER_GRAPHIC           3036
#define UMOVER_GRAPHIC           3037
#define DMOVER_GRAPHIC           3038
#define ENLARGER_GRAPHIC         3039
#define REDUCER_GRAPHIC          3040
#define ADJUSTER_COMP            3041
#define BITMAP_GRAPHIC           3044
#define BITMAP_COMP              3045
#define PANNER_GRAPHIC           3046
#define GRBLOCK_GRAPHIC          3047

#define TEXTEDIT_COMP            3048
#define FRAME_COMP		 3049
#define MARGINFRAME_COMP	 3050
#define FRAME_GRAPHIC	 	 3051
#define SHADOWFRAME_GRAPHIC	 3055
#define MARGINFRAME_GRAPHIC	 3056

#define MONOSCENE_COMP		 3057
#define MONOSCENE_CMD		 3058
#define MONOSCENE_CLASS		 3059
#define DIALOG_CLASS		 3060

#define PROCNAME_VAR		 3042
#define BUTTONSHAREDNAME	 3043

#define DECK_COMP		 3061
#define EXAMINE_TOOL             3062
#define RELATE_TOOL		 3064
#define MENUBAR_COMP		 3066
#define IBGRAPHIC_COMP_TOOL	 3067
#define MENUITEM_COMP            3068
#define MENUITEM_GRAPHIC         3069
#define MENUBODY_COMP		 3071
#define PULLMENU_COMP	 	 3072
#define PDMENU_GRAPHIC	 	 3073
#define PRMENU_GRAPHIC	 	 3074
#define SLIDER_COMP	 	 3075
#define SLIDER_GRAPHIC	 	 3076
#define PANNER_COMP              3077
#define REORDER_CMD              3081
#define POPUPMENU_COMP		 3082
#define VIEWPORT_COMP            3083
#define VIEWPORT_GRAPHIC         3084
#define SUBCLASSNAME_VAR         3085
#define IBNAME_VAR               3086
#define MEMBERNAME_VAR           3087
#define STRBROWSER_COMP          3088
#define NARROW_TOOL              3089
#define BOOLEANSTATE_VAR         3090
#define FBROWSER_VAR             3091
#define GRBLOCK_COMP             3092

#define ITEXT_COMP               3093
#define ILINE_COMP               3094
#define IMULTILINE_COMP          3095
#define ISPLINE_COMP             3096
#define IRECT_COMP               3097
#define IELLIPSE_COMP            3098
#define IPOLYGON_COMP            3099
#define ICLOSEDSPLINE_COMP       3100
#define IGRAPHIC_COMPS           3101
#define IRASTER_COMP             3102
#define ISTENCIL_COMP            3103
#define EDITOR_COMP              3104
#define IBVIEWER_COMP            3105
#define IBVIEWER_GRAPHIC         3106
#define PANELCONTROL_COMP        3107
#define PANELCONTROL_GRAPHIC     3108
#define COMMANDCONTROL_COMP      3109
#define COMMANDCONTROL_GRAPHIC   3110
#define IKEY_COMP                3111
#define MEMBERSHAREDNAME         3112
#define IBIMPORT_CMD             3113
#define GETCLONES_CMD            3114
#define ID_VAR                   3115
#define GRAPHICCODE_VIEW         3116
#define IPS_VIEW   	         3117

#define NEWTOOL_CMD              2008
#define TOOLS_CMD                2009
#define EXE_CMD                  2010
#define RELATE_CMD               2011
#define EDIT_CMD	 	 2012
#define PROPS_CMD	 	 2013
#define NAVIGATE_CMD	 	 2014
#define SCENE_CMD	 	 2015
#define TAB_CMD 	 	 2016
#define IDRAW_CMD 	 	 2017

#define GETFIREWALL_CMD  	 2018
#define GETTOPLEVEL_CMD  	 2019
#define GETCONFLICT_CMD  	 2020
#define GETNAMEVARS_CMD  	 2021
#define GETCLASSLIST_CMD  	 2022
#define SCAN_CMD   	         2023
#define TAB_TOOL   	         2024
#define COMPCHECK_CMD            2025
#define IDMAP_CMD                2026

#define ADJUSTER_CODE            Combine(ADJUSTER_COMP, CODE_VIEW)
#define ADJUSTER_VIEW            Combine(ADJUSTER_COMP, COMPONENT_VIEW)
#define BORDER_CODE              Combine(BORDER_COMP, CODE_VIEW)
#define BORDER_VIEW              Combine(BORDER_COMP, COMPONENT_VIEW)
#define BUTTON_CODE              Combine(BUTTON_COMP, CODE_VIEW)
#define BUTTON_VIEW              Combine(BUTTON_COMP, COMPONENT_VIEW)
#define BITMAP_VIEW              Combine(BITMAP_COMP, COMPONENT_VIEW)
#define DECK_CODE                Combine(DECK_COMP, CODE_VIEW)
#define DECK_VIEW                Combine(DECK_COMP, COMPONENT_VIEW)
#define FBROWSER_CODE            Combine(FBROWSER_COMP, CODE_VIEW)
#define FBROWSER_VIEW            Combine(FBROWSER_COMP, COMPONENT_VIEW)
#define STRBROWSER_CODE          Combine(STRBROWSER_COMP, CODE_VIEW)
#define STRBROWSER_VIEW          Combine(STRBROWSER_COMP, COMPONENT_VIEW)
#define GRBLOCK_CODE             Combine(GRBLOCK_COMP, CODE_VIEW)
#define GRBLOCK_VIEW             Combine(GRBLOCK_COMP, COMPONENT_VIEW)
#define FRAME_CODE               Combine(FRAME_COMP, CODE_VIEW)
#define FRAME_VIEW               Combine(FRAME_COMP, COMPONENT_VIEW)
#define MARGINFRAME_CODE         Combine(MARGINFRAME_COMP, CODE_VIEW)
#define MARGINFRAME_VIEW         Combine(MARGINFRAME_COMP, COMPONENT_VIEW)
#define GLUE_CODE                Combine(GLUE_COMP, CODE_VIEW)
#define GLUE_VIEW                Combine(GLUE_COMP, COMPONENT_VIEW)
#define HBOX_CODE                Combine(HBOX_COMP, CODE_VIEW)
#define HBOX_VIEW                Combine(HBOX_COMP, COMPONENT_VIEW)
#define SHAPER_CODE              Combine(SHAPER_COMP, CODE_VIEW)
#define SHAPER_VIEW              Combine(SHAPER_COMP, COMPONENT_VIEW)
#define MENUBAR_CODE             Combine(MENUBAR_COMP, CODE_VIEW)
#define MENUBAR_VIEW             Combine(MENUBAR_COMP, COMPONENT_VIEW)
#define HVVIEW                   Combine(HVCOMP, COMPONENT_VIEW)
#define INTERACTOR_VIEW          Combine(INTERACTOR_COMP, COMPONENT_VIEW)
#define MESSAGE_CODE             Combine(MESSAGE_COMP, CODE_VIEW)
#define MESSAGE_VIEW             Combine(MESSAGE_COMP, COMPONENT_VIEW)
#define MENUITEM_VIEW            Combine(MENUITEM_COMP, COMPONENT_VIEW)
#define MENUITEM_CODE            Combine(MENUITEM_COMP, CODE_VIEW)
#define MENUBODY_VIEW            Combine(MENUBODY_COMP, COMPONENT_VIEW)
#define POPUPMENU_CODE           Combine(POPUPMENU_COMP, CODE_VIEW)
#define POPUPMENU_VIEW           Combine(POPUPMENU_COMP, COMPONENT_VIEW)
#define PULLMENU_VIEW            Combine(PULLMENU_COMP, COMPONENT_VIEW)
#define PULLMENU_CODE            Combine(PULLMENU_COMP, CODE_VIEW)
#define SCENE_CODE               Combine(SCENE_COMP, CODE_VIEW)
#define SCENE_VIEW               Combine(SCENE_COMP, COMPONENT_VIEW)
#define MONOSCENECLASS_CODE      Combine(MONOSCENE_CLASS, CODE_VIEW)
#define MONOSCENECLASS_VIEW      Combine(MONOSCENE_CLASS, COMPONENT_VIEW)
#define EDITOR_CODE              Combine(EDITOR_COMP, CODE_VIEW)
#define EDITOR_VIEW              Combine(EDITOR_COMP, COMPONENT_VIEW)
#define IBVIEWER_CODE            Combine(IBVIEWER_COMP, CODE_VIEW)
#define IBVIEWER_VIEW            Combine(IBVIEWER_COMP, COMPONENT_VIEW)
#define PANELCONTROL_CODE        Combine(PANELCONTROL_COMP, CODE_VIEW)
#define PANELCONTROL_VIEW        Combine(PANELCONTROL_COMP, COMPONENT_VIEW)
#define COMMANDCONTROL_CODE      Combine(COMMANDCONTROL_COMP, CODE_VIEW)
#define COMMANDCONTROL_VIEW      Combine(COMMANDCONTROL_COMP, COMPONENT_VIEW)
#define DIALOGCLASS_CODE         Combine(DIALOG_CLASS, CODE_VIEW)
#define DIALOGCLASS_VIEW         Combine(DIALOG_CLASS, COMPONENT_VIEW)
#define MONOSCENE_CODE           Combine(MONOSCENE_COMP, CODE_VIEW)
#define MONOSCENE_VIEW           Combine(MONOSCENE_COMP, COMPONENT_VIEW)
#define PANNER_CODE              Combine(PANNER_COMP, CODE_VIEW)
#define PANNER_VIEW              Combine(PANNER_COMP, COMPONENT_VIEW)
#define SCROLLER_CODE            Combine(SCROLLER_COMP, CODE_VIEW)
#define SCROLLER_VIEW            Combine(SCROLLER_COMP, COMPONENT_VIEW)
#define SLIDER_CODE              Combine(SLIDER_COMP, CODE_VIEW)
#define SLIDER_VIEW              Combine(SLIDER_COMP, COMPONENT_VIEW)
#define STREDIT_CODE             Combine(STREDIT_COMP, CODE_VIEW)
#define STREDIT_VIEW             Combine(STREDIT_COMP, COMPONENT_VIEW)
#define TEXTEDIT_CODE            Combine(TEXTEDIT_COMP, CODE_VIEW)
#define TEXTEDIT_VIEW            Combine(TEXTEDIT_COMP, COMPONENT_VIEW)
#define VBOX_CODE                Combine(VBOX_COMP, CODE_VIEW)
#define VBOX_VIEW                Combine(VBOX_COMP, COMPONENT_VIEW)
#define VIEWPORT_CODE            Combine(VIEWPORT_COMP, CODE_VIEW)
#define VIEWPORT_VIEW            Combine(VIEWPORT_COMP, COMPONENT_VIEW)

#define ITEXT_CODE                Combine(ITEXT_COMP, CODE_VIEW)
#define ILINE_CODE                Combine(ILINE_COMP, CODE_VIEW)
#define IMULTILINE_CODE           Combine(IMULTILINE_COMP, CODE_VIEW)
#define ISPLINE_CODE              Combine(ISPLINE_COMP, CODE_VIEW)
#define IRECT_CODE                Combine(IRECT_COMP, CODE_VIEW)
#define IELLIPSE_CODE             Combine(IELLIPSE_COMP, CODE_VIEW)
#define IPOLYGON_CODE             Combine(IPOLYGON_COMP, CODE_VIEW)
#define ICLOSEDSPLINE_CODE        Combine(ICLOSEDSPLINE_COMP, CODE_VIEW)
#define IGROUP_CODE               Combine(IGRAPHIC_COMPS, CODE_VIEW)
#define ISTENCIL_CODE             Combine(ISTENCIL_COMP, CODE_VIEW)
#define IRASTER_CODE              Combine(IRASTER_COMP, CODE_VIEW)

#define ITEXT_VIEW               Combine(ITEXT_COMP, COMPONENT_VIEW)
#define IKEY_VIEW                Combine(IKEY_COMP, COMPONENT_VIEW)
#define ILINE_VIEW               Combine(ILINE_COMP, COMPONENT_VIEW)
#define IMULTILINE_VIEW          Combine(IMULTILINE_COMP, COMPONENT_VIEW)
#define ISPLINE_VIEW             Combine(ISPLINE_COMP, COMPONENT_VIEW)
#define IRECT_VIEW               Combine(IRECT_COMP, COMPONENT_VIEW)
#define IELLIPSE_VIEW            Combine(IELLIPSE_COMP, COMPONENT_VIEW)
#define IPOLYGON_VIEW            Combine(IPOLYGON_COMP, COMPONENT_VIEW)
#define ICLOSEDSPLINE_VIEW       Combine(ICLOSEDSPLINE_COMP, COMPONENT_VIEW)
#define IGRAPHIC_VIEWS           Combine(IGRAPHIC_COMPS, COMPONENT_VIEW)
#define ISTENCIL_VIEW            Combine(ISTENCIL_COMP, COMPONENT_VIEW)
#define IRASTER_VIEW             Combine(IRASTER_COMP, COMPONENT_VIEW)

#define ITEXT_PS               Combine(ITEXT_COMP, POSTSCRIPT_VIEW)
#define IKEY_PS                Combine(IKEY_COMP, POSTSCRIPT_VIEW)
#define ILINE_PS               Combine(ILINE_COMP, POSTSCRIPT_VIEW)
#define IMULTILINE_PS          Combine(IMULTILINE_COMP, POSTSCRIPT_VIEW)
#define ISPLINE_PS             Combine(ISPLINE_COMP, POSTSCRIPT_VIEW)
#define IRECT_PS               Combine(IRECT_COMP, POSTSCRIPT_VIEW)
#define IELLIPSE_PS            Combine(IELLIPSE_COMP, POSTSCRIPT_VIEW)
#define IPOLYGON_PS            Combine(IPOLYGON_COMP, POSTSCRIPT_VIEW)
#define ICLOSEDSPLINE_PS       Combine(ICLOSEDSPLINE_COMP, POSTSCRIPT_VIEW)
#define IPS_VIEWS              Combine(IGRAPHIC_COMPS, POSTSCRIPT_VIEW)
#define ISTENCIL_PS            Combine(ISTENCIL_COMP, POSTSCRIPT_VIEW)
#define IRASTER_PS             Combine(IRASTER_COMP, POSTSCRIPT_VIEW)
#define GRBLOCK_PS             Combine(GRBLOCK_COMP, POSTSCRIPT_VIEW)

#endif

#endif
