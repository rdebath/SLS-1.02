/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Unique unidraw class identifiers.
 */

#ifndef unidraw_classes_h
#define unidraw_classes_h

#include <Unidraw/globals.h>

inline ClassId Combine (ClassId subj, ClassId view) {return 10000*subj + view;}

#define UNDEFINED_CLASS        0

#define ALIGN_CMD	    9001
#define BACK_CMD	    9002
#define BRUSH_CMD	    9003
#define CENTER_CMD	    9004
#define COMMAND		    9006
#define COMPONENT	    9007
#define COMPONENT_VIEW	    9008
#define COMPONENT_VIEWS     9009
#define CONNECT_CMD         9010
#define CONNECT_TOOL	    9011
#define CONNECTOR	    9012
#define CONNECTOR_VIEW      9013
#define COPY_CMD	    9015
#define CUT_CMD		    9016
#define	DELETE_CMD	    9017
#define DUP_CMD		    9018
#define EXTERN_VIEW	    9020
#define FONT_CMD	    9021
#define FRONT_CMD	    9023
#define GRAPHIC_COMP	    9024
#define GRAPHIC_COMPS       9025
#define GRAPHIC_COMP_TOOL   9026
#define GRID_CMD	    9027
#define GROUP_CMD	    9028
#define GROUP_COMP          9029
#define LINE_COMP	    9030
#define MACRO_CMD           9031
#define MAGNIFY_TOOL	    9032
#define MOVE_TOOL	    9033
#define MOBILITY_CMD        9034
#define MOVE_CMD            9035
#define NEWCOMP_CMD         9036
#define NORMSIZE_CMD	    9037
#define PAD_COMP            9038
#define PASTE_CMD	    9039
#define PATTERN_CMD	    9040
#define PIN_COMP	    9041
#define STATE_VAR           9042
#define NAME_VAR            9043
#define QUIT_CMD	    9044
#define RECT_COMP	    9045
#define MODIFSTATUS_VAR     9046
#define REDTOFIT_CMD	    9047
#define REVERT_CMD          9048
#define ROTATE_CMD          9049
#define ROTATE_TOOL	    9050
#define SAVECOMP_CMD	    9051
#define SCALE_CMD           9052
#define SCALE_TOOL	    9053
#define SELECT_TOOL	    9054
#define SLCTALL_CMD	    9055
#define SLOT_COMP           9056
#define MAGNIF_VAR          9057
#define TOOL		    9058
#define UNDO_CMD	    9059
#define UNGROUP_CMD	    9060
#define VIEWCOMP_CMD        9061
#define VIEWER_VIEW         9062
#define FONT_VAR      	    9063
#define SAVECOMPAS_CMD      9064
#define	BRUSH_VAR           9065
#define	PATTERN_VAR	    9066
#define COMPONENT_MAP       9067
#define COMMAND_MAP         9068
#define TOOL_MAP            9069
#define COMPNAME_VAR        9070
#define ALIGNTOGRID_CMD     9071
#define PERSPECTIVE_INFO    9072
#define	TRANSFER_FUNCT      9073
#define STRUCT_CMD          9074
#define TF_2PORT            9075
#define ELLIPSE_COMP        9076
#define TF_DIRECT           9077
#define POLYGON_COMP        9078
#define VERTICES_COMP       9079
#define CLOSEEDITOR_CMD     9080
#define SPLINE_COMP         9081
#define PRINT_CMD           9082
#define CLOSEDSPLINE_COMP   9083
#define COLOR_VAR           9084
#define COLOR_CMD           9085
#define GRAVITY_VAR         9086
#define GRAVITY_CMD         9087
#define ORIENTATION_CMD     9088
#define GRIDSPACING_CMD     9089
#define TEXT_COMP	    9090
#define MULTILINE_COMP	    9091
#define DIRTY_CMD           9092
#define RESHAPE_TOOL        9093
#define STRETCH_TOOL        9094
#define HSLOT_COMP          9095
#define VSLOT_COMP          9096
#define RASTER_COMP         9097
#define REPLACE_CMD         9098
#define REDO_CMD            9099
#define PREORDER_VIEW       9100
#define INORDER_VIEW        9101
#define POSTORDER_VIEW      9102
#define	POSTSCRIPT_VIEW     9103
#define LINK_COMP           9104
#define STENCIL_COMP        9105
#define IMPORT_CMD          9106
#define NOP_CMD             9107

/* Composite ids associating subjects with their views */

#define CLOSEDSPLINE_VIEW   Combine(CLOSEDSPLINE_COMP, COMPONENT_VIEW)
#define ELLIPSE_VIEW        Combine(ELLIPSE_COMP, COMPONENT_VIEW)
#define GRAPHIC_VIEW	    Combine(GRAPHIC_COMP, COMPONENT_VIEW)
#define GRAPHIC_VIEWS	    Combine(GRAPHIC_COMPS, COMPONENT_VIEW)
#define GROUP_VIEW          Combine(GROUP_COMP, COMPONENT_VIEW)
#define HSLOT_VIEW	    Combine(HSLOT_COMP, COMPONENT_VIEW)
#define LINE_VIEW	    Combine(LINE_COMP, COMPONENT_VIEW)
#define LINK_VIEW	    Combine(LINK_COMP, COMPONENT_VIEW)
#define MULTILINE_VIEW	    Combine(MULTILINE_COMP, COMPONENT_VIEW)
#define PAD_VIEW	    Combine(PAD_COMP, COMPONENT_VIEW)
#define PIN_VIEW	    Combine(PIN_COMP, COMPONENT_VIEW)
#define POLYGON_VIEW        Combine(POLYGON_COMP, COMPONENT_VIEW)
#define POSTSCRIPT_VIEWS    Combine(GRAPHIC_COMPS, POSTSCRIPT_VIEW)
#define PS_CLOSEDSPLINE     Combine(CLOSEDSPLINE_COMP, POSTSCRIPT_VIEW)
#define PS_ELLIPSE	    Combine(ELLIPSE_COMP, POSTSCRIPT_VIEW)
#define PS_GROUP            Combine(GROUP_COMP, POSTSCRIPT_VIEW)
#define PS_HSLOT            Combine(HSLOT_COMP, POSTSCRIPT_VIEW)
#define PS_LINE	            Combine(LINE_COMP, POSTSCRIPT_VIEW)
#define PS_LINK	            Combine(LINK_COMP, POSTSCRIPT_VIEW)
#define PS_MULTILINE        Combine(MULTILINE_COMP, POSTSCRIPT_VIEW)
#define PS_PAD              Combine(PAD_COMP, POSTSCRIPT_VIEW)
#define PS_PIN              Combine(PIN_COMP, POSTSCRIPT_VIEW)
#define PS_POLYGON          Combine(POLYGON_COMP, POSTSCRIPT_VIEW)
#define PS_RASTER	    Combine(RASTER_COMP, POSTSCRIPT_VIEW)
#define PS_RECT	            Combine(RECT_COMP, POSTSCRIPT_VIEW)
#define PS_SLOT             Combine(SLOT_COMP, POSTSCRIPT_VIEW)
#define PS_SPLINE           Combine(SPLINE_COMP, POSTSCRIPT_VIEW)
#define PS_STENCIL          Combine(STENCIL_COMP, POSTSCRIPT_VIEW)
#define PS_TEXT             Combine(TEXT_COMP, POSTSCRIPT_VIEW)
#define PS_VERTICES         Combine(VERTICES_COMP, POSTSCRIPT_VIEW)
#define PS_VSLOT            Combine(VSLOT_COMP, POSTSCRIPT_VIEW)
#define RASTER_VIEW	    Combine(RASTER_COMP, COMPONENT_VIEW)
#define RECT_VIEW	    Combine(RECT_COMP, COMPONENT_VIEW)
#define SLOT_VIEW           Combine(SLOT_COMP, COMPONENT_VIEW)
#define STENCIL_VIEW        Combine(STENCIL_COMP, COMPONENT_VIEW)
#define SPLINE_VIEW         Combine(SPLINE_COMP, COMPONENT_VIEW)
#define TEXT_VIEW	    Combine(TEXT_COMP, COMPONENT_VIEW)
#define VERTICES_VIEW       Combine(VERTICES_COMP, COMPONENT_VIEW)
#define VSLOT_VIEW	    Combine(VSLOT_COMP, COMPONENT_VIEW)

#endif
