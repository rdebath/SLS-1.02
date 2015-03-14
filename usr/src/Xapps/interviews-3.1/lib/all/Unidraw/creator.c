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
 * Object creator class implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/creator.h>
#include <Unidraw/statevars.h>
#include <Unidraw/transfns.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Commands/command.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/font.h>
#include <Unidraw/Commands/import.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Commands/nop.h>
#include <Unidraw/Commands/patcmd.h>
#include <Unidraw/Commands/struct.h>
#include <Unidraw/Commands/transforms.h>
#include <Unidraw/Commands/viewcmds.h>

#include <Unidraw/Components/ellipse.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/line.h>
#include <Unidraw/Components/link.h>
#include <Unidraw/Components/pad.h>
#include <Unidraw/Components/pin.h>
#include <Unidraw/Components/polygon.h>
#include <Unidraw/Components/rastercomp.h>
#include <Unidraw/Components/rect.h>
#include <Unidraw/Components/slot.h>
#include <Unidraw/Components/stencilcomp.h>
#include <Unidraw/Components/spline.h>
#include <Unidraw/Components/text.h>

#include <Unidraw/Tools/connect.h>
#include <Unidraw/Tools/grcomptool.h>
#include <Unidraw/Tools/magnify.h>
#include <Unidraw/Tools/move.h>
#include <Unidraw/Tools/reshape.h>
#include <Unidraw/Tools/rotate.h>
#include <Unidraw/Tools/scale.h>
#include <Unidraw/Tools/select.h>
#include <Unidraw/Tools/stretch.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Creator::Creator () { }

void* Creator::Create (ClassId id, istream& in, ObjectMap* objmap, int objid) {
    switch (id) {
        case ALIGN_CMD:  	CREATE(AlignCmd, in, objmap, objid);
        case ALIGNTOGRID_CMD:  	CREATE(AlignToGridCmd, in, objmap, objid);
        case BACK_CMD:  	CREATE(BackCmd, in, objmap, objid);
        case BRUSH_CMD:  	CREATE(BrushCmd, in, objmap, objid);
        case BRUSH_VAR:  	CREATE(BrushVar, in, objmap, objid);
        case CENTER_CMD:  	CREATE(CenterCmd, in, objmap, objid);
        case CLOSEDSPLINE_COMP: CREATE(ClosedSplineComp, in, objmap, objid);
        case CLOSEEDITOR_CMD:   CREATE(CloseEditorCmd, in, objmap, objid);
        case COLOR_CMD:		CREATE(ColorCmd, in, objmap, objid);
        case COMPNAME_VAR:	CREATE(CompNameVar, in, objmap, objid);
        case CONNECT_CMD:  	CREATE(ConnectCmd, in, objmap, objid);
        case CONNECT_TOOL:  	CREATE(ConnectTool, in, objmap, objid);
        case COPY_CMD:  	CREATE(CopyCmd, in, objmap, objid);
        case CUT_CMD:  		CREATE(CutCmd, in, objmap, objid);
        case DELETE_CMD:  	CREATE(DeleteCmd, in, objmap, objid);
        case DUP_CMD:  		CREATE(DupCmd, in, objmap, objid);
        case ELLIPSE_COMP:  	CREATE(EllipseComp, in, objmap, objid);
        case FONT_CMD:  	CREATE(FontCmd, in, objmap, objid);
        case FONT_VAR:  	CREATE(FontVar, in, objmap, objid);
        case FRONT_CMD:  	CREATE(FrontCmd, in, objmap, objid);
        case GRAPHIC_COMPS:  	CREATE(GraphicComps, in, objmap, objid);
        case GRAPHIC_COMP_TOOL: CREATE(GraphicCompTool, in, objmap, objid);
        case GRAVITY_CMD:  	CREATE(GravityCmd, in, objmap, objid);
        case GRAVITY_VAR:  	CREATE(GravityVar, in, objmap, objid);
        case GRID_CMD:  	CREATE(GridCmd, in, objmap, objid);
        case GRIDSPACING_CMD:  	CREATE(GridSpacingCmd, in, objmap, objid);
        case GROUP_CMD:  	CREATE(GroupCmd, in, objmap, objid);
        case HSLOT_COMP:  	CREATE(HSlotComp, in, objmap, objid);
        case IMPORT_CMD:        CREATE(ImportCmd, in, objmap, objid);
        case LINE_COMP:  	CREATE(LineComp, in, objmap, objid);
        case LINK_COMP:  	CREATE(LinkComp, in, objmap, objid);
        case MACRO_CMD:  	CREATE(MacroCmd, in, objmap, objid);
        case MAGNIFY_TOOL:  	CREATE(MagnifyTool, in, objmap, objid);
        case MAGNIF_VAR:  	CREATE(MagnifVar, in, objmap, objid);
        case MOBILITY_CMD:  	CREATE(MobilityCmd, in, objmap, objid);
        case MODIFSTATUS_VAR:  	CREATE(ModifStatusVar, in, objmap, objid);
        case MOVE_CMD:  	CREATE(MoveCmd, in, objmap, objid);
        case MOVE_TOOL:  	CREATE(MoveTool, in, objmap, objid);
        case MULTILINE_COMP:  	CREATE(MultiLineComp, in, objmap, objid);
        case NAME_VAR:    	CREATE(NameVar, in, objmap, objid);
        case NEWCOMP_CMD:  	CREATE(NewCompCmd, in, objmap, objid);
        case NOP_CMD:    	CREATE(NOPCmd, in, objmap, objid);
        case NORMSIZE_CMD:  	CREATE(NormSizeCmd, in, objmap, objid);
        case ORIENTATION_CMD:  	CREATE(OrientationCmd, in, objmap, objid);
        case PASTE_CMD:  	CREATE(PasteCmd, in, objmap, objid);
        case PATTERN_CMD:  	CREATE(PatternCmd, in, objmap, objid);
        case PATTERN_VAR:  	CREATE(PatternVar, in, objmap, objid);
        case PAD_COMP:  	CREATE(PadComp, in, objmap, objid);
        case PIN_COMP:  	CREATE(PinComp, in, objmap, objid);
        case POLYGON_COMP:  	CREATE(PolygonComp, in, objmap, objid);
        case PRINT_CMD:  	CREATE(PrintCmd, in, objmap, objid);
        case QUIT_CMD:  	CREATE(QuitCmd, in, objmap, objid);
        case RASTER_COMP:  	CREATE(RasterComp, in, objmap, objid);
        case RECT_COMP:  	CREATE(RectComp, in, objmap, objid);
        case REDO_CMD:  	CREATE(RedoCmd, in, objmap, objid);
        case REDTOFIT_CMD:  	CREATE(RedToFitCmd, in, objmap, objid);
        case REPLACE_CMD:  	CREATE(ReplaceCmd, in, objmap, objid);
        case RESHAPE_TOOL:  	CREATE(ReshapeTool, in, objmap, objid);
        case REVERT_CMD:  	CREATE(RevertCmd, in, objmap, objid);
        case ROTATE_CMD:  	CREATE(RotateCmd, in, objmap, objid);
        case ROTATE_TOOL:  	CREATE(RotateTool, in, objmap, objid);
        case SAVECOMPAS_CMD:  	CREATE(SaveCompAsCmd, in, objmap, objid);
        case SAVECOMP_CMD:  	CREATE(SaveCompCmd, in, objmap, objid);
        case SCALE_CMD:  	CREATE(ScaleCmd, in, objmap, objid);
        case SCALE_TOOL:  	CREATE(ScaleTool, in, objmap, objid);
        case SELECT_TOOL:  	CREATE(SelectTool, in, objmap, objid);
        case SLCTALL_CMD:  	CREATE(SlctAllCmd, in, objmap, objid);
        case SPLINE_COMP:  	CREATE(SplineComp, in, objmap, objid);
        case STENCIL_COMP:  	CREATE(StencilComp, in, objmap, objid);
        case STRETCH_TOOL:  	CREATE(StretchTool, in, objmap, objid);
        case TEXT_COMP:  	CREATE(TextComp, in, objmap, objid);
        case TF_DIRECT:         CREATE(TF_Direct, in, objmap, objid);
        case UNDO_CMD:  	CREATE(UndoCmd, in, objmap, objid);
        case UNGROUP_CMD:  	CREATE(UngroupCmd, in, objmap, objid);
        case VIEWCOMP_CMD:  	CREATE(ViewCompCmd, in, objmap, objid);
        case VSLOT_COMP:  	CREATE(VSlotComp, in, objmap, objid);

        default:    return nil;
    }
}

void* Creator::Create (ClassId id) {
    if (id == CLOSEDSPLINE_VIEW)   return new ClosedSplineView;
    if (id == ELLIPSE_VIEW)        return new EllipseView;
    if (id == GRAPHIC_VIEWS)       return new GraphicViews;
    if (id == HSLOT_VIEW)          return new HSlotView;
    if (id == LINE_VIEW)           return new LineView;
    if (id == LINK_VIEW)           return new LinkView;
    if (id == MULTILINE_VIEW)      return new MultiLineView;
    if (id == PAD_VIEW)            return new PadView;
    if (id == PIN_VIEW)            return new PinView;
    if (id == POLYGON_VIEW)        return new PolygonView;
    if (id == POSTSCRIPT_VIEWS)    return new PostScriptViews;
    if (id == PS_CLOSEDSPLINE)     return new PSClosedSpline;
    if (id == PS_ELLIPSE)          return new PSEllipse;
    if (id == PS_HSLOT)            return new PSSlot;
    if (id == PS_LINE)             return new PSLine;
    if (id == PS_LINK)             return new PSLink;
    if (id == PS_MULTILINE)        return new PSMultiLine;
    if (id == PS_PAD)              return new PSPad;
    if (id == PS_PIN)              return new PSPin;
    if (id == PS_POLYGON)          return new PSPolygon;
    if (id == PS_RASTER)           return new PSRaster;
    if (id == PS_RECT)             return new PSRect;
    if (id == PS_SLOT)             return new PSSlot;
    if (id == PS_SPLINE)           return new PSSpline;
    if (id == PS_STENCIL)          return new PSStencil;
    if (id == PS_TEXT)             return new PSText;
    if (id == PS_VSLOT)            return new PSSlot;
    if (id == RASTER_VIEW)         return new RasterView;
    if (id == RECT_VIEW)           return new RectView;
    if (id == SPLINE_VIEW)         return new SplineView;
    if (id == STENCIL_VIEW)        return new StencilView;
    if (id == TEXT_VIEW)           return new TextView;
    if (id == VSLOT_VIEW)          return new VSlotView;

    return nil;
}
