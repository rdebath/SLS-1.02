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
 * IBuild object constructor class implementation.
 */

#include "ibline.h"
#include "ibspline.h"
#include "ibtext.h"
#include "ibrect.h"
#include "ibellipse.h"
#include "ibpolygon.h"
#include "ibgrcomp.h"

#include "ibborder.h"
#include "ibbox.h"
#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcommandctrl.h"
#include "ibcode.h"
#include "ibcreator.h"
#include "ibdialog.h"
#include "ibeditor.h"
#include "ibfbrowser.h"
#include "ibglue.h"
#include "ibgrblock.h"
#include "ibinteractor.h"
#include "ibmessage.h"
#include "ibscene.h"
#include "ibscroller.h"
#include "ibstred.h"
#include "ibtools.h"
#include "ibadjuster.h"
#include "ibbitmap.h"
#include "ibframe.h"
#include "ibdeck.h"
#include "ibmenu.h"
#include "ibslider.h"
#include "ibpanner.h"
#include "ibpanelctrl.h"
#include "ibraster.h"
#include "ibshaper.h"
#include "ibstencil.h"
#include "ibstrbrowser.h"
#include "ibtextedit.h"
#include "ibviewer.h"
#include "ibviewport.h"
#include "ibvars.h"

#include <Unidraw/Components/psview.h>
#include <Unidraw/catalog.h>

/*****************************************************************************/
boolean IBCreator::_lock = true;
/*****************************************************************************/

IBCreator::IBCreator () {}

void IBCreator::SetLock(boolean lock) { _lock = lock; }

boolean IBCreator::GetLock() { return _lock; }

void* IBCreator::Create (ClassId id, istream& in, ObjectMap* objmap,int objid){
  if (!_lock) {
    switch (id) {
        case LINE_COMP:         CREATE(ILineComp, in, objmap, objid);
        case MULTILINE_COMP:    CREATE(IMultiLineComp, in,objmap,objid);
        case SPLINE_COMP:       CREATE(ISplineComp, in, objmap, objid);
        case TEXT_COMP:         CREATE(ITextComp, in, objmap, objid);
        case RECT_COMP:         CREATE(IRectComp, in, objmap, objid);
        case ELLIPSE_COMP:      CREATE(IEllipseComp, in, objmap, objid);
        case POLYGON_COMP:      CREATE(IPolygonComp, in, objmap, objid);
        case CLOSEDSPLINE_COMP: CREATE(IClosedSplineComp, in, objmap, objid);
        case GRAPHIC_COMPS:     CREATE(IGraphicComps, in, objmap, objid);
        case STENCIL_COMP:      CREATE(IStencilComp, in, objmap, objid);
        case RASTER_COMP:       CREATE(IRasterComp, in, objmap, objid);
        default:    return Default(id, in, objmap, objid);
    }
  } else {
      return Default(id, in, objmap, objid);
  }
}

void* IBCreator::Default(ClassId id, istream& in, ObjectMap* objmap,int objid){
    switch (id) {
        case ILINE_COMP:        CREATE(ILineComp, in, objmap, objid);
        case IMULTILINE_COMP:   CREATE(IMultiLineComp, in,objmap,objid);
        case ISPLINE_COMP:      CREATE(ISplineComp, in, objmap, objid);
        case ITEXT_COMP:        CREATE(ITextComp, in, objmap, objid);
        case IRECT_COMP:        CREATE(IRectComp, in, objmap, objid);
        case IELLIPSE_COMP:     CREATE(IEllipseComp, in, objmap, objid);
        case IPOLYGON_COMP:     CREATE(IPolygonComp, in, objmap, objid);
        case ICLOSEDSPLINE_COMP:CREATE(IClosedSplineComp, in, objmap, objid);
        case IGRAPHIC_COMPS:    CREATE(IGraphicComps, in, objmap, objid);
        case ISTENCIL_COMP:     CREATE(IStencilComp, in, objmap, objid);
        case IRASTER_COMP:      CREATE(IRasterComp, in, objmap, objid);
        case IKEY_COMP:         CREATE(ITextComp, in, objmap, objid);

	case ABOUT_CMD:         CREATE(AboutCmd, in, objmap, objid);
        case BORDER_COMP:       CREATE(BorderComp, in, objmap, objid);
        case BOOLEANSTATE_VAR:  CREATE(BooleanStateVar, in, objmap, objid);
        case BUTTONSHAREDNAME:  CREATE(ButtonSharedName, in, objmap, objid);
        case MEMBERSHAREDNAME:  CREATE(MemberSharedName, in, objmap, objid);
        case BUTTONSTATE_VAR:   CREATE(ButtonStateVar, in, objmap, objid);
        case BUTTON_COMP:       CREATE(ButtonComp, in, objmap, objid);
        case BITMAP_COMP:       CREATE(BitmapComp, in, objmap, objid);
        case ADJUSTER_COMP:     CREATE(AdjusterComp, in, objmap, objid);
        case CANVAS_VAR:        CREATE(CanvasVar, in, objmap, objid);
        case CODE_CMD:          CREATE(CodeCmd, in, objmap, objid);
        case DECK_COMP:         CREATE(DeckComp, in, objmap, objid);
        case DIALOG_CLASS:      CREATE(DialogClass, in, objmap, objid);
        case EXAMINE_TOOL:      CREATE(ExamineTool, in, objmap, objid);
        case FBROWSER_COMP:     CREATE(FBrowserComp, in, objmap, objid);
        case FBROWSER_VAR:      CREATE(FBrowserVar, in, objmap, objid);
        case ID_VAR:            CREATE(IDVar, in, objmap, objid);
        case STRBROWSER_COMP:   CREATE(StrBrowserComp, in, objmap, objid);
        case GRBLOCK_COMP:      CREATE(GrBlockComp, in, objmap, objid);
        case PANELCONTROL_COMP: CREATE(PanelCtrlComp, in, objmap, objid);
        case COMMANDCONTROL_COMP: CREATE(CommandCtrlComp, in, objmap, objid);
        case FRAME_COMP:        CREATE(FrameComp, in, objmap, objid);
        case GLUEVISIBILITY_CMD:CREATE(GlueVisibilityCmd, in, objmap, objid);
        case GLUE_COMP:         CREATE(GlueComp, in, objmap, objid);
        case HBOX_COMP:         CREATE(HBoxComp, in, objmap, objid);
        case SHAPER_COMP:       CREATE(ShaperComp, in, objmap, objid);
        case MENUBAR_COMP:      CREATE(MenuBarComp, in, objmap, objid);
        case POPUPMENU_COMP:    CREATE(PopupMenuComp, in, objmap, objid);
        case IBNAME_VAR:        CREATE(IBNameVar, in, objmap, objid);
        case IDRAW_CMD:         CREATE(IdrawCmd, in, objmap, objid);
        case INFO_CMD:          CREATE(InfoCmd, in, objmap, objid);
        case INSTANCENAME_VAR:  CREATE(InstanceNameVar, in, objmap, objid);
        case MEMBERNAME_VAR:    CREATE(MemberNameVar, in, objmap, objid);
        case MARGINFRAME_COMP:  CREATE(MarginFrameComp, in, objmap, objid);
        case MENUITEM_COMP:     CREATE(MenuItemComp, in, objmap, objid);
        case MESSAGE_COMP:      CREATE(MessageComp, in, objmap, objid);
        case MONOSCENE_CMD:     CREATE(MonoSceneCmd, in, objmap, objid);
        case MONOSCENE_COMP:    CREATE(MonoSceneComp, in, objmap, objid);
        case MONOSCENE_CLASS:   CREATE(MonoSceneClass, in, objmap, objid);
        case EDITOR_COMP:       CREATE(EditorComp, in, objmap, objid);
        case IBVIEWER_COMP:     CREATE(IBViewerComp, in, objmap, objid);
        case NEWTOOL_CMD:       CREATE(NewToolCmd, in, objmap, objid);
        case NEWVIEW_CMD:       CREATE(NewViewCmd, in, objmap, objid);
        case PANNER_COMP:       CREATE(PannerComp, in, objmap, objid);
        case PLACE_CMD:         CREATE(PlaceCmd, in, objmap, objid);
        case PROCNAME_VAR:      CREATE(TrackNameVar, in, objmap, objid);
        case PULLMENU_COMP:     CREATE(PullMenuComp, in, objmap, objid);
        case MENUBODY_COMP:     CREATE(MenuBodyComp, in, objmap, objid);
        case SCENE_CMD:         CREATE(SceneCmd, in, objmap, objid);
        case SCENE_COMP:        CREATE(SceneComp, in, objmap, objid);
        case SCROLLER_COMP:     CREATE(ScrollerComp, in, objmap, objid);
        case SLIDER_COMP:       CREATE(SliderComp, in, objmap, objid);
        case SHAPE_VAR:         CREATE(ShapeVar, in, objmap, objid);
        case STREDIT_COMP:      CREATE(StrEditComp, in, objmap, objid);
        case TEXTEDIT_COMP:     CREATE(TextEditComp, in, objmap, objid);
	case TOOLS_CMD:         CREATE(ToolsCmd, in, objmap, objid);
	case RELATE_CMD:        CREATE(RelateCmd, in, objmap, objid);
	case EDIT_CMD:    	CREATE(EditCmd, in, objmap, objid);
	case NAVIGATE_CMD:    	CREATE(NavigateCmd, in, objmap, objid);
	case TAB_CMD:    	CREATE(TabCmd, in, objmap, objid);
        case VBOX_COMP:         CREATE(VBoxComp, in, objmap, objid);
        case SUBCLASSNAME_VAR:  CREATE(SubclassNameVar, in, objmap, objid);
        case REORDER_CMD:       CREATE(ReorderCmd, in, objmap, objid);
        case IBGRAPHIC_COMP_TOOL: CREATE(IBGraphicCompTool, in, objmap, objid);
	case VIEWPORT_COMP:     CREATE(ViewportComp, in, objmap, objid);
            
        default:    return Creator::Create(id, in, objmap, objid);
    }
}

void* IBCreator::Create (ClassId id) {
    if (id == ILINE_VIEW)           return new IView;
    if (id == IMULTILINE_VIEW)      return new IView;
    if (id == ISPLINE_VIEW)         return new IView;
    if (id == ITEXT_VIEW)           return new ITextView;
    if (id == IKEY_VIEW)            return new ITextView;
    if (id == IRECT_VIEW)           return new IRectView;
    if (id == IELLIPSE_VIEW)        return new IView;
    if (id == IPOLYGON_VIEW)        return new IView;
    if (id == ICLOSEDSPLINE_VIEW)   return new IView;
    if (id == ISTENCIL_VIEW)        return new IView;
    if (id == IRASTER_VIEW)         return new IView;
    if (id == IGRAPHIC_VIEWS)       return new IGraphicViews;

    if (id == ITEXT_CODE)           return new TextCode;
    if (id == ILINE_CODE)           return new LineCode;
    if (id == IMULTILINE_CODE)      return new MultiLineCode;
    if (id == ISPLINE_CODE)         return new SplineCode;

    if (id == IRECT_CODE)           return new RectCode;
    if (id == IELLIPSE_CODE)        return new EllipseCode;
    if (id == IPOLYGON_CODE)        return new PolygonCode;
    if (id == ICLOSEDSPLINE_CODE)   return new ClosedSplineCode;
    if (id == IGROUP_CODE)          return new GroupCode;
    if (id == ISTENCIL_CODE)        return new StencilCode;
    if (id == IRASTER_CODE)         return new RasterCode;

    if (id == ADJUSTER_CODE)       return new AdjusterCode;
    if (id == ADJUSTER_VIEW)       return new AdjusterView;
    if (id == ADJUSTER_GRAPHIC)    return new AdjusterGraphic;
    if (id == BITMAP_GRAPHIC)      return new BitmapGraphic;
    if (id == BORDER_CODE)         return new BorderCode;
    if (id == BORDER_VIEW)         return new BorderView;
    if (id == BUTTON_CODE)         return new ButtonCode;
    if (id == BUTTON_VIEW)         return new ButtonView;
    if (id == BITMAP_VIEW)         return new BitmapView;
    if (id == CHECKBOX_GRAPHIC)    return new CheckBoxGraphic;
    if (id == COMMANDCONTROL_CODE) return new CommandCtrlCode;
    if (id == COMMANDCONTROL_VIEW) return new CommandCtrlView;
    if (id == DECK_CODE)           return new DeckCode;
    if (id == DECK_VIEW)           return new DeckView;
    if (id == DIALOGCLASS_CODE)    return new DialogClassCode;
    if (id == DIALOGCLASS_VIEW)    return new DialogClassView;
    if (id == FBROWSER_CODE)       return new FBrowserCode;
    if (id == STRBROWSER_GRAPHIC)  return new StrBrowserGraphic;
    if (id == FBROWSER_VIEW)       return new FBrowserView;
    if (id == STRBROWSER_CODE)     return new StrBrowserCode;
    if (id == STRBROWSER_VIEW)     return new StrBrowserView;
    if (id == GRBLOCK_CODE)        return new GrBlockCode;
    if (id == GRBLOCK_VIEW)        return new GrBlockView;
    if (id == GRBLOCK_GRAPHIC)     return new GrBlockGraphic;
    if (id == PANELCONTROL_CODE)   return new PanelCtrlCode;
    if (id == PANELCONTROL_VIEW)   return new PanelCtrlView;
    if (id == PANELCONTROL_GRAPHIC)return new PanelCtrlGraphic;
    if (id == FRAME_CODE)          return new FrameCode;
    if (id == FRAME_VIEW)      	   return new FrameView;
    if (id == FRAME_GRAPHIC)       return new FrameGraphic;
    if (id == MARGINFRAME_CODE)    return new FrameCode;
    if (id == MARGINFRAME_VIEW)    return new MarginFrameView;
    if (id == GLUE_CODE)           return new GlueCode;
    if (id == GLUE_VIEW)           return new GlueView;
    if (id == HBOX_CODE)           return new BoxCode;
    if (id == HBOX_VIEW)           return new SceneView;
    if (id == SHAPER_CODE)         return new ShaperCode;
    if (id == SHAPER_VIEW)         return new SceneView;
    if (id == MENUBAR_CODE)        return new MenuBarCode;
    if (id == MENUBAR_VIEW)        return new SceneView;
    if (id == MENUITEM_GRAPHIC)    return new MenuItemGraphic;
    if (id == MENUITEM_VIEW)       return new MenuItemView;
    if (id == PDMENU_GRAPHIC)	   return new PDMenuGraphic;
    if (id == PRMENU_GRAPHIC)	   return new PRMenuGraphic;
    if (id == PULLMENU_VIEW)   	   return new PullMenuView;
    if (id == MESSAGE_CODE)        return new MessageCode;
    if (id == MESSAGE_GRAPHIC)     return new MessageGraphic;
    if (id == MESSAGE_VIEW)        return new MessageView;
    if (id == POPUPMENU_CODE)      return new BoxCode;
    if (id == POPUPMENU_VIEW)      return new SceneView;
    if (id == PANNER_CODE)         return new PannerCode;
    if (id == PANNER_VIEW)         return new PannerView;
    if (id == PUSHBUTTON_GRAPHIC)  return new PushButtonGraphic;
    if (id == RADIOBUTTON_GRAPHIC) return new RadioButtonGraphic;
    if (id == MENUITEM_CODE)       return new MenuItemCode;
    if (id == MENUBODY_VIEW)       return new SceneView;
    if (id == MONOSCENECLASS_CODE) return new MonoSceneClassCode;
    if (id == MONOSCENECLASS_VIEW) return new MonoSceneClassView;
    if (id == EDITOR_CODE)         return new EditorCode;
    if (id == EDITOR_VIEW)         return new EditorView;
    if (id == IBVIEWER_CODE)       return new IBViewerCode;
    if (id == IBVIEWER_VIEW)       return new IBViewerView;
    if (id == IBVIEWER_GRAPHIC)    return new IBViewerGraphic;
    if (id == MONOSCENE_CODE)      return new MonoSceneCode;
    if (id == MONOSCENE_VIEW)      return new MonoSceneView;
    if (id == PULLMENU_CODE)       return new PullMenuCode;
    if (id == SCENE_CODE)          return new RootCodeView;
    if (id == SCENE_VIEW)          return new SceneView;
    if (id == SCROLLER_CODE)       return new ScrollerCode;
    if (id == SCROLLER_VIEW)       return new ScrollerView;
    if (id == SLIDER_CODE)         return new SliderCode;
    if (id == SLIDER_VIEW)         return new SliderView;
    if (id == SLIDER_GRAPHIC)      return new SliderGraphic;
    if (id == STREDIT_CODE)        return new StrEditCode;
    if (id == STREDIT_GRAPHIC)     return new StrEditGraphic;
    if (id == STREDIT_VIEW)        return new StrEditView;
    if (id == VBOX_CODE)           return new BoxCode;
    if (id == VBOX_VIEW)           return new SceneView;
    if (id == LMOVER_GRAPHIC)      return new LMoverGraphic;
    if (id == RMOVER_GRAPHIC)      return new RMoverGraphic;
    if (id == UMOVER_GRAPHIC)      return new UMoverGraphic;
    if (id == DMOVER_GRAPHIC)      return new DMoverGraphic;
    if (id == ENLARGER_GRAPHIC)    return new EnlargerGraphic;
    if (id == REDUCER_GRAPHIC)     return new ReducerGraphic;
    if (id == SHADOWFRAME_GRAPHIC) return new ShadowFrameGraphic;
    if (id == TEXTEDIT_CODE)       return new TextEditCode;
    if (id == TEXTEDIT_VIEW)       return new TextEditView;
    if (id == MARGINFRAME_GRAPHIC) return new MarginFrameGraphic;
    if (id == VIEWPORT_CODE)       return new ViewportCode;
    if (id == VIEWPORT_VIEW)       return new ViewportView;
    if (id == VIEWPORT_GRAPHIC)    return new ViewportGraphic;

    if (id == ILINE_COMP)          return new ILineComp;
    if (id == IMULTILINE_COMP)     return new IMultiLineComp;
    if (id == ISPLINE_COMP)        return new ISplineComp;
    if (id == ITEXT_COMP)          return new ITextComp;
    if (id == IRECT_COMP)          return new IRectComp;
    if (id == IELLIPSE_COMP)       return new IEllipseComp;
    if (id == IPOLYGON_COMP)       return new IPolygonComp;
    if (id == ICLOSEDSPLINE_COMP)  return new IClosedSplineComp;
    if (id == IGRAPHIC_COMPS)      return new IGraphicComps;
    if (id == ISTENCIL_COMP)       return new IStencilComp;
    if (id == IRASTER_COMP)        return new IRasterComp;
    
    if (id == BORDER_COMP)         return new BorderComp;
    if (id == BUTTON_COMP)         return new ButtonComp;
    if (id == BITMAP_COMP)         return new BitmapComp;
    if (id == ADJUSTER_COMP)       return new AdjusterComp;
    if (id == DECK_COMP)           return new DeckComp;
    if (id == DIALOG_CLASS)        return new DialogClass;
    if (id == FBROWSER_COMP)       return new FBrowserComp;
    if (id == STRBROWSER_COMP)     return new StrBrowserComp;
    if (id == GRBLOCK_COMP)        return new GrBlockComp;
    if (id == PANELCONTROL_COMP)   return new PanelCtrlComp;
    if (id == FRAME_COMP)          return new FrameComp;
    if (id == GLUE_COMP)           return new GlueComp;
    if (id == HBOX_COMP)           return new HBoxComp;
    if (id == SHAPER_COMP)         return new ShaperComp;
    if (id == MENUBAR_COMP)        return new MenuBarComp;
    if (id == POPUPMENU_COMP)      return new PopupMenuComp;
    if (id == MARGINFRAME_COMP)    return new MarginFrameComp;
    if (id == MENUITEM_COMP)       return new MenuItemComp;
    if (id == MESSAGE_COMP)        return new MessageComp;
    if (id == MONOSCENE_COMP)      return new MonoSceneComp;
    if (id == MONOSCENE_CLASS)     return new MonoSceneClass;
    if (id == EDITOR_COMP)         return new EditorComp;
    if (id == IBVIEWER_COMP)       return new IBViewerComp;
    if (id == PANNER_COMP)         return new PannerComp;
    if (id == PULLMENU_COMP)       return new PullMenuComp;
    if (id == MENUBODY_COMP)       return new MenuBodyComp;
    if (id == SCENE_COMP)          return new SceneComp;
    if (id == SCROLLER_COMP)       return new ScrollerComp;
    if (id == SLIDER_COMP)         return new SliderComp;
    if (id == STREDIT_COMP)        return new StrEditComp;
    if (id == TEXTEDIT_COMP)       return new TextEditComp;
    if (id == VBOX_COMP)           return new VBoxComp;
    if (id == VIEWPORT_COMP)       return new ViewportComp;
    if (id == COMMANDCONTROL_COMP) return new CommandCtrlComp;

    if (id == ITEXT_PS)            return new IPSView;
    if (id == IKEY_PS)             return new IPSView;
    if (id == ILINE_PS)            return new IPSView;
    if (id == IMULTILINE_PS)       return new IPSView;
    if (id == ISPLINE_PS)          return new IPSView;
    if (id == IRECT_PS)            return new IPSView;
    if (id == IELLIPSE_PS)         return new IPSView;
    if (id == IPOLYGON_PS)         return new IPSView;
    if (id == ICLOSEDSPLINE_PS)    return new IPSView;
    if (id == ISTENCIL_PS)         return new IPSView;
    if (id == IRASTER_PS)          return new IPSView;
    if (id == GRBLOCK_PS)          return new IPSView;
    if (id == IPS_VIEWS)           return new PostScriptViews;
            

    return Creator::Create(id);
}
