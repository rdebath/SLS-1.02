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
 * Implementation of IBHBox and ToolPanel.
 */

#include "ibadjuster.h"
#include "ibbitmap.h"
#include "ibborder.h"
#include "ibbox.h"
#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcommandctrl.h"
#include "ibctrl.h"
#include "ibed.h"
#include "ibfbrowser.h"
#include "ibframe.h"
#include "ibglue.h"
#include "ibgrblock.h"
#include "ibkybd.h"
#include "ibmenu.h"
#include "ibmessage.h"
#include "ibpanner.h"
#include "ibpanelctrl.h"
#include "ibscroller.h"
#include "ibslider.h"
#include "ibstred.h"
#include "ibstrbrowser.h"
#include "ibtextedit.h"
#include "ibtoolpanel.h"
#include "ibviewer.h"
#include "ibvars.h"

#include <Unidraw/catalog.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/keymap.h>
#include <Unidraw/kybd.h>
#include <Unidraw/iterator.h>
#include <Unidraw/uarray.h>
#include <Unidraw/unidraw.h>

#include <Unidraw/Tools/grcomptool.h>

#include <InterViews/box.h>
#include <InterViews/shape.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*****************************************************************************/

class IBHBox : public MonoScene {
public:
    IBHBox();

    UArray& Components();

    virtual void Reconfig();
protected:
    virtual void Resize();
    virtual void DoInsert(Interactor*, boolean, Coord&, Coord&);
    virtual void DoRemove(Interactor*);
private:
    class HBox* LastHBox();
    class VBox* vbox();
    Interactor* elem(int);

    void Recompose();
    void DeleteHBoxes();
private:
    boolean _recomposing;
    UArray _elems;
};

inline VBox* IBHBox::vbox () { return (VBox*) interior(); }
inline Interactor* IBHBox::elem (int i) { return (Interactor*) _elems[i]; }
inline UArray& IBHBox::Components () { return _elems; }

IBHBox::IBHBox () {
    Insert(new VBox);
    _recomposing = false;
}

void IBHBox::Reconfig () {
    MonoScene::Reconfig();
    shape->Rect(0, max(shape->height, 1));
    shape->Rigid(hfil, hfil, 0, 0);
}

void IBHBox::Resize () {
    if (_recomposing) {
        MonoScene::Resize();

    } else {
        _recomposing = true;
        Recompose();
        _recomposing = false;
    }
}

void IBHBox::DoInsert (Interactor* i, boolean b, Coord& x, Coord& y) {
    if (interior() == nil) {
        MonoScene::DoInsert(i, b, x, y);

    } else {
        _elems.Insert(i, _elems.Count());
        LastHBox()->Insert(i);
    }
}

void IBHBox::DoRemove (Interactor* i) {
    if (i == interior()) {
        MonoScene::DoRemove(i);

    } else if (i->Parent() != this) {
        i->Parent()->Remove(i);

        for (int j = 0; j < _elems.Count(); ++j) {
            if (elem(j) == i) {
                _elems.Remove(j);
            }
        }
    }
}

HBox* IBHBox::LastHBox () {
    HBox* hbox;
    Interactor** interactors;
    int n;
    vbox()->GetComponents(nil, 0, interactors, n);
    
    if (n == 0) {
        vbox()->Insert(hbox = new HBox);
    } else {
        hbox = (HBox*) interactors[n-1];
    }
    delete interactors;
    return hbox;
}

void IBHBox::DeleteHBoxes () {
    for (int i = 0; i < _elems.Count(); ++i) {
        elem(i)->Parent()->Remove(elem(i));
    }

    Interactor* inter = interior();
    Remove(inter);
    delete inter;
    interior(nil);
    Insert(new VBox);
}

void IBHBox::Recompose () {
    DeleteHBoxes();

    for (int i = 0; i < _elems.Count();) {
        int width = 0;
        HBox* new_hbox = nil;

        do {
            new_hbox = (new_hbox == nil) ? new HBox : new_hbox;
            int new_width = width + elem(i)->GetShape()->width;

            if (new_width > xmax+1 && width > 0) {
                break;
            } else {
                width = new_width;
            }

            new_hbox->Insert(elem(i));

        } while (++i < _elems.Count());

        if (new_hbox != nil) vbox()->Insert(new_hbox);
    }

    if (_elems.Count() == 0) {
        Change(interior());

    } else {
        elem(0)->Parent()->Change(elem(0));
    }
}

/*****************************************************************************/
static void GetToolsPath(char* toolspath) {
    const char* tooldir = getenv("TOOLDIR");
    if (tooldir == nil) {
	sprintf(toolspath, "./");

    } else {
	sprintf(toolspath, "%s", tooldir);
    }
}
/*****************************************************************************/

ToolPanel::ToolPanel (ControlState* crtl, KeyMap* key) {
    SetClassName("ToolPanel");
    Insert(new IBHBox);

    _curCtrl = crtl;
    _curCtrl->Reference();
    _keymap = key;

    InitEditorInfo();
    InitTools();

    if (_initEdInfo) {
        char setup[CHARBUFSIZE];
        char toolspath[CHARBUFSIZE];

	GetToolsPath(toolspath);
	sprintf(setup, "%s/TOOLS", toolspath);
        unidraw->GetCatalog()->Save(_edInfo, setup);
    }
}

ToolPanel::~ToolPanel () {
    Unref(_curCtrl);

    Iterator ed;
    unidraw->First(ed);

    if (unidraw->Done(ed) && _edInfo != nil) {
        delete _edInfo;
    }
}

Tool* ToolPanel::GetCurTool () {
    Tool* tool = nil;

    UControl* c = (UControl*) _curCtrl->Selection();
    if (c != nil) {
        tool = (Tool*) c->GetControlInfo()->GetOwner();
    }
    return tool;
}

Scene* ToolPanel::GetScene () { return (Scene*) interior(); }
void ToolPanel::AddTool (UControl* ctrl) { GetScene()->Insert(ctrl); }

void ToolPanel::RemoveTool (UControl* ctrl) {
    GetScene()->Remove(ctrl);
    Tool* tool = (Tool*) ctrl->GetControlInfo()->GetOwner();

    if (tool == GetCurTool()) {
        _keymap->Execute(CODE_SELECT);
    }

    _keymap->Unregister(ctrl);
    delete ctrl;
}

void ToolPanel::Install (const char* toolName, Tool* tool) {
    char setup[CHARBUFSIZE];
    char filename[CHARBUFSIZE];
    char toolspath[CHARBUFSIZE];

    Catalog* catalog = unidraw->GetCatalog();
    GetToolsPath(toolspath);

    sprintf(setup, "%s/TOOLS", toolspath);
    sprintf(filename, "%s/%s", toolspath, toolName);

    if (tool == nil) {
        catalog->Retrieve(filename, tool);

        if (tool != nil) {
	    Uninstall(toolName);
            ControlInfo* info = tool->GetControlInfo();
            UControl* ctrl = new ToolPanelControl(info, _curCtrl);
	    AddTool(ctrl);

            if (_edInfo != nil ) {
                _edInfo->UnregisterName(toolName);
                _edInfo->Register(toolName);
                catalog->Save(tool, filename);
                catalog->Save(_edInfo, setup);
            }
	}

    } else {
	Uninstall(toolName);
        ControlInfo* info = tool->GetControlInfo();
	UControl* ctrl = new ToolPanelControl(info, _curCtrl);
        AddTool(ctrl);

        if (_edInfo != nil ) {
            _edInfo->UnregisterName(toolName);
            _edInfo->Register(toolName);
            catalog->Save(tool, filename);
            catalog->Save(_edInfo, setup);
        }
    }
}

void ToolPanel::Uninstall(const char* toolName) {
    UControl* ctrl = FindControl(toolName);
    Catalog* catalog = unidraw->GetCatalog();

    char setup[CHARBUFSIZE];
    char toolspath[CHARBUFSIZE];

    GetToolsPath(toolspath);
    sprintf(setup, "%s/TOOLS", toolspath);

    if (ctrl != nil) {
        if (_edInfo != nil) {
            _edInfo->UnregisterName(toolName);
            catalog->Save(_edInfo, setup);
        }
        RemoveTool(ctrl);
    }
}

boolean ToolPanel::InstallOrRemove (Tool* tool, const char* toolName) {
    char setup[CHARBUFSIZE];
    char filename[CHARBUFSIZE];
    char toolspath[CHARBUFSIZE];

    boolean install = false;

    UControl* ctrl = FindControl(toolName);
    Catalog* catalog = unidraw->GetCatalog();

    GetToolsPath(toolspath);

    sprintf(setup, "%s/TOOLS", toolspath);
    sprintf(filename, "%s/%s", toolspath, toolName);

    if (ctrl == nil) {
        if (tool == nil) {
            catalog->Retrieve(toolName, tool);
        }

	if (tool != nil) {
            ctrl = new ToolPanelControl(tool->GetControlInfo(), _curCtrl);
	    AddTool(ctrl);

            if (_edInfo != nil ) {
                _edInfo->Register(toolName);
                catalog->Save(tool, filename);
                catalog->Save(_edInfo, setup);
            }
	    install = true;
	}

    } else {
        if (_edInfo != nil) {
            _edInfo->UnregisterName(toolName);
            catalog->Save(_edInfo, setup);
        }
	RemoveTool(ctrl);
    }

    return install;
}

void ToolPanel::Include (Tool* tool, const char* toolName) {
    char path[CHARBUFSIZE];
    char toolspath[CHARBUFSIZE];

    GetToolsPath(toolspath);

    ControlInfo* ctrlInfo = tool->GetControlInfo();
    UControl* ctrl = new ToolPanelControl(ctrlInfo, _curCtrl);
    _keymap->Register(ctrl);
    AddTool(ctrl);

    if (_initEdInfo && *toolName != '\0') {
        sprintf(path, "%s/%s", toolspath, toolName);
        _edInfo->Register(toolName);
        unidraw->GetCatalog()->Save(tool, path);
    }
}

void ToolPanel::InitEditorInfo () {
    char setup[CHARBUFSIZE];
    char toolspath[CHARBUFSIZE];

    Catalog* catalog = unidraw->GetCatalog();
    GetToolsPath(toolspath);

    sprintf(setup, "%s/TOOLS", toolspath);

    if (catalog->Retrieve(setup, _edInfo)) {
        _initEdInfo = false;
    } else {
        _edInfo = new EditorInfo;
        _initEdInfo = true;
    }
}

void ToolPanel::InitTools () {
    if (_edInfo == nil || _initEdInfo ) {
        InitCompTools();
    } else {
        ReadCompTools();
    }
}

void ToolPanel::InitCompTools () {
    const int unit = round(.5*cm);
    AdjusterComp* adjcomp;

    GlueGraphic* hglue = new GlueGraphic(0, Horizontal, nil, stdgraphic);
    GlueComp* hglueComp = new GlueComp(hglue);
    Include(
        new GraphicCompTool(
            new ControlInfo("HGlue", KLBL_HGLUE, CODE_HGLUE), hglueComp
        ), "HGlue.Tool"
    );

    GlueGraphic* vglue = new GlueGraphic(0, Vertical, nil, stdgraphic);
    GlueComp* vglueComp = new GlueComp(vglue);
    Include(
        new GraphicCompTool(
            new ControlInfo("VGlue", KLBL_VGLUE, CODE_VGLUE), vglueComp
        ), "VGlue.Tool"
    );

    MenuItemGraphic* mig = new MenuItemGraphic("MenuItem", nil, stdgraphic);
    MenuItemComp* micomp = new MenuItemComp(mig);
    Include(
        new GraphicCompTool(
            new ControlInfo(micomp, KLBL_MENUITEM, CODE_MENUITEM), micomp
        ), "Menu_Item.Tool"
    );

    PushButtonGraphic* pbg = new PushButtonGraphic("push", nil, stdgraphic);
    ButtonComp* pbcomp = new ButtonComp(pbg);
    Include(
        new GraphicCompTool(
            new ControlInfo(pbcomp, KLBL_PUSHBUTTON, CODE_PUSHBUTTON), pbcomp
        ), "Push_Button.Tool"
    );

    RadioButtonGraphic* rbg = new RadioButtonGraphic("radio", nil, stdgraphic);
    ButtonComp* rbcomp = new ButtonComp(rbg);
    Include(
        new GraphicCompTool(
            new ControlInfo(rbcomp, KLBL_RADIOBUTTON, CODE_RADIOBUTTON), rbcomp
        ), "Radio_Button.Tool"
    );

    CheckBoxGraphic* cbg = new CheckBoxGraphic("check", nil, stdgraphic);
    ButtonComp* cbcomp = new ButtonComp(cbg);
    Include(
        new GraphicCompTool(
            new ControlInfo(cbcomp, KLBL_CHECKBOX, CODE_CHECKBOX), cbcomp
        ), "Check_Box.Tool"
    );

    BorderGraphic* hborder = new BorderGraphic(Horizontal,nil,stdgraphic,unit);
    BorderComp* hborderComp = new BorderComp(hborder);
    Include(
        new GraphicCompTool(
            new ControlInfo(hborderComp, KLBL_HBORDER,CODE_HBORDER),hborderComp
        ), "HBorder.Tool"
    );

    BorderGraphic* vborder = new BorderGraphic(Vertical, nil, stdgraphic,unit);
    BorderComp* vborderComp = new BorderComp(vborder);
    Include(
        new GraphicCompTool(
            new ControlInfo(vborderComp, KLBL_VBORDER,CODE_VBORDER),vborderComp
        ), "VBorder.Tool"
    );

    ScrollerGraphic* hscr =new ScrollerGraphic(Horizontal,nil,stdgraphic,unit);
    ScrollerComp* hscrComp = new ScrollerComp(hscr);
    Include(
        new GraphicCompTool(
            new ControlInfo(hscrComp, KLBL_HSCROLLER, CODE_HSCROLLER), hscrComp
        ), "HScroller.Tool"
    );

    ScrollerGraphic* vscr = new ScrollerGraphic(Vertical, nil,stdgraphic,unit);
    ScrollerComp* vscrComp = new ScrollerComp(vscr);
    Include(
        new GraphicCompTool(
            new ControlInfo(vscrComp, KLBL_VSCROLLER, CODE_VSCROLLER), vscrComp
        ), "VScroller.Tool"
    );

    SliderGraphic* slgr = new SliderGraphic(unit, unit, nil,stdgraphic);
    SliderComp* sliderComp = new SliderComp(slgr);
    Include(
        new GraphicCompTool(
            new ControlInfo(sliderComp, KLBL_SLIDER, CODE_SLIDER), sliderComp
        ), "Slider.Tool"
    );

    PannerComp* panComp = new PannerComp;
    Include(
        new GraphicCompTool(
            new ControlInfo("Panner", KLBL_PANNER,CODE_PANNER), panComp
        ), "Panner.Tool"
    );

    MessageGraphic* msg = new MessageGraphic("Message", nil,stdgraphic);
    MessageComp* msgComp = new MessageComp(msg);
    Include(
        new GraphicCompTool(
            new ControlInfo(msgComp, KLBL_MESSAGE, CODE_MESSAGE), msgComp
        ), "Message.Tool"
    );

    StrEditGraphic* stred = new StrEditGraphic("",nil,stdgraphic);
    StrEditComp* stredComp = new StrEditComp(stred);
    Include(
        new GraphicCompTool(
            new ControlInfo("StringEditor", KLBL_STREDIT, CODE_STREDIT),
            stredComp
        ), "String_Editor.Tool"
    );

    GrBlockComp* grblockComp = new GrBlockComp(
        new GrBlockGraphic(nil, stdgraphic)
    );
    Include(
        new GraphicCompTool(
            new ControlInfo("GraphicBlock", KLBL_GRBLOCK, CODE_GRBLOCK),
            grblockComp
        ), "Graphic_Block.Tool"
    );


    StrBrowserGraphic* fb = new StrBrowserGraphic(
	"FileBrowser", 0, 0, nil, stdgraphic
    );
    FBrowserComp* fbComp = new FBrowserComp(fb);
    Include(
        new GraphicCompTool(
            new ControlInfo("FileBrowser", KLBL_FBROWSER,CODE_FBROWSER),fbComp
        ), "File_Browser.Tool"
    );

    StrBrowserGraphic* tg = new StrBrowserGraphic(
	"TextEditor", 0, 0, nil, stdgraphic
    );
    TextEditComp* tComp = new TextEditComp(tg);
    Include(
        new GraphicCompTool(
            new ControlInfo(
		"TextEditor", KLBL_TEXTEDITOR, CODE_TEXTEDITOR
	    ),tComp
        ), "TextEditor.Tool"
    );

    StrBrowserGraphic* strg = new StrBrowserGraphic(
	"StringBrowser", 0, 0, nil, stdgraphic
    );
    StrBrowserComp* strBrowserComp = new StrBrowserComp(strg);
    Include(
        new GraphicCompTool(
            new ControlInfo(
		"StringBrowser", KLBL_STRBROWSER, CODE_STRBROWSER
	    ),strBrowserComp
        ), "StringBrowser.Tool"
    );

    LMoverGraphic* lgr = new LMoverGraphic(nil, stdgraphic);
    adjcomp = new AdjusterComp(lgr);
    Include(
        new GraphicCompTool(
            new ControlInfo(adjcomp, KLBL_LMOVER, CODE_LMOVER), adjcomp
        ), "Left_Mover.Tool"
    );

    RMoverGraphic* rgr = new RMoverGraphic(nil, stdgraphic);
    adjcomp = new AdjusterComp(rgr);
    Include(
        new GraphicCompTool(
            new ControlInfo(adjcomp, KLBL_RMOVER, CODE_RMOVER), adjcomp
        ), "Right_Mover.Tool"
    );

    UMoverGraphic* ugr = new UMoverGraphic(nil, stdgraphic);
    adjcomp = new AdjusterComp(ugr);
    Include(
        new GraphicCompTool(
            new ControlInfo(adjcomp, KLBL_UMOVER, CODE_UMOVER), adjcomp
        ), "Up_Mover.Tool"
    );

    DMoverGraphic* dgr = new DMoverGraphic(nil, stdgraphic);
    adjcomp = new AdjusterComp(dgr);
    Include(
        new GraphicCompTool(
            new ControlInfo(adjcomp, KLBL_DMOVER, CODE_DMOVER), adjcomp
        ), "Down_Mover.Tool"
    );

    EnlargerGraphic* engr = new EnlargerGraphic(nil, stdgraphic);
    adjcomp = new AdjusterComp(engr);
    Include(
        new GraphicCompTool(
            new ControlInfo(adjcomp, KLBL_ENLARGER, CODE_ENLARGER), adjcomp
        ), "Enlarger.Tool"
    );

    ReducerGraphic* regr = new ReducerGraphic(nil, stdgraphic);
    adjcomp = new AdjusterComp(regr);
    Include(
        new GraphicCompTool(
            new ControlInfo(adjcomp, KLBL_REDUCER, CODE_REDUCER), adjcomp
        ), "Reducer.Tool"
    );
    MarginFrameGraphic* mg = new MarginFrameGraphic(nil, stdgraphic);
    MarginFrameComp* mfc = new MarginFrameComp(mg);
    Include(
        new GraphicCompTool(
            new ControlInfo(
		"MarginFrame", KLBL_MARGINFRAME, CODE_MARGINFRAME
	    ), mfc
        ), "MarginFrame.Tool"
    );
    PDMenuGraphic* pdgraphic = new PDMenuGraphic(
        "PulldownMenu", nil, stdgraphic
    );
    PullMenuComp* pdcomp = new PullMenuComp(pdgraphic);
    Include(
        new GraphicCompTool(
            new ControlInfo("PulldownMenu", KLBL_PDCMD, CODE_PDCMD), pdcomp
	), "PullDownMenu.Tool"
    );

    PRMenuGraphic* prgraphic = new PRMenuGraphic(
        "PullRightMenu", nil, stdgraphic
    );
    PullMenuComp* prcomp = new PullMenuComp(prgraphic);
    Include(
        new GraphicCompTool(
            new ControlInfo("PullrightMenu", KLBL_PRCMD, CODE_PRCMD), prcomp
	), "PullRightMenu.Tool"
    );
    PanelCtrlGraphic* comgr = new PanelCtrlGraphic(
        "CommandControl", Horizontal, nil, stdgraphic
    );
    CommandCtrlComp* comComp = new CommandCtrlComp(comgr);
    Include(
        new GraphicCompTool(
            new ControlInfo(
                "CommandControl", KLBL_COMMANDCTRL, CODE_COMMANDCTRL
            ), comComp
        ), "CommandControl.Tool"
    );

    PanelCtrlGraphic* hpcgr = new PanelCtrlGraphic(
        "HPanelControl", Horizontal, nil, stdgraphic
    );
    PanelCtrlComp* hpcComp = new PanelCtrlComp(hpcgr);
    Include(
        new GraphicCompTool(
            new ControlInfo("HPanelControl", KLBL_HPCTRL, CODE_HPCTRL),
            hpcComp
        ), "HPanelControl.Tool"
    );

    PanelCtrlGraphic* vpcgr = new PanelCtrlGraphic(
        "VPanelControl", Vertical, nil, stdgraphic
    );
    PanelCtrlComp* vpcComp = new PanelCtrlComp(vpcgr);
    Include(
        new GraphicCompTool(
            new ControlInfo("VPanelControl", KLBL_VPCTRL, CODE_VPCTRL),
            vpcComp
        ), "VPanelControl.Tool"
    );
    IBViewerComp* vcomp = new IBViewerComp(new IBViewerGraphic(
        nil, stdgraphic)
    );
    Include(
        new GraphicCompTool(
            new ControlInfo(
		"Viewer", KLBL_IBVIEWER, CODE_IBVIEWER
	    ), vcomp
        ), "Viewer.Tool"
    );
}

void ToolPanel::ReadCompTools () {
    char path[CHARBUFSIZE];
    char toolspath[CHARBUFSIZE];

    GetToolsPath(toolspath);

    for (int i = 0; i < _edInfo->Count(); ++i) {
        const char* toolName = _edInfo->GetName(i);
        const char* info = _edInfo->GetInfo(i);
        Tool* tool;

        sprintf(path, "%s/%s", toolspath, toolName);
        unidraw->GetCatalog()->Retrieve(path, tool);
	if (tool != nil) {
	    GraphicCompTool* grtool = (GraphicCompTool*) tool;
            Include(tool, toolName);
	}
    }
}

UControl* ToolPanel::FindControl (const char* toolName) {
    UArray& elems = ((IBHBox*) GetScene())->Components();
    UControl* ctrl = nil;

    for (int i = 0; i < elems.Count(); ++i) {
        UControl* testCtrl = (UControl*) elems[i];
        Tool* testTool = (Tool*) testCtrl->GetControlInfo()->GetOwner();
        const char* testToolName = unidraw->GetCatalog()->GetName(testTool);
	if (testToolName != nil) {
            char* filename = strrchr(testToolName, '/') + 1;

            if (filename != nil && strcmp(toolName, filename) == 0) {
                ctrl = testCtrl;
                break;
            }
	}
    }
    return ctrl;
}
