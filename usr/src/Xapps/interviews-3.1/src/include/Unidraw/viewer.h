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
 * Viewer - a GraphicBlock that displays the graphic contributed by a
 * GraphicView.
 */

#ifndef unidraw_viewer_h
#define unidraw_viewer_h

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/grblock.h>

#include <IV-2_6/_enter.h>

class Damage;
class Editor;
class GraphicComp;
class GraphicView;
class Grid;
class KeyMap;
class Manipulator;
class Rubberband;
class Selection;
class State;
class TextDisplay;
class Tool;
class Transformer;
class UPage;
class ViewerView;

class Viewer : public GraphicBlock {
public:
    Viewer(
        Editor*, GraphicView*, UPage*, Grid* = nil, 
        Coord = 0, Coord = 0, Orientation = Normal,
	Alignment = Center, Zooming = Binary
    );
    Viewer(
        const char*, Editor*, GraphicView*, UPage*, Grid* = nil, 
        Coord = 0, Coord = 0, Orientation = Normal,
	Alignment = Center, Zooming = Binary
    );
    virtual ~Viewer();

    virtual void Update();
    virtual void Adjust(Perspective&);
    virtual void Handle(Event&);
    virtual void Reconfig();
    virtual void Draw();

    virtual void SetGraphicView(GraphicView*);
    virtual void SetPage(UPage*);
    virtual void SetGrid(Grid*);
    virtual void SetMagnification(float);
    virtual void SetOrientation(Orientation);

    virtual Graphic* GetGraphic();
    virtual GraphicView* GetGraphicView();
    virtual UPage* GetPage();
    virtual Grid* GetGrid();
    virtual Orientation GetOrientation();
    virtual Editor* GetEditor();
    virtual Selection* GetSelection();
    virtual Damage* GetDamage();

    virtual void InitRubberband(Rubberband*);
    virtual void InitTextDisplay(TextDisplay*, Painter*);
    virtual void IncurTextDisplayDamage(TextDisplay*, Painter*);

    virtual void CenterOp();
    virtual void Magnify(Coord, Coord, Coord, Coord);
    virtual void ReduceToFit();
    virtual void Constrain(Coord&, Coord&);
    virtual void UseTool(Tool*);
    virtual void Align(GraphicComp*, Alignment);
protected:
    void Init(Editor*, GraphicView*, UPage*, Grid*, Coord, Coord, Orientation);
    void Init(Editor*, GraphicView*, UPage*, Grid*);
    Tool* CurTool();
    Transformer* GetTransformer();

    virtual void Manipulate(Manipulator*, Event&); // direct manipulation loop
    virtual void UseTool(Tool*, Event&);
    virtual void MomentaryUseTool(const char*, Event&);

    virtual void GetGraphicBox(Coord&, Coord&, Coord&, Coord&);
    virtual void Reorient();
    virtual void UpdateMagnifVar();
    virtual float LimitMagnification(float);
    virtual ClassId ViewCategory();

    virtual void Redraw(Coord, Coord, Coord, Coord);
    virtual void Resize();
private:
    Editor* _editor;
    Damage* _damage;
    GraphicView* _gview;
    UPage* _page;
    Grid* _grid;
    Orientation _orientation;
    ViewerView* _viewerView;
};

#include <IV-2_6/_leave.h>

#endif
