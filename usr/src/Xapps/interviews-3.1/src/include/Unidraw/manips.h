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
 * ManipGroup - composition of Manipulators
 * DragManip - downclick-drag-upclick-style manipulation
 * VertexManip - multi-click DragManip
 * ConnectManip - DragManip that snaps to nearest connector
 * TextManip - creates and edits text
 */

#ifndef unidraw_manips_h
#define unidraw_manips_h

#include <Unidraw/globals.h>
#include <Unidraw/manip.h>
#include <InterViews/event.h>

#include <IV-2_6/_enter.h>

class ConnectorView;
class GrowingVertices;
class Iterator;
class Painter;
class Rubberband;
class Selection;
class TextBuffer;
class TextDisplay;
class Transformer;
class UList;

class ManipGroup : public Manipulator {
public:
    ManipGroup(Viewer* = nil, Tool* = nil);
    virtual ~ManipGroup();

    virtual void Grasp(Event&);
    virtual boolean Manipulating(Event&);
    virtual void Effect(Event&);

    virtual void SetViewer(Viewer*);
    virtual void SetTool(Tool*);

    virtual Viewer* GetViewer();
    virtual Tool* GetTool();

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    virtual Manipulator* GetManip(Iterator);
    virtual void SetManip(Manipulator*, Iterator&);

    virtual void Append(
        Manipulator*, Manipulator* =nil, Manipulator* =nil, Manipulator* =nil
    );
    virtual void Prepend(
        Manipulator*, Manipulator* =nil, Manipulator* =nil, Manipulator* =nil
    );
    virtual void InsertBefore(Iterator, Manipulator*);
    virtual void InsertAfter(Iterator, Manipulator*);
    virtual void Remove(Manipulator*);
    virtual void Remove(Iterator&);
protected:
    Manipulator* Manip(UList*);
    UList* Elem(Iterator);
protected:
    UList* _kids;
    Viewer* _viewer;
    Tool* _tool;
};

class DragManip : public Manipulator {
public:
    DragManip(
        Viewer*, Rubberband*, Transformer* = nil, Tool* = nil, 
        DragConstraint = None
    );
    DragManip(
        Viewer*, Rubberband*, Transformer*, Tool*, DragConstraint, Coord, Coord
    );
    virtual ~DragManip();
    
    virtual void Grasp(Event&);
    virtual boolean Manipulating(Event&);
    virtual void Effect(Event&);

    virtual void SetViewer(Viewer*);
    virtual void SetRubberband(Rubberband*);
    virtual void SetTransformer(Transformer*);
    virtual void SetTool(Tool*);
    virtual void SetConstraint(DragConstraint);

    virtual Viewer* GetViewer();
    virtual Rubberband* GetRubberband();
    virtual Transformer* GetTransformer();
    virtual Tool* GetTool();
    virtual DragConstraint GetConstraint();

    const Event& GraspEvent();
protected:
    void Init(Viewer*, Rubberband*, Transformer*, Tool*, DragConstraint);
    virtual void Constrain(Event&);
private:
    Viewer* _viewer;
    Rubberband* _r;
    Transformer* _relative;
    Tool* _tool;
    DragConstraint _constraint;
    boolean _origPreset;
    Coord _origx, _origy;
    Event _grasp_e;
};    

inline const Event& DragManip::GraspEvent () { return _grasp_e; }

class VertexManip : public DragManip {
public:
    VertexManip(
        Viewer*, GrowingVertices*, Transformer* = nil, Tool* = nil,
        DragConstraint = None
    );

    virtual boolean Manipulating(Event&);

    GrowingVertices* GetGrowingVertices();
};

class ConnectManip : public DragManip {
public:
    ConnectManip(Viewer*, Rubberband*, Transformer* = nil, Tool* = nil);

    virtual boolean Manipulating(Event&);
    ConnectorView* GetTarget();
protected:
    ConnectorView* _target;
};

class TextManip : public Manipulator {
public:
    TextManip(Viewer*, Painter*, Coord tab, Tool* = nil);
    TextManip(Viewer*, Painter*, Coord h, Coord tab, Tool* = nil);
    TextManip(
        Viewer*, const char*, int, Coord, Coord, Painter*, Coord tab,
        Tool* = nil
    );
    TextManip(
        Viewer*, const char*, int, Coord, Coord, Painter*, Coord h, Coord tab,
        Tool* = nil
    );
    virtual ~TextManip();

    virtual void Grasp(Event&);
    virtual boolean Manipulating(Event&);
    virtual void Effect(Event&);

    virtual void SetViewer(Viewer*);
    virtual void SetTool(Tool*);

    virtual Viewer* GetViewer();
    virtual Tool* GetTool();

    virtual Painter* GetPainter();
    virtual Coord GetLineHeight();
    virtual Coord GetTabWidth();

    virtual const char* GetText(int&);
    virtual void GetPosition(Coord&, Coord&);

    const Event& GraspEvent();

    virtual boolean HandleKey(Event&);

    int Dot();
    int Mark();

    void InsertCharacter(char);
    void DeleteCharacter(int);

    void InsertText(const char*, int);
    void DeleteText(int);
    void DeleteLine();
    void DeleteSelection();

    void BackwardCharacter(int = 1),    ForwardCharacter(int = 1);
    void BackwardLine(int = 1),         ForwardLine(int = 1);
    void BackwardWord(int = 1),         ForwardWord(int = 1);

    void BeginningOfLine(),             EndOfLine();
    void BeginningOfWord(),             EndOfWord();
    void BeginningOfSelection(),        EndOfSelection();
    void BeginningOfText(),             EndOfText();

    void Select(int dot);
    void SelectMore(int mark);
    void SelectAll();
    void Select(int dot, int mark);

    boolean Contains(Coord, Coord);
    int Locate(Coord, Coord);
private:
    void Init(
        Viewer*, Painter*, Coord, Coord, Tool*, boolean,const char* =nil,int =0
    );
    void InitTextDisplay(const char*, int);
    void PlaceTextDisplay (Coord, Coord);
    void CheckBuf(int more);
private:
    boolean _prepositioned;
    boolean _selecting;
    Coord _xpos, _ypos;
    Viewer* _viewer;
    Selection* _selection;
    Painter* _painter;
    Tool* _tool;
    PSFont* _font;
    Coord _lineHt, _tabWidth;
    boolean _multiline;
    TextBuffer* _text;
    TextDisplay* _display;
    char* _buf;
    int _bufsize;
    int _dot, _mark;
    Event _grasp_e;
};

inline int TextManip::Dot () { return _dot; }
inline int TextManip::Mark () { return _mark; }
inline const Event& TextManip::GraspEvent () { return _grasp_e; }

#include <IV-2_6/_leave.h>

#endif
