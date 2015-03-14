/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Tray - composes interactors into (possibly) overlapping layouts.
 */

#ifndef iv2_6_tray_h
#define iv2_6_tray_h

#include <InterViews/resource.h>

#include <IV-2_6/InterViews/scene.h>
#include <IV-2_6/InterViews/shape.h>

#include <IV-2_6/_enter.h>

class TrayElement;
class TSolver;

class TGlue : public Resource {
public:
    TGlue(int w = 0, int h = 0, int hstretch = hfil, int vstretch = vfil);
    TGlue(int w, int h, int hshr, int hstr, int vshr, int vstr);
    ~TGlue();

    Shape* GetShape();
private:
    Shape* shape;
};

class Tray : public Scene {
public:
    Tray(Interactor* background = nil);
    Tray(const char*, Interactor* background = nil);
    ~Tray();
    
    virtual void Draw();
    virtual void Reshape(Shape&);
    virtual void GetComponents(Interactor**, int, Interactor**&, int&);

    void Align(Alignment, Interactor*, TGlue* = nil);
    void Align(Alignment, Interactor*, Alignment, Interactor*, TGlue* = nil);
    void Align(
	Alignment, Interactor*, Interactor*,
	Interactor* = nil, Interactor* = nil, Interactor* = nil,
	Interactor* = nil, Interactor* = nil
    );
    void HBox(
	Interactor*, Interactor*,
	Interactor* = nil, Interactor* = nil, Interactor* = nil,
	Interactor* = nil, Interactor* = nil
    );
    void VBox(
	Interactor*, Interactor*,
	Interactor* = nil, Interactor* = nil, Interactor* = nil,
	Interactor* = nil, Interactor* = nil
    );
protected:
    virtual void DoInsert(Interactor*, boolean, Coord& x, Coord& y);
    virtual void DoChange(Interactor*);
    virtual void DoRemove(Interactor*);
    virtual void Reconfig();
    virtual void Resize();
private:
    int nelements;
    TrayElement* head;
    TrayElement* tail;
    Interactor* bg;
    TSolver* tsolver;

    void Init(Interactor*);
    void ComponentBounds(int&, int&);
    boolean AlreadyInserted(Interactor*);
    void CalcShape();
    void PlaceElement(TrayElement*);
    boolean TrayOrBg(Interactor*);
};

inline Shape* TGlue::GetShape () { return shape; }

#include <IV-2_6/_leave.h>

#endif
