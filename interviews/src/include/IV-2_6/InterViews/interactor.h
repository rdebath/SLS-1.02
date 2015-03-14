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
 * Base class for interactive objects.
 */

#ifndef iv2_6_interactor_h
#define iv2_6_interactor_h

#include <InterViews/enter-scope.h>
#include <InterViews/boolean.h>
#include <InterViews/coord.h>
#include <InterViews/geometry.h>
#include <InterViews/glyph.h>
#include <IV-2_6/InterViews/alignment.h>
#include <IV-2_6/InterViews/minmax.h>
#include <IV-2_6/InterViews/textstyle.h>

#include <IV-2_6/_enter.h>

class Bitmap;
class Cursor;
class Event;
class InteractorHandler;
class InteractorWindow;
class ManagedWindow;
class Painter;
class Perspective;
class Scene;
class Sensor;
class Shape;
class Style;
class Window;
class World;

enum CanvasType {
    CanvasShapeOnly, CanvasInputOnly, CanvasInputOutput,
    CanvasSaveUnder, CanvasSaveContents, CanvasSaveBoth
};

class Interactor : public Glyph {
protected:
    Interactor();
    Interactor(const char*);
public:
    virtual ~Interactor();

    /* configuration */
    void Align(Alignment, int w, int h, Coord& l, Coord& b) const;
    void Config(Scene* = nil);
    void Config(World*);
    virtual void Reconfig();
    const char* GetAttribute(const char*) const;
    boolean AttributeIsSet(const char*) const;
    virtual void Reshape(Shape&);
    Shape* GetShape() const;
    void SetCursor(Cursor*);
    Cursor* GetCursor() const;
    const char* GetClassName() const;
    const char* GetInstance() const;
    void GetRelative(Coord& x, Coord &y, Interactor* = nil) const;
    void GetRelative(Coord& x, Coord &y, World*) const;
    void GetPosition(Coord& left, Coord& bottom) const;

    /* traversing hierarchy */
    virtual void GetComponents(Interactor**, int, Interactor**&, int&);
    Scene* Parent() const;
    World* GetWorld() const;

    /* output */
    Canvas* GetCanvas() const;
    virtual void Draw();
    virtual void Highlight(boolean on);
    void Flush();
    void Sync();

    /* input events */
    boolean Check();
    int CheckQueue();
    void Listen(Sensor*);
    void Poll(Event&);
    void Read(Event&);
    boolean Read(long sec, long usec, Event&);
    void UnRead(Event&);
    void Run();
    void QuitRunning(Event&);
    virtual void Handle(Event&);

    /* subject-view communication */
    virtual void Adjust(Perspective&);
    Perspective* GetPerspective() const;
    virtual void Update();

    /* canvas properties */
    void SetCanvasType(CanvasType);
    CanvasType GetCanvasType() const;

    /* top-level interactors */
    ManagedWindow* GetTopLevelWindow() const;

    /* glyph */
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();
protected:
    Shape* shape;			/* desired shape characteristics */
    Canvas* canvas;			/* actual display area */
    Perspective* perspective;		/* portion displayed */
    Coord xmax;				/* canvas->Width() - 1 */
    Coord ymax;				/* canvas->Height() - 1 */
    Sensor* input;			/* normal input event interest */
    Painter* output;			/* normal output parameters */

    virtual void Redraw(Coord left, Coord bottom, Coord right, Coord top);
    virtual void RedrawList(int n, Coord[], Coord[], Coord[], Coord[]);
    virtual void Resize();

    virtual void Activate();
    virtual void Deactivate();

    void SetClassName(const char*);
    void SetInstance(const char*);
private:
    friend class InteractorHandler;
    friend class InteractorWindow;
    friend class Scene;
    friend class World;

    Scene* parent;
    Style* style;
    const char* classname;
    const char* instance;
    Sensor* cursensor;
    InteractorWindow* window;
    World* world;
    Window* insert_window;
    ManagedWindow* managed_window;
    Cursor* cursor_;
    CanvasType canvas_type_;
    InteractorHandler* handler_;

    void Init();
    void DefaultConfig(boolean&);
    void DoConfig(boolean);

    virtual void Orphan();
};

inline Scene* Interactor::Parent() const { return parent; }
inline Perspective* Interactor::GetPerspective() const { return perspective; }
inline Shape* Interactor::GetShape() const { return shape; }

#include <IV-2_6/_leave.h>

#endif
