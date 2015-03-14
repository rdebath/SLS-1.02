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
 * Basic composite object for interaction.
 */

#ifndef iv2_6_scene_h
#define iv2_6_scene_h

#include <IV-2_6/InterViews/interactor.h>

#include <IV-2_6/_enter.h>

class Scene : public Interactor {
protected:
    Scene();
public:
    virtual ~Scene();

    void Insert(Interactor*);
    void Insert(Interactor*, Coord x, Coord y, Alignment = BottomLeft);
    void Change(Interactor* = nil);
    void Remove(Interactor*);

    void Move(Interactor*, Coord x, Coord y, Alignment = BottomLeft);
    void Raise(Interactor*);
    void Lower(Interactor*);

    void Propagate(boolean);

    virtual void Highlight(boolean);
protected:
    boolean propagate;

    void PrepareToInsert(Interactor*);
    virtual Interactor* Wrap(Interactor*);
    virtual void DoInsert(Interactor*, boolean, Coord& x, Coord& y);
    virtual void DoChange(Interactor*);
    virtual void DoMove(Interactor*, Coord& x, Coord& y);
    virtual void DoRemove(Interactor*);
    virtual void DoRaise(Interactor*);
    virtual void DoLower(Interactor*);

    void Place(Interactor*, Coord, Coord, Coord, Coord, boolean map = true);

    void Map(Interactor*, boolean raised = true);
    void Unmap(Interactor*);
private:
    void DoAlign(Interactor*, Alignment, Coord& x, Coord& y);

    virtual void Orphan();
};

/* Scene with a single component */
class MonoScene : public Scene {
protected:
    MonoScene();
public:
    virtual ~MonoScene();

    virtual void Draw();
    virtual void GetComponents(Interactor**, int, Interactor**&, int&);
    virtual void Resize();
protected:
    Interactor* interior();
    void interior(Interactor*);

    virtual void DoInsert(Interactor*, boolean, Coord&, Coord&);
    virtual void DoChange(Interactor*);
    virtual void DoRemove(Interactor*);

    virtual void Reconfig();
private:
    Interactor* interior_;
};

inline Interactor* MonoScene::interior() { return interior_; }
inline void MonoScene::interior(Interactor* i) { interior_ = i; }

#include <IV-2_6/_leave.h>

#endif
