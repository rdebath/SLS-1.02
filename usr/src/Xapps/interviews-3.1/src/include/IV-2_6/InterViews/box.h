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
 * Boxes are used to compose side-by-side.
 */

#ifndef iv2_6_box_h
#define iv2_6_box_h

#include <IV-2_6/InterViews/scene.h>

#include <IV-2_6/_enter.h>

class BoxElement;
class BoxCanonical;

class Box : public Scene {
public:
    virtual ~Box();

    void Align(Alignment);
    void Draw();
    void GetComponents(Interactor**, int, Interactor**&, int&);
    void Resize();
protected:
    Alignment align;

    Box();

    virtual void ComputeShape(Shape*);
    virtual void GetActual(int& major, int& minor);
    virtual void GetCanonical(Shape*, BoxCanonical&);
    BoxElement* Head();
    virtual void PlaceElement(
	Interactor*, Coord major, int len, int thick, int minor
    );
    virtual void Reconfig();
private:
    int nelements;
    BoxElement* head;
    BoxElement* tail;

    void DoInsert(Interactor*, boolean, Coord& x, Coord& y);
    void DoChange(Interactor*);
    void DoRemove(Interactor*);
};

class HBox : public Box {
public:
    HBox();
    HBox(Interactor*);
    HBox(Interactor*, Interactor*);
    HBox(Interactor*, Interactor*, Interactor*);
    HBox(Interactor*, Interactor*, Interactor*, Interactor*);
    HBox(Interactor*, Interactor*, Interactor*, Interactor*, Interactor*);
    HBox(
	Interactor*, Interactor*, Interactor*, Interactor*, Interactor*,
	Interactor*
    );
    HBox(
	Interactor*, Interactor*, Interactor*, Interactor*, Interactor*,
	Interactor*, Interactor*
    );
    virtual ~HBox();
protected:
    void ComputeShape(Shape*);
    void GetActual(int& major, int& minor);
    void GetCanonical(Shape*, BoxCanonical&);
    void PlaceElement(Interactor*, Coord, int, int, int);
private:
    void Init();
};

class VBox : public Box {
public:
    VBox();
    VBox(Interactor*);
    VBox(Interactor*, Interactor*);
    VBox(Interactor*, Interactor*, Interactor*);
    VBox(Interactor*, Interactor*, Interactor*, Interactor*);
    VBox(Interactor*, Interactor*, Interactor*, Interactor*, Interactor*);
    VBox(
	Interactor*, Interactor*, Interactor*, Interactor*, Interactor*,
	Interactor*
    );
    VBox(
	Interactor*, Interactor*, Interactor*, Interactor*, Interactor*,
	Interactor*, Interactor*
    );
    virtual ~VBox();
protected:
    void ComputeShape(Shape*);
    void GetActual(int& major, int& minor);
    void GetCanonical(Shape*, BoxCanonical&);
    void PlaceElement(Interactor*, Coord, int, int, int);
private:
    void Init();
};

#include <IV-2_6/_leave.h>

#endif
