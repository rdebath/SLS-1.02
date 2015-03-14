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
 * RubberGroup - a group of Rubberbands to track simultaneously.
 */

#ifndef iv2_6_rubgroup_h
#define iv2_6_rubgroup_h

#include <IV-2_6/InterViews/rubband.h>

class RubberList;

class RubberGroup : public Rubberband {
public:
    RubberGroup(Painter*, Canvas*);
    virtual ~RubberGroup();

    virtual void Draw();
    virtual void Erase();
    virtual void Track(IntCoord, IntCoord);
    virtual void SetPainter(Painter*);
    virtual void SetCanvas(Canvas*);

    void Append(
	Rubberband*, Rubberband* = nil, Rubberband* = nil, Rubberband* = nil
    );
    void Remove(Rubberband*);
    void RemoveCur();
    void SetCurrent(Rubberband*);
    Rubberband* GetCurrent();
    Rubberband* First();
    Rubberband* Last();
    Rubberband* Next();
    Rubberband* Prev();
    boolean IsEmpty();
    boolean AtEnd();
private:
    RubberList* rlist, *cur;
};

#endif
