/*
 * Copyright (c) 1992 Stanford University
 * Copyright (c) 1992 Silicon Graphics, Inc.
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
 * Dialog -- dialog box
 */

#ifndef iv_dialog_h
#define iv_dialog_h

#include <InterViews/input.h>

#include <InterViews/_enter.h>

class Window;

/*
 * The post*_aligned operations replaced default parameters
 * due to an apparent bug in cfront 3.0.
 */

class Dialog : public InputHandler {
public:
    Dialog(Glyph*, Style*);
    virtual ~Dialog();

    boolean post_for(Window*);
    virtual boolean post_for_aligned(Window*, float xalign, float yalign);
    boolean post_at(Coord x, Coord y);
    virtual boolean post_at_aligned(
	Coord x, Coord y, float xalign, float yalign
    );
    virtual boolean run();
    virtual void dismiss(boolean accept);
private:
    boolean done_;
    boolean accepted_;
};

inline boolean Dialog::post_for(Window* w) {
    return post_for_aligned(w, 0.5, 0.5);
}

inline boolean Dialog::post_at(Coord x, Coord y) {
    return post_at_aligned(x, y, 0.5, 0.5);
}

#include <InterViews/_leave.h>

#endif
