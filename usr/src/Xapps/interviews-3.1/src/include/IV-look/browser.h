/*
 * Copyright (c) 1991 Stanford University
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
 * Browser -- select an item
 */

#ifndef ivlook_browser_h
#define ivlook_browser_h

#include <InterViews/input.h>
#include <InterViews/observe.h>

class Action;
class BrowserList;
class TelltaleState;

class Browser : public InputHandler, public Observer {
public:
    Browser(Glyph*, Style*, Action* accept, Action* cancel);
    virtual ~Browser();

    virtual void append_selectable(TelltaleState*);
    virtual void replace_selectable(GlyphIndex, TelltaleState*);
    virtual void remove_selectable(GlyphIndex);

    virtual TelltaleState* state(GlyphIndex) const;

    virtual void select(GlyphIndex);
    virtual GlyphIndex selected() const;

    virtual void choose(GlyphIndex) const;
    virtual void cancel();

    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);
    virtual void double_click(const Event&);

    virtual void update(Observable*);
private:
    Action* accept_;
    Action* cancel_;
    BrowserList* items_;
    GlyphIndex item_;

    void active(GlyphIndex i, boolean);
};

#endif
