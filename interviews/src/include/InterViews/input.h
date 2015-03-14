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
 * InputHandler - glyph that handles input
 */

#ifndef iv_input_h
#define iv_input_h

#include <InterViews/monoglyph.h>

class Event;
class Handler;
class InputHandlerImpl;
class Style;
class Transformer;

class InputHandler : public MonoGlyph {
public:
    InputHandler(Glyph*, Style*);
    virtual ~InputHandler();

    virtual Handler* handler() const;
    virtual InputHandler* parent() const;
    virtual Style* style() const;

    virtual void append_input_handler(InputHandler*);
    virtual void remove_input_handler(GlyphIndex);
    virtual void remove_all_input_handlers();
    virtual GlyphIndex input_handler_count() const;
    virtual InputHandler* input_handler(GlyphIndex) const;

    virtual void focus(InputHandler*);
    virtual void next_focus();
    virtual void prev_focus();

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();

    virtual void move(const Event&);
    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);
    virtual void keystroke(const Event&);
    virtual void double_click(const Event&);

    virtual InputHandler* focus_in();
    virtual void focus_out();

    virtual void allocation_changed(Canvas*, const Allocation&);
    virtual boolean inside(const Event&);

    virtual Canvas* canvas() const;
    virtual const Transformer& transformer() const;
    virtual const Allocation& allocation() const;

    virtual void redraw() const;
    virtual void repick(int depth, Hit&);
private:
    friend class InputHandlerImpl;

    InputHandlerImpl* impl_;
};

class ActiveHandler : public InputHandler {
protected:
    ActiveHandler(Glyph*, Style*);
public:
    virtual ~ActiveHandler();

    virtual void undraw();

    virtual void move(const Event&);
    virtual void drag(const Event&);
    virtual void enter();
    virtual void leave();
private:
    boolean inside_;
};

#endif
