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

#ifndef ivlook_choice_h
#define ivlook_choice_h

/*
 * ChoiceItem - telltale with different look for each state
 */

#include <InterViews/telltale.h>

class Deck;

class ChoiceItem : public Telltale {
public:
    ChoiceItem(TelltaleState*);
    ChoiceItem(TelltaleState*, Glyph* normal, Glyph* pressed);
    ChoiceItem(
	TelltaleState*,
	Glyph* disabled,
	Glyph* enabled, Glyph* visible, Glyph* visible_active, Glyph* active,
	Glyph* chosen, Glyph* visible_chosen, Glyph* active_chosen,
	Glyph* visible_active_chosen, Glyph* disabled_chosen
    );

    virtual ~ChoiceItem();

    virtual void look(
	const TelltaleFlags include, const TelltaleFlags exclude, Glyph*
    );
    virtual Glyph* look(const TelltaleFlags) const;

    virtual void update(Observable*);
private:
    Deck* deck_;
    GlyphIndex index_[TelltaleState::max_flags];

    void init();
};

#endif
