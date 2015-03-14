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

#include <InterViews/align.h>
#include <InterViews/box.h>
#include <InterViews/canvas.h>
#include <InterViews/composition.h>
#include <InterViews/compositor.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <InterViews/tile.h>
#include <OS/list.h>
#include <OS/math.h>

class CompositionComponent {
public:
    Glyph* glyph_;
};

declareList(CompositionComponent_List,CompositionComponent)
implementList(CompositionComponent_List,CompositionComponent)

static const int BreakViewed = 0x01;
static const int BreakValid = 0x02;

class Break {
public:
    Break();

    boolean viewed() const;
    void viewed(boolean);

    boolean valid() const;
    void valid(boolean);

    Patch* patch_;
    int status_;
    Coord begin_;
    Coord end_;
    GlyphIndex first_;
    GlyphIndex last_;
};

Break::Break() {
    status_ = 0;
}

inline boolean Break::viewed() const {
    return (status_ & BreakViewed) != 0;
}

inline void Break::viewed(boolean v) {
    if (v) {
        status_ |= BreakViewed;
    } else {
        status_ &= ~BreakViewed;
    }
}

inline boolean Break::valid() const {
    return (status_ & BreakValid) != 0;
}

inline void Break::valid(boolean v) {
    if (v) {
        status_ |= BreakValid;
    } else {
        status_ &= ~BreakValid;
    }
}

declareList(Break_List,Break)
implementList(Break_List,Break)

const float epsilon = 0.1;

static CompositorIndex __length;
static Coord* __natural;
static Coord* __stretch;
static Coord* __shrink;
static int* __penalties; 
static Coord* __spans;
static CompositorIndex* __breaks;

static void grow_arrays (CompositorIndex length) {
    Coord* natural = new Coord[length];
    Coord* stretch = new Coord[length];
    Coord* shrink = new Coord[length];
    int* penalties = new int[length];
    Coord* spans = new Coord[length];
    CompositorIndex* breaks = new CompositorIndex[length];

    for (CompositorIndex i = 0; i < __length; ++i) {
        natural[i] = __natural[i];
        stretch[i] = __stretch[i];
        shrink[i] = __shrink[i];
        penalties[i] = __penalties[i];
        spans[i] = __spans[i];
        breaks[i] = __breaks[i];
    }

    delete __natural; __natural = natural;
    delete __stretch; __stretch = stretch;
    delete __shrink; __shrink = shrink;
    delete __penalties; __penalties = penalties;
    delete __spans; __spans = spans;
    delete __breaks; __breaks = breaks;

    __length = length;
}

static GlyphIndex prev_forced_break(
    GlyphIndex i, CompositionComponent_List* component
) {
    while (i >= 0) {
        const CompositionComponent& comp = component->item_ref(i);
	Glyph* g = comp.glyph_;
        if (g != nil) {
            Requisition r;
            g->request(r);
            if (r.penalty() == PenaltyGood) {
                break;
            }
        }
        --i;
    }
    return i;
}

static GlyphIndex next_forced_break(
    GlyphIndex i, CompositionComponent_List* component
) {
    GlyphIndex count = component->count();
    while (i < count) {
        const CompositionComponent& comp = component->item_ref(i);
	Glyph* g = comp.glyph_;
        if (g != nil) {
            Requisition r;
            g->request(r);
            if (r.penalty() == PenaltyGood) {
                break;
            }
        }
        ++i;
    }
    return Math::min(i, count - 1);
}

static GlyphIndex fill_arrays (
    GlyphIndex first, DimensionName dimension,
    CompositionComponent_List* component
) {
    GlyphIndex count = component->count();
    GlyphIndex last = first;
    while (last < count) {
        GlyphIndex index = last - first;
        if (index >= __length) {
            grow_arrays(next_forced_break(last, component) - first + 1);
        }
        __natural[index] = 0;
        __stretch[index] = 0;
        __shrink[index] = 0;
        __penalties[index] = PenaltyBad;

        const CompositionComponent& comp = component->item_ref(last);
	Glyph* g = comp.glyph_;
        if (g != nil) {
            Requisition r;
            g->request(r);
            Requirement& req = r.requirement(dimension);
            if (req.defined()) {
                __natural[index] = req.natural();
                __stretch[index] = req.stretch();
                __shrink[index] = req.shrink();
            }
            __penalties[index] = r.penalty();
        }
        if (__penalties[index] == PenaltyGood) {
            break;
        } else {
            ++last;
        }
    }
    return Math::min(last, count - 1);
}

Composition::Composition(
    Glyph* context, Compositor* compositor, Glyph* separator,
    DimensionName dimension, Coord span, Coord stretch, Coord shrink,
    GlyphIndex size
) : MonoGlyph(
    LayoutKit::instance()->variable_span(context, stretch, shrink)
) {
    compositor_ = compositor;
    component_ = new CompositionComponent_List(size);
    breaks_ = new Break_List(size/50);
    separator_ = separator;
    if (separator_ != nil) {
        separator_->ref();
    }
    view_all_ = true;
    damaged_ = true;
    first_damage_ = -1;
    last_damage_ = -1;
    item_ = 0;  
    dimension_ = dimension;
    span_ = span;
    resizable_ = (
	!Math::equal(stretch, Coord(0), float(1e-4)) ||
	!Math::equal(shrink, Coord(0), float(1e-4))
    );
}

Composition::~Composition() {
    Resource::unref(separator_);
    compositor_ = nil;
    GlyphIndex count = component_->count();
    for (GlyphIndex i = 0; i < count; ++i) {
        CompositionComponent& component = component_->item_ref(i);
	Resource::unref(component.glyph_);
    }
    delete component_;
    delete breaks_;
}

void Composition::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    if (resizable_) {
	Coord size = a.allotment(dimension_).span();
	if (!Math::equal(size, span_, float(1e-4))) {
	    span_ = size;
	    GlyphIndex break_count = breaks_->count();
	    for (GlyphIndex b = 0; b < break_count; b++) {
		breaks_->item_ref(b).valid(false);
	    }
	    damage(0, component_->count() - 1);
	    repair();
	}
    }
    MonoGlyph::allocate(c, a, ext);
}

void Composition::damage(GlyphIndex first, GlyphIndex last) {
    if (damaged_) {
        first_damage_ = Math::min(first_damage_, first);
        last_damage_ = Math::max(last_damage_, last);
    } else {
        first_damage_ = first;
        last_damage_ = last;
        damaged_ = true;
    }
}

boolean Composition::repair() {
    if (damaged_) {
        GlyphIndex component_count = component_->count();
        CompositorIndex break_count = breaks_->count();

        CompositorIndex forced = prev_forced_break(first_damage_, component_);

        GlyphIndex break_index = 0;
        for (;;) {
	    if (break_index < break_count) {
		const Break& br = breaks_->item_ref(break_index);
                if (br.first_ <= forced && br.last_ < forced) {
		    ++break_index;
		} else {
		    break;
		}
	    } else {
		break;
	    }
        }

        while (forced < component_count - 1 && forced < last_damage_) {
            GlyphIndex next = fill_arrays(forced + 1, dimension_, component_);

            GlyphIndex count = next - forced;
	    GlyphIndex b;
            for (b = 0; b < count && (b+break_index) < break_count; ++b) {
                const Break& br = breaks_->item_ref(b + break_index);
                __spans[b] = span_ - br.begin_ - br.end_;
            }
            if (b < count) {
                __spans[b] = span_;
            }

            CompositorIndex compose_count = compositor_->compose(
                __natural, __stretch, __shrink, __penalties, count,
                __spans, break_count - break_index + 1,
                __breaks, count
            );

            do_repair(forced + 1, break_index, __breaks, compose_count);

            forced = next;
            break_index += compose_count;
            break_count = breaks_->count();
        }
        damaged_ = false;
        return false;
    } else {
        return true;
    }
}

void Composition::do_repair(
    GlyphIndex first_component, GlyphIndex first_break,
    CompositorIndex* breaks, CompositorIndex count
) {
    GlyphIndex break_index = first_break;
    Glyph* contents = body();
    for (GlyphIndex i = 0; i < count; ++i) {
        Break b;
        if (break_index < breaks_->count()) {
            const Break& br = breaks_->item_ref(break_index);
            b.begin_ = br.begin_;
            b.end_ = br.end_;
        } else {
            b.begin_ = 0;
            b.end_ = 0;
        }
        b.first_ = first_component + ((i == 0) ? 0 : breaks[i-1] + 1);
        b.last_ = first_component + (breaks[i] - 1);
	boolean re_break = (break_index == breaks_->count());
	if (!re_break) {
	    const Break& br = breaks_->item_ref(break_index);
	    re_break = (
		!br.valid() || br.first_ != b.first_ || br.last_ != b.last_
	    );
	}
	if (re_break) {
            while (
                break_index < breaks_->count() - 1 &&
                breaks_->item_ref(break_index + 1).last_ <= b.last_
            ) {
                contents->remove(break_index * 2 + 1);
                contents->remove(break_index * 2);
                breaks_->remove(break_index);
            }
	    re_break = (break_index == breaks_->count());
	    if (!re_break) {
		const Break& br = breaks_->item_ref(break_index);
		re_break = (
		    (i < count-1 &&
			br.last_ >= first_component + breaks[i+1] - 1)
		    ||
		    (i == count-1 && br.first_ > b.last_ + 1)
		);
	    }
	    if (re_break) {
                contents->insert(break_index * 2, make_item(b, view_all_));
                contents->insert(break_index * 2 + 1, separator(b));
                breaks_->insert(break_index, b);
            } else {
		Break& br = breaks_->item_ref(break_index);
		if (br.patch_ != nil) {
		    br.patch_->redraw();
		}
                contents->replace(break_index * 2, make_item(b, view_all_));
                contents->replace(break_index * 2 + 1, separator(b));
                br = b;
            }
        }
        ++break_index;
    }
}

void Composition::view(GlyphIndex first, GlyphIndex last) {
    GlyphIndex bc = breaks_->count();
    Glyph* contents = body();
    for (GlyphIndex b = 0; b < bc; ++b) {
        Break& br = breaks_->item_ref(b);
        if (br.last_ >= first && br.first_ <= last) {
            if (!br.viewed()) {
                contents->replace(b * 2, make_item(br, true));
            }
        } else {
            if (br.viewed()) {
                contents->replace(b * 2, make_item(br, false));
            }
        }
    }
    view_all_ = false;
}

Glyph* Composition::separator(Break& b) {
    if (b.last_ >= component_->count()-1) {
        return separator_;
    } else {
        Glyph* inbreak = component_->item_ref(b.last_ + 1).glyph_;
        if (inbreak != nil) {
            Glyph* g = inbreak->compose(in_break);
            if (g != nil) {
                return g;
            } else {
                return separator_;
            }
        } else {
            return separator_;
        }
    }
}

void Composition::margin(CompositorIndex item, Coord begin, Coord end) {
    CompositorIndex b = item/2;
    if (b < breaks_->count() && item%2 == 0) {
        Break& br = breaks_->item_ref(b);
        if (br.begin_ != begin || br.end_ != end) {
            br.begin_ = begin;
            br.end_ = end;
            br.valid(false);
            damage(br.first_ - 1, br.last_ + 1);
        }
    }
}

GlyphIndex Composition::beginning_of(CompositorIndex item) const {
    CompositorIndex i = Math::max(0L, Math::min(item/2, breaks_->count()-1));
    return breaks_->item_ref(i).first_;
}

GlyphIndex Composition::end_of(CompositorIndex item) const {
    CompositorIndex i = Math::max(0L, Math::min(item/2, breaks_->count()-1));
    return breaks_->item_ref(i).last_;
}

GlyphIndex Composition::count() const {
    return component_->count();
}

Glyph* Composition::component(GlyphIndex index) const {
    return component_->item_ref(index).glyph_;
}

void Composition::change(GlyphIndex index) {
    CompositionComponent& component = component_->item_ref(index);
    Glyph* contents = body();
    for (CompositorIndex b = item(index)/2; b < breaks_->count(); ++b) {
        Break& br = breaks_->item_ref(b);
        if (br.viewed() && index >= br.first_ && index <= br.last_+1) {
            contents->component(2 * b)->change(index - br.first_ + 2);
            contents->change(2 * b);
        }
        if (br.first_ > index) {
            break;
        }
    }
}

void Composition::append(Glyph* glyph) {
    insert(component_->count(), glyph);
}

void Composition::prepend(Glyph* glyph) {
    insert(0, glyph);
}

void Composition::insert(GlyphIndex index, Glyph* glyph) {
    if (glyph != nil) {
        glyph->ref();
    }
    CompositionComponent component;
    component.glyph_ = glyph;
    component_->insert(index, component);
    Glyph* contents = body();
    for (CompositorIndex b = item(index)/2; b < breaks_->count(); ++b) {
        Break& br = breaks_->item_ref(b);
        if (br.viewed() && index >= br.first_ && index <= br.last_+1) {
            br.valid(false);
            contents->component(2 * b)->insert(index - br.first_ + 2, nil);
            contents->change(2 * b);
        }
        if (index < br.first_) {
            ++br.first_;
        }
        if (index <= br.last_ + 1) {
            ++br.last_;
        }
    }
    damage(index - 1, index + 1);
}

void Composition::remove(GlyphIndex index) {
    CompositionComponent& component = component_->item_ref(index);
    if (component.glyph_ != nil) {
        component.glyph_->unref();
    }
    component_->remove(index);
    Glyph* contents = body();
    for (CompositorIndex b = item(index)/2; b < breaks_->count(); ++b) {
        Break& br = breaks_->item_ref(b);
        if (br.viewed() && index >= br.first_ && index <= br.last_+1) {
            br.valid(false);
            contents->component(2 * b)->remove(index - br.first_ + 2);
            contents->change(2 * b);
        }
        if (index < br.first_) {
            --br.first_;
        }
        if (index <= br.last_ + 1) {
            --br.last_;
        }
    }
    damage(index - 1, index);
}

void Composition::replace(GlyphIndex index, Glyph* glyph) {
    Requisition oldr, newr;
    if (glyph != nil) {
        glyph->request(newr);
        glyph->ref();
    }
    CompositionComponent& component = component_->item_ref(index);
    if (component.glyph_ != nil) {
        component.glyph_->request(oldr);
        component.glyph_->unref();
    }
    component.glyph_ = glyph;
    Glyph* contents = body();
    for (CompositorIndex b = item(index)/2; b < breaks_->count(); ++b) {
        Break& br = breaks_->item_ref(b);
        if (br.viewed() && index >= br.first_-1 && index <= br.last_+1) {
            if (newr.equals(oldr, epsilon)) {
                Glyph* g = (
                    glyph == nil ? nil
                    : index == br.first_-1 ? glyph->compose(post_break)
                    : index == br.last_+1 ? glyph->compose(pre_break)
                    : glyph->compose(no_break)
                );
                contents->component(2 * b)->replace(index-br.first_+2, g);
            } else {
                br.valid(false);
                damage(index - 1, index + 1);
            }
            contents->change(2 * b);
        }
        if (br.first_ > index) {
            break;
        }
    }
}

CompositorIndex Composition::item(GlyphIndex index) const {
    CompositorIndex count = breaks_->count();
    Composition* c = (Composition*)this;
    c->item_ = Math::min(Math::max(0L, item_), count-1);
    while (c->item_ < count-1 && c->breaks_->item_ref(item_).last_ < index) {
	c->item_ += 1;
    }
    while (c->item_ > 0 && c->breaks_->item_ref(item_).first_ > index) {
        c->item_ -= 1;
    }
    return Math::max(0L, c->item_) * 2;
}

void Composition::allotment(
    GlyphIndex i, DimensionName res, Allotment& a
) const {
    for (GlyphIndex b = item(i)/2; b < breaks_->count(); ++b) {
        Break& br = breaks_->item_ref(b);
        if (i >= br.first_ && i <= br.last_+1) {
            if (br.viewed()) {
                body()->component(2*b)->allotment(i - br.first_ + 2, res, a);
            } else {
                body()->allotment(2*b, res, a);
            }
        }
    }
}

/*
 * Translate a pick on the composition contents into what looks
 * like a pick on the elements.  That is, if the composition
 * is a list of characters, it will be arranged into lines.
 * The pick on the lines will return <line, col>, which needs
 * to be compressed into a single index for the composition.
 */

void Composition::pick(
    Canvas* c, const Allocation& allocation, int depth, Hit& h
) {
    long n = h.count();
    MonoGlyph::pick(c, allocation, depth, h);
    if (h.count() > n) {
	/*
	 * Retrieve the index of the line that was hit into "item".
	 * Compositions create two glyphs per formatted line:
	 * even-numbered elements refer to the lines and odd-numbered ones
	 * refer to the separators between lines.  We will compute
	 * the element "index" from "item" and the break information.
	 */
        GlyphIndex item = h.index(depth);
	GlyphIndex b_count = breaks_->count();
        GlyphIndex index;
        if (item % 2 == 0) {
	    GlyphIndex b_index = item / 2;
	    if (b_index < b_count) {
		/*
		 * Hit on a line: calculate the character index by adding
		 * the line's start to the offset into the line.
		 */
		const Break& br = breaks_->item_ref(b_index);
		index = br.first_ + h.index(depth+1) - 2;
		index = Math::max(br.first_, Math::min(br.last_ + 1, index));
	    } else {
		/*
		 * This case only seems to happen when there is
		 * a float at the end of the composition.
		 */
		index = breaks_->item_ref(b_count - 1).last_ + 1;
	    }
	} else {
	    GlyphIndex b_index = item / 2 + 1;
	    if (b_index < b_count) {
		/*
		 * Hit on the separator between lines: assume that
		 * the hit refers to the beginning of the following line.
		 */
		index = breaks_->item_ref(b_index).first_;
	    } else {
		/*
		 * Hit on separator at the end of the composition:
		 * return the end of the last line.
		 */
		index = breaks_->item_ref(b_index - 1).last_ + 1;
	    }
        }

	/*
	 * Remove the hit target for depth + 1, effectively
	 * shifting information below it up one level.
	 */
	h.remove(depth + 1);

	/*
	 * Finally, add the information for this level.
	 */
        h.retarget(depth, this, index);
    }
}

Glyph* Composition::make_item(Break& nb, boolean created) {
    LayoutKit* layout = LayoutKit::instance();
    nb.valid(true);
    nb.viewed(created);
    if (created) {
        Glyph* g = make(nb);
        if (span_ > 0 && span_ < fil) {
            g = layout->fixed_span_dimension(g, dimension_, span_);
        }
	nb.patch_ = new Patch(g);
	return nb.patch_;
    } else {
        static GlyphIndex __req_count;
        static Requisition* __req;
        GlyphIndex count = nb.last_ - nb.first_ + 3;
        if (count >= __req_count) {
            delete __req;
            __req = new Requisition[count];
            __req_count = count;
        }
        GlyphIndex index = 0;
        if (nb.first_ > 0) {
            Glyph* g = component_->item_ref(nb.first_ - 1).glyph_;
            if (g != nil) {
                g = g->compose(post_break);
            }
            if (g != nil) {
                g->request(__req[index]);
                ++index;
            }
        }
        for (GlyphIndex k = nb.first_; k <= nb.last_; ++k) {
            Glyph* g = component_->item_ref(k).glyph_;
            if (g != nil) {
                g = g->compose(no_break);
            }
            if (g != nil) {
                g->request(__req[index]);
                ++index;
            }
        }
        if (nb.last_ < component_->count()-1) {
            Glyph* g = component_->item_ref(nb.last_ + 1).glyph_;
            if (g != nil) {
                g = g->compose(pre_break);
            }
            if (g != nil) {
                g->request(__req[index]);
                ++index;
            }
        }
        Requisition r;
        DimensionName cross;
        if (dimension_ == Dimension_X) {
            if (span_ > 0 && span_ < fil) {
                Requirement require(span_);
                r.require(dimension_, require);
            } else {
                Tile tile(dimension_);
                tile.request(index, __req, r);
            }
            cross = Dimension_Y;
        } else {
            if (span_ > 0 && span_ < fil) {
                Requirement require(span_, 0, 0, 1.0);
                r.require(dimension_, require);
            } else {
                TileReversed tile(dimension_);
                tile.request(index, __req, r);
            }
            cross = Dimension_X;
        }
        Align align(cross);
        align.request(index, __req, r);
	nb.patch_ = nil;
        return layout->glue(r);
    }
}

Glyph* Composition::make(Break&) {
    return nil;
}

LRComposition::LRComposition(
    Glyph* context, Compositor* compositor, Glyph* separator,
    Coord width, Coord stretch, Coord shrink, GlyphIndex size
) : Composition(
    context, compositor, separator, Dimension_X, width, stretch, shrink, size
) { }

LRComposition::~LRComposition() { }

Glyph* LRComposition::make(Break& nb) {
    LayoutKit* layout = LayoutKit::instance();
    Glyph* glyph = layout->hbox(nb.last_ - nb.first_ + 5);
    glyph->append(layout->hglue(nb.begin_, 0, 0, 0.0));
    if (nb.first_ > 0) {
        Glyph* g = component(nb.first_ - 1);
        if (g != nil) {
            glyph->append(g->compose(post_break));
        } else {
            glyph->append(nil);
        }
    } else {
        glyph->append(nil);
    }
    for (GlyphIndex k = nb.first_; k <= nb.last_; ++k) {
        Glyph* g = component(k);
        if (g != nil) {
            glyph->append(g->compose(no_break));
        } else {
            glyph->append(nil);
        }
    }
    if (nb.last_ < count()-1) {
        Glyph* g = component(nb.last_ + 1);
        if (g != nil) {
            glyph->append(g->compose(pre_break));
        } else {
            glyph->append(nil);
        }
    } else {
        glyph->append(nil);
    }
    glyph->append(layout->hglue(nb.end_, 0, 0, 1.0));
    return glyph;
}

TBComposition::TBComposition(
    Glyph* context, Compositor* compositor, Glyph* separator, 
    Coord height, Coord stretch, Coord shrink, GlyphIndex size
) : Composition(
    context, compositor, separator, Dimension_Y, height, stretch, shrink, size
) { }

TBComposition::~TBComposition() { }

Glyph* TBComposition::make(Break& nb) {
    LayoutKit* layout = LayoutKit::instance();
    Glyph* glyph = layout->vbox(nb.last_ - nb.first_ + 5);
    glyph->append(layout->vglue(nb.begin_, 0, 0, 1.0));
    if (nb.first_ > 0) {
        Glyph* g = component(nb.first_ - 1);
        if (g != nil) {
            glyph->append(g->compose(post_break));
        } else {
            glyph->append(nil);
        }
    } else {
        glyph->append(nil);
    }
    for (GlyphIndex k = nb.first_; k <= nb.last_; ++k) {
        Glyph* g = component(k);
        if (g != nil) {
            glyph->append(g->compose(no_break));
        } else {
            glyph->append(nil);
        }
    }
    if (nb.last_ < count()-1) {
        Glyph* g = component(nb.last_ + 1);
        if (g != nil) {
            glyph->append(g->compose(pre_break));
        } else {
            glyph->append(nil);
        }
    } else {
        glyph->append(nil);
    }
    glyph->append(layout->vglue(nb.end_, 0, 0, 0.0));
    return glyph;
}
