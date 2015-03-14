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
 * FileBrowser -- browse a directory
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <IV-look/fbrowser.h>
#include <IV-look/kit.h>
#include <InterViews/canvas.h>
#include <InterViews/font.h>
#include <InterViews/event.h>
#include <InterViews/scrbox.h>
#include <InterViews/style.h>
#include <InterViews/window.h>

class FileBrowserImpl;
typedef void (FileBrowserImpl::*FileBrowserKeyFunc)();

struct FileBrowserKeyInfo {
    char key;
    const char* name;
    FileBrowserKeyFunc func;
};

static const int keymap_size = 256;

class FileBrowserImpl {
public:
    FileBrowser* browser_;
    WidgetKit* kit_;
    GlyphIndex selected_;
    TBScrollBox* box_;
    enum { selecting, grab_scrolling, rate_scrolling } mode_;
    Coord scale_;
    Cursor* save_cursor_;
    Coord start_scroll_pointer_;
    Coord cur_scroll_pointer_;
    Coord start_scroll_pos_;
    IOHandler* rate_handler_;
    long usec_rate_;
    FileBrowserKeyFunc key_[keymap_size];

    void rate_scroll_timer(long, long);

    void open();
    void cancel();
    void scroll_to_first();
    void scroll_to_last();
    void select_all();
    void unselect_all();
    void next_focus();
    void select_previous();
    void select_next();
    void select_top();
    void select_bottom();
    void scroll_down();
    void scroll_up();
    void page_down();
    void page_up();
    void half_page_down();
    void half_page_up();
};

declareIOCallback(FileBrowserImpl)
implementIOCallback(FileBrowserImpl)

static FileBrowserKeyInfo default_key_map[] = {
    { '\r', "open", &FileBrowserImpl::open },
    { '\007', "cancel", &FileBrowserImpl::cancel },
    { '\033', "cancel", &FileBrowserImpl::cancel },
    { 'g', "scroll-to-first", &FileBrowserImpl::scroll_to_first },
    { 'G', "scroll-to-last", &FileBrowserImpl::scroll_to_last },
    { 'a', "select-all", &FileBrowserImpl::select_all },
    { '\177', "unselect-all", &FileBrowserImpl::unselect_all },
    { '\010', "unselect-all", &FileBrowserImpl::unselect_all },
    { '\t', "next-focus", &FileBrowserImpl::next_focus },
    { 'p', "select-previous", &FileBrowserImpl::select_previous },
    { 'n', "select-next", &FileBrowserImpl::select_next },
    { '<', "select-top", &FileBrowserImpl::select_top },
    { '>', "select-bottom", &FileBrowserImpl::select_bottom },
    { 'j', "scroll-down", &FileBrowserImpl::scroll_down },
    { 'k', "scroll-up", &FileBrowserImpl::scroll_up },
    { ' ', "page-down", &FileBrowserImpl::page_down },
    { 'b', "page-up", &FileBrowserImpl::page_up },
    { 'd', "half-page-down", &FileBrowserImpl::half_page_down },
    { 'u', "half-page-up", &FileBrowserImpl::half_page_up },
    { 0, nil }
};

FileBrowser::FileBrowser(
    WidgetKit* kit, Action* accept, Action* cancel
) : Browser(nil, kit->style(), accept, cancel) {
    impl_ = new FileBrowserImpl;
    FileBrowserImpl& fb = *impl_;
    fb.browser_ = this;
    fb.kit_ = kit;
    fb.selected_ = -1;
    fb.box_ = new TBScrollBox;
    const Font* f = kit->font();
    FontBoundingBox bbox;
    f->font_bbox(bbox);
    fb.scale_ = 1.0 / (bbox.ascent() + bbox.descent());
    fb.save_cursor_ = nil;
    for (int i = 0; i < keymap_size; i++) {
	fb.key_[i] = nil;
    }
    for (FileBrowserKeyInfo* k = &default_key_map[0]; k->key != 0; k++) {
	fb.key_[k->key] = k->func;
    }
    fb.rate_handler_ = new IOCallback(FileBrowserImpl)(
	impl_, &FileBrowserImpl::rate_scroll_timer
    );
    Style* s = kit->style();
    long milliseconds = 75;
    s->find_attribute("scrollRate", milliseconds);
    fb.usec_rate_ = 1000 * milliseconds;
    body(fb.box_);
}

FileBrowser::~FileBrowser() {
    delete impl_->rate_handler_;
    delete impl_;
}

void FileBrowser::press(const Event& e) {
    FileBrowserImpl& fb = *impl_;
    EventButton b = e.pointer_button();
    Window* w = canvas()->window();
    switch (b) {
    case Event::left:
	Browser::press(e);
	fb.mode_ = FileBrowserImpl::selecting;
	break;
    case Event::middle:
	fb.mode_ = FileBrowserImpl::grab_scrolling;
	fb.save_cursor_ = w->cursor();
	fb.start_scroll_pointer_ = e.pointer_y();
	fb.start_scroll_pos_ = fb.box_->cur_lower(Dimension_Y);
	w->cursor(fb.kit_->hand_cursor());
	break;
    case Event::right:
	fb.mode_ = FileBrowserImpl::rate_scrolling;
	fb.start_scroll_pointer_ = e.pointer_y();
	fb.start_scroll_pos_ = fb.box_->cur_lower(Dimension_Y);
	fb.save_cursor_ = w->cursor();
	break;
    default:
	break;
    }
}

void FileBrowser::drag(const Event& e) {
    FileBrowserImpl& fb = *impl_;
    WidgetKit& kit = *fb.kit_;
    Coord delta;
    Window* w = canvas()->window();
    switch (fb.mode_) {
    case FileBrowserImpl::selecting:
	Browser::drag(e);
	break;
    case FileBrowserImpl::grab_scrolling:
	delta = e.pointer_y() - fb.start_scroll_pointer_;
	fb.box_->scroll_to(
	    Dimension_Y, fb.start_scroll_pos_ - delta * fb.scale_
	);
	break;
    case FileBrowserImpl::rate_scrolling:
	fb.cur_scroll_pointer_ = e.pointer_y();
	if (fb.cur_scroll_pointer_ > fb.start_scroll_pointer_) {
	    w->cursor(kit.ufast_cursor());
	} else {
	    w->cursor(kit.dfast_cursor());
	}
	Dispatcher::instance().stopTimer(fb.rate_handler_);
	fb.rate_scroll_timer(0, 0);
	break;
    }
}

void FileBrowser::release(const Event& e) {
    FileBrowserImpl& fb = *impl_;
    Window* w = canvas()->window();
    Coord delta;
    switch (fb.mode_) {
    case FileBrowserImpl::selecting:
	Browser::release(e);
	break;
    case FileBrowserImpl::grab_scrolling:
	delta = e.pointer_y() - fb.start_scroll_pointer_;
	fb.box_->scroll_to(
	    Dimension_Y, fb.start_scroll_pos_ - delta * fb.scale_
	);
	w->cursor(fb.save_cursor_);
	break;
    case FileBrowserImpl::rate_scrolling:
	Dispatcher::instance().stopTimer(fb.rate_handler_);
	w->cursor(fb.save_cursor_);
	break;
    }
}

void FileBrowser::keystroke(const Event& e) {
    char c;
    if (e.mapkey(&c, 1) != 0) {
	FileBrowserImpl& fb = *impl_;
	FileBrowserKeyFunc f = fb.key_[c];
	if (f != nil) {
	    (fb.*f)();
	}
    }
}

InputHandler* FileBrowser::focus_in() {
    FileBrowserImpl& fb = *impl_;
    if (fb.selected_ == -1) {
	fb.select_top();
    } else {
	Browser::select(fb.selected_);
    }
    return Browser::focus_in();
}

void FileBrowser::focus_out() {
    FileBrowserImpl& fb = *impl_;
    fb.selected_ = selected();
    Browser::select(-1);
}

void FileBrowser::select(GlyphIndex i) {
    FileBrowserImpl& fb = *impl_;
    fb.selected_ = i;
    Browser::select(i);
}

Adjustable* FileBrowser::adjustable() const {
    return impl_->box_;
}

void FileBrowser::refresh() {
    FileBrowserImpl& fb = *impl_;
    fb.box_->scroll_to(Dimension_Y, Coord(fb.box_->count()));
}

/* class FileBrowserImpl */

void FileBrowserImpl::rate_scroll_timer(long, long) {
    Coord delta = cur_scroll_pointer_ - start_scroll_pointer_;
    box_->scroll_to(
	Dimension_Y, box_->cur_lower(Dimension_Y) + delta * scale_
    );
    Dispatcher::instance().startTimer(0, usec_rate_, rate_handler_);
}

void FileBrowserImpl::open() {
    FileBrowser& b = *browser_;
    GlyphIndex i = b.selected();
    if (i >= 0 && i < b.count()) {
	b.choose(i);
    }
}

void FileBrowserImpl::cancel() {
    browser_->cancel();
}

void FileBrowserImpl::scroll_to_first() {
    box_->scroll_to(Dimension_Y, Coord(box_->count()));
}

void FileBrowserImpl::scroll_to_last() {
    box_->scroll_to(Dimension_Y, Coord(0));
}

void FileBrowserImpl::select_all() { }

void FileBrowserImpl::unselect_all() {
    browser_->select(-1);
}

void FileBrowserImpl::next_focus() {
    browser_->next_focus();
}

void FileBrowserImpl::select_previous() {
    FileBrowser& b = *browser_;
    GlyphIndex i = b.selected();
    if (!box_->shown(i)) {
	box_->scroll_to(Dimension_Y, Coord(box_->count() - i - 1));
    }
    if (i > 0) {
	--i;
	if (!box_->shown(i)) {
	    box_->scroll_forward(Dimension_Y);
	}
	b.select(i);
    }
}

void FileBrowserImpl::select_next() {
    FileBrowser& b = *browser_;
    GlyphIndex i = b.selected();
    if (!box_->shown(i)) {
	box_->scroll_to(
	    Dimension_Y, Coord(
		box_->count() - 1 - i +
		box_->first_shown() - box_->last_shown()
	    )
	);
    }
    if (i < b.count() - 1) {
	++i;
	if (!box_->shown(i)) {
	    box_->scroll_backward(Dimension_Y);
	}
	b.select(i);
    }
}

void FileBrowserImpl::select_top() {
    browser_->select(box_->first_shown());
}

void FileBrowserImpl::select_bottom() {
    browser_->select(box_->last_shown());
}

void FileBrowserImpl::scroll_down() {
    box_->scroll_forward(Dimension_Y);
}

void FileBrowserImpl::scroll_up() {
    box_->scroll_backward(Dimension_Y);
}

void FileBrowserImpl::page_down() {
    box_->page_backward(Dimension_Y);
}

void FileBrowserImpl::page_up() {
    box_->page_forward(Dimension_Y);
}

void FileBrowserImpl::half_page_down() {
    GlyphIndex n = box_->last_shown() - box_->first_shown() + 1;
    GlyphIndex half = n >> 1;
    for (GlyphIndex i = 0; i < half; i++) {
	box_->scroll_backward(Dimension_Y);
    }
}

void FileBrowserImpl::half_page_up() {
    GlyphIndex n = box_->last_shown() - box_->first_shown() + 1;
    GlyphIndex half = n >> 1;
    for (GlyphIndex i = 0; i < half; i++) {
	box_->scroll_forward(Dimension_Y);
    }
}
