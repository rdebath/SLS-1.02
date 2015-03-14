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
 * Button management.
 */

#include <InterViews/bitmap.h>
#include <InterViews/font.h>
#include <InterViews/pattern.h>
#include <IV-2_6/InterViews/button.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/subject.h>
#include <InterViews/Bitmaps/radioBoth.bm>
#include <InterViews/Bitmaps/radioChosen.bm>
#include <InterViews/Bitmaps/radioHit.bm>
#include <InterViews/Bitmaps/radioMask.bm>
#include <InterViews/Bitmaps/radio.bm>
#include <OS/math.h>
#include <string.h>

#include <IV-2_6/_enter.h>

static const int sep = 3;
static const int pad = 3;

inline int HalfRadioButtonSize (int h) { return Math::round(.4*h); }
inline int HalfCheckBoxSize (int h) { return Math::round(.4*h); }

/*
 * A button state is a value that is settable from one or more buttons.
 * When the state is modified, it updates all its buttons.
 */

ButtonState::ButtonState() {
    value = nil;
    ref();
}

ButtonState::ButtonState(int v) {
    value = (void*)v;
    ref();
}

ButtonState::ButtonState(void* v) {
    value = v;
    ref();
}

ButtonState::~ButtonState() { }

void ButtonState::SetValue(int v) {
    Modify((void*)v);
}

void ButtonState::SetValue(void* v) {
    Modify(v);
}

void ButtonState::operator =(ButtonState& s) {
    Modify(s.value);
}

void ButtonState::Modify(void* v) {
    if (value != v) {
	value = v;
	Notify();
    }
}

/*
 * Simple list of buttons.
 */

class ButtonList {
public:
    Button* cur;
    ButtonList* next;

    ButtonList (Button* b) { cur = b; next = nil; }
};

static void Remove(ButtonList*& blist, Button* b) {
    register ButtonList* bl;
    register ButtonList* prev;

    prev = nil;
    for (bl = blist; bl != nil; bl = bl->next) {
	if (bl->cur == b) {
	    if (prev == nil) {
		blist = bl->next;
	    } else {
		prev->next = bl->next;
	    }
	    delete bl;
	    break;
	}
	prev = bl;
    }
}

static void DeleteList(ButtonList* blist) {
    register ButtonList* bl;
    register ButtonList* next;

    for (bl = blist; bl != nil; bl = next) {
	next = bl->next;
	delete bl;
    }
}

/*
 * A button has a ButtonState subject that it modifies when pressed.
 * Also, a button may have associated buttons that are enabled/disabled
 * when it is chosen/unchosen.
 */

Button::Button(ButtonState* s, void* v) {
    Init(s, v);
}

Button::Button(const char* name, ButtonState* s, void* v) {
    SetInstance(name);
    Init(s, v);
}

void Button::Init(ButtonState* s, void* v) {
    SetClassName("Button");
    value = v;
    subject = s;
    associates = nil;
    enabled = true;
    chosen = false;
    hit = false;
    subject->Attach(this);
    Update();
    input = new Sensor(updownEvents);
    input->Catch(EnterEvent);
    input->Catch(LeaveEvent);
}

Button::~Button() {
    if (subject != nil) {
	subject->Detach(this);
    }
    DeleteList(associates);
}

void Button::Attach(Button* b) {
    ButtonList* head;

    head = new ButtonList(b);
    head->next = associates;
    associates = head;
    if (chosen) {
	b->Enable();
    } else {
	b->Disable();
    }
}

void Button::Detach(Button* b) {
    Remove(associates, b);
}

void Button::Enable() {
    if (!enabled) {
	enabled = true;
	if (canvas != nil) {
	    Draw();
	}
    }
}

void Button::Disable() {
    if (enabled) {
	enabled = false;
	if (canvas != nil) {
	    Draw();
	}
    }
}

void Button::Choose() {
    register ButtonList* bl;

    if (!chosen) {
	chosen = true;
	if (enabled) {
	    if (canvas != nil) {
		Refresh();
	    }
	    for (bl = associates; bl != nil; bl = bl->next) {
		bl->cur->Enable();
	    }
	}
    }
}

void Button::UnChoose() {
    register ButtonList* bl;

    if (chosen) {
	chosen = false;
	if (enabled) {
	    if (canvas != nil) {
		Refresh();
	    }
	    for (bl = associates; bl != nil; bl = bl->next) {
		bl->cur->Disable();
	    }
	}
    }
}

void Button::Refresh() { }

void Button::Handle(register Event& e) {
    if (e.eventType == DownEvent && e.target == this) {
	boolean inside = true;
	do {
	    if (enabled && e.target == this) {
		if (e.eventType == EnterEvent) {
		    inside = true;
		} else if (e.eventType == LeaveEvent) {
		    inside = false;
		}
		if (inside) {
		    if (!hit) {
			hit = true;
			Refresh();
		    }
		} else {
		    if (hit) {
			hit = false;
			Refresh();
		    }
		}
	    }
	    Read(e);
	} while (e.eventType != UpEvent);
	if (hit) {
	    hit = false;
	    Refresh();
	}
	if (enabled && inside) {
	    Press();
	}
    }
}

void Button::Press() {
    if (subject != nil) {
	subject->SetValue(value);
    } else {
	Refresh();
    }
}

void Button::Update() {
    void* v;
    subject->GetValue(v);
    if (!chosen && value == v) {
	Choose();
    } else if (chosen && value != v) {
	UnChoose();
    }
}

TextButton::TextButton(
    const char* str, ButtonState* s, void* v
) : Button(s, v) {
    Init(str);
}

TextButton::TextButton(
    const char* name, const char* str, ButtonState* s, void* v
) : Button(name, s, v) {
    Init(str);
}

void TextButton::Init(const char* str) {
    SetClassName("TextButton");
    if (str == nil) {
	text = nil;
    } else {
	text = new char[strlen(str) + 1];
	strcpy(text, str);
    }
    background = nil;
    grayout = nil;
}

void TextButton::Reconfig() {
    const char* a = GetAttribute("text");
    if (a != nil) {
	delete text;
	text = new char[strlen(a) + 1];
	strcpy(text, a);
    }
}

void TextButton::MakeBackground() {
    Unref(background);
    background = new Painter(output);
    background->Reference();
    background->SetColors(output->GetBgColor(), output->GetFgColor());

    static Pattern* gray_pat;
    if (gray_pat == nil) {
	gray_pat = new Pattern(Pattern::gray);
	gray_pat->Reference();
    }
    Unref(grayout);
    grayout = new Painter(background);
    grayout->Reference();
    grayout->SetPattern(gray_pat);
    grayout->FillBg(false);
}

void TextButton::MakeShape() {
    if (text != nil) {
	const Font* f = output->GetFont();
	shape->width += f->Width(text);
	shape->height += f->Height();
    }
    shape->Rigid();
}

TextButton::~TextButton() {
    delete text;
    Unref(background);
    Unref(grayout);
}

PushButton::PushButton(
    const char* str, ButtonState* s, int v
) : TextButton(str, s, (void*)v) {
    Init();
}

PushButton::PushButton(
    const char* str, ButtonState* s, void* v
) : TextButton(str, s, v) {
    Init();
}

PushButton::PushButton(
    const char* name, const char* str, ButtonState* s, int v
) : TextButton(name, str, s, (void*)v) {
    Init();
}
    
PushButton::PushButton(
    const char* name, const char* str, ButtonState* s, void* v
) : TextButton(name, str, s, v) {
    Init();
}

PushButton::~PushButton() { }

void PushButton::Init() {
    SetClassName("PushButton");
}

void PushButton::Reconfig() {
    TextButton::Reconfig();
    MakeBackground();
    if (shape->Undefined()) {
	MakeShape();
	shape->width += output->GetFont()->Width("    ");
	shape->height += 2*pad;
    }
}

void PushButton::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    output->ClearRect(canvas, x1, y1, x2, y2);
    Refresh();
}

void PushButton::Refresh() {
    register int r;
    IntCoord x[16], y[16];
    IntCoord tx, ty;

    r = Math::min(10*pixels, Math::min(xmax+1, ymax+1)/6);
    x[0] = 0; y[0] = r;
    x[1] = 0; y[1] = r + r;
    x[2] = 0; y[2] = ymax - r - r;
    x[3] = 0; y[3] = ymax - r;
    x[4] = r; y[4] = ymax;
    x[5] = r + r; y[5] = ymax;
    x[6] = xmax - r - r; y[6] = ymax;
    x[7] = xmax - r; y[7] = ymax;
    x[8] = xmax; y[8] = ymax - r;
    x[9] = xmax; y[9] = ymax - r - r;
    x[10] = xmax; y[10] = r + r;
    x[11] = xmax; y[11] = r;
    x[12] = xmax - r; y[12] = 0;
    x[13] = xmax - r - r; y[13] = 0;
    x[14] = r + r; y[14] = 0;
    x[15] = r; y[15] = 0;
    tx = (xmax - output->GetFont()->Width(text))/2;
    ty = pad;
    if (chosen || hit) {
	output->FillBSpline(canvas, x, y, 16);
	background->Text(canvas, text, tx, ty);
    } else {
	background->FillRect(canvas, 0, 0, xmax, ymax);
	output->ClosedBSpline(canvas, x, y, 16);
	output->Text(canvas, text, tx, ty);
    }
    if (!enabled) {
	grayout->FillRect(canvas, 0, 0, xmax, ymax);
    }
}

static Bitmap* radioMask;
static Bitmap* radioPlain;
static Bitmap* radioHit;
static Bitmap* radioChosen;
static Bitmap* radioBoth;

RadioButton::RadioButton(
    const char* str, ButtonState* s, int v
) : TextButton(str, s, (void*)v) {
    Init();
}

RadioButton::RadioButton(
    const char* str, ButtonState* s, void* v
) : TextButton(str, s, v) {
    Init();
}

RadioButton::RadioButton(
    const char* name, const char* str, ButtonState* s, int v
) : TextButton(name, str, s, (void*)v) {
    Init();
}

RadioButton::RadioButton(
    const char* name, const char* str, ButtonState* s, void* v
) : TextButton(name, str, s, v) {
    Init();
}

RadioButton::~RadioButton() { }

void RadioButton::Init() {
    SetClassName("RadioButton");
}

void RadioButton::Reconfig() {
    TextButton::Reconfig();
    MakeBackground();
    if (shape->Undefined()) {
	MakeShape();
	shape->width += shape->height + sep;
    }
    if (radioMask == nil) {
        radioMask = new Bitmap(
            radio_mask_bits, radio_mask_width, radio_mask_height
        );
	radioMask->Reference();
        radioPlain = new Bitmap(
            radio_plain_bits, radio_plain_width, radio_plain_height
        );
	radioPlain->Reference();
        radioHit = new Bitmap(
            radio_hit_bits, radio_hit_width, radio_hit_height
        );
	radioHit->Reference();
        radioChosen = new Bitmap(
            radio_chosen_bits, radio_chosen_width, radio_chosen_height
        );
	radioChosen->Reference();
        radioBoth = new Bitmap(
            radio_both_bits, radio_both_width, radio_both_height
        );
	radioBoth->Reference();
    }
}

void RadioButton::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    int h = output->GetFont()->Height();
    int r = radio_plain_width;
    output->ClearRect(canvas, x1, y1, x2, y2);
    IntCoord tx = r + sep;
    IntCoord ty = (ymax + 1 - h) / 2;
    output->Text(canvas, text, tx, ty);
    Refresh();
}

void RadioButton::Refresh() {
    IntCoord x = 0;
    IntCoord y = (ymax+1 - radio_plain_height)/2;
    if (!hit && !chosen) {
        output->Stencil(canvas, x, y, radioPlain, radioMask);
    } else if (hit && !chosen) {
        output->Stencil(canvas, x, y, radioHit, radioMask);
    } else if (!hit && chosen) {
        output->Stencil(canvas, x, y, radioChosen, radioMask);
    } else if (hit && chosen) {
        output->Stencil(canvas, x, y, radioBoth, radioMask);
    }
    if (!enabled) {
	grayout->FillRect(canvas, 0, 0, xmax, ymax);
    }
}

CheckBox::CheckBox(
    const char* str, ButtonState* s, int on, int off
) : TextButton(str, s, (void*)on) {
    Init((void*)off);
}

CheckBox::CheckBox(
    const char* str, ButtonState* s, void* on, void* off
) : TextButton(str, s, on) {
    Init(off);
}

CheckBox::CheckBox(
    const char* name, const char* str, ButtonState* s, int on, int off
) : TextButton(name, str, s, (void*)on) {
    Init((void*)off);
}

CheckBox::CheckBox(
    const char* name, const char* str, ButtonState* s, void* on, void* off
) : TextButton(name, str, s, on) {
    Init(off);
}

CheckBox::~CheckBox() { }

void CheckBox::Init(void* v) {
    SetClassName("CheckBox");
    offvalue = v;
}

void CheckBox::Reconfig() {
    TextButton::Reconfig();
    MakeBackground();
    if (shape->Undefined()) {
	MakeShape();
	shape->width += shape->height + sep;
    }
    Update();
}

void CheckBox::Press() {
    if (chosen) {
	subject->GetValue(value);
	subject->SetValue(offvalue);
    } else {
	subject->SetValue(value);
    }
}

void CheckBox::Update() {
    void* v;
    subject->GetValue(v);
    if (v != offvalue) {
	Choose();
	value = v;
    } else {
	UnChoose();
    }
}

void CheckBox::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    int h = output->GetFont()->Height();
    int t = HalfCheckBoxSize(h);
    output->ClearRect(canvas, x1, y1, x2, y2);
    IntCoord tx = 2*t + sep;
    IntCoord ty = (ymax + 1 - h) / 2;
    output->Text(canvas, text, tx, ty);
    Refresh();
}

void CheckBox::Refresh() {
    int h = output->GetFont()->Height();
    int t = HalfCheckBoxSize(h);
    IntCoord cx = t;
    IntCoord cy = (ymax + 1)/2;
    IntCoord left = cx - t;
    IntCoord right = cx + t;
    IntCoord bottom = cy - t;
    IntCoord top = cy + t;
    output->Rect(canvas, left, bottom, right, top);
    background->FillRect(canvas, left+1, bottom+1, right-1, top-1);
    if (hit) {
	output->Rect(canvas, left+1, bottom+1, right-1, top-1);
    }
    if (chosen) {
	output->Line(canvas, left, bottom, right, top);
	output->Line(canvas, left, top, right, bottom);
    }
    if (!enabled) {
	grayout->FillRect(canvas, 0, 0, xmax, ymax);
    }
}
