#include <IV-look/kit.h>
#include <InterViews/brush.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/image.h>
#include <InterViews/input.h>
#include <InterViews/layout.h>
#include <InterViews/raster.h>
#include <InterViews/tiff.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/transformer.h>
#include <InterViews/tformsetter.h>
#include <InterViews/window.h>
#include <stdio.h>
#include <stdlib.h>

class Selector : public InputHandler {
public:
    Selector(Glyph*, Style*);
    virtual ~Selector();

    virtual void draw(Canvas*, const Allocation&) const;

    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);
private:
    Brush* brush_;
    Color* color_;
    Coord x0_;
    Coord y0_;
    Coord x1_;
    Coord y1_;
    Coord x2_;
    Coord y2_;
    boolean drawn_;
    boolean overlay_damage_;
    boolean overlay_done_;
};

Selector::Selector(Glyph* g, Style* s) : InputHandler(g, s) {
    Coord w = 0;
    s->find_attribute("brushWidth", w);
    brush_ = new Brush(w);
    Resource::ref(brush_);
    color_ = new Color(*WidgetKit::instance()->background(), 1.0, Color::Xor);
    Resource::ref(color_);
    drawn_ = false;
    overlay_damage_ = false;
    overlay_done_ = false;
}

Selector::~Selector() {
    Resource::unref(brush_);
    Resource::unref(color_);
}

void Selector::draw(Canvas* c, const Allocation& a) const {
    if (overlay_damage_) {
	Transformer identity;
	c->push_transform();
	c->transformer(identity);
	if (drawn_) {
	    c->rect(x0_, y0_, x1_, y1_, color_, brush_);
	}
	Selector* s = (Selector*)this;
	if (overlay_done_) {
	    s->drawn_ = false;
	    s->overlay_damage_ = false;
	    s->overlay_done_ = false;
	} else {
	    c->rect(x0_, y0_, x2_, y2_, color_, brush_);
	    s->x1_ = s->x2_;
	    s->y1_ = s->y2_;
	    s->drawn_ = true;
	}
	c->pop_transform();
    } else {
	InputHandler::draw(c, a);
    }
}

void Selector::press(const Event& e) {
    x0_ = e.pointer_x();
    y0_ = e.pointer_y();
    InputHandler::press(e);
}

void Selector::drag(const Event& e) {
    x2_ = e.pointer_x();
    y2_ = e.pointer_y();
    overlay_damage_ = true;
    redraw();
}

void Selector::release(const Event& e) {
    overlay_damage_ = true;
    overlay_done_ = true;
    redraw();
    InputHandler::release(e);
}

static PropertyData props[] = {
    { "Rubband*brushWidth", "0" },
//  { "Rubband*visual", "TrueColor" },
    { nil }
};

static OptionDesc options[] = {
    { "-brush", "*brushWidth", OptionValueNext },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Rubband", argc, argv, options, props);
    if (argc == 1) {
	fprintf(stderr, "Usage: %s <file>\n", argv[0]);
	exit(1);
    }

    Raster* rast = TIFFRaster::load(argv[1]);
    if (rast == nil) {
	fprintf(stderr, "%s: open tiff image %s failed\n", argv[0], argv[1]);
	exit(1);
    }

    Style* style = session->default_display()->style();
    const LayoutKit& layout = *LayoutKit::instance();
    session->run_window(
	new ApplicationWindow(
	    layout.flexible(
		new Selector(
		    new TransformFitter(new Image(rast)), style
		)
	    )
	)
    );
}
