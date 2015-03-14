#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/window.h>

class Circle : public Glyph {
public:
    Circle(Coord radius, const Color*, const Brush*);
    virtual ~Circle();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
private:
    Coord radius_;
    const Color* color_;
    const Brush* brush_;
};

Circle::Circle(Coord r, const Color* c, const Brush* b) {
    radius_ = r;
    Resource::ref(c);
    color_ = c;
    Resource::ref(b);
    brush_ = b;
}

Circle::~Circle() {
    Resource::unref(color_);
    Resource::unref(brush_);
}

void Circle::request(Requisition& req) const {
    Coord w = brush_->width();
    Coord diameter = radius_ + radius_ + w + w;
    Requirement rx(diameter, 0, 0, 0.5);
    Requirement ry(diameter, 0, 0, 0.5);
    req.require(Dimension_X, rx);
    req.require(Dimension_Y, ry);
}

void Circle::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

void Circle::draw(Canvas* c, const Allocation& a) const {
    const Coord r = radius_, x = a.x(), y = a.y();
    const Coord p0 = 1.00000000 * r;
    const Coord p1 = 0.89657547 * r;   // cos 30 * sqrt(1 + tan 15 * tan 15)
    const Coord p2 = 0.70710678 * r;   // cos 45
    const Coord p3 = 0.51763809 * r;   // cos 60 * sqrt(1 + tan 15 * tan 15)
    const Coord p4 = 0.26794919 * r;   // tan 15
    c->new_path();
    c->move_to(x+p0, y);
    c->curve_to(x+p2, y+p2, x+p0, y+p4, x+p1, y+p3);
    c->curve_to(x, y+p0, x+p3, y+p1, x+p4, y+p0);
    c->curve_to(x-p2, y+p2, x-p4, y+p0, x-p3, y+p1);
    c->curve_to(x-p0, y, x-p1, y + p3, x-p0, y+p4);
    c->curve_to(x-p2, y-p2, x-p0, y-p4, x-p1, y-p3);
    c->curve_to(x, y-p0, x-p3, y-p1, x-p4, y-p0);
    c->curve_to(x+p2, y-p2, x+p4, y-p0, x+p3, y-p1);
    c->curve_to(x+p0, y, x+p1, y-p3, x+p0, y-p4);
    c->close_path();
    c->stroke(color_, brush_);
}

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    session->run_window(
	new ApplicationWindow(
	    new Background(
		layout.variable_span(
		    layout.margin(
			new Circle(100, kit.foreground(), new Brush(1.0)),
			10.0
		    )
		),
		kit.background()
	    )
	)
    );
}
