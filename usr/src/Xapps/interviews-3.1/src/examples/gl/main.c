#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/glcontext.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/enter-scope.h>
#include <OS/leave-scope.h>
#include <gl/gl.h>

class GLPolygonTest : public Glyph {
public:
    GLPolygonTest();
    virtual ~GLPolygonTest();

    virtual void draw(Canvas*, const Allocation&) const;
};

GLPolygonTest::GLPolygonTest() { }
GLPolygonTest::~GLPolygonTest() { }

static void vertex(float x, float y, float z) {
    float v[3];
    v[0] = x; v[1] = y; v[2] = z;
    v3f(v);
}

static void triangle(
    float x1, float y1, float z1,
    float x2, float y2, float z2,
    float x3, float y3, float z3
) {
    bgnpolygon();
    vertex(x1, y1, z1);
    vertex(x2, y2, z2);
    vertex(x3, y3, z3);
    endpolygon();
}

void GLPolygonTest::draw(Canvas*, const Allocation& a) const {
    RGBcolor(255, 255, 255);
    clear();

    Coord x_size = a.x_allotment().span();
    Coord y_size = a.y_allotment().span();
    Coord x_mid = x_size * 0.5;
    Coord y_mid = y_size * 0.5;

    RGBcolor(255, 0, 0);
    triangle(
	0.0, 0.0, 0.0,
	x_mid, y_size, 0.0,
	x_size, 0.0, 0.0
    );
}

int main(int argc, char** argv) {
    Session* session = new Session("glx", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    Style* style = session->style();
    session->run_window(
	new ApplicationWindow(
	    new Background(
		layout.vbox(
		    layout.rmargin(kit.label("hi mom!"), 0, fil, 0),
		    layout.flexible(
			layout.natural(
			    new GLContext(new GLPolygonTest), 100, 100
			)
		    )
		),
		kit.background()
	    )
	)
    );
}
