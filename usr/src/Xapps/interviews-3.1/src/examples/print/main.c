#include <IV-look/dialogs.h>
#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <InterViews/printer.h>
#include <InterViews/session.h>
#include <InterViews/window.h>
#include <fstream.h>

class Printable : public Patch {
public:
    Printable(Glyph*);
    virtual ~Printable();

    virtual void do_print();
};

Printable::Printable(Glyph* g) : Patch(g) { }
Printable::~Printable() { }

void Printable::do_print() {
    ofstream out("out.ps");
    Printer* p = new Printer(&out);
    p->prolog("tmp");
    const Allocation& a = allocation();
    p->resize(a.left(), a.bottom(), a.right(), a.top());
    p->page("1");
    Glyph* g = body();
    g->print(p, a);
    p->epilog();
    g->undraw();
    reallocate();
    redraw();
    out.flush();
    delete p;
}

declareActionCallback(Printable)
implementActionCallback(Printable)

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& widgets = *WidgetKit::instance();
    DialogKit& dialogs = *DialogKit::instance();
    LayoutKit& layout = *LayoutKit::instance();
    Printable* p = new Printable(nil);
    p->body(
	widgets.inset_frame(
	    layout.margin(
		layout.vbox(
		    dialogs.field_editor("abcdefghijk", session->style()),
		    layout.vglue(10),
		    layout.hbox(
			layout.hglue(20, fil, 20),
			widgets.push_button(
			    "Print",
			    new ActionCallback(Printable)(
				p, &Printable::do_print
			    )
			),
			layout.hglue(5),
			widgets.push_button("Quit", widgets.quit())
		    )
		),
		10.0
	    )
	)
    );
    return session->run_window(new ApplicationWindow(p));
}
