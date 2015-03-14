#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/window.h>

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    return session->run_window(
	new ApplicationWindow(
	    new Background(
		layout.hbox(
		    kit.label("good"), layout.hglue(), kit.label("bye")
		),
		kit.background()
	    )
	)
    );
}
