#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/session.h>
#include <InterViews/window.h>

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    session->run_window(
	new ApplicationWindow(
	    new Background(
		kit.label("hi mom!"),
		kit.background()
	    )
	)
    );
}
