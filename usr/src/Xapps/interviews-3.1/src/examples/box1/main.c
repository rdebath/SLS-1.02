#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/character.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/window.h>

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    Style* style = session->style();
    const Font* f = kit.font();
    const Color* fg = kit.foreground();
    const LayoutKit& layout = *LayoutKit::instance();
    session->run_window(
	new ApplicationWindow(
	    new Background(
		layout.hbox(
		    new Character('g', f, fg),
		    new Character('o', f, fg),
		    new Character('o', f, fg),
		    new Character('d', f, fg),
		    new Character('b', f, fg),
		    new Character('y', f, fg),
		    new Character('e', f, fg)
		),
		kit.background()
	    )
	)
    );
}
