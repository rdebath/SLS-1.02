#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/bitmap.h>
#include <InterViews/character.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/stencil.h>
#include <InterViews/window.h>
#include <InterViews/Bitmaps/hand.bm>

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const Font* f = kit.font();
    const Color* fg = kit.foreground();
    const LayoutKit& layout = *LayoutKit::instance();
    session->run_window(
	new ApplicationWindow(
	    new Background(
		layout.hbox(
		    new Stencil(
			new Bitmap(
			    hand_bits, hand_width, hand_height
			),
			fg
		    ),
		    new Stencil(
			new Bitmap(
			    hand_bits, hand_width, hand_height,
			    hand_x_hot, hand_y_hot
			),
			fg
		    ),
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
