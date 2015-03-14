#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/patch.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/window.h>

class App {
public:
    Glyph* make(WidgetKit&, const LayoutKit&);
    void next();
    void prev();
private:
    Deck* deck_;
    Patch* patch_;

    void flip(GlyphIndex);
};

declareActionCallback(App)
implementActionCallback(App)

Glyph* App::make(WidgetKit& kit, const LayoutKit& layout) {
    deck_ = layout.deck();
    deck_->append(kit.label("Hi mom!"));
    deck_->append(kit.label("Big Bird"));
    deck_->append(kit.label("Oscar"));
    deck_->flip_to(0);
    patch_ = new Patch(deck_);
    return patch_;
}

void App::next() {
    GlyphIndex cur = deck_->card() + 1;
    flip(cur == deck_->count() ? 0 : cur);
}

void App::prev() {
    GlyphIndex cur = deck_->card() - 1;
    flip(cur == -1 ? deck_->count() - 1 : cur);
}

void App::flip(GlyphIndex cur) {
    deck_->flip_to(cur);
    patch_->redraw();
}

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    App* a = new App;
    session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.margin(
		    layout.vbox(
			a->make(kit, layout),
			layout.vglue(5.0),
			layout.hbox(
			    kit.push_button(
				"Next",
				new ActionCallback(App)(a, &App::next)
			    ),
			    layout.hglue(10.0),
			    kit.push_button(
				"Previous",
				new ActionCallback(App)(a, &App::prev)
			    )
			)
		    ),
		    10.0
		)
	    )
	)
    );
}
