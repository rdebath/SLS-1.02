#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/window.h>
#include <stdio.h>

class App {
public:
    void msg();
};

declareActionCallback(App)
implementActionCallback(App)

void App::msg() {
    printf("hi mom!\n");
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
		    kit.push_button(
			"Push me", new ActionCallback(App)(a, &App::msg)
		    ),
		    10.0
		)
	    )
	)
    );
}
