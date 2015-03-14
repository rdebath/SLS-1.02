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
    TelltaleGroup* group = new TelltaleGroup;
    session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.margin(
		    layout.hbox(
			layout.vcenter(
			    kit.check_box(
				"Toggle me",
				new ActionCallback(App)(a, &App::msg)
			    )
			),
			layout.hglue(15.0),
			layout.vcenter(
			    layout.vbox(
				kit.radio_button(group, "One", nil),
				kit.radio_button(group, "Two", nil)
			    )
			)
		    ),
		    10.0
		)
	    )
	)
    );
}
