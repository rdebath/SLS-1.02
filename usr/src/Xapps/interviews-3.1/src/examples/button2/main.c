#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/image.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/tiff.h>
#include <InterViews/window.h>
#include <stdio.h>
#include <stdlib.h>

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
    const char* filename;
    if (argc == 1) {
	filename = "../../../lib/images/jello.tif";
    } else {
	filename = argv[1];
    }
    Raster* rast = TIFFRaster::load(filename);
    if (rast == nil) {
	fprintf(stderr, "%s: open tiff image %s failed\n", argv[0], argv[1]);
	exit(1);
    }

    Style* style = session->style();
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    App* a = new App;
    session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.margin(
		    kit.push_button(
			new Image(rast), new ActionCallback(App)(a, &App::msg)
		    ),
		    10.0
		)
	    )
	)
    );
}
