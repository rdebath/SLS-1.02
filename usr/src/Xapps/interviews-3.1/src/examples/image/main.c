#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/image.h>
#include <InterViews/layout.h>
#include <InterViews/tiff.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/transformer.h>
#include <InterViews/tformsetter.h>
#include <InterViews/window.h>
#include <stdio.h>
#include <stdlib.h>

static PropertyData props[] = {
/*
    { "*visual", "TrueColor" },
 */
    { nil }
};

static OptionDesc options[] = {
    { "-rotate", "Image*rotate", OptionValueNext },
    { "-scale", "Image*scale", OptionValueNext },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Image", argc, argv, options, props);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    if (argc == 1) {
	fprintf(stderr, "Usage: %s <file>\n", argv[0]);
	exit(1);
    }

    Raster* r = TIFFRaster::load(argv[1]);
    if (r == nil) {
	fprintf(stderr, "%s: open tiff image %s failed\n", argv[0], argv[1]);
	exit(1);
    }
    Style* style = session->style();
    Transformer t;
    float f;
    if (style->find_attribute("scale", f)) {
	t.scale(f, f);
    }
    if (style->find_attribute("rotate", f)) {
	t.rotate(f);
    }
    session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.variable_span(
		    new TransformFitter(
			new TransformSetter(
			    new Image(r), t
			)
		    )
		)
	    )
	)
    );
}
