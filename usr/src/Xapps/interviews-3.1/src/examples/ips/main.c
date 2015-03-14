#include "idraw.h"
#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/color.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/transformer.h>
#include <InterViews/tformsetter.h>
#include <InterViews/window.h>
#include <OS/file.h>
#include <OS/string.h>
#include <stdio.h>
#include <stdlib.h>

static PropertyData props[] = {
    { nil }
};

static OptionDesc options[] = {
    { "-rot", "Ips*rotation", OptionValueNext },
    { "-rotate", "Ips*rotation", OptionValueNext },
    { "-scale", "Ips*scale", OptionValueNext },
    { "-sx", "Ips*xscale", OptionValueNext },
    { "-sy", "Ips*yscale", OptionValueNext },
    { "-tx", "Ips*xtranslate", OptionValueNext },
    { "-ty", "Ips*ytranslate", OptionValueNext },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Ips", argc, argv, options, props);
    const LayoutKit& layout = *LayoutKit::instance();
    Style* style = session->style();
    if (argc < 2) {
	fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
	exit(1);
    }
    InputFile* f = InputFile::open(String(argv[1]));
    if (f == nil) {
	fprintf(stderr, "can't open %s\n", argv[1]);
	exit(1);
    }
    Glyph* g = IdrawReader::load(f);
    if (g == nil) {
	fprintf(stderr, "%s is not idraw format?\n", argv[1]);
	exit(1);
    }
    Transformer t;
    float fx = 0, fy = 0;
    if (style->find_attribute("xtranslate", fx) &&
	style->find_attribute("ytranslate", fy)
    ) {
	t.translate(fx, fy);
    }
    fx = 1; fy = 1;
    if (style->find_attribute("xscale", fx) ||
	style->find_attribute("yscale", fy)
    ) {
	t.scale(fx, fy);
    }
    if (style->find_attribute("scale", fx)) {
	t.scale(fx, fx);
    }
    if (style->find_attribute("rotation", fx)) {
	t.rotate(fx);
    }
    WidgetKit& kit = *WidgetKit::instance();
    return session->run_window(
	new ApplicationWindow(
	    new Background(
		layout.variable_span(
		    new TransformFitter(
			new TransformSetter(g, t)
		    )
		),
		kit.background()
	    )
	)
    );
}
