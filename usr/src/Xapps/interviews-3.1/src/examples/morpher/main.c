#include "glypheditor.h"
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>

static PropertyData props[] = {
    { "*GlyphEditor*name", "InterViews glyph editor" },
    { "*GlyphEditor*iconName", "Glypher" },
    { nil }
};

static OptionDesc options[] = {
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Glypher", argc, argv, options, props);
    GlyphEditor* ged = new GlyphEditor;
    ApplicationWindow* w = new ApplicationWindow(ged);
    Style* s = new Style(session->style());
    s->attribute("name", "InterViews glyph editor");
    s->attribute("iconName", "Glypher");
    w->style(s);
    return session->run_window(w);
}
