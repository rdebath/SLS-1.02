#include <InterViews/font.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <stdio.h>
#include <stdlib.h>

static PropertyData props[] = {
    { "*family", "Times" },
    { "*face", "Roman" },
    { "*size", "12" },
    { nil }
};

static OptionDesc options[] = {
    { "-family", "*family", OptionValueNext },
    { "-face", "*face", OptionValueNext },
    { "-size", "*size", OptionValueNext },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv, options, props);
    Style* style = session->style();

    String family;
    style->find_attribute("family", family);
    NullTerminatedString family_ns(family);
    FontFamily* fm = new FontFamily(family_ns.string());

    String face;
    style->find_attribute("face", face);

    long size;
    style->find_attribute("size", size);

    const char* name;
    float scale;
    NullTerminatedString ns(face);
    if (fm->font(int(size), ns.string(), name, scale)) {
	printf("font '%s', scale %.2f\n", name, scale);
    } else {
	printf(
	    "no match for %.*s-%.*s-%d\n", family.length(), family.string(),
	    face.length(), face.string(), size
	);
    }
}
