#include "pued.h"

#include <Unidraw/catalog.h>
#include <Unidraw/creator.h>
#include <Unidraw/unidraw.h>

#include <InterViews/world.h>

static PropertyData properties[] = {
    { "*domain",        "drawing" },
    { "*history",	"20" },
    { nil }
};

static OptionDesc options[] = {
    { nil }
};

int main (int argc, char** argv) {
    Creator creator;
    Unidraw* unidraw = new Unidraw(
        new Catalog("punidraw", &creator), argc, argv, options, properties
    );

    unidraw->Open(new Punidraw);
    unidraw->Run();

    delete unidraw;
    return 0;
}
