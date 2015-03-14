#include <Unidraw/catalog.h> 
#include <Unidraw/unidraw.h> 
#include <Unidraw/creator.h> 
#include <InterViews/canvas.h> 
#include <InterViews/painter.h> 
#include <InterViews/sensor.h> 
#include <InterViews/world.h> 
#include "Pie.h"
#include <InterViews/perspective.h> 
#include <IV-2_6/_enter.h>

static PropertyData properties[] = {
#include "pie-props"
    { nil }
};

static OptionDesc options[] = {
    { nil }
};

int main (int argc, char** argv) {
    Creator creator;
    Unidraw* unidraw = new Unidraw(
        new Catalog("/****/", &creator), argc, argv, options, properties
    );
    World* w = unidraw->GetWorld();
    Pie* _pie = new Pie("_instance_1");
    _pie->Append(0.173, "Berkeley");
    _pie->Append(0.12, "Harvard");
    _pie->Append(0.23, "Yale");
    _pie->Append(0.42, "Stanford");
    w->InsertApplication(_pie);
    unidraw->Run();
    delete unidraw;
    return 0;
}
