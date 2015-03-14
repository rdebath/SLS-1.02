#include <Unidraw/catalog.h> 
#include <Unidraw/unidraw.h> 
#include <Unidraw/creator.h> 
#include <InterViews/canvas.h> 
#include <InterViews/painter.h> 
#include <InterViews/sensor.h> 
#include <InterViews/world.h> 
#include "Grapher.h"
#include <IV-2_6/_enter.h>

static PropertyData properties[] = {
#include "grapher-props"
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
    Grapher* _grapher = new Grapher("_grapher", 2, 5, 100, 102);
    _grapher->Append(2.2, 100.2);
    _grapher->Append(2.6, 100.2);
    _grapher->Append(2.8, 100.8);
    _grapher->Append(3.2, 101.2);
    _grapher->Append(3.4, 101.4);
    _grapher->Append(3.8, 101.0);
    _grapher->Append(4.2, 100.6);
    w->InsertApplication(_grapher);
    w->Run();
    delete w;
    return 0;
}
