#include <Unidraw/catalog.h> 
#include <Unidraw/unidraw.h> 
#include <Unidraw/creator.h> 
#include <InterViews/canvas.h> 
#include <InterViews/painter.h> 
#include <InterViews/sensor.h> 
#include <InterViews/world.h> 
#include <InterViews/perspective.h> 
#include "Clock.h"
#include <IV-2_6/_enter.h>

static PropertyData properties[] = {
#include "clock-props"
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
    Clock* clock = new Clock("clock");
    w->InsertApplication(clock);
    clock->Run();
    delete unidraw;
    return 0;
}
