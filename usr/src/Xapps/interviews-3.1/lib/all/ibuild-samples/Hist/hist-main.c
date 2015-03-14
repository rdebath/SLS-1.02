#include <Unidraw/catalog.h> 
#include <Unidraw/unidraw.h> 
#include <Unidraw/creator.h> 
#include <InterViews/canvas.h> 
#include <InterViews/painter.h> 
#include <InterViews/sensor.h> 
#include <InterViews/world.h> 
#include <InterViews/perspective.h> 
#include "Hist.h"
#include <IV-2_6/_enter.h>

static PropertyData properties[] = {
#include "hist-props"
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
    Hist* hist = new Hist("hist", 5.0);

    hist->Include("wheat", 4.9);
    hist->Include("corn", 4.3);
    hist->Include("rice", 3.2);
    hist->Include("oats", 2.1);
    hist->Include("alfalfa", .5);
    hist->Include("soy", 1.2);
    hist->Include("barley", 1.6);

    w->InsertApplication(hist);
    unidraw->Run();
    delete unidraw;
    return 0;
}
