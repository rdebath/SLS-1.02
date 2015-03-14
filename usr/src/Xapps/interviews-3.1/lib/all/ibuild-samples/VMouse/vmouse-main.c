#include <Unidraw/catalog.h> 
#include <Unidraw/unidraw.h> 
#include <Unidraw/creator.h> 
#include <InterViews/canvas.h> 
#include <InterViews/painter.h> 
#include <InterViews/sensor.h> 
#include <InterViews/world.h> 
#include "VMouse.h"
#include <InterViews/perspective.h> 
#include <IV-2_6/_enter.h>
#include <stream.h>

static PropertyData properties[] = {
#include "vmouse-props"
    { nil }
};

static OptionDesc options[] = {
    { nil }
};

int main (int argc, char** argv) {
    int exit_status = 0;
    Creator creator;
    Unidraw* unidraw = new Unidraw(
        new Catalog("/****/", &creator), argc, argv, options, properties
    );
    World* w = unidraw->GetWorld();
    if (argc != 2) {
	cerr << "Usage: vmouse.exe hostname" << "\n";
	exit_status = 1;
        
    } else {
        VMouse* _vmouse = new VMouse("VMouse", argv[1]);
        w->InsertApplication(_vmouse);
        unidraw->Run();
        delete unidraw;
    }
    return exit_status;
}
