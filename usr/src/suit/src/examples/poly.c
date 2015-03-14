#include "suit.h"

SUIT_object PolygonWidget;

void ChangeSideCount (SUIT_object thermometer)
{
    SUIT_setInteger (PolygonWidget, "number of sides",
		     (int) SUIT_getDouble (thermometer, CURRENT_VALUE));
}


void main (int argc, char *argv[])
{
    SUIT_object slider;

    SUIT_init(argv[0]);

    slider = SUIT_createBoundedValue ("Number of Sides", ChangeSideCount);
    SUIT_setDouble(slider, MINIMUM_VALUE, 3.0);
    SUIT_setDouble(slider, MAXIMUM_VALUE, 20.0);
    SUIT_setDouble(slider, GRANULARITY, 1.0);
    PolygonWidget = SUIT_createPolygon ("Polygon");
    SUIT_setInteger (PolygonWidget, "number of sides", 5);
    
    SUIT_createDoneButton (NULL);
    SUIT_beginStandardApplication();
}
