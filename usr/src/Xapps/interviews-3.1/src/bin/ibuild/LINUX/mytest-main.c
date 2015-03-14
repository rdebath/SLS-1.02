#include <InterViews/canvas.h> 
#include <InterViews/painter.h> 
#include <InterViews/perspective.h> 
#include <InterViews/world.h> 
#include <InterViews/button.h> 
#include <InterViews/button.h> 
#include <InterViews/panner.h> 
#include <IV-2_6/_enter.h>

static PropertyData properties[] = {
#include "mytest-props"
    { nil }
};

static OptionDesc options[] = {
    { nil }
};

static Interactor* _instance_0() {
    ButtonState* _BS_0 = new ButtonState(0);
    PushButton* _PushButton_13 = new PushButton("_instance_0", "push", _BS_0, 0);
    return _PushButton_13;
};

static Interactor* _instance_2() {
    ButtonState* _BS_2 = new ButtonState(0);
    PushButton* _PushButton_15 = new PushButton("_instance_2", "push", _BS_2, 0);
    return _PushButton_15;
};

static Interactor* _instance_4() {
    return _Panner_17;
};

int main (int argc, char** argv) {
    World* w = new World("/****/", argc, argv, options, properties);
    w->InsertApplication(_instance_0());
    w->InsertApplication(_instance_2());
    w->InsertApplication(_instance_4());
    w->Run();
    delete w;
    return 0;
}
