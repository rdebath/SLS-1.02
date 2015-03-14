#include <stdlib.h>
#include <stream.h>
#include <string.h>
#include <IV-look/kit.h>
#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <InterViews/session.h>
#include <InterViews/window.h>

#include "Graph.h"
#include "rpc_read.h"

Service reader(8001);

static PropertyData props[] = {
    { "*plot_color", "black" },
    { "*mark_color", "black" },
    { "*axis_color", "black" },
    { "*tick_color", "black" },
    { "*background", "wheat" },
    { nil }
};

static OptionDesc options[] = {
    { "-plot", "*plot_color", OptionValueNext },
    { "-mark", "*mark_color", OptionValueNext },
    { "-axis", "*axis_color", OptionValueNext },
    { "-tick", "*tick_color", OptionValueNext },
    { nil }
};

Graph* graph;

void Callback(char* msg) {
    float x, y;
	char header[80];
    if (strcmp(msg, "exit") == 0) {
        Session::instance()->quit();
    } else {
        sscanf(msg, "%s %f %f ", header, &x, &y);
        graph->AddPt(x, y);
    } 
    return;
}

int main(int argc, char** argv) {
    char* symbol = "FOO";
    Session* session = new Session("plot", argc, argv, options, props);
    WidgetKit& kit = *WidgetKit::instance();
    graph = new Graph(
        300.0, 300.0, 0.0, 6.0, 92.0, 94.0, kit.background(), symbol
    );
    Window* w = new ApplicationWindow(graph);
    session->run_window(w);
    return 0;
}
