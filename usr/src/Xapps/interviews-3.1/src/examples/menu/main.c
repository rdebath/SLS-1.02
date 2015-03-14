#include <IV-look/kit.h>
#include <InterViews/action.h>
#include <InterViews/background.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <stdio.h>

struct CmdInfo;

class App {
public:
    void open(), save(), quit();

    void cut();
    void copy();
    void paste();

    void black();
    void red();
    void green();
    void blue();

    Menu* menubar(CmdInfo*, WidgetKit&, const LayoutKit&);
private:
    Menu* pulldown(CmdInfo*, int, WidgetKit&, const LayoutKit&);
};

declareActionCallback(App)
implementActionCallback(App)

struct CmdInfo {
    const char* str;
    ActionMemberFunction(App) func;
    CmdInfo* submenu;
    int options;
};

CmdInfo filemenu[] = {
    { "Open", &App::open },
    { "Save", &App::save },
    { "", nil },
    { "Quit", &App::quit },
    { nil }
};

CmdInfo editmenu[] = {
    { "Cut", &App::cut },
    { "Copy", &App::copy },
    { "Paste", &App::paste },
    { nil }
};

CmdInfo colormenu[] = {
    { "Black", &App::black },
    { "Red", &App::red },
    { "Green", &App::green },
    { "Blue", &App::blue },
    { nil }
};

CmdInfo bar[] = {
    { "File", nil, filemenu, 0 },
    { "Edit", nil, editmenu, 1 },
    { "Color", nil, colormenu, 2 },
    { nil }
};

void App::open() { printf("open\n"); }
void App::save() { printf("save\n"); }
void App::quit() { Session::instance()->quit(); }

void App::cut() { printf("cut\n"); }
void App::copy() { printf("copy\n"); }
void App::paste() { printf("paste\n"); }

void App::black() { printf("black\n"); }
void App::red() { printf("red\n"); }
void App::green() { printf("green\n"); }
void App::blue() { printf("blue\n"); }

Menu* App::menubar(CmdInfo* info, WidgetKit& kit, const LayoutKit& layout) {
    Menu* m = kit.menubar();
    for (CmdInfo* i = info; i->str != nil; i++) {
	MenuItem* mi = kit.menubar_item(kit.fancy_label(i->str));
	mi->menu(pulldown(i->submenu, i->options, kit, layout));
	m->append_item(mi);
    }
    m->item(0)->menu()->item(1)->state()->set(
	TelltaleState::is_enabled, false
    );
    return m;
}

Menu* App::pulldown(
    CmdInfo* info, int opt, WidgetKit& k, const LayoutKit& layout
) {
    Menu* m = k.pulldown();
    TelltaleGroup* group = nil;
    for (CmdInfo* i = info; i->str != nil; i++) {
        if (i->str[0] == '\0') {
            m->append_item(k.menu_item_separator());
        } else {
	    Glyph* g = layout.r_margin(
		k.fancy_label(i->str), 0.0, fil, 0.0
	    );
	    MenuItem* mi;
	    switch (opt) {
	    case 1:
		mi = k.check_menu_item(g);
		break;
	    case 2:
		if (group == nil) {
		    group = new TelltaleGroup;
		}
		mi = k.radio_menu_item(group, g);
		break;
	    default:
		mi = k.menu_item(g);
		break;
	    }
	    if (i->func == nil && i->submenu != nil) {
		mi->menu(pulldown(i->submenu, i->options, k, layout));
	    } else {
		mi->action(new ActionCallback(App)(this, i->func));
	    }
	    m->append_item(mi);
        }
    }
    return m;
}

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    Style* style = session->style();
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    App* a = new App;
    return session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.margin(a->menubar(bar, kit, layout), 10.0)
	    )
	)
    );
}
