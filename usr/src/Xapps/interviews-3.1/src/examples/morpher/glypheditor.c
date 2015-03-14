/*
 * glypheditor implementation
 */

#include <InterViews/action.h>
#include <InterViews/box.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/label.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/telltale.h>
#include <IV-look/button.h>
#include <IV-look/dialogs.h>
#include <IV-look/fchooser.h>
#include <IV-look/menu.h>
#include <IV-look/kit.h>
#include <OS/file.h>
#include <OS/string.h>
#include <stdio.h>
#include <string.h>
#include "figure.h"
#include "morpher.h"
#include "glypheditor.h"
#include "glyphviewer.h"
#include "idraw.h"

typedef void (GlyphViewer::*ToolSetter) (unsigned int);

class CatalogButton : public Button {
public:
    CatalogButton(Glyph*, Style*, TelltaleState*, Action*);
    virtual ~CatalogButton();
    virtual void enter();
    virtual void leave();
};

CatalogButton::CatalogButton(
    Glyph* g, Style* s, TelltaleState* t, Action* a
) : Button(g, s, t, a) {}

CatalogButton::~CatalogButton () {}

void CatalogButton::enter () {}
void CatalogButton::leave () {}

class GraphicCallback : public Action {
public:
    GraphicCallback(GlyphViewer*, ToolSetter, unsigned int);
    virtual void execute();
private:
    GlyphViewer* _gv;
    ToolSetter _tool;
    unsigned _type;
};

GraphicCallback::GraphicCallback (
    GlyphViewer* gv, ToolSetter tool, unsigned int t
) {
    _gv = gv;
    _tool = tool;
    _type = t;
}

void GraphicCallback::execute () {
    /* workaround for cfront bug */
    GlyphViewer* gv = _gv;
    ToolSetter tool = _tool;
    unsigned int type = _type;
    (gv->*tool)(type);
}

GlyphEditor::GlyphEditor () {
    _morpher = new Morpher(10, 20, this);
    body(interior());
    engage_tool("pause");
}

Glyph* GlyphEditor::interior () {
    LayoutKit* layout = LayoutKit::instance();
    WidgetKit* widget = WidgetKit::instance();
    
    _gviewer = new GlyphViewer(300, 300);
    return widget->inset_frame(
        layout->vbox(
            menus(),
            layout->hbox(
                layout->vbox(
                    widget->inset_frame(
                        _gviewer
                    )
                ),
                widget->outset_frame(
                    layout->vbox(
                        layout->vglue(5, fil/2.0, 0),
                        buttons(),
                        layout->vglue(40, fil, 0)
                    )
                )
            ),
            toolpal() 
        )
    );
}

class Teller : public TelltaleState {
public:
    Teller(const TelltaleFlags, Action*);
    virtual void set(const TelltaleFlags, boolean);
protected:
    Action* _action;
};

Teller::Teller(const TelltaleFlags f, Action* a) : TelltaleState (f) {
    _action = a;
}

void Teller::set (const TelltaleFlags f, boolean flag) {
    if (flag && (f == is_active)) {
        TelltaleState::set(is_active | is_chosen, flag);
        if (_action != nil) {
            _action->execute();
        }
    } else if (!flag && f == is_chosen) {
        TelltaleState::set(is_active | is_chosen, flag);

    } else if (!flag && f == is_active) {

    } else if (flag && f == is_chosen) {

    } else {
        TelltaleState::set(f, flag);
    }
}

void GlyphEditor::new_session () {
    _morpher->new_session();
}

Glyph* GlyphEditor::buttons () {
    LayoutKit* layout = LayoutKit::instance();
    WidgetKit* widget = WidgetKit::instance();
    TelltaleGroup* teller = new TelltaleGroup;

    TelltaleFlags f = TelltaleState::is_enabled | TelltaleState::is_choosable;
    widget->begin_style("PushButton", "Button");
    Action* a = new GraphicCallback(
        _gviewer, &GlyphViewer::cur_tool, Tool::rotate
    );
    TelltaleState* t1 = new Teller(f, a);
    t1->join(teller);
    Button* rotate = new CatalogButton(
        widget->push_button_look(widget->label("  Rotate  "), t1), 
        widget->style(), t1, nil
    );

    Action* b = new GraphicCallback(
        _gviewer, &GlyphViewer::cur_tool, Tool::scale
    );
    TelltaleState* t2 = new Teller(f, b);
    t2->join(teller);
    Button* scale = new CatalogButton(
        widget->push_button_look(widget->label("  Scale  "), t2), 
        widget->style(), t2, nil
    );

    Action* c = new GraphicCallback(
        _gviewer, &GlyphViewer::cur_tool, Tool::move
    );
    TelltaleState* t3 = new Teller(f, c);
    t3->join(teller);
    t3->set(TelltaleState::is_active, true);
    Button* move = new CatalogButton(
        widget->push_button_look(widget->label("  Move  "), t3), 
        widget->style(), t3, nil
    );
    widget->end_style();
    
    return layout->vbox(
        move, scale, rotate 
    );
}

void GlyphEditor::allocate (Canvas* c, const Allocation& a, Extension& ext) {
    _w = c->window();
    MonoGlyph::allocate(c, a, ext);
}

class Importer : public Action {
public:
    Importer(GlyphEditor* ed);
    virtual void execute();
protected:
    GlyphEditor* _ed;
    static FileChooser* _chooser;
};

FileChooser* Importer::_chooser;

Importer::Importer (GlyphEditor* ed) { _ed = ed; }

void Importer::execute () {
    Style* style;
    boolean reset_caption = false;
    if (_chooser == nil) {
        style = new Style(Session::instance()->style());
        _chooser = DialogKit::instance()->file_chooser(".", style);
        Resource::ref(_chooser);
        char buf[256];
        sprintf(buf, "Select an idraw drawing to import:");
        style->attribute("caption", "");
        style->attribute("subcaption", buf);
    } else {
        style = _chooser->style();
    }
    while (_chooser->post_for(_ed->window())) {
        const String* s = _chooser->selected();
        InputFile* f = InputFile::open(*s);
        if (f != nil) {
            GraphicMaster* gr = IdrawReader::load(f);
            if (gr != nil) {
                gr->flush();
                _ed->viewer()->root(gr);
                break;
            } else {
                style->attribute("caption", "Import failed!");
                reset_caption = true;
            }
            delete f;
        } else {
            style->attribute("caption", "Import failed!");
            reset_caption = true;
        }
    }
    if (reset_caption) {
        style->attribute("caption", "");
    }
}
    
class QuitAction : public Action {
public:
    QuitAction();
    virtual void execute();
};

QuitAction::QuitAction () {}

void QuitAction::execute () { Session::instance()->quit(); }

class Cleaner : public Action {
public:
    Cleaner(GlyphEditor* ed);
    virtual void execute();
protected:
    GlyphEditor* _ed;
};

Cleaner::Cleaner (GlyphEditor* ed) {
    _ed = ed;
}

void Cleaner::execute () {
    _ed->new_session();
}

class MorphAction : public Action {
public:
    MorphAction(Morpher* m, unsigned int m_code);
    virtual void execute();
protected:
    Morpher* _m;
    unsigned int _m_code;
};

MorphAction::MorphAction (Morpher* m, unsigned int m_code) {
    _m = m;
    _m_code = m_code;
}

void MorphAction::execute () {
    _m->execute(_m_code);
}

Glyph* GlyphEditor::menus () {
    WidgetKit* widget = WidgetKit::instance();

    Menu* mb = widget->menubar();
    MenuItem* mi = widget->menubar_item("File");
    Menu* pulldown = widget->pulldown();
    MenuItem* cleaner = widget->menu_item("Purge");
    MenuItem* import = widget->menu_item("Import");
    MenuItem* quit = widget->menu_item("Quit");
    cleaner->action(new Cleaner(this));
    import->action(new Importer(this));
    quit->action(new QuitAction);

    mb->append_item(mi);
    mi->menu(pulldown);
    pulldown->append_item(cleaner);
    pulldown->append_item(import);
    pulldown->append_item(widget->menu_item_separator());
    pulldown->append_item(quit);

    return mb;
}

struct MorphCtrl {
    const char* _filename;
    unsigned int _morph_code;
    TelltaleState* _tts;
};

static MorphCtrl morph_array[] = {
    { "fast_reverse", Morpher::fr, nil },
    { "reverse_pause", Morpher::rps, nil },
    { "reverse_play", Morpher::rp, nil },
    { "record", Morpher::r, nil },
    { "pause", Morpher::p, nil },
    { "cont_play", Morpher::cp, nil },
    { "forward_play", Morpher::fp, nil },
    { "forward_pause", Morpher::fps, nil },
    { "fast_forward", Morpher::ff, nil },
    { nil }
};
 
class PaletteGroup : public TelltaleGroup {
public:
    PaletteGroup();
    void record(TelltaleState*);
    void play(TelltaleState*);
    virtual void update(TelltaleState*);
protected:
    TelltaleState* _record;
    TelltaleState* _play;
};

inline void PaletteGroup::record (TelltaleState* t) { _record = t; }
inline void PaletteGroup::play (TelltaleState* t) { _play = t; }

PaletteGroup::PaletteGroup () {
    _record = nil;
    _play = nil;
}

void PaletteGroup::update (TelltaleState* s) {
    if (s == _record && s->test(TelltaleState::is_chosen)) {
        _play->TelltaleState::set(TelltaleState::is_active, true);
        remove(_play);
    } else if (s != _play && s->test(TelltaleState::is_chosen)) {
        _play->set(TelltaleState::is_chosen, false);
        remove(_play);
    }
    TelltaleGroup::update(s);
}
    

static const char* icon_dir = "../icons/";

Glyph* GlyphEditor::toolpal () {
    LayoutKit* layout = LayoutKit::instance();
    WidgetKit* widget = WidgetKit::instance();
    PaletteGroup* teller = new PaletteGroup;
    
    TelltaleFlags f = TelltaleState::is_enabled | TelltaleState::is_choosable;
    PolyGlyph* hbox = layout->hbox(12);

    for (int i = 0; morph_array[i]._filename != nil; i++) {
        Coord l, b, r, t;
        char buf[256];
        strcpy(buf, icon_dir);
        strcat(buf, morph_array[i]._filename);

        Graphic* gr = IdrawReader::load(buf);
        gr->getbounds(l, b, r, t);
        gr->translate(-l-(r-l)/2.0, -b-(t-b)/2.0);
        TelltaleState* ttstate = new Teller(
            f, new MorphAction(_morpher, morph_array[i]._morph_code)
        );
        morph_array[i]._tts = ttstate;
        ttstate->join(teller);
        Button* button = new CatalogButton(
            widget->push_button_look(gr, ttstate), 
            widget->style(), ttstate, nil
        );
        hbox->append(button);
        if (strcmp("record", morph_array[i]._filename) == 0) {
            teller->record(ttstate);
        } else if (strcmp("forward_play", morph_array[i]._filename) == 0) {
            teller->play(ttstate);
        }
    }

    return widget->outset_frame(
        layout->hbox(
            hbox,
            layout->hglue(120, fil, 0)
        )
    );
}

void GlyphEditor::engage_tool (const char* name) {
    int result = -1;
    for (int i = 0; morph_array[i]._filename != nil; i++) {
        if (strcmp(morph_array[i]._filename, name) == 0) {
            result = i;
            break;
        }
    }
    if (result >= 0) {
        morph_array[i]._tts->set(TelltaleState::is_active, true);
    }
}

void GlyphEditor::fast_reverse () {
    engage_tool("fast_reverse");
}

void GlyphEditor::reverse_play () {
    engage_tool("reverse_play");
}

void GlyphEditor::record () {
    engage_tool("record");
}

void GlyphEditor::pause () {
    engage_tool("pause");
}

void GlyphEditor::forward_play () {
    engage_tool("forward_play");
}

void GlyphEditor::fast_forward () {
    engage_tool("fast_forward");
}
