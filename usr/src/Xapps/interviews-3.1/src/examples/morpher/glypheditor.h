/*
 * glypheditor - a simple drawing editor-like application for tutorial purposes
 */

#include <InterViews/monoglyph.h>

class GlyphViewer;
class Morpher;
class Window;

class GlyphEditor : public MonoGlyph {
public:
    GlyphEditor();
    Window* window();
    GlyphViewer* viewer();
    Morpher* morpher();

    virtual void allocate(Canvas*, const Allocation&, Extension&);

    void fast_reverse();
    void reverse_play();
    void record();
    void pause();
    void forward_play();
    void fast_forward();
    void new_session();
protected:
    GlyphViewer* _gviewer;
    Morpher* _morpher;
    Window* _w;
private:
    Glyph* interior();
    Glyph* buttons();
    Glyph* menus();
    Glyph* toolpal();
    void engage_tool(const char*);
};

inline Window* GlyphEditor::window () { return _w; }
inline GlyphViewer* GlyphEditor::viewer () { return _gviewer; }
inline Morpher* GlyphEditor::morpher () { return _morpher; }
