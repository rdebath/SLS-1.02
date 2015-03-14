/*
 * Copyright (c) 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * WidgetKit -- object for creating common UI components
 */

#ifndef ivlook_kit_h
#define ivlook_kit_h

#include <IV-look/button.h>
#include <IV-look/menu.h>
#include <IV-look/stepper.h>
#include <InterViews/action.h>
#include <InterViews/telltale.h>

#include <InterViews/_enter.h>

class Adjustable;
class Color;
class Cursor;
class Font;
class String;
class Style;
class WidgetKitImpl;

class WidgetKit {
protected:
    WidgetKit();
public:
    static WidgetKit* instance();
    virtual ~WidgetKit();

    virtual const char* gui() const;

    /* style management */
    virtual void style(Style*);
    virtual Style* style() const;
    virtual const Font* font() const;
    virtual const Color* foreground() const;
    virtual const Color* background() const;
    virtual void begin_style(const String&);
    virtual void begin_style(const char*);
    virtual void begin_style(const String& name, const String& alias);
    virtual void begin_style(const char* name, const char* alias);
    virtual void alias(const String&);
    virtual void alias(const char*);
    virtual void end_style();
    virtual void push_style();
    virtual void push_style(Style*);
    virtual void pop_style();
    virtual void style_changed(Style*);

    /* shared objects */
    virtual Cursor* hand_cursor() const;
    virtual Cursor* lfast_cursor() const;
    virtual Cursor* lufast_cursor() const;
    virtual Cursor* ufast_cursor() const;
    virtual Cursor* rufast_cursor() const;
    virtual Cursor* rfast_cursor() const;
    virtual Cursor* rdfast_cursor() const;
    virtual Cursor* dfast_cursor() const;
    virtual Cursor* ldfast_cursor() const;

    /* shading */
    virtual MonoGlyph* outset_frame(Glyph*) const = 0;
    virtual MonoGlyph* inset_frame(Glyph*) const = 0;
    virtual MonoGlyph* bright_inset_frame(Glyph*) const = 0;

    /* styled low-level glyphs */
    virtual Glyph* label(const char*) const;
    virtual Glyph* label(const String&) const;
    virtual Glyph* chiseled_label(const char*) const;
    virtual Glyph* chiseled_label(const String&) const;
    virtual Glyph* raised_label(const char*) const;
    virtual Glyph* raised_label(const String&) const;
    virtual Glyph* fancy_label(const char*) const;
    virtual Glyph* fancy_label(const String&) const;

    /* menus */
    virtual Menu* menubar() const;
    virtual Menu* pulldown() const;
    virtual Menu* pullright() const;

    virtual MenuItem* menubar_item(const char*) const;
    virtual MenuItem* menubar_item(const String&) const;
    virtual MenuItem* menubar_item(Glyph*) const;

    virtual MenuItem* menu_item(const char*) const;
    virtual MenuItem* menu_item(const String&) const;
    virtual MenuItem* menu_item(Glyph*) const;

    virtual MenuItem* check_menu_item(const char*) const;
    virtual MenuItem* check_menu_item(const String&) const;
    virtual MenuItem* check_menu_item(Glyph*) const;

    virtual MenuItem* radio_menu_item(TelltaleGroup*, const char*) const;
    virtual MenuItem* radio_menu_item(TelltaleGroup*, const String&) const;
    virtual MenuItem* radio_menu_item(TelltaleGroup*, Glyph*) const;

    virtual MenuItem* menu_item_separator() const;

    /* buttons */
    virtual Button* push_button(const char*, Action*) const;
    virtual Button* push_button(const String&, Action*) const;
    virtual Button* push_button(Glyph*, Action*) const;

    virtual Button* default_button(const char*, Action*) const;
    virtual Button* default_button(const String&, Action*) const;
    virtual Button* default_button(Glyph*, Action*) const;

    virtual Button* check_box(const char*, Action*) const;
    virtual Button* check_box(const String&, Action*) const;
    virtual Button* check_box(Glyph*, Action*) const;

    virtual Button* palette_button(const char*, Action*) const;
    virtual Button* palette_button(const String&, Action*) const;
    virtual Button* palette_button(Glyph*, Action*) const;

    virtual Button* radio_button(TelltaleGroup*, const char*, Action*) const;
    virtual Button* radio_button(TelltaleGroup*, const String&, Action*) const;
    virtual Button* radio_button(TelltaleGroup*, Glyph*, Action*) const;

    virtual Action* quit() const;

    /* adjusters */
    virtual Glyph* hslider(Adjustable*) const;
    virtual Glyph* hscroll_bar(Adjustable*) const;
    virtual Glyph* vslider(Adjustable*) const;
    virtual Glyph* vscroll_bar(Adjustable*) const;
    virtual Glyph* panner(Adjustable*, Adjustable*) const;
    virtual Stepper* enlarger(Adjustable*) const;
    virtual Stepper* reducer(Adjustable*) const;
    virtual Stepper* up_mover(Adjustable*) const;
    virtual Stepper* down_mover(Adjustable*) const;
    virtual Stepper* left_mover(Adjustable*) const;
    virtual Stepper* right_mover(Adjustable*) const;

    /* looks implemented by subclass, usually not called directly */
    virtual Glyph* menubar_look() const = 0;
    virtual Glyph* pulldown_look() const = 0;
    virtual Glyph* pullright_look() const;
    virtual Glyph* menubar_item_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* menu_item_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* check_menu_item_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* radio_menu_item_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* menu_item_separator_look() const = 0;

    virtual Glyph* push_button_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* default_button_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* check_box_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* palette_button_look(Glyph*, TelltaleState*) const = 0;
    virtual Glyph* radio_button_look(Glyph*, TelltaleState*) const = 0;

    virtual Glyph* slider_look(DimensionName, Adjustable*) const = 0;
    virtual Glyph* scroll_bar_look(DimensionName, Adjustable*) const = 0;
    virtual Glyph* panner_look(Adjustable*, Adjustable*) const = 0;
    virtual Glyph* enlarger_look(TelltaleState*) const = 0;
    virtual Glyph* reducer_look(TelltaleState*) const = 0;
    virtual Glyph* up_mover_look(TelltaleState*) const = 0;
    virtual Glyph* down_mover_look(TelltaleState*) const = 0;
    virtual Glyph* left_mover_look(TelltaleState*) const = 0;
    virtual Glyph* right_mover_look(TelltaleState*) const = 0;
private:
    WidgetKitImpl* impl_;
};

#include <InterViews/_leave.h>

#endif
