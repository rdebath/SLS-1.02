/*
 * Copyright (c) 1991, 1992 Stanford University
 * Copyright (c) 1991, 1992 Silicon Graphics, Inc.
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

#ifndef ivlook_olkit_h
#define ivlook_olkit_h

#include <IV-look/kit.h>

class OLKitImpl;
class Window;

class OLKit : public WidgetKit {
/*
 *  OpenLook ==> WidgetKit mappings:
 *
 *    exclusive setting	      ==>  radio_button
 *    non-exclusive setting   ==>  pallette_button
 *    exclusive menu setting  ==>  radio_menu_item
 *    non-exclusive setting   ==>  radio_menu_item(*)
 *    scrolling button	      ==>  {up,down,left,down}_mover
 *    text field	      ==>  DialogKit::make_field_editor 
 *
 *    *  Remove the telltale state of each exclusive menu item
 *	 from the radio_menu_item's telltale group.
 *
 *  Use -xrm "olglyph:size" to specify the font size used for the OpenLook
 *  font, where size is one of (10, 12, 14, 19).  Default size is 12.
 */
public:
    OLKit();
    virtual ~OLKit();

    virtual const char* gui() const;
    
    /* shading */
    virtual MonoGlyph* outset_frame(Glyph*) const;
    virtual MonoGlyph* inset_frame(Glyph*) const;
    virtual MonoGlyph* bright_inset_frame(Glyph*) const;

    /* menus */
    virtual Menu* menubar() const;
    
    /* looks implemented by subclass, usually not called directly */
    virtual Glyph* menubar_look() const;
    virtual Glyph* pulldown_look() const;
    virtual Glyph* pullright_look() const;
    virtual Glyph* menubar_item_look(Glyph*, TelltaleState*) const;
    virtual Glyph* menu_item_look(Glyph*, TelltaleState*) const;
    virtual Glyph* check_menu_item_look(Glyph*, TelltaleState*) const;
    virtual Glyph* radio_menu_item_look(Glyph*, TelltaleState*) const;
    virtual Glyph* menu_item_separator_look() const;

    virtual Glyph* push_button_look(Glyph*, TelltaleState*) const;
    virtual Glyph* default_button_look(Glyph*, TelltaleState*) const;
    virtual Glyph* check_box_look(Glyph*, TelltaleState*) const;
    virtual Glyph* palette_button_look(Glyph*, TelltaleState*) const;
    virtual Glyph* radio_button_look(Glyph*, TelltaleState*) const;

    virtual Glyph* slider_look(DimensionName, Adjustable*) const;
    virtual Glyph* scroll_bar_look(DimensionName, Adjustable*) const;
    virtual Glyph* panner_look(Adjustable*, Adjustable*) const;
    virtual Glyph* enlarger_look(TelltaleState*) const;
    virtual Glyph* reducer_look(TelltaleState*) const;
    virtual Glyph* up_mover_look(TelltaleState*) const;
    virtual Glyph* down_mover_look(TelltaleState*) const;
    virtual Glyph* left_mover_look(TelltaleState*) const;
    virtual Glyph* right_mover_look(TelltaleState*) const;

    /* OLKit-specific */
    virtual const Color* white() const;
    virtual const Color* bg1() const;
    virtual const Color* bg2() const;
    virtual const Color* bg3() const;
    virtual const Color* black() const;
    virtual const Color* inactive() const;
    virtual const Color* busy() const;

    virtual Button* pushpin(const Window* unpinned, Window* pinned) const;
    virtual void pinnable(Menu*, const Window* unpinned) const;

    virtual MenuItem* habbrev_menu_button() const;
    virtual MenuItem* vabbrev_menu_button() const;

    virtual Glyph* hgauge(Adjustable*) const;
    virtual Glyph* vgauge(Adjustable*) const;
    virtual Glyph* htick() const;
    virtual Glyph* vtick() const;
    
    virtual Glyph* top_end_box(Adjustable*) const;
    virtual Glyph* bottom_end_box(Adjustable*) const;
    virtual Glyph* left_end_box(Adjustable*) const;
    virtual Glyph* right_end_box(Adjustable*) const;
private:
    OLKitImpl* impl_;
};

#endif
