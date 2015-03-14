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
 * SMFKit -- object for creating SGI Motif components
 */

#ifndef ivlook_smfkit_h
#define ivlook_smfkit_h

#include <IV-look/mf_kit.h>

class SMFKitImpl;

class SMFKit : public WidgetKit {
public:
    SMFKit();
    virtual ~SMFKit();

    virtual const char* gui() const;

    virtual void style_changed(Style*);

    virtual MonoGlyph* outset_frame(Glyph*) const;
    virtual MonoGlyph* inset_frame(Glyph*) const;
    virtual MonoGlyph* bright_inset_frame(Glyph*) const;

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
private:
    SMFKitImpl* impl_;
};

#endif
