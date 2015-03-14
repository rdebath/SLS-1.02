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
 * DialogKit -- object for creating common dialog boxes
 */

#ifndef ivlook_dialogs_h
#define ivlook_dialogs_h

#include <IV-look/field.h>
#include <IV-look/fchooser.h>

#include <InterViews/_enter.h>

class DialogKitImpl;
class String;
class Style;
class WidgetKit;

class DialogKit {
protected:
    DialogKit();
public:
    static DialogKit* instance();
    virtual ~DialogKit();

    virtual WidgetKit* widget_kit() const;
    virtual FieldEditor* field_editor(
	const char* sample, Style*, FieldEditorAction* = nil
    ) const;
    virtual FieldEditor* field_editor(
	const String& sample, Style*, FieldEditorAction* = nil
    ) const;
    virtual FileChooser* file_chooser(
	const char* dir, Style*, FileChooserAction* = nil
    ) const;
    virtual FileChooser* file_chooser(
	const String& dir, Style*, FileChooserAction* = nil
    ) const;

    virtual FieldEditor* make_field_editor(
	const String& sample, WidgetKit*, Style*, FieldEditorAction* = nil
    ) const;
    virtual FileChooser* make_file_chooser(
	const String& dir, WidgetKit*, Style*, FileChooserAction* = nil
    ) const;
private:
    DialogKitImpl* impl_;
};

#include <InterViews/_leave.h>

#endif
