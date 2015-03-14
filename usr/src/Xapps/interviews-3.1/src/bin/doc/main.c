/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include "Application.h"
#include "Document.h"
#include "DocViewer.h"

#include "properties.h"

#include <InterViews/session.h>
#include <InterViews/style.h>
#include <OS/string.h>

#include <stdio.h>

const char* ACTIVE_HIGHLIGHT_COLOR = "active_highlight_color";
const char* DEFAULT_STYLE = "default_style";
const char* DOCUMENT_FILE_EXTENSION = "document_file_extension";
const char* DOCUMENT_PATH = "document_path";
const char* HIGHLIGHT_COLOR = "highlight";
const char* IDRAW_FONT_METRICS = "idraw_font_metrics";
const char* INIT_PSFIG_MODE = "init_psfig_mode";
const char* INSERT_FLASH_RATE = "insert_flash_rate";
const char* INSERT_HIGHLIGHT_COLOR = "insert_highlight_color";
const char* PAGE_ICON_FONT = "page_icon_font";
const char* POSTSCRIPT_FILE_EXTENSION = "postscript_file_extension";
const char* SCREEN_METRICS = "screen_metrics";
const char* SELECT_HIGHLIGHT_COLOR = "select_highlight_color";
const char* STYLE_FILE_EXTENSION = "style_file_extension";
const char* STYLE_PATH = "style_path";
const char* VERSION = "version";
const char* DEFAULT_PSFIG_MENUBAR = "default_psfig_menubar";
const char* DEFAULT_TABULAR_MENUBAR = "default_tabular_menubar";
const char* DEFAULT_TEXT_MENUBAR = "default_text_menubar";
const char* DEFAULT_PSFIG_KEYMAP = "default_psfig_keymap";
const char* DEFAULT_TABULAR_KEYMAP = "default_tabular_keymap";
const char* DEFAULT_ENCODED_KEYMAP = "default_encoded_keymap";
const char* DEFAULT_VERBATIM_KEYMAP = "default_verbatim_keymap";
const char* COMPLAINT_MODE = "complaint_mode";
const char* USER_INTERFACE_STYLE = "user_interface_style";
const char* AUTORAISE_ON_POST = "autoraise_on_post";
const char* PAGE_BACKGROUND_COLOR = "page_background_color";

static PropertyData props[] = {
    { "*page_background_color", "white" },
    { "*autoraise_on_post", "off" },
    { "*user_interface_style", "default" },
    { "*active_highlight_color", "" },
    { "*default_style", "article" },
    { "*document_file_extension", "doc" },
    { "*document_path", "." },
    { "*highlight_color", "#CCC" },
    { "*idraw_font_metrics", "on" },
    { "*init_psfig_mode", "draft" },
    { "*insert_flash_rate", "0.5", },
    { "*insert_highlight_color", "#000", },
    { "*page_icon_font", "5x8" },
    { "*postscript_file_extension", "ps" },
    { "*screen_metrics", "off" },
    { "*select_highlight_color", "" },
    { "*style_file_extension", "sty" },
    { "*style_path", DEFAULT_STYLE_PATH },
    { "*version", "1.0" },
    { "*complaint_mode", "bell" },
    { "*default_psfig_menubar", "default_menubar" },
    { "*default_tabular_menubar", "default_menubar" },
    { "*default_text_menubar", "default_menubar" },
    { "*default_menubar", "\
        <file>  (menu default_file_menu)   File\n\
        <edit>  (menu default_edit_menu)   Edit" },
    { "*default_file_menu", "\
        <new>       (viewer new)        New\\hfil\n\
        <open>      (viewer open)       Open ...\\hfil\n\
        <save>      (viewer save)       Save\\hfil\\quad^S\n\
        <saveas>    (viewer saveas)     Save as ...\\hfil\n\
        \n\
        <view>      (viewer view)       New view\\hfil\n\
        <close>     (viewer close)      Close\\hfil\n\
        <quit>      (application quit)  Quit\\hfil" },
    { "*default_edit_menu", "\
        <cut>       (clip cut)          Cut\\hfil\\quad^X\n\
        <copy>      (clip copy)         Copy\\hfil\\quad^C\n\
        <paste>     (clip paste)        Paste\\hfil\\quad^V" },
    { "*default_psfig_keymap", "" },
    { "*default_tabular_keymap", "" },
    { "*default_verbatim_keymap", "" },
    { "*default_encoded_keymap", "default_keymap" },
    { "*default_keymap", "\
        <015>  (character parbreak)\n\
        <040>  (character wordspace)\n\
        <055>  (character visiblehyphen)\n\
        <177>  (delete preceding)\n\
        <010>  (delete preceding)\n\
        <023>  (viewer save)\n\
        <030>  (clip cut)\n\
        <003>  (clip copy)\n\
        <026>  (clip paste)" },
    { nil }
};

static OptionDesc options[] = {
    { "-screenmetrics", "*screen_metrics", OptionValueImplicit, "on" },
    { "-style", "*default_style", OptionValueNext },
    { "-psfinal", "*init_psfig_mode", OptionValueImplicit, "final" },
    { nil }
};

int main (int argc, char** argv) {
    Session* session = new Session("Doc", argc, argv, options, props);
    Style* style = session->style();
    Application* application = new Application();

    String doc_path_string;
    style->find_attribute(DOCUMENT_PATH, doc_path_string);
    NullTerminatedString doc_path_ntstring(doc_path_string);
    const char* doc_path = doc_path_ntstring.string();

    String doc_ext_string;
    style->find_attribute(DOCUMENT_FILE_EXTENSION, doc_ext_string);
    NullTerminatedString doc_ext_ntstring(doc_ext_string);
    const char* doc_ext = doc_ext_ntstring.string();

    if (argc > 1) {
        char name[256];
        while (argc > 1) {
            if (application->file_path(argv[1], doc_ext, doc_path, name)) {
                Document* document = application->read(name);
                if (document != nil) {
                    document->name(name);
                    application->open(
                        new DocumentViewer(application, document)
                    );
                } else {
                    fprintf(stderr, "Can't read file %s\n", name);
                }
            } else {
                fprintf(stderr, "Can't find document %s\n", argv[1]);
            }
            ++argv;
            --argc;
        }
    } else {
        Document* document = application->read("");
        application->open(new DocumentViewer(application, document));
    }

    return session->run();
}
