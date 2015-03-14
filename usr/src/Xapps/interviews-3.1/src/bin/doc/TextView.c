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

/*
 * TextView
 */

#include "TextView.h"

#include "Application.h"
#include "Document.h"
#include "DocViewer.h"
#include "TextItem.h"

#include "codes.h"
#include "properties.h"

#include <InterViews/event.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/math.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <strstream.h>
#include <fstream.h>

class LigatureInfo {
public:
    long _character;
    long _preceding;
    long _ligature;
};

LigatureInfo ligatures[] = {
    {wordspace, wordspace, sentencespace},
    {wordspace, sentencespace, sentencespace},
    {wordspace, parbreak, parbreak},
    {wordspace, linebreak, linebreak},
    {wordspace, quadspace, quadspace},
    {linebreak, wordspace, linebreak},
    {linebreak, sentencespace, linebreak},
    {linebreak, linebreak, linebreak},
    {parbreak, wordspace, parbreak},
    {parbreak, sentencespace, parbreak},
    {parbreak, parbreak, parbreak},
    {visiblehyphen, visiblehyphen, endash},
    {visiblehyphen, endash, emdash},
    {visiblehyphen, emdash, emdash},
    {smallskip, wordspace, smallskip},
    {smallskip, parbreak, smallskip},
    {smallskip, smallskip, medskip},
    {smallskip, medskip, bigskip},
    {smallskip, bigskip, bigskip},
    {hfil, hfil, hfill},
    {hfil, hfill, hfill},
    {'`', '`', 0252},
    {'`', 0252, 0252},
    {'\'', '\'', 0272},
    {'\'', 0272, 0272},
    {'.', '.', 0267},
    {'.', 0267, 0267},
/*
    {'i', 'f', 0256},
    {'l', 'f', 0257},
*/
    {0, 0, 0}
};

TextView::TextView (
    DocumentViewer* viewer, ItemView* parent, TextItem* text
) : ItemView(viewer, parent) {
    _text = text;
    _text->ref();
    _text->attach(this);
    _active = false;
    _dot = 0;
    _mark = 0;
    _style = text->style();
    _source = text->source();
    _menubar = nil;
    _encoded_keymap = nil;
    _verbatim_keymap = nil;
}

TextView::~TextView () {
    delete _encoded_keymap;
    delete _verbatim_keymap;
    delete _menubar;
    if (_active) {
        _viewer->focus(nil);
    }
    _text->detach(this);
    _text->unref();
}

void TextView::repair () {
    _text->notify();
}

void TextView::update () { }

void TextView::item_changed (long, long) { }

void TextView::item_replaced (long, long) { }

void TextView::item_inserted (long index, long count) {
    if (index < _dot) {
        _dot += count;
    }
    if (index < _mark) {
        _mark += count;
    }
}

void TextView::item_removed (long index, long count) {
    if (index + count < _dot) {
        _dot -= count;
    } else if (index < _dot) {
        _dot = index;
    }
    if (index + count < _mark) {
        _mark -= count;
    } else if (index < _mark) {
        _mark = index;
    }
}

long TextView::dot () {
    return _dot;
}

void TextView::dot (long dot) {
    if (dot >= 0L) {
        _dot = Math::min(Math::max(dot, 0L), _text->item_count());
        _mark = _dot;
    }
}

long TextView::mark () {
    return _mark;
}

void TextView::mark (long mark) {
    if (mark >= 0L) {
        _mark = Math::min(Math::max(mark, 0L), _text->item_count());
    }
}

void TextView::mark_selection () { }

boolean TextView::safe_to_edit (long d, long m) {
    Document* document = _text->document();
    if (d == m) {
        return document->text_source(_source)._editable;
    } else {
        TextSource& outer = document->text_source(
            document->common_source(
                _text->item_source(d-1), _text->item_source(m)
            )
        );
        TextSource& inner = document->text_source(
            document->common_source(
                _text->item_source(d), _text->item_source(m-1)
            )
        );
        TextSource& begin = document->text_source(
            document->common_source(
                _text->item_source(d-1), _text->item_source(d)
            )
        );
        TextSource& end = document->text_source(
            document->common_source(
                _text->item_source(m-1), _text->item_source(m)
            )
        );
        return (
            inner._depth >= begin._depth
            && inner._depth >= end._depth
            && (
                begin._depth == end._depth
                ? outer._editable||inner._editable : inner._editable
            )
        );
    }
}

boolean TextView::insert_text (long character) {
    long d = Math::min(_dot, _mark);
    long m = Math::max(_dot, _mark);
    if (!safe_to_edit(d, m)) {
        _viewer->application()->complain(_viewer, "Can't edit mixed text");
        return false;
    } else {
        if (d != m) {
            dot(_text->remove(d, m - d));
        }
        if (character != 0 && _dot > 0 && safe_to_edit(_dot - 1, _dot)) {
            for (long i = 0; ligatures[i]._character != 0; ++i) {
                LigatureInfo& l = ligatures[i];
                if (
                    l._character == character
                    && l._preceding == _text->item_code(_dot - 1)
                ) {
                    dot(_text->remove(_dot - 1, 1));
                    dot(_text->insert(_dot, l._ligature,_style,_source,nil));
                    return true;
                }
            }
        }
        if (character != 0) {
            dot(_text->insert(_dot, character, _style, _source, nil));
        }
        return true;
    }
}

static boolean interword_code (long code) {
    int int_code = int(code);
    return (
        isascii(int_code) && (isspace(int_code) || ispunct(int_code))
        || code == hfil
        || code == vfil
        || code == hrule
        || code == smallskip
        || code == medskip
        || code == bigskip
        || code == smallvspace
        || code == medvspace
        || code == bigvspace
        || code == newpage
        || code == pagebreak
        || code == parbreak
        || code == linebreak
        || code == nobreakspace
        || code == wordspace
        || code == sentencespace
        || code == quadspace
        || code == endash
        || code == emdash
    );
}

static boolean interpar_code (long code) {
    return (
        code == vfil
        || code == hrule
        || code == smallskip
        || code == medskip
        || code == bigskip
        || code == smallvspace
        || code == medvspace
        || code == bigvspace
        || code == newpage
        || code == pagebreak
        || code == parbreak
    );
}

boolean TextView::command (const char* command) {
    Document* document = _text->document();
    if (strncmp(command, "control", 7) == 0) {
        return false;
    } else if (strncmp(command, "select", 6) == 0) {
        long d = Math::min(_dot, _mark);
        long m = Math::max(_dot, _mark);
        long count = _text->item_count();
        if (strcmp(command+7, "all") == 0) {
            d = 0;
            m = count;
        } else if (strcmp(command+7, "word") == 0) {
            while (d > 0 && interword_code(_text->item_code(d - 1))) { --d; }
            while (d > 0 && !interword_code(_text->item_code(d - 1))) { --d; }
            while (m < count && !interword_code(_text->item_code(m))) { ++m; }
        } else if (strcmp(command+7, "paragraph") == 0) {
            while (d > 0 && interpar_code(_text->item_code(d - 1))) { --d; }
            while (d > 0 && !interpar_code(_text->item_code(d - 1))) { --d; }
            while (m < count && !interpar_code(_text->item_code(m))) { ++m; }
        }
        dot(d);
        mark(m);
        mark_selection();
        selection_changed(true);
        return false;
    } else if (strncmp(command, "delete", 6) == 0) {
        if (strcmp(command+7, "following") == 0) {
            if (_dot == _mark) mark(_dot + 1);
            insert_text(0);
        } else if (strcmp(command+7, "preceding") == 0) {
            if (_dot == _mark) mark(_dot - 1);
            insert_text(0);
        }
        return true;
    } else if (strncmp(command, "go", 2) == 0) {
        long d = Math::min(_dot, _mark);
        long m = Math::max(_dot, _mark);
        if (strcmp(command+3, "forward_character") == 0) {
            dot(m + ((d == m) ? 1 : 0));
        } else if (strcmp(command+3, "backward_character") == 0) {
            dot(d - ((d == m) ? 1 : 0));
        } else if (strcmp(command+3, "forward_paragraph") == 0) {
            long count = _text->item_count();
            d = m + 1;
            while (d < count && !interpar_code(_text->item_code(d))) { ++d; }
            dot(d);
        } else if (strcmp(command+3, "backward_paragraph") == 0) {
            d = d - 1;
            while (d > 0 && !interpar_code(_text->item_code(d - 1))) { --d; }
            dot(d);
        } else if (strcmp(command+3, "beginning") == 0) {
            dot(0);
        } else if (strcmp(command+3, "end") == 0) {
            dot(_text->item_count());
        }
        mark_selection();
        selection_changed(true);
        return true;
    } else if (strncmp(command, "font", 4) == 0) {
        long dot = Math::min(_dot, _mark);
        long mark = Math::max(_dot, _mark);
        if (!safe_to_edit(dot, mark)) {
            _viewer->application()->complain(_viewer, "Can't edit mixed text");
            return false;
        } else {
            const char* font;
            if (strlen(command) == 4) {
                font = _viewer->application()->ask(
                    _viewer, "Font name:", ""
                );
            } else {
                font = command + 5;
            }
            if (font == nil) {
                return false;
            } else {
                char style[100];
                sprintf(style, "\\font{%s}", font);
                long old_style = 0;
                long new_style = 0;
                do {
                    if (_text->item_style(dot) != old_style) {
                        old_style = _text->item_style(dot);
                        new_style = document->parse_style(
                            style, old_style
                        );
                    }
                    if (dot < mark && new_style != old_style) {
                        _text->replace(dot, 1, new_style);
                    }
                    ++dot;
                } while (dot < mark);
                _style = document->parse_style(style, _style);
                selection_changed(false);
                return true;
            }
        }
    } else if (strncmp(command, "color", 5) == 0) {
        long dot = Math::min(_dot, _mark);
        long mark = Math::max(_dot, _mark);
        if (!safe_to_edit(dot, mark)) {
            _viewer->application()->complain(_viewer, "Can't edit mixed text");
            return false;
        } else {
            const char* color;
            if (strlen(command) == 5) {
                color = _viewer->application()->ask(
                    _viewer, "Color name:", ""
                );
            } else {
                color = command + 6;
            }
            if (color == nil) {
                return false;
            } else {
                char style[100];
                sprintf(style, "\\color{%s}", color);
                long old_style = 0;
                long new_style = 0;
                do {
                    if (_text->item_style(dot) != old_style) {
                        old_style = _text->item_style(dot);
                        new_style = document->parse_style(
                            style, old_style
                        );
                    }
                    if (dot < mark && new_style != old_style) {
                        _text->replace(dot, 1, new_style);
                    }
                    ++dot;
                } while (dot < mark);
                _style = document->parse_style(style, _style);
                selection_changed(false);
                return true;
            }
        }
    } else if (strncmp(command, "size", 4) == 0) {
        long dot = Math::min(_dot, _mark);
        long mark = Math::max(_dot, _mark);
        if (!safe_to_edit(dot, mark)) {
            _viewer->application()->complain(_viewer, "Can't edit mixed text");
            return false;
        } else {
            const char* size;
            if (strlen(command) == 4) {
                size = _viewer->application()->ask(
                    _viewer, "Point size:", ""
                );
            } else {
                size = command + 5;
            }
            if (size == nil) {
                return false;
            } else {
                char style[100];
                sprintf(style, "\\size{%s}", size);
                long old_style = 0;
                long new_style = 0;
                do {
                    if (_text->item_style(dot) != old_style) {
                        old_style = _text->item_style(dot);
                        new_style = document->parse_style(
                            style, old_style
                        );
                    }
                    if (dot < mark && new_style != old_style) {
                        _text->replace(dot, 1, new_style);
                    }
                    ++dot;
                } while (dot < mark);
                _style = document->parse_style(style, _style);
                selection_changed(false);
                return true;
            }
        }
    } else if (strncmp(command, "align", 5) == 0) {
        long dot = Math::min(_dot, _mark);
        long mark = Math::max(_dot, _mark);
        if (!safe_to_edit(dot, mark)) {
            _viewer->application()->complain(_viewer, "Can't edit mixed text");
            return false;
        } else {
            const char* align = command + 6;
            if (align == nil) {
                return false;
            } else {
                char style[100];
                sprintf(style, "\\align{%s}", align);
                long old_style = 0;
                long new_style = 0;
                do {
                    if (_text->item_style(dot) != old_style) {
                        old_style = _text->item_style(dot);
                        new_style = document->parse_style(
                            style, old_style
                        );
                    }
                    if (dot < mark && new_style != old_style) {
                        _text->replace(dot, 1, new_style);
                    }
                    ++dot;
                } while (dot < mark);
                _style = document->parse_style(style, _style);
                selection_changed(false);
                return true;
            }
        }
    } else if (strncmp(command, "character", 9) == 0) {
        long c = document->parse_text(command + 10);
        if (c == 0) {
            c = document->parse_text(
                _viewer->application()->ask(
                    _viewer, "Octal character code:", ""
                )
            );
        }
        if (c > 0) {
            insert_text(c);
            selection_changed(true);
            return true;
        } else {
            return false;
        }
    } else if (strncmp(command, "macro", 5) == 0) {
        const char* macro;
        boolean cancelled = false;
        strstream scratch;
        if (strlen(command) == 5) {
            macro = _viewer->application()->ask(
                _viewer, "Macro name:", ""
            );
        } else {
            macro = command + 6;
        }
        if (macro == nil) {
            cancelled = true;
        }
        if (!cancelled) {
            scratch << "\\" << macro << "{";
            long d = Math::min(_dot, _mark);
            long m = Math::max(_dot, _mark);
            if (m > d) {
                document->copy(scratch, _text, d, m-d, _style, _source);
            } else {
                scratch << "<" << macro << ">";
            }
            scratch << "}";
            scratch.seekg(0);
            boolean safe = insert_text(0);
            if (safe) {
                dot(document->paste(scratch, _text, _dot, _style, _source));
                selection_changed(true);
            }
        }
        return !cancelled;
    } else if (strncmp(command, "item", 4) == 0) {
        boolean cancelled = false;
        strstream scratch;
        const char* keyword;
        if (strlen(command) == 4) {
            keyword = _viewer->application()->ask(
                _viewer, "Item:", ""
            );
        } else {
            keyword = command + 5;
        }
        if (keyword == nil) {
            cancelled = true;
        } else if (strcmp(keyword, "verbatim") == 0) {
            const char* file = _viewer->application()->choose(
                _viewer, "Insert text from file:", nil
            );
            if (file == nil) {
                cancelled = true;
            } else {
                scratch << "\\begin{verbatim}\n";
                if (strlen(file) == 0) {
                    scratch << "<verbatim text>\n";
                } else {
                    ifstream in(file);
                    scratch << in.rdbuf();
                }
                scratch << "\\end{verbatim}%\n";
            }
        } else if (strcmp(keyword, "import") == 0) {
            const char* file = _viewer->application()->choose(
                _viewer, "Import text from file:", nil
            );
            if (file == nil) {
                cancelled = true;
            } else {
                scratch << "\\import{" << file << "}";
            }
        } else if (strcmp(keyword, "float") == 0) {
            const char* params = _viewer->application()->ask(
                _viewer, "Float context:", "figure"
            );
            if (params == nil) {
                cancelled = true;
            } else {
                scratch << "\\float{" << params << "}{<Float>}";
            }
        } else if (strcmp(keyword, "tabular") == 0) {
            scratch << "\\begin{tabular}{|l|}\n";
            scratch << "\\hline\n";
            scratch << "<Cell>\\\\\n";
            scratch << "\\hline\n";
            scratch << "\\end{tabular}";
        } else if (strcmp(keyword, "parbox") == 0) {
            const char* params = _viewer->application()->ask(
                _viewer, "Parbox width:", ""
            );
            if (params == nil) {
                cancelled = true;
            } else {
                scratch << "\\parbox{" << params << "}{<Parbox>}";
            }
        } else if (strcmp(keyword, "psfig") == 0) {
            const char* file = _viewer->application()->choose(
                _viewer, "Import graphic from file:", nil
            );
            const char* params = nil;
            if (file != nil) {
                params = _viewer->application()->ask(
                    _viewer, "PSFig parameter list:", ""
                );
            }
            if (file == nil || params == nil) {
                cancelled = true;
            } else {
                scratch << "\\psfig{figure=" << file;
                if (strlen(params) > 0) {
                    scratch << "," << params;
                }
                scratch << "}";
            }
        } else if (strcmp(keyword, "label") == 0) {
            const char* label = _viewer->application()->ask(
                _viewer, "Label tag:", ""
            );
            if (label == nil) {
                cancelled = true;
            } else {
                scratch << "\\label{" << label << "}";
            }
        } else if (strcmp(keyword, "ref") == 0) {
            const char* ref = _viewer->application()->ask(
                _viewer, "Reference to what label tag:", ""
            );
            if (ref == nil) {
                cancelled = true;
            } else {
                scratch << "\\ref{" << ref << "}";
            }
        } else {
            scratch << keyword;
        }
        if (!cancelled) {
            scratch.seekg(0);
            boolean safe = insert_text(0);
            if (safe) {
                dot(document->paste(scratch, _text, _dot, _style, _source));
                selection_changed(true);
            } else {
                cancelled = true;
            }
        }
        return !cancelled;
    } else if (strncmp(command, "clip", 4) == 0) {
        long d = Math::min(_dot, _mark);
        long m = Math::max(_dot, _mark);
        if (!safe_to_edit(d, m)) {
            _viewer->application()->complain(_viewer, "Can't edit mixed text");
            return false;
        } else {
            const char* keyword = command + 5;
            _viewer->highlight(keyword, true);
            if (strcmp(keyword, "cut") == 0 || strcmp(keyword, "copy") == 0) {
                ofstream clip(".clipboard");
                if (clip) {
                    document->copy(clip, _text, d, m-d, _style, _source);
                }
            }
            if (strcmp(keyword, "cut") == 0 || strcmp(keyword, "paste") == 0) {
                insert_text(0);
            }
            if (strcmp(keyword, "paste") == 0) {
                ifstream clip(".clipboard");
                if (clip) {
                    dot(document->paste(clip, _text, _dot, _style, _source));
                }
            }
            selection_changed(true);
            _viewer->highlight(keyword, false);
            return true;
        }
    } else {
        return ItemView::command(command);
    }
}

void TextView::activate (boolean active) {
    if (_active != active) {
        if (active) {
            World* w = World::current();
            if (_menubar == nil) {
                const char* menubar = w->property_value(
                    DEFAULT_TEXT_MENUBAR
                );
                _menubar = strcpy(new char[strlen(menubar)+1], menubar);
            }
            if (_encoded_keymap == nil) {
                const char* keymap = w->property_value(
                    DEFAULT_ENCODED_KEYMAP
                );
                delete _encoded_keymap;
                _encoded_keymap = strcpy(new char[strlen(keymap)+1], keymap);
            }
            if (_verbatim_keymap == nil) {
                const char* keymap = w->property_value(
                    DEFAULT_VERBATIM_KEYMAP
                );
                delete _verbatim_keymap;
                _verbatim_keymap = strcpy(new char[strlen(keymap)+1], keymap);
            }
            w->flush();
            _viewer->menubar(_menubar);
            w->flush();
            _source = -1;
            _style = -1;
        }
        _active = active;
        mark_selection();
        selection_changed(true);
    }
}

boolean TextView::handle_char (long c) {
    if (!iscntrl(char(c & 0x7f))) {
        return insert_text(c);
    } else {
        return false; // undefined mapping;
    }
}

long TextView::index (Coord, Coord) {
    return -1;
}

void TextView::selection_changed (boolean compute_style) {
    Document* document = _text->document();
    long new_style = _style;
    long new_source = _source;
    if (compute_style) {
        long pre_source = _text->item_source(_dot - 1);
        long post_source = _text->item_source(_dot);
        if (pre_source == post_source) {
            new_source = pre_source;
            long pre_code = _text->item_code(_dot - 1);
            long post_code = _text->item_code(_dot);
            if (interword_code(pre_code) && !interword_code(post_code)) {
                new_style = _text->item_style(_dot - 1);
            } else {
                new_style = _text->item_style(_dot);
            }
        } else if (
            _source == -1 ||
            document->common_source(pre_source, _source) != _source
            && document->common_source(post_source, _source) != _source
        ) {
            new_source = document->common_source(pre_source, post_source);
            new_style = _text->style();
        }
    }
    if (_mark != _dot || new_source != _source || new_style != _style) {
        _viewer->choose(nil, false);
        TextSource& source = document->text_source(new_source);
        if (strcmp(source._source, "document") == 0) {
            _viewer->keymap(_encoded_keymap);
        } else if (strcmp(source._source, "verbatim") == 0) {
            _viewer->keymap(_verbatim_keymap);
        } else if (strcmp(source._source, "import") == 0) {
        } else if (strcmp(source._source, "macro") == 0) {
            _viewer->choose("macro", true);
            _viewer->choose(source._name, true);
        } else if (strcmp(source._source, "parameter") == 0) {
            _viewer->keymap(_encoded_keymap);
        } else if (strcmp(source._source, "styled") == 0) {
            _viewer->choose(source._name, true);
            _viewer->keymap(_encoded_keymap);
        }
        if (_mark == _dot) {
            if (new_style != _style) {
                TextStyle& style = document->text_style(new_style);
                _viewer->choose(style._font, true);
                _viewer->choose(style._size, true);
                _viewer->choose(style._color, true);
                _viewer->choose(style._alignment, true);
            }
        } else {
            long dot = Math::min(_dot, _mark);
            long mark = Math::max(_dot, _mark);
            long style = 0;
            for (long i = dot; i < mark; ++i) {
                if (_text->item_style(i) != style) {
                    style = _text->item_style(i);
                    TextStyle& info = document->text_style(style);
                    _viewer->choose(info._font, true);
                    _viewer->choose(info._size, true);
                    _viewer->choose(info._color, true);
                    _viewer->choose(info._alignment, true);
                }
            }
        }
    }
    _source = new_source;
    _style = new_style;
}

void TextView::keystroke (Event& e) {
    char s[1];
    if (e.mapkey(s, sizeof(s)) > 0) {
        boolean pending_repair = handle_char(s[0]);
        if (pending_repair && !e.pending()) {
            repair();
        }
    }
}

void TextView::select (Event& e) {
    if (!e.shift_is_down()) {
        dot(index(e.pointer_x(), e.pointer_y()));
    }
    do {
        if (!e.pending()) {
            mark(index(e.pointer_x(), e.pointer_y()));
            mark_selection();
        }
        e.read();
    } while (e.type() != Event::up);
    selection_changed(true);
}
