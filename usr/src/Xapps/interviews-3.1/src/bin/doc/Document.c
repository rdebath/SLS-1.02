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
 * Document
 */

#include "Document.h"

#include "Application.h"
#include "DocViewer.h"

#include "CounterItem.h"
#include "LabelItem.h"
#include "RefItem.h"
#include "PSFigItem.h"
#include "TabularItem.h"
#include "TextItem.h"
#include "FloatItem.h"
#include "PagenoItem.h"
#include "Leader.h"

#include "codes.h"
#include "properties.h"

#include <InterViews/character.h>
#include <InterViews/color.h>
#include <InterViews/compositor.h>
#include <InterViews/font.h>
#include <InterViews/layout.h>
#include <InterViews/psfont.h>
#include <InterViews/rule.h>
#include <IV-2_6/InterViews/world.h>

#include <OS/list.h>
#include <OS/string.h>
#include <OS/ustring.h>

#include <fstream.h>
#include <strstream.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

const int END = -2;
const int LINE = -3;
const int FIELD = -4;
const int COMMAND = -5;
const int BLOCK = -6;
const int COMMENT = -7;
const int VERBATIM = -8;
const int IMPORT = -9;
const int MACRO = -10;
const int PARAMETER = -11;
const int PARBOX = -12;
const int FLOAT = -13;
const int TABULAR = -14;
const int PSFIG = -15;
const int COUNTER = -16;
const int LABEL = -17;
const int REF = -18;
const int COLOR = -19;
const int FONT = -20;
const int SIZE = -21;
const int ALIGN = -22;
const int PAGENUMBER = -23;

int FOLDCOLUMN = 65;
static int column = 0;
static istream* parameter = nil;
static FloatItem* expecting_position = nil;

class CounterFormat {
public:
    const char* _name;
    void (*_formatter)(long, char*);
};

void arabic_formatter (long value, char* buffer) {
    sprintf(buffer, "%d", value);
}

void alph_formatter (long value, char* buffer) {
    sprintf(buffer, "%c", 'a' + value - 1);
}

void Alph_formatter (long value, char* buffer) {
    sprintf(buffer, "%c", 'A' + value - 1);
}

static const char* roman_hundreds[] = {
    "", "c", "cc", "ccc", "cd", "d", "dc", "dcc", "dccc", "cm"
};

static const char* roman_tens[] = {
    "", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "xc"
};

static const char* roman_units[] = {
    "", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"
};

void roman_formatter (long value, char* buffer) {
    sprintf(
        buffer, "%s%s%s",
        roman_hundreds[(value / 100) % 10],
        roman_tens[(value / 10) % 10],
        roman_units[value % 10]
    );
}

static const char* Roman_hundreds[] = {
    "", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"
};

static const char* Roman_tens[] = {
    "", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"
};

static const char* Roman_units[] = {
    "", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"
};

void Roman_formatter (long value, char* buffer) {
    sprintf(
        buffer, "%s%s%s",
        Roman_hundreds[(value / 100) % 10],
        Roman_tens[(value / 10) % 10],
        Roman_units[value % 10]
    );
}

CounterFormat counter_format [] = {
    {"arabic", &arabic_formatter },
    {"alph", &alph_formatter },
    {"Alph", &Alph_formatter },
    {"roman", &roman_formatter },
    {"Roman", &Roman_formatter },
    {"", &arabic_formatter },
    {nil, nil }
};

static long parse_octal_trigraph (const char* buffer) {
    long i = 0;
    for (int d = 0; d < 3; ++d) {
        i = i * 8 + (buffer[d] - '0');
    }
    return i;
}
static void unparse_octal_trigraph (char* buffer, long code) {
    long i = code;
    for (int d = 2; d >= 0; --d) {
        buffer[d] = char(i % 8) + '0';
        i = i / 8;
    }
}
class MetricUnit {
public:
    const char* _name;
    float _conversion;
    UniqueString* _uname;
};

MetricUnit metric_unit[] = {
    {"pt", 1.0 },
    {"pts", 1.0 },
    {"point", 1.0 },
    {"points", 1.0 },
    {"in", 72.0 },
    {"ins", 72.0 },
    {"inch", 72.0 },
    {"inches", 72.0 },
    {"cm", 72.0 / 2.54 },
    {"mm", 72.0 / 25.4 },
    {"pc", 72.0 / 12 },
    {"pica", 72.0 / 12 },
    {nil, 0 }
};

class DefaultParameterInfo {
public:
    const char* _name;
    const char* _value;
};

DefaultParameterInfo default_parameter[] = {
    {"face", "Times-Roman"},
    {"pointsize", "10pt"},
    {"color", "black"},
    {"alignment", "justify"},

    {"textwidth", "6in"},
    {"textheight", "8.5in"},
    {"leftsidemargin", "0.25in"},
    {"rightsidemargin", "0.25in"},
    {"bottommargin", "0.25in"},
    {"topmargin", "0.25in"},

    {"baselineskip", "0.1"},
    {"baselinestretch", "0"},
    {"parskip", "0.2"},
    {"parstretch", "0.5"},
    {"bigskip", "2.0"},
    {"bigstretch", "2.0"},
    {"medskip", "1.0"},
    {"medstretch", "1.0"},
    {"smallskip", "0.5"},
    {"smallstretch", "0.5"},
    {"gutter", "0.25in"},
    {"columns", "1"},

    {"hyphenpenalty", "50"},
    {"parbreakpenalty", "-500"},
    {"skippenalty", "-1000"},
    {"linepenalty", "10"},
    {"pagepenalty", "100"},

    {"floatrepelright", "0.1in"},
    {"floatrepelleft", "0.1in"},
    {"floatrepeltop", "0.15in"},
    {"floatrepelbottom", "0.15in"},

    {"cellmargin", "4pt" },
    {"tabularmargin", "3pt" },
    {"tabularseparatorthickness", "1pt" },

    {"rulethickness", "1pt" },

    {"pagenumberformat", "arabic"},

    {nil, nil}
};

class DefaultMacroInfo {
public:
    const char* _name;
    const char* _def;
};

DefaultMacroInfo default_macro[] = {
    {"verbatim", "\\font{Courier}\\align{left}"},
    {"figure", "\\align{center}\\float{figure}{#}"},
    {"figure*", "\\align{center}\\float{figure}{#}"},
    {"table", "\\align{center}\\float{table}{#}"},
    {"table*", "\\align{center}\\float{table}{#}"},

    {nil, nil}
};

const char* quoted_characters = " $&%#_{}~";

class TextParseInfo {
public:
    const char* _format;
    long _code;
    long _fold;
};

TextParseInfo text_map[] = {
    {"hfil", hfil, -1},
    {"hfill", hfill, -1},
    {"hleader", hleader, 0},
    {"vfil", vfil, -1},
    {"hrule", hrule, -1},
    {"smallskip", smallskip, -1},
    {"medskip", medskip, -1},
    {"bigskip", bigskip, -1},
    {"smallvspace", smallvspace, -1},
    {"medvspace", medvspace, -1},
    {"bigvspace", bigvspace, -1},
    {"newpage", newpage, -1},
    {"pagebreak", pagebreak, -1},
    {"parbreak", parbreak, -1},
    {"linebreak", linebreak, -1},
    {"nobreakspace", nobreakspace, 1},
    {"wordspace", wordspace, 0},
    {"sentencespace", sentencespace, 1},
    {"quad", quadspace, 0},
    {"discretionaryhyphen", discretionaryhyphen, 1},
    {"visiblehyphen", visiblehyphen, 1},
    {"anchor", anchor, 1},
    {"thinspace", thinspace, 0},
    {"negthinspace", negthinspace, 0},
    {"S", section, 0},
    {"section", section, 0},
    {"P", paragraph, 0},
    {"paragraph", paragraph, 0},
    {"endash", endash, 0},
    {"emdash", emdash, 0},
    {"bullet", bullet, 0},
    {"dag", dag, 0},
    {"ddag", ddag, 0},
    {"cdot", cdot, 0},
    {"backslash", '\\', 0},
    {nil, 0}
};

TextParseInfo special_map[] = {
    {"-", visiblehyphen, 1},
    {"--", endash, 1},
    {"---", emdash, 1},
    {"\\-", discretionaryhyphen, 1},
    {"~", nobreakspace, 1},
    {"  ", sentencespace, 1},
    {"\n", parbreak, -1},
    {"\\\\", linebreak, -1},
    {"\\,", thinspace, 1},
    {"\\!", negthinspace, 1},
    {"", wordspace, 0},
    {nil, 0}
};

class StyleInfo {
public:
    TextStyle _style;

    const Font* _font;
    const Color* _color;
    Glyph* _begin_line_strut;
    Glyph* _end_line_strut;
    Glyph* _begin_par_strut;
    Glyph* _end_par_strut;
    Glyph* _line_strut;
    Glyph* _fil_strut;
    Glyph* _hyphen;
    Glyph* _interline_glue;
    Glyph* _interpar_glue;
    Glyph* _hfil_glue;
    Glyph* _hfill_glue;
    Glyph* _hleader;
    Glyph* _vfil_glue;
    Glyph* _bigskip_glue;
    Glyph* _medskip_glue;
    Glyph* _smallskip_glue;
    Glyph* _word_space;
    Glyph* _sentence_space;
    Glyph* _glyph[256];
};    

class SourceInfo {
public:
    TextSource _source;

    long _enclosing;
    long _style;
};

class FloatInfo {
public:
    Item* _item;
    float _x;
    float _y;
    long _page;
};

class LabelInfo {
public:
    char* _name;
    char* _text;
    boolean _save;
};

class MacroInfo {
public:
    char* _name;
    char* _def;
    boolean _save;
};

class CounterInfo {
public:
    char* _name;
    char* _within;
    char* _format;
    long _initial;
    long _value;
    boolean _save;
};

class DocumentParameterInfo {
public:
    char* _name;
    char* _value;
    boolean _save;
};

class DocumentViewerInfo {
public:
    DocumentViewer* _viewer;
};

class FontFamilyInfo {
public:
    char* _name;
    FontFamily* _family;
};

declareList(StyleInfo_List,StyleInfo)
implementList(StyleInfo_List,StyleInfo)

declareList(SourceInfo_List,SourceInfo)
implementList(SourceInfo_List,SourceInfo)

declareList(MacroInfo_List,MacroInfo)
implementList(MacroInfo_List,MacroInfo)

declareList(FloatInfo_List,FloatInfo)
implementList(FloatInfo_List,FloatInfo)

declareList(CounterInfo_List,CounterInfo)
implementList(CounterInfo_List,CounterInfo)

declareList(LabelInfo_List,LabelInfo)
implementList(LabelInfo_List,LabelInfo)

declareList(DocumentParameterInfo_List,DocumentParameterInfo)
implementList(DocumentParameterInfo_List,DocumentParameterInfo)

declareList(DocumentViewerInfo_List,DocumentViewerInfo)
implementList(DocumentViewerInfo_List,DocumentViewerInfo)

declareList(FontFamilyInfo_List,FontFamilyInfo)
implementList(FontFamilyInfo_List,FontFamilyInfo)

FontFamily* find_font_family (const char* name) {
    static FontFamilyInfo_List* _families;
    if (_families == nil) {
        _families = new FontFamilyInfo_List();
    }
    if (strlen(name) == 0) {
        return nil;
    } else {
        long count = _families->count();
        for (long i = 0; i < count; ++i) {
            FontFamilyInfo& info = _families->item_ref(0);
            if (strcmp(info._name, name) == 0) {
                break;
            }
        }
        if (i == count) {
            FontFamilyInfo info;
            info._name = strcpy(new char[strlen(name) + 1], name);
            info._family = new FontFamily(name);
            _families->append(info);
        }
        return _families->item_ref(i)._family;
    }
}

Document::Document (Application* application, int size_hint) {
    _application = application;
    _touched = false;
    _name = nil;
    _relabel = false;
    _size_hint = size_hint;
    _body = nil;
    _style_name = nil;

    _style = new StyleInfo_List();
    _source = new SourceInfo_List();
    _macro = new MacroInfo_List();
    _counter = new CounterInfo_List();
    _label = new LabelInfo_List();
    _float = new FloatInfo_List();
    _parameter = new DocumentParameterInfo_List(20);
    _viewer = new DocumentViewerInfo_List();

    long i = 0;
    while (default_parameter[i]._name != nil) {
        DefaultParameterInfo& info = default_parameter[i];
        define_parameter(find_parameter(info._name), info._value, false);
        ++i;
    };
    i = 0;
    while (default_macro[i]._name != nil) {
        DefaultMacroInfo& info = default_macro[i];
        define_macro(find_macro(info._name), info._def, false);
        ++i;
    };

    find_style(-1, "", "", "10", "");
    find_source(-1, 1, "document", "", true, true);
}

Document::~Document () {
    delete _name;
    if (_body != nil) {
        _body->unref();
        _body = nil;
    }
    while (_float->count() > 0) {
        cleanup_float(0);
        _float->remove(0);
    }
    delete _float;
    while (_style->count() > 0) {
        cleanup_style(0);
        _style->remove(0);
    }
    delete _style;
    while (_source->count() > 0) {
        cleanup_source(0);
        _source->remove(0);
    }
    delete _source;
    while (_macro->count() > 0) {
        cleanup_macro(0);
        _macro->remove(0);
    }
    delete _macro;
    while (_counter->count() > 0) {
        cleanup_counter(0);
        _counter->remove(0);
    }
    delete _counter;
    while (_label->count() > 0) {
        cleanup_label(0);
        _label->remove(0);
    }
    delete _label;
    while (_parameter->count() > 0) {
        cleanup_parameter(0);
        _parameter->remove(0);
    }
    delete _parameter;
    if (_style_name != nil) {
        delete _style_name;
    }
}

void Document::touch (boolean touched) {
    _touched = touched;
}

boolean Document::touched () {
    return _touched;
}

const char* Document::name () {
    return _name;
}

void Document::name (const char* name) {
    if (name != _name) {
	delete _name;
	if (name != nil) {
	    _name = strcpy(new char[strlen(name)+1], name);
	} else {
	    _name = nil;
	}
    }
}

TextItem* Document::body () {
    return _body;
}

void Document::insert_float (Item* item) {
    FloatInfo info;
    info._x = 0;
    info._y = 0;
    info._page = -3;
    touch(true);
    long count = _viewer->count();
    for (long i = 0; i < count; ++i) {
        DocumentViewerInfo& vinfo = _viewer->item_ref(i);
        vinfo._viewer->float_inserted(item);
    }
}

void Document::remove_float (Item* item) {
    touch(true);
    long count = _viewer->count();
    for (long i = 0; i < count; ++i) {
        DocumentViewerInfo& info = _viewer->item_ref(i);
        info._viewer->float_removed(item);
    }
    i = find_float(item);
    cleanup_float(i);
    _float->remove(i);
}

void Document::change_float (Item* item ) {
    FloatInfo& info = _float->item_ref(find_float(item));
    touch(true);
    long count = _viewer->count();
    for (long i = 0; i < count; ++i) {
        DocumentViewerInfo& viewer = _viewer->item_ref(i);
        viewer._viewer->float_changed(item);
    }
}

void Document::adjust_float (Item* item, float x, float y, long page) {
    FloatInfo& info = _float->item_ref(find_float(item));
    info._x = x;
    info._y = y;
    info._page = page;
    touch(true);
    long count = _viewer->count();
    for (long i = 0; i < count; ++i) {
        DocumentViewerInfo& info = _viewer->item_ref(i);
        info._viewer->float_adjusted(item, x, y, page);
    }
}

const char* Document::document_parameter (const char* name) {
    DocumentParameterInfo& info = _parameter->item_ref(find_parameter(name));
    const char* value = info._value;
    if (value != nil) {
        while (*value == ' ' || *value == '\t') ++value;
    }
    return value;
}

float Document::document_metric (const char* name) {
    DocumentParameterInfo& info = _parameter->item_ref(find_parameter(name));
    return convert_metric(info._value);
}

float Document::convert_metric (const char* value) {
    if (value == nil || value[0] == '\0') {
        return 0;
    }

    float f;
    float pts = 1.0;
    String v(value);
    String units(v);
    const char* p = v.string();
    const char* end = p + v.length();
    for (; isspace(*p); p++);
    if (*p == '-' || *p == '+') {
	++p;
    }
    boolean dot = false;
    for (; p < end; p++) {
	if (!dot && *p == '.') {
	    dot = true;
	} else if (!isdigit(*p)) {
	    int i = p - v.string();
	    units.set_to_right(i);
	    v.set_to_left(i);
	    if (!v.convert(f)) {
		return 0;
	    }
	    UniqueString u_units(units);
	    for (MetricUnit* m = &metric_unit[0]; m->_name != nil; m++) {
		if (m->_uname == nil) {
		    m->_uname = new UniqueString(m->_name);
		}
		if (u_units == *m->_uname) {
		    return f * m->_conversion;
		}
	    }
	    return 0;
	}
    }
    if (!v.convert(f)) {
	return 0;
    }
    return f;
}

TextStyle& Document::text_style (long index) {
    return _style->item_ref(index)._style;
}

TextSource& Document::text_source (long index) {
    return _source->item_ref(index)._source;
}

long Document::common_source (long s1, long s2) {
    while (
        _source->item_ref(s1)._source._depth
        > _source->item_ref(s2)._source._depth
    ) {
        s1 = _source->item_ref(s1)._enclosing;
    }
    while (
        _source->item_ref(s2)._source._depth
        > _source->item_ref(s1)._source._depth
    ) {
        s2 = _source->item_ref(s2)._enclosing;
    }
    while (s1 != s2) {
        s1 = _source->item_ref(s1)._enclosing;
        s2 = _source->item_ref(s2)._enclosing;
    }
    return s1;
}

Glyph* Document::character (long code, long style) {
    StyleInfo& info = _style->item_ref(style);
    const LayoutKit& layout = *LayoutKit::instance();
    if (info._glyph[code] == nil) {
        switch (code) {
        case '\0':
            info._glyph[code] = info._line_strut;
            break;
        case '\n':
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._end_par_strut,
                info._end_par_strut,
                layout.discretionary(
                    0, nil, info._vfil_glue, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case hfil:
            info._glyph[code] = info._hfil_glue;
            break;
        case hfill:
            info._glyph[code] = info._hfill_glue;
            break;
        case hleader:
            info._glyph[code] = info._hleader;
            break;
        case visiblehyphen:
            info._glyph[code] = layout.discretionary(
                int(document_metric("hyphenpenalty")), info._hyphen,
                info._hyphen,
                info._interline_glue,
                info._begin_line_strut
            );
            break;
        case discretionaryhyphen:
            info._glyph[code] = layout.discretionary(
                int(document_metric("hyphenpenalty")), info._line_strut,
                info._hyphen,
                info._interline_glue,
                info._begin_line_strut
            );
            break;
        case hrule:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, nil,
                nil,
		new HRule(info._color, document_metric("rulethickness")),
		nil
            );
            break;
        case parbreak:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._fil_strut,
                info._end_par_strut,
                layout.discretionary(
                    int(document_metric("parbreakpenalty")),
                    info._interpar_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case linebreak:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._line_strut,
                info._end_line_strut,
                info._interline_glue,
                info._begin_line_strut
            );
            break;
        case bigvspace:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, nil,
                nil, info._bigskip_glue, nil
            );
            break;
        case bigskip:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._fil_strut,
                info._end_par_strut,
                layout.discretionary(
                    int(document_metric("skippenalty")),
                    info._bigskip_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case medvspace:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, nil,
                nil, info._medskip_glue, nil
            );
            break;
        case medskip:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._fil_strut,
                info._end_par_strut,
                layout.discretionary(
                    int(document_metric("skippenalty")),
                    info._medskip_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case smallvspace:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, nil,
                nil, info._smallskip_glue, nil
            );
            break;
        case smallskip:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._fil_strut,
                info._end_par_strut,
                layout.discretionary(
                    int(document_metric("skippenalty")),
                    info._smallskip_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case vfil:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._line_strut,
                info._end_par_strut,
                info._vfil_glue,
                info._begin_par_strut
            );
            break;
        case newpage:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._fil_strut,
                info._end_par_strut,
                layout.discretionary(
                    PenaltyGood, info._vfil_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case pagebreak:
            info._glyph[code] = layout.discretionary(
                PenaltyGood, info._fil_strut,
                info._end_par_strut,
                layout.discretionary(
                    PenaltyGood, nil,
                    nil, nil, nil
                ),
                info._begin_par_strut
            );
            break;
        case wordspace:
            info._glyph[code] = layout.discretionary(
                0, info._word_space,
                info._end_line_strut,
                layout.discretionary(
                    0, info._interline_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_line_strut
            );
            break;
        case sentencespace:
            info._glyph[code] = layout.discretionary(
                0, info._sentence_space,
                info._end_line_strut,
                layout.discretionary(
                    0, info._interline_glue,
                    info._vfil_glue, nil, nil
                ),
                info._begin_line_strut
            );
            break;
        case nobreakspace:
            info._glyph[code] = info._word_space;
            break;
        case quadspace:
            info._glyph[code] = layout.shape_of(
                new Character('M', info._font, info._color)
            );
            break;
        case thinspace:
            info._glyph[code] = layout.hspace(1);
            break;
        case negthinspace:
            info._glyph[code] = layout.hspace(-1);
            break;
        default:
            info._glyph[code] = new Character(code, info._font, info._color);
            break;
        }
        info._glyph[code]->ref();
    }
    return info._glyph[code];
}

long Document::paste (
    istream& in, TextItem* text, long index, long style, long source
) {
    expecting_position = nil;
    index = read_encoded_text(in, text, index, style, source, END);
    if (expecting_position != nil) {
        Coord x = document_metric("textwidth") / 2;
        Coord y = - (document_metric("textheight") / 2);
        long page = -1;
        adjust_float(expecting_position->item(), x, y, page);
        expecting_position = nil;
    }
    return index;
}

void Document::copy (
    ostream& out, TextItem* text,
    long index, long count, long style, long source
) {
    column = 0;
    write_encoded_text(out, text, index, count, style, source);
}

void Document::read (istream& in) {
    static boolean reading_style = false;
    World* world = World::current();
    const char* style_path = world->property_value(STYLE_PATH);
    const char* style_ext = world->property_value(STYLE_FILE_EXTENSION);
    char line[1000];
    while (in.getline(line, 1000)) {
        if (strchr(line, '\n') != nil) {
            *strchr(line, '\n') = '\0';
        }
        if (strcmp(line, "") == 0) {
            ;
        } else if (strncmp(line, "\\begin{document}", 16) == 0) {
            break;
        } else if (strncmp(line, "\\documentstyle", 14) == 0) {
            char* beginname = strchr(line, '{') + 1;
            char* endname = strchr(beginname, '}');
            int namelen = endname - beginname;
            char name[256];
            strncpy(name, beginname, namelen);
            name[namelen] = '\0';
            if (!reading_style) {
                _style_name = strcpy(new char[namelen + 1], name);
            }
            char path[256];
            if (_application->file_path(name, style_ext, style_path, path)) {
                ifstream style(path);
                if (!reading_style) {
                    reading_style = true;
                    read(style);
                    reading_style = false;
                } else {
                    read(style);
                }
            } else {
                fprintf(stderr, "Can't find style %s\n", name);
            }
        } else if (strncmp(line, "%macro", 6) == 0) {
            char name[100], def[1000];
            char* beginname = strchr(line, '{') + 1;
            char* endname = strchr(beginname, '}');
            strcpy(name, "");
            strncat(name, beginname, endname - beginname);
            char* begindef = strchr(endname, '{') + 1;
            char* enddef = strrchr(line, '}');
            strcpy(def, "");
            strncat(def, begindef, enddef - begindef);
            define_macro(find_macro(name), def, !reading_style);
        } else if (strncmp(line, "%counter", 8) == 0) {
            char name[100], within[100], format[100];
            int initial;
            char* p = line;
            char* beginname = strchr(p, '{') + 1;
            char* endname = strchr(beginname, '}');
            strcpy(name, "");
            strncat(name, beginname, endname - beginname);
            p = endname + 1;
            if (strchr(p, '{') != nil) {
                char* beginwithin = strchr(p, '{') + 1;
                char* endwithin = strchr(beginwithin, '}');
                p = endwithin + 1;
                strcpy(within, "");
                strncat(within, beginwithin, endwithin - beginwithin);
            } else {
                strcpy(within, "");
            }
            if (strchr(p, '[') != nil) {
                char* beginformat = strchr(p, '[') + 1;
                char* endformat = strchr(beginformat, ']');
                p = endformat + 1;
                strcpy(format, "");
                strncat(format, beginformat, endformat - beginformat);
            }  else {
                strcpy(format, "");
            }
            if (strchr(p, '=') != nil) {
                sscanf(strchr(p, '=') + 1, "%d", &initial);
            } else {
                initial = 0;
            }
            define_counter(
                find_counter(name), within, format, initial, !reading_style
            );
        } else if (strncmp(line, "%label", 6) == 0) {
            char name[100], text[100];
            char* beginname = strchr(line, '{') + 1;
            char* endname = strchr(beginname, '}');
            strcpy(name, "");
            strncat(name, beginname, endname - beginname);
            if (strchr(endname, '{') != nil) {
                char* begintext = strchr(endname, '{') + 1;
                char* endtext = strrchr(line, '}');
                strcpy(text, "");
                strncat(text, begintext, endtext - begintext);
            } else {
                strcpy(text, "");
            }
            define_label(find_label(name), text, !reading_style);
        } else if (strchr(line, '%') != nil) {
            char name[100];
            char* beginname = strchr(line, '%') + 1;
            char* endname = strchr(beginname, ' ');
	    if (endname == nil) {
		endname = strchr(beginname, '\0');
	    }
            strcpy(name, "");
            strncat(name, beginname, endname - beginname);
            define_parameter(find_parameter(name), endname, !reading_style);
        }
    }
    if (!reading_style) {
        float format_width = document_metric("formatwidth");
        if (format_width == 0) {
            long column_count = long(document_metric("columns"));
            format_width = (
                (document_metric("textwidth")
                 - (column_count - 1) * document_metric("gutter"))
                / column_count
            );
            char buf[100];
            sprintf(buf, "%gpt", format_width);
            define_parameter(find_parameter("formatwidth"), buf, false);
        }
        find_style(
            0,
            document_parameter("face"), document_parameter("color"),
            document_parameter("pointsize"), document_parameter("alignment")
        );
        if (_body == nil) {
            _body = new TextItem(
                this, nil, 1, 0, document_parameter("formatwidth"),
                _size_hint
            );
            _body->ref();
        }
        expecting_position = nil;
        read_encoded_text(in, _body, 0, 1, 0, END);
        if (expecting_position != nil) {
            Coord x = document_metric("textwidth") / 2;
            Coord y = - (document_metric("textheight") / 2);
            long page = -1;
            adjust_float(expecting_position->item(), x, y, page);
            expecting_position = nil;
        }
        while (in.getline(line, 256)) {
            if (strcmp(line, "\\end{document}\n") == 0) {
                break;
            }
        }
        notify();
    }
}

void Document::write (ostream& out) {
    out << "\\documentstyle{" << _style_name << "}\n\n";

    long count, i;
    count = _parameter->count();
    for (i = 0; i < count; ++i) {
        DocumentParameterInfo& info = _parameter->item_ref(i);
        if (info._save && info._value != nil) {
            out << "%" << info._name << info._value << "\n";
        }
    }
    count = _macro->count();
    for (i = 0; i < count; ++i) {
        MacroInfo& info = _macro->item_ref(i);
        if (info._save && info._def != nil) {
            out << "%macro{" << info._name << "}";
            out << "{" << info._def << "}\n";
        }
    }
    count = _counter->count();
    for (i = 0; i < count; ++i) {
        CounterInfo& info = _counter->item_ref(i);
        if (info._save) {
            out << "%counter{" << info._name << "}";
            if (info._within != nil && strlen(info._within) > 0) {
                out << "{" << info._within << "}";
            }
            if (info._format != nil && strlen(info._format) > 0) {
                out << "[" << info._format << "]";
            }
            if (info._initial != 0) {
                out << " = " << info._initial;
            }
            out << "\n";
        }
    }
    count = _label->count();
    for (i = 0; i < count; ++i) {
        LabelInfo& info = _label->item_ref(i);
        if (info._save) {
            out << "%label{" << info._name << "}";
            out << "{" << info._text << "}\n";
        }
    }
    column = 0;
    out << "\n\\begin{document}\n";
    write_encoded_text(out, _body, 0, _body->item_count(), 1, 0);
    out << "%\n\\end{document}\n";
}

void Document::read (istream& in, TextItem* text, long style, long source) {
    read_encoded_text(in, text, 0, style, source, END);
}

void Document::write (ostream& out, TextItem* text, long style, long source) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\parbox{" << text->parameters() << "}{";
    }
    write_encoded_text(out, text, 0, text->item_count(), style, source);
    if (info._source._editable) {
        out << "}";
    };
}

void Document::read (
    istream& in, TabularItem* tabular, long style, long source
) {
    int c;
    RowSeparator rsep = RowSeparatorOff;
    ColumnSeparator csep = ColumnSeparatorOff;
    if ((c = in.get()) == '{') {
        int column = 0;
        while ((c = in.get()) != '}') {
            if (c == 'l') {
                tabular->insert_column(column, "", ColumnAlignLeft);
		tabular->change_column_separator(column, csep);
                ++column;
		csep = ColumnSeparatorOff;
            } else if (c == 'c') {
                tabular->insert_column(column, "", ColumnAlignCenter);
		tabular->change_column_separator(column, csep);
                ++column;
		csep = ColumnSeparatorOff;
            } else if (c == 'r') {
                tabular->insert_column(column, "", ColumnAlignRight);
		tabular->change_column_separator(column, csep);
                ++column;
		csep = ColumnSeparatorOff;
            } else if (c == '|') {
		csep = ColumnSeparatorSingle;
            }
        }
        while ((c = in.get()) == '\n') { }
        in.putback(c);
	tabular->change_column_separator(column, csep);
    } else {
        tabular->change_row_separator(0, RowSeparatorSingle);
        tabular->insert_column(0, "", ColumnAlignLeft);
        tabular->change_column_separator(0, ColumnSeparatorSingle);
        rsep = RowSeparatorSingle;
    }
    int column = 0;
    int row = 0;
    char buffer [1000];
    while (true) {
        streampos tell = in.tellg();
        c = token(in, buffer);
        if (c == EOF) {
            break;
        } else if (c == END) {
            in.seekg(tell);
            break;
        } else if (c == COMMENT) {
            ;
        } else if (c == LINE) {
            rsep = RowSeparatorOff;
            column = 0;
            row += 1;
        } else if (c == FIELD) {
            column += 1;
        } else if (c == COMMAND && strcmp(buffer, "hline") == 0) {
            rsep = RowSeparatorSingle;
        } else {
            if (row >= tabular->row_count()) {
                tabular->insert_row(row, "");
                tabular->change_row_separator(row, rsep);
            }
            TextItem* text = tabular->cell(row, column);
            in.seekg(tell);
            if (column < tabular->column_count() - 1) {
                read_encoded_text(in, text, 0, style, source, FIELD);
            } else {
                read_encoded_text(in, text, 0, style, source, LINE);
            }
        }
    }
    tabular->change_row_separator(row, rsep);
}

void Document::write (
    ostream& out, TabularItem* tabular, long style, long source
) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\begin{tabular}";
        long i;
        long column_count = tabular->column_count();
        out << "{";
        for (i = 0; i < column_count + 1; ++i) {
            if (tabular->column_separator(i) == ColumnSeparatorSingle) {
                out << "|";
            }
            if (i < column_count) {
                ColumnAlignment a = tabular->column_alignment(i);
                if (a == ColumnAlignLeft) {
                    out << "l";
                } else if (a == ColumnAlignCenter) {
                    out << "c";
                } else if (a == ColumnAlignRight) {
                    out << "r";
                }
            }
        }
        out << "}\n";
        long row_count = tabular->row_count();
        for (i = 0; i < row_count + 1; ++i) {
            if (tabular->row_separator(i) == RowSeparatorSingle) {
                out << "\\hline\n";
            }
            if (i < row_count) {
                for (long j = 0; j < column_count; ++j) {
                    TextItem* text = tabular->cell(i, j);
                    write_encoded_text(
                        out, text, 0, text->item_count(), style, source
                    );
                    if (j < column_count - 1) {
                        out << "&\n";
                    } else {
                        out << "\\\\\n";
                    }
                }
            }
        }
        out << "\\end{tabular}%\n";
        column = 0;
    };
}

void Document::read (istream& in, PSFigItem* psfig, long, long) {
    char params[256];
    in.get(params, 256, '}');
    psfig->parameters(params);
}

void Document::write (ostream& out, PSFigItem* psfig, long, long source) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        if (column > 0) {
            out << "%\n";
        }
        out << "\\psfig{";
        out << psfig->parameters();
        out << "}%\n";
        column = 0;
    }
}

void Document::read (istream& in, CounterItem* counter, long, long) {
    char name[100];
    in.get(name, 100, '}');
    counter->name(name);
}

void Document::write (
    ostream& out, CounterItem* counter, long, long source
) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\counter{";
        out << counter->name();
        out << "}";
        column += strlen(counter->name()) + 10;
    }
}

void Document::read (istream& in, LabelItem* label, long, long) {
    char name[100];
    in.get(name, 100, '}');
    define_label(find_label(name), "", true);
    label->name(name);
}

void Document::write (ostream& out, LabelItem* label, long, long source) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\label{";
        out << label->name();
        out << "}";
        column += strlen(label->name()) + 8;
    }
}

void Document::read (istream& in, RefItem* ref, long, long) {
    char label[100];
    in.get(label, 100, '}');
    ref->name(label);
}

void Document::write (ostream& out, RefItem* ref, long, long source) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\ref{";
        out << ref->name();
        out << "}";
        column += strlen(ref->name()) + 6;
    }
}

void Document::read (istream& in, FloatItem* f, long style, long source) {
    char context[100];
    in.getline(context, 100, '}');
    in.ignore(1);
    f->context(context);
    TextItem* text = new TextItem(this, f, style, source, "");
    f->item(text);
    read_encoded_text(in, text, 0, style, source, END);
}

void Document::write (ostream& out, FloatItem* f, long, long source) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\float{" << f->context() << "}";
        out << "{";
        column += strlen(f->context()) + 9;
    }
    TextItem* text = f->item();
    write_encoded_text(
        out, text, 0, text->item_count(), text->style(), text->source()
    );
    if (info._source._editable) {
        out << "}";
        column += 1;
    }
    FloatInfo& finfo = _float->item_ref(find_float(f->item()));
    out << "%" << finfo._x << " " << finfo._y << " " << finfo._page << "\n";
    column = 0;
}

void Document::read (istream& in, PagenumberItem* p, long, long) {
    char sample[100];
    in.get(sample, 100, '}');
    p->sample(sample);
}

void Document::write (ostream& out, PagenumberItem* p, long, long source) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "\\pagenumber{";
        out << p->sample();
        out << "}";
        column += strlen(p->sample()) + 13;
    }
}

long Document::parse_text (const char* buffer) {
    if (buffer == nil) {
        return 0;
    }
    for (long i = 0; text_map[i]._format != nil; ++i) {
        if (strcmp(buffer, text_map[i]._format) == 0) {
            return text_map[i]._code;
        }
    }
    if (isdigit(buffer[0])) {
        return parse_octal_trigraph(buffer);
    } else {
        return 0;
    }
}

long Document::unparse_text (char* buffer, long code) {
    int ch = int(code);
    if (ch == '\\') {
        sprintf(buffer, "\\backslash");
        return 0;
    } else if (ch == '\n') {
        sprintf(buffer, "\\");
        return -1;
    } else if (strchr(quoted_characters, ch) != NULL) {
        sprintf(buffer, "\\%c", ch);
        return 1;
    } else if (isascii(ch)) {
        sprintf(buffer, "%c", ch);
        return 1;
    } else {
        long i;
        for (i = 0; special_map[i]._format != nil; ++i) {
            if (code == special_map[i]._code) {
                sprintf(buffer, special_map[i]._format);
                return special_map[i]._fold;
            }
        }
        for (i = 0; text_map[i]._format != nil; ++i) {
            if (code == text_map[i]._code) {
                sprintf(buffer, "\\%s", text_map[i]._format);
                return text_map[i]._fold;
            }
        }
        buffer[0] = '\\';
        unparse_octal_trigraph(buffer+1, ch);
        buffer[4] = '\0';
        return 1;
    }
}

long Document::parse_style (const char* buffer, long current_style) {
    char font[100]; font[0] = '\0';
    char color[100]; color[0] = '\0';
    char size[100]; size[0] = '\0';
    char alignment[100]; alignment[0] = '\0';

    char def[1000]; strcpy(def, buffer);
    istrstream in(def);
    char name[100];
    while (in) {
        int c = token(in, name);
        if (c == EOF) {
            ;
        } else if (c == FONT) {
            strcpy(font, name);
        } else if (c == SIZE) {
            strcpy(size, name);
        } else if (c == COLOR) {
            strcpy(color, name);
        } else if (c == ALIGN) {
            strcpy(alignment, name);
        } else if (c == COMMAND) {
            current_style = find_style(
                current_style, font, color, size, alignment
            );
            strcpy(font, "");
            strcpy(color, "");
            strcpy(size, "");
            strcpy(alignment, "");
            MacroInfo& info = _macro->item_ref(find_macro(name));
            current_style = parse_style(info._def, current_style);
        } else {
            ;
        }
    }
    return find_style(current_style, font, color, size, alignment);
}

void Document::unparse_style (char* buffer, long style, long old) {
    TextStyle& new_style = _style->item_ref(style)._style;
    TextStyle& old_style = _style->item_ref(old)._style;
    buffer[0] = '\0';
    if (strcmp(new_style._font, old_style._font) != 0) {
        strcat(buffer, "\\font{");
        strcat(buffer, new_style._font);
        strcat(buffer, "}");
    }
    if (strcmp(new_style._color, old_style._color) != 0) {
        strcat(buffer, "\\color{");
        strcat(buffer, new_style._color);
        strcat(buffer, "}");
    }
    if (strcmp(new_style._size, old_style._size) != 0) {
        strcat(buffer, "\\size{");
        strcat(buffer, new_style._size);
        strcat(buffer, "}");
    }
    if (strcmp(new_style._alignment, old_style._alignment) != 0) {
        strcat(buffer, "\\align{");
        strcat(buffer, new_style._alignment);
        strcat(buffer, "}");
    }
}

long Document::read_verbatim_text (
    istream& in, TextItem* text, long index, long style, long source
) {
    source = find_source(source, style, "verbatim", "", true, true);
    style = parse_style(_macro->item_ref(find_macro("verbatim"))._def, style);
    char line[1000];
    boolean done = false;
    const char* term = "\\end{verbatim}";
    int termlength = strlen(term);
    in.getline(line, 1000);
    do {
        streampos bol = in.tellg();
        if (!in.getline(line, 1000)) {
            done = true;
        } else if (strncmp(line, term, termlength) == 0) {
            in.seekg(bol);
            done = true;
        } else {
            int length = strlen(line);
            for (int i = 0; i < length; ++i) {
                index = text->insert(index, line[i], style, source, nil);
            }
            index = text->insert(index, '\n', style, source, nil);
        }
    } while (!done);
    return index;
}

void Document::write_verbatim_text (
    ostream& out, TextItem* text,
    long index, long count, long style, long source
) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        if (info._style != style) {
            char buffer[100];
            unparse_style(buffer, style, info._style);
            out << "{" << buffer;
        }
        out << "%\n\\begin{verbatim}\n";
        column = 0;
        for (long i = index; i < index + count; ++i) {
            out.put(char(text->item_code(i)));
        }
        out << "\\end{verbatim}%\n";
        column = 0;
        if (info._style != style) {
            out << "}";
            column += 1;
        }
    }
}

long Document::read_import_text (
    istream&, TextItem* text, long index, long style, long source,
    const char* filename
) {
    source = find_source(source, style, "import", filename, false, true);
    style = parse_style(_macro->item_ref(find_macro("verbatim"))._def, style);
    boolean done = false;
    ifstream file(filename);
    char line[1000];
    while (file) {
        file.getline(line, 1000);
        if (file) {
            int length = strlen(line);
            for (int i = 0; i < length; ++i) {
                index = text->insert(index, line[i], style, source, nil);
            }
            index = text->insert(index, '\n', style, source, nil);
        }
    }
    return index;
}

void Document::write_import_text (
    ostream& out, TextItem*, long, long,
    long style, long source, const char* filename
) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        if (info._style != style) {
            char buffer[100];
            unparse_style(buffer, style, info._style);
            out << "{" << buffer;
        }
        out << "%\n\\import{" << filename << "}%\n";
        column = 0;
        if (info._style != style) {
            out << "}";
            column += 1;
        }
    }
}

long Document::read_macro_text (
    istream& in, TextItem* text, long index, long style, long source,
    const char* name
) {
    source = find_source(source, style, "macro", name, false, true);
    istrstream def(_macro->item_ref(find_macro(name))._def);
    parameter = &in;
    index = read_encoded_text(def, text, index, style, source, END);
    return index;
}

void Document::write_macro_text (
    ostream& out, TextItem* text, long index, long count,
    long, long source, const char* name
) {
    SourceInfo& info = _source->item_ref(source);
    if (info._source._editable) {
        out << "%\n%\n\\" << name << "{";
        column = strlen(name) + 2;
    }
    source = nested_source(source, text->item_source(index));
    write_encoded_text(
        out, text, index, count, _source->item_ref(source)._style, source
    );
    if (info._source._editable) {
        out << "}%\n%\n";
        column = 0;
    }
}

long Document::read_parameter_text (
    istream& in, TextItem* text, long index, long style, long source
) {
    source = find_source(source, style, "parameter", "", true, true);
    index = read_encoded_text(in, text, index, style, source, END);
    return index;
}

void Document::write_parameter_text (
    ostream& out, TextItem* text, long index, long count,
    long, long source
) {
    SourceInfo& info = _source->item_ref(source);
    source = nested_source(source, text->item_source(index));
    write_encoded_text(
        out, text, index, count, _source->item_ref(source)._style, source
    );
}

void Document::write_styled_text (
    ostream& out, TextItem* text, long index, long count,
    long style, long source, const char* name
) {
    SourceInfo& info = _source->item_ref(source);
    source = nested_source(source, text->item_source(index));
    style = parse_style(_macro->item_ref(find_macro(name))._def, style);
    if (info._source._editable) {
        out << "{\\" << name << " ";
        column += strlen(name) + 3;
    }
    write_encoded_text(out, text, index, count, style, source);
    if (info._source._editable) {
        out << "}";
        column += 1;
    }
}

long Document::read_encoded_text (
    istream& in, TextItem* text, long index, long style, long source,
    long terminator
) {
    boolean done = false;
    char buffer[1000];
    long i = index;
    do {
        streampos tell = in.tellg();
        int c = token(in, buffer);
        if (c < 0 && c >= terminator) {
            in.seekg(tell);
            done = true;
        } else if (c == COMMENT) {
            if (expecting_position != nil) {
                float x, y;
                long page;
                if (sscanf(buffer, "%g %g %d", &x, &y, &page) == 3) {
                    adjust_float(expecting_position->item(), x, y, page);
                    expecting_position = nil;
                }
            }
        } else if (c == BLOCK) {
            i = read_encoded_text(in, text, i, style, source, END);
            if (token(in, buffer) != END) {
                ; // error
            }
        } else if (c == FIELD) {
            ; // error
        } else if (c == LINE) {
            i = text->insert(i, linebreak, style, source, nil);
        } else if (c == FONT) {
            style = find_style(style, buffer, "", "", "");
        } else if (c == SIZE) {
            style = find_style(style, "", "", buffer, "");
        } else if (c == COLOR) {
            style = find_style(style, "", buffer, "", "");
        } else if (c == ALIGN) {
            style = find_style(style, "", "", "", buffer);
        } else if (c == COMMAND) {
            long j;
            if ((j = parse_text(buffer)) > 0) {
                i = text->insert(i, j, style, source, nil);
            } else {
                MacroInfo& info = _macro->item_ref(find_macro(buffer));
                SourceInfo& sourceinfo = _source->item_ref(source);
                if (i == index && sourceinfo._source._editable) {
                    source = find_source(
                        source, style, "styled", buffer, true, false
                    );
                }
                style = parse_style(info._def, style);
            }
        } else if (c == VERBATIM) {
            i = read_verbatim_text(in, text, i, style, source);
            if (token(in, buffer) != END) {
                ; // error
            }
        } else if (c == IMPORT) {
            i = read_import_text(in, text, i, style, source, buffer);
        } else if (c == MACRO) {
            i = read_macro_text(in, text, i, style, source, buffer);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == PARAMETER) {
            if (parameter != nil) {
                i = read_parameter_text(
                    *parameter, text, i, style, source
                );
                parameter = nil;
            }
        } else if (c == PARBOX) {
            TextItem* parbox = new TextItem(this, text, style, source, buffer);
            read_encoded_text(in, parbox, 0, style, source, END);
            i = text->insert(i, 0, style, source, parbox);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == FLOAT) {
            FloatItem* f = new FloatItem(this, text, style, source);
            if (expecting_position != nil) {
                Coord x = document_metric("textwidth") / 2;
                Coord y = - (document_metric("textheight") / 2);
                long page = -1;
                adjust_float(expecting_position->item(), x, y, page);
                expecting_position = nil;
            }
            expecting_position = f;
            f->read(in);
            i = text->insert(i, 0, style, source, f);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == TABULAR) {
            TabularItem* tabular = new TabularItem(this, text, style, source);
            tabular->read(in);
            i = text->insert(i, 0, style, source, tabular);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == PSFIG) {
            PSFigItem* psfig = new PSFigItem(this, text, style, source);
            psfig->read(in);
            i = text->insert(i, 0, style, source, psfig);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == COUNTER) {
            CounterItem* counter = new CounterItem(this, text, style, source);
            counter->read(in);
            i = text->insert(i, 0, style, source, counter);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == LABEL) {
            LabelItem* label = new LabelItem(this, text, style, source);
            label->read(in);
            i = text->insert(i, 0, style, source, label);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == REF) {
            RefItem* ref = new RefItem(this, text, style, source);
            ref->read(in);
            i = text->insert(i, 0, style, source, ref);
            if (token(in, buffer) != END) {
                ;
            }
        } else if (c == PAGENUMBER) {
            PagenumberItem* p = new PagenumberItem(this, text, style, source);
            p->read(in);
            i = text->insert(i, 0, style, source, p);
            if (token(in, buffer) != END) {
                ;
            }
        } else {
            i = text->insert(i, c, style, source, nil);
        }
    } while (!done);
    return i;
}

void Document::write_encoded_text (
    ostream& out, TextItem* text,
    long index, long count, long style, long source
) {
    long current_style = style;
    boolean generated = !_source->item_ref(source)._source._editable;
    char buffer[1000];
    long i = index;
    while (i < index + count) {
        long new_source = text->item_source(i);
        if (new_source != source) {
            long nested = nested_source(source, text->item_source(i));
            long new_style = _source->item_ref(nested)._style;
            if (!generated && new_style != current_style) {
                out << "{";
                column += 1;
                unparse_style(buffer, new_style, current_style);
                out << buffer;
                column += strlen(buffer);
            }
            long j = i + 1;
            while (
                j < index + count
                && nested == nested_source(source, text->item_source(j))
            ) {
                ++j;
            }
            TextSource& info = _source->item_ref(nested)._source;
            if (strcmp(info._source, "document") == 0) {
                write_encoded_text(
                    out, text, i, j - i, current_style, nested
                );
            } else if (strcmp(info._source, "verbatim") == 0) {
                write_verbatim_text(
                    out, text, i, j - i, current_style, source
                );
            } else if (strcmp(info._source, "macro") == 0) {
                write_macro_text(
                    out, text, i, j - i, current_style, source, info._name
                );
            } else if (strcmp(info._source, "parameter") == 0) {
                write_parameter_text(
                    out, text, i, j - i, current_style, source
                );
            } else if (strcmp(info._source, "import") == 0) {
                write_import_text(
                    out, text, i, j - i, current_style, source, info._name
                );
            } else if (strcmp(info._source, "styled") == 0) {
                write_styled_text(
                    out, text, i, j - i, current_style, source, info._name
                );
            } else {
                break;
            }
            if (!generated && new_style != current_style) {
                out << "}";
                column += 1;
            }
            i = j;
        } else {
            long new_style = text->item_style(i);
            if (!generated) {
                if (new_style != current_style) {
                    if (current_style == style) {
                        out << "{";
                        column += 1;
                    } else if (new_style == style) {
                        column += 1;
                        out << "}";
                    }
                    if (new_style != style) {
                        unparse_style(buffer, new_style, current_style);
                        column += strlen(buffer);
                        out << buffer;
                    }
                }
            }
            current_style = new_style;
            Item* item = text->item(i);
            if (item == nil) {
                if (!generated) {
                    long fold = unparse_text(buffer, text->item_code(i));
                    out << buffer;
                    if (fold > 0) {
                        column += strlen(buffer);
                    } else if (fold < 0) {
                        out << "\n";
                        column = 0;
                    } else {
                        if (column >= FOLDCOLUMN) {
                            out << "\n";
                            column = 0;
                        } else {
                            out << " ";
                            column += strlen(buffer) + 1;
                        }
                    }
                }
            } else {
                item->write(out);
            }
            i = i + 1;
        }
    }
    if (current_style != style && !generated) {
        out <<  "}";
        column += 1;
    }
}

void Document::relabel () {
    _relabel = true;
}

void Document::format_counter (long value, const char* format, char* buffer) {
    long j = 0;
    while (counter_format[j]._name != nil) {
        if (strcmp(counter_format[j]._name, format) == 0) {
            (*counter_format[j]._formatter)(value, buffer);
            break;
        }
        ++j;
    }
}

void Document::step (const char* counter, const char* context) {
    long count = _counter->count();
    for (long i = 0; i < count; ++i) {
        CounterInfo& info = _counter->item_ref(i);
        if (strcmp(info._name, counter) == 0) {
            info._value += 1;
            char buffer[100];
            buffer[0] = '\0';
            if (info._within != nil && strlen(info._within) > 0) {
                strcat(buffer, label(info._within));
                strcat(buffer, ".");
            }
            format_counter(info._value, info._format, strchr(buffer, '\0'));
            label(info._name, buffer);
            label(context, buffer);
        } else if (info._within != nil && strcmp(info._within, counter) == 0) {
            info._value = 0;
            label(info._name, "");
        }
    }
}

void Document::label (const char* name, const char* text) {
    long label = find_label(name);
    define_label(label, text, _label->item_ref(label)._save);
}

const char* Document::label (const char* name) {
    LabelInfo& info = _label->item_ref(find_label(name));
    if (info._text != nil) {
        return info._text;
    } else {
        return "?";
    }
}

void Document::attach (DocumentViewer* viewer) {
    DocumentViewerInfo info;
    info._viewer = viewer;
    _viewer->append(info);
    long count = _float->count();
    for (long i = 0; i < count; ++i) {
        FloatInfo& info = _float->item_ref(i);
        viewer->float_inserted(info._item);
        viewer->float_adjusted(info._item, info._x, info._y, info._page);
    }
    viewer->update();
}

void Document::detach (DocumentViewer* viewer) {
    long count = _viewer->count();
    for (long i = 0; i < count; ++i) {
        DocumentViewerInfo& info = _viewer->item_ref(i);
        if (info._viewer == viewer) {
            _viewer->remove(i);
            break;
        }
    }
}

void Document::notify () {
    while (_body != nil && _relabel) {
        long count = _counter->count();
        for (long i = 0; i < count; ++i) {
            CounterInfo& info = _counter->item_ref(i);
            info._value = info._initial;
        }
        _relabel = false;
        _body->label("");
    }
    long count = _viewer->count();
    for (long i = 0; i < count; ++i) {
        DocumentViewerInfo& info = _viewer->item_ref(i);
        info._viewer->update();
    }
}

long Document::find_style (
    long oldstyle, 
    const char* font, const char* color,
    const char* size, const char* alignment
) {
    long count = _style->count();
    long i;
    if (oldstyle >= 0) {
        TextStyle& info = _style->item_ref(oldstyle)._style;
        if (strlen(font) == 0) font = info._font;
        if (strlen(color) == 0) color = info._color;
        if (strlen(size) == 0) size = info._size;
        if (strlen(alignment) == 0) alignment = info._alignment;

        for (i = 0; i < count; ++i) {
            TextStyle& info = _style->item_ref(i)._style;
            if (
                strcmp(font, info._font) == 0
                && strcmp(color, info._color) == 0
                && strcmp(size, info._size) == 0
                && strcmp(alignment, info._alignment) == 0
            ) {
                break;
            }
        }
    } else {
        i = count;
    }
    if (i == count) {
        StyleInfo info;
        info._style._font = strcpy(new char[strlen(font) + 1], font);
        info._style._color = strcpy(new char[strlen(color) + 1], color);
        info._style._size = strcpy(new char[strlen(size) + 1], size);
        info._style._alignment =
            strcpy(new char[strlen(alignment) + 1], alignment);
        _style->append(info);
        initialize_style(i);
    }
    return i;
}

void Document::initialize_style (long index) {
    static FontFamily* times;
    static FontFamily* helvetica;
    static FontFamily* courier;

    StyleInfo& style = _style->item_ref(index);
    char screenfont[100];
    strcpy(screenfont, style._style._font);
    long len = strlen(screenfont);
    for (long i = 0; i < len; ++i) {
        if (isupper(screenfont[i])) {
            screenfont[i] += 'a' - 'A';
        }
    }
    char* hyphen = strchr(screenfont, '-');
    char* screenstyle;
    if (hyphen != nil) {
        *hyphen = '\0';
        screenstyle = hyphen+1;
    } else {
        screenstyle = "";
    }
    FontFamily* family = find_font_family(screenfont);
    float pointsize;
    sscanf(style._style._size, "%g", &pointsize);
    float scale;
    const char* name;
    if (family != nil && family->font(
	int(pointsize), screenstyle, name, scale)
    ) {
        if (!World::current()->property_is_on(SCREEN_METRICS) &&
	    PSFont::exists(style._style._font)
	) {
            style._font = new PSFont(
                style._style._font, pointsize, name, scale
            );
        } else {
            style._font = new Font(name, scale);
        }
    } else {
        style._font = World::current()->font();
    }
    style._font->ref();

    style._color = Color::lookup(
	World::current()->display(), style._style._color
    );
    if (style._color == nil) {
	style._color = World::current()->foreground();
    }
    style._color->ref();

    Coord baseline_skip = pointsize * document_metric("baselineskip");
    Coord baseline_stretch = pointsize * document_metric("baselinestretch");
    Coord par_skip = pointsize * document_metric("parskip");
    Coord par_stretch = pointsize * document_metric("parstretch");
    Coord big_skip = pointsize * document_metric("bigskip");
    Coord big_stretch = pointsize * document_metric("bigstretch");
    Coord med_skip = pointsize * document_metric("medskip");
    Coord med_stretch = pointsize * document_metric("medstretch");
    Coord small_skip = pointsize * document_metric("smallskip");
    Coord small_stretch = pointsize * document_metric("smallstretch");

    const LayoutKit& layout = *LayoutKit::instance();
    if (strcmp(style._style._alignment, "left") == 0) {
        style._begin_line_strut = layout.vstrut(0);
        style._end_line_strut = layout.strut(style._font, 0, fil, 0);
        style._begin_par_strut = layout.vstrut(0);
        style._end_par_strut = layout.strut(style._font, 0, fil, 0);
    } else if(strcmp(style._style._alignment, "right") == 0) {
        style._begin_line_strut = layout.vstrut(0, 0, 0, fil, 0);
        style._end_line_strut = layout.strut(style._font);
        style._begin_par_strut = layout.vstrut(0, 0, 0, fil, 0);
        style._end_par_strut = layout.strut(style._font);
    } else if (strcmp(style._style._alignment, "center") == 0) {
        style._begin_line_strut = layout.vstrut(0, 0, 0, fil, 0);
        style._end_line_strut = layout.strut(style._font, 0, fil, 0);
        style._begin_par_strut = layout.vstrut(0, 0, 0, fil, 0);
        style._end_par_strut = layout.strut(style._font, 0, fil, 0);
    } else if (strcmp(style._style._alignment, "justify") == 0) {
        style._begin_line_strut = layout.vstrut(0);
        style._end_line_strut = layout.strut(style._font);
        style._begin_par_strut = layout.vstrut(0);
        style._end_par_strut = layout.strut(style._font, 0, fil, 0);
    } else {
        style._begin_line_strut = layout.vstrut(0);
        style._end_line_strut = layout.strut(style._font, 0, fil, 0);
        style._begin_par_strut = layout.vstrut(0);
        style._end_par_strut = layout.strut(style._font, 0, fil, 0);
    }
    style._begin_line_strut->ref();
    style._end_line_strut->ref();
    style._begin_par_strut->ref();
    style._end_par_strut->ref();

    style._line_strut = layout.strut(style._font);
    style._line_strut->ref();
    style._fil_strut = layout.strut(style._font, 0, fil, 0);
    style._fil_strut->ref();
    style._hyphen = new Character('-', style._font, style._color);
    style._hyphen->ref();
    style._interline_glue = layout.vglue(baseline_skip, baseline_stretch, 0);
    style._interline_glue->ref();
    style._interpar_glue = layout.vglue(par_skip, par_stretch, 0);
    style._interpar_glue->ref();
    style._hfil_glue = layout.hglue(0, fil, 0);
    style._hfil_glue->ref();
    style._hfill_glue = layout.hglue(0, 1000000*fil, 0);
    style._hfill_glue->ref();
    style._hleader = new HLeader(
        0, fil, 0,
        layout.h_margin(new Character('.', style._font, style._color), 3)
    );
    style._hleader->ref();
    style._vfil_glue = layout.vglue(0, fil, 0);
    style._vfil_glue->ref();
    style._bigskip_glue = layout.vglue(big_skip, big_stretch, 0);
    style._bigskip_glue->ref();
    style._medskip_glue = layout.vglue(med_skip, med_stretch, 0);
    style._medskip_glue->ref();
    style._smallskip_glue = layout.vglue(small_skip, small_stretch, 0);
    style._smallskip_glue->ref();
    style._word_space = layout.spaces(2, 0.5, style._font, style._color);
    style._word_space->ref();
    style._sentence_space = layout.spaces(3, 0.5, style._font, style._color);
    style._sentence_space->ref();
    for (int c = 0; c < 256; ++c) {
        style._glyph[c] = nil;
    }
}

void Document::cleanup_style (long index) {
    StyleInfo& style = _style->item_ref(index);

    delete style._style._font;
    delete style._style._color;
    delete style._style._alignment;

    style._font->unref();
    style._color->unref();
    style._begin_line_strut->unref();
    style._end_line_strut->unref();
    style._begin_par_strut->unref();
    style._end_par_strut->unref();
    style._line_strut->unref();
    style._fil_strut->unref();
    style._hyphen->unref();
    style._interline_glue->unref();
    style._interpar_glue->unref();
    style._hfil_glue->unref();
    style._hfill_glue->unref();
    style._hleader->unref();
    style._vfil_glue->unref();
    style._bigskip_glue->unref();
    style._medskip_glue->unref();
    style._smallskip_glue->unref();
    style._word_space->unref();
    style._sentence_space->unref();
    for (int c = 0; c < 256; ++c) {
        if (style._glyph[c] != nil) {
            style._glyph[c]->unref();
        }
    }
}

long Document::find_source (
    long enclosing, long style, const char* source, const char* name,
    boolean editable, boolean unique
) {
    long count = _source->count();
    long i;
    if (!unique) {
        for (i = 0; i < count; ++i) {
            SourceInfo& info = _source->item_ref(i);
            if (
                info._enclosing == enclosing
                && info._style == style
                && strcmp(info._source._source, source) == 0
                && strcmp(info._source._name, name) == 0
            ) {
                break;
            }
        }
    } else {
        i = count;
    }
    if (i == count) {
        SourceInfo info;
        info._enclosing = enclosing;
        info._style = style;
        info._source._name = strcpy(new char[strlen(name) + 1], name);
        info._source._source = strcpy(new char[strlen(source) + 1], source);
        if (enclosing >= 0) {
            info._source._depth =
		_source->item_ref(enclosing)._source._depth + 1;
        } else {
            info._source._depth = 0;
        }
        info._source._editable = editable;
        _source->append(info);
    }
    return i;
}

long Document::nested_source (long enclosing, long source) {
    while (source != 0) {
        SourceInfo& info = _source->item_ref(source);
        if (info._enclosing == enclosing) {
            break;
        } else {
            source = info._enclosing;
        }
    };
    return source;
}

void Document::cleanup_source (long index) {
    SourceInfo& info = _source->item_ref(index);
    delete info._source._name;
    delete info._source._source;
}

long Document::find_macro (const char* name) {
    long count = _macro->count();
    for (long i = 0; i < count; ++i) {
        MacroInfo& info = _macro->item_ref(i);
        if (strcmp(info._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        MacroInfo info;
        info._name = strcpy(new char[strlen(name) + 1], name);
        info._def = strcpy(new char[2], "#");
        info._save = false;
        _macro->append(info);
    }
    return i;
}

void Document::define_macro (long index, const char* def, boolean save) {
    MacroInfo& info = _macro->item_ref(index);
    if (info._def != nil) {
        delete info._def;
    }
    info._def = strcpy(new char[strlen(def) + 1], def);
    info._save = save;
}

void Document::cleanup_macro (long index) {
    MacroInfo& info = _macro->item_ref(index);
    if (info._name != nil) {
        delete info._name;
    }
    if (info._def != nil) {
        delete info._def;
    }
}

long Document::find_counter (const char* name) {
    long count = _counter->count();
    for (long i = 0; i < count; ++i) {
        CounterInfo& info = _counter->item_ref(i);
        if (strcmp(info._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        CounterInfo info;
        info._name = strcpy(new char[strlen(name) + 1], name);
        info._within = nil;
        info._format = nil;
        info._initial = 0;
        info._value = 0;
        info._save = false;
        _counter->append(info);
    }
    return i;
}

void Document::define_counter (
    long index, const char* within, const char* format,
    long initial, boolean save
) {
    CounterInfo& info = _counter->item_ref(index);
    if (info._within != nil) {
        delete info._within;
    }
    info._within = strcpy(new char[strlen(within) + 1], within);
    if (info._format != nil) {
        delete info._format;
    }
    info._format = strcpy(new char[strlen(format) + 1], format);
    info._initial = initial;
    info._save = save;
}

void Document::cleanup_counter (long index) {
    CounterInfo& info = _counter->item_ref(index);
    if (info._name != nil) {
        delete info._name;
    }
    if (info._within != nil) {
        delete info._within;
    }
    if (info._format != nil) {
        delete info._format;
    }
}

long Document::find_parameter (const char* name) {
    long count = _parameter->count();
    for (long i = 0; i < count; ++i) {
        DocumentParameterInfo& info = _parameter->item_ref(i);
        if (strcmp(info._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        DocumentParameterInfo info;
        info._name = strcpy(new char[strlen(name) + 1], name);
        info._value = nil;
        info._save = false;
        _parameter->append(info);
    }
    return i;
}

void Document::define_parameter (long index, const char* value, boolean save) {
    DocumentParameterInfo& info = _parameter->item_ref(index);
    if (info._value != nil) {
        delete info._value;
    }
    if (value != nil) {
        info._value = strcpy(new char[strlen(value) + 1], value);
    } else {
        info._value = nil;
    }
    info._save = save;
}

void Document::cleanup_parameter (long index) {
    DocumentParameterInfo& info = _parameter->item_ref(index);
    if (info._name != nil) {
        delete info._name;
    }
    if (info._value != nil) {
        delete info._value;
    }
}

long Document::find_label (const char* name) {
    long count = _label->count();
    for (long i = 0; i < count; ++i) {
        LabelInfo& info = _label->item_ref(i);
        if (strcmp(info._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        LabelInfo info;
        info._name = strcpy(new char[strlen(name) + 1], name);
        info._text = nil;
        info._save = false;
        _label->append(info);
    }
    return i;
}

void Document::define_label (long index, const char* text, boolean save) {
    LabelInfo& info = _label->item_ref(index);
    if (info._text != nil) {
        delete info._text;
    }
    if (text != nil) {
        info._text = strcpy(new char[strlen(text) + 1], text);
    } else {
        info._text = nil;
    }
    info._save = save;
}

void Document::cleanup_label (long index) {
    LabelInfo& info = _label->item_ref(index);
    if (info._name != nil) {
        delete info._name;
    }
    if (info._text != nil) {
        delete info._text;
    }
}

long Document::find_float (Item* item) {
    long count = _float->count();
    for (long i = 0; i < count; ++i) {
        FloatInfo& info = _float->item_ref(i);
        if (info._item == item) {
            break;
        }
    }
    if (i == count) {
        FloatInfo info;
        info._item = item;
        info._item->ref();
        info._x = 0;
        info._y = 0;
        info._page = -1;
        _float->append(info);
    }
    return i;
}

void Document::cleanup_float (long index) {
    FloatInfo& info = _float->item_ref(index);
    info._item->unref();
}

void Document::read_parameter (istream& in, char* buffer) {
    int c;
    char* p = buffer;
    do {
        *p++ = (c = in.get());
    } while (c != '}');
    *--p = '\0';
    in.putback(c);
}

void Document::read_name (istream& in, char* buffer) {
    int c;
    char* p = buffer;
    do {
        *p++ = (c = in.get());
    } while (isalpha(c) || c == '*');
    *--p = '\0';
    in.putback(c);
}

int Document::token (istream& in, char* buffer) {
    int c = in.get();
    if (c == EOF) {
        return EOF;
    } else if (c == '%') {
        char* p = buffer;
        while ((*p++ = in.get()) != '\n') { }
        *p = '\0';
        return COMMENT;
    } else if (c == '{') {
        return BLOCK;
    } else if (c == '}') {
        return END;
    } else if (c == '&') {
        while ((c = in.get()) == ' ') { }
        if (c == '\n' && (c = in.get()) == '\n') {
            in.putback(c);
        }
        in.putback(c);
        return FIELD;
    } else if (c == '\\') {
        c = in.get();
        if (isalpha(c)) {
            in.putback(c);
            read_name(in, buffer);
            if ((c = in.get()) == '{') {
                if (strcmp(buffer, "end") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                    return END;
                }
                if (strcmp(buffer, "begin") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                }
                if (strcmp(buffer, "color") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                    return COLOR;
                } else if (strcmp(buffer, "font") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                    return FONT;
                } else if (strcmp(buffer, "size") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                    return SIZE;
                } else if (strcmp(buffer, "align") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                    return ALIGN;
                } else if (strcmp(buffer, "import") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(1);
                    return IMPORT;
                } else if (strcmp(buffer, "parbox") == 0) {
                    read_parameter(in, buffer);
                    in.ignore(2);
                    return PARBOX;
                } else if (strcmp(buffer, "float") == 0) {
                    return FLOAT;
                } else if (strcmp(buffer, "verbatim") == 0) {
                    return VERBATIM;
                } else if (strcmp(buffer, "tabular") == 0) {
                    return TABULAR;
                } else if (strcmp(buffer, "psfig") == 0) {
                    return PSFIG;
                } else if (strcmp(buffer, "counter") == 0) {
                    return COUNTER;
                } else if (strcmp(buffer, "label") == 0) {
                    return LABEL;
                } else if (strcmp(buffer, "ref") == 0) {
                    return REF;
                } else if (strcmp(buffer, "pagenumber") == 0) {
                    return PAGENUMBER;
                } else {
                    return MACRO;
                }
            } else {
                in.putback(c);
                while ((c = in.get()) == ' ') { }
                if (c == '\n' && (c = in.get()) == '\n') {
                    in.putback(c);
                }
                in.putback(c);
                return COMMAND;
            }
        } else if (isdigit(c)) {
            int i = c - '0';
            if (isdigit(c = in.get())) {
                i = i * 8 + (c - '0');
            } else {
                in.putback(c);
            }
            if (isdigit(c = in.get())) {
                i = i * 8 + (c - '0');
            } else {
                in.putback(c);
            }
            return i;
        } else if (c == '\\') {
            while ((c = in.get()) == '\n') { }
            in.putback(c);
            return LINE;
        } else if (c == '-') {
            return discretionaryhyphen;
        } else if (c == ',') {
            return thinspace;
        } else if (c == '!') {
            return negthinspace;
        } else {
            return c;
        }
    } else if (c == '-') {
        if ((c = in.get()) == '-') {
            if ((c = in.get()) == '-') {
                return emdash;
            } else {
                in.putback(c);
                return endash;
            }
        } else {
            in.putback(c);
            return visiblehyphen;
        }
    } else if (c == ' ') {
        int count = 1;
        while ((c = in.get()) == ' ') {
            ++count;
        }
        in.putback(c);
        if (count > 1) {
            return sentencespace;
        } else if (c == '\n') {
            return token(in, buffer);
        } else {
            return wordspace;
        }
    } else if (c == '\n') {
        if ((c = in.get()) == '\n') {
            in.putback(c);
            while ((c = in.get()) == '\n') { }
            in.putback(c);
            while ((c = in.get()) == ' ') { }
            in.putback(c);
            return parbreak;
        } else {
            in.putback(c);
            while ((c = in.get()) == ' ') { }
            in.putback(c);
            return wordspace;
        }
    } else if (c == '#') {
        return PARAMETER;
    } else if (c == '~') {
        return nobreakspace;
    } else {
        return c;
    }
}
