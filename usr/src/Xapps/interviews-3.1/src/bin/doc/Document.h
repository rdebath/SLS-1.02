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

#ifndef Document_h
#define Document_h

#include <InterViews/resource.h>
#include <InterViews/boolean.h>

class Application;
class CounterInfo_List;
class CounterItem;
class DocumentParameterInfo_List;
class DocumentViewer;
class DocumentViewerInfo_List;
class FloatInfo_List;
class FloatItem;
class Glyph;
class Item;
class LabelInfo_List;
class LabelItem;
class MacroInfo_List;
class PagenumberItem;
class PSFigItem;
class RefItem;
class StyleInfo_List;
class SourceInfo_List;
class TabularItem;
class TextItem;

class istream;
class ostream;

class TextStyle {
public:
    char* _font;
    char* _color;
    char* _size;
    char* _alignment;
};

class TextSource {
public:
    char* _source;
    char* _name;
    boolean _editable;
    int _depth;
};

class Document : public Resource {
public:
    Document (Application*, int size_hint = 0);

    virtual void touch (boolean);
    virtual boolean touched ();

    virtual const char* name ();
    virtual void name (const char*);

    virtual const char* document_parameter (const char*);
    virtual float document_metric (const char*);
    virtual float convert_metric (const char*);
    virtual void format_counter (long value, const char* format, char* buffer);

    virtual TextItem* body ();

    virtual void insert_float (Item*);
    virtual void remove_float (Item*);
    virtual void change_float (Item*);
    virtual void adjust_float (Item*, float x, float y, long page);

    virtual Glyph* character (long code, long style);
    virtual TextStyle& text_style (long);
    virtual TextSource& text_source (long);
    virtual long common_source (long source1, long source2);

    virtual void relabel ();
    virtual void step (const char* counter, const char* context);
    virtual void label (const char* name, const char* text);
    virtual const char* label (const char* name);

    virtual long paste (
        istream&, TextItem*, long index, long style, long source
    );
    virtual void copy (
        ostream&, TextItem*, long index, long count, long style, long source
    );

    virtual void read (istream&);
    virtual void read (istream&, TextItem*, long style, long source);
    virtual void read (istream&, PSFigItem*, long style, long source);
    virtual void read (istream&, TabularItem*, long style, long source);
    virtual void read (istream&, CounterItem*, long style, long source);
    virtual void read (istream&, LabelItem*, long style, long source);
    virtual void read (istream&, RefItem*, long style, long source);
    virtual void read (istream&, FloatItem*, long style, long source);
    virtual void read (istream&, PagenumberItem*, long style, long source);

    virtual void write (ostream&);
    virtual void write (ostream&, TextItem*, long style, long source);
    virtual void write (ostream&, PSFigItem*, long style, long source);
    virtual void write (ostream&, TabularItem*, long style, long source);
    virtual void write (ostream&, CounterItem*, long style, long source);
    virtual void write (ostream&, LabelItem*, long style, long source);
    virtual void write (ostream&, RefItem*, long style, long source);
    virtual void write (ostream&, FloatItem*, long style, long source);
    virtual void write (ostream&, PagenumberItem*, long style, long source);

    virtual long parse_text (const char* buffer);
    virtual long unparse_text (char* buffer, long code);

    virtual long parse_style (const char* buffer, long old_style);
    virtual void unparse_style (char* buffer, long style, long old_style);

    virtual void attach (DocumentViewer*);
    virtual void detach (DocumentViewer*);
    virtual void notify ();
protected:
    virtual ~Document ();

    virtual long read_verbatim_text (
        istream& in, TextItem* text, long index, long style, long source
    );
    virtual void write_verbatim_text (
        ostream& out, TextItem* text, long index, long count,
        long style, long source
    );

    virtual long read_import_text (
        istream& in, TextItem* text, long index, long style, long source,
        const char* filename
    );
    virtual void write_import_text (
        ostream& out, TextItem* text, long index, long count,
        long style, long source, const char* filename
    );

    virtual long read_macro_text (
        istream& in, TextItem* text, long index, long style, long source,
        const char* name
    );
    virtual void write_macro_text (
        ostream& out, TextItem* text, long index, long count,
        long style, long source, const char* name
    );

    virtual long read_parameter_text (
        istream& in, TextItem* text, long index, long style, long source
    );
    virtual void write_parameter_text (
        ostream& out, TextItem* text, long index, long count,
        long style, long source
    );
    virtual void write_styled_text (
        ostream& out, TextItem* text, long index, long count,
        long style, long source, const char* name
    );

    virtual long read_encoded_text (
        istream& in, TextItem* text, long index, long style, long source,
        long terminator
    );
    virtual void write_encoded_text (
        ostream& out, TextItem* text, long index, long count,
        long style, long source
    );

    long find_style (
        long oldstyle,
        const char* font, const char* color,
        const char* size, const char* align
    );
    void initialize_style (long style);
    void cleanup_style (long style);

    long find_source (
        long enclosing, long style,
        const char* source, const char* name,
	boolean generated, boolean unique
    );
    long nested_source (long enclosing, long source);
    void cleanup_source (long source);

    long find_macro (const char* name);
    void define_macro (long macro, const char* def, boolean save);
    void cleanup_macro (long macro);

    long find_counter (const char* name);
    void define_counter (
        long, const char* within, const char* format, long init, boolean save
    );
    void cleanup_counter (long counter);

    long find_parameter (const char* name);
    void define_parameter (long, const char* value, boolean save);
    void cleanup_parameter (long);

    long find_label (const char* name);
    void define_label (long, const char* text, boolean save);
    void cleanup_label (long label);

    long find_float (Item*);
    void cleanup_float (long);

    void read_name (istream& in, char* buffer);
    void read_parameter (istream& in, char* buffer);
    int token (istream& in, char* buffer);

    int _size_hint;
    Application* _application;
    char* _name;
    boolean _touched;
    boolean _relabel;
    TextItem* _body;
    char* _style_name;
    float _format_width;
    StyleInfo_List* _style;
    SourceInfo_List* _source;
    MacroInfo_List* _macro;
    CounterInfo_List* _counter;
    LabelInfo_List* _label;
    FloatInfo_List* _float;
    DocumentParameterInfo_List* _parameter;
    DocumentViewerInfo_List* _viewer;
};

#endif
