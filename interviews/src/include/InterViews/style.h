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
 * Style - style information
 */

#ifndef iv_style_h
#define iv_style_h

#include <InterViews/coord.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class Action;
class Brush;
class Color;
class Font;
class String;
class StyleRep;

class Style : public Resource {
public:
    Style();
    Style(const String& name);
    Style(Style* parent);
    Style(const String& name, Style* parent);
    Style(const Style&);
    virtual ~Style();

    virtual void name(const String&);
    virtual const String* name() const;
    virtual void alias(const String&);
    virtual long alias_count() const;
    virtual const String* alias(long) const;
    void name(const char*);
    void alias(const char*);

    virtual Style* parent() const;
    virtual void append(Style*);
    virtual void remove(Style*);
    virtual long children() const;
    virtual Style* child(long) const;

    virtual void attribute(const String& name, const String& value, int = 0);
    virtual void remove_attribute(const String& name);
    virtual long attribute_count() const;
    virtual boolean attribute(long, String& name, String& value) const;
    void attribute(const char* name, const char* value, int = 0);
    void remove_attribute(const char*);

    virtual void load_file(const String& filename, int = 0);
    virtual void load_list(const String&, int = 0);
    virtual void load_property(const String&, int = 0);

    virtual void add_trigger(const String& name, Action*);
    virtual void remove_trigger(const String& name, Action* = nil);
    virtual void add_trigger_any(Action*);
    virtual void remove_trigger_any(Action*);
    void add_trigger(const char*, Action*);
    void remove_trigger(const char*, Action* = nil);

    virtual boolean find_attribute(const String& name, String& value) const;
    boolean find_attribute(const char* name, String& value) const;
    boolean find_attribute(const String& name, long&) const;
    boolean find_attribute(const char* name, long&) const;
    boolean find_attribute(const String& name, double&) const;
    boolean find_attribute(const char* name, double&) const;
    boolean find_attribute(const String& name, Coord&) const;
    boolean find_attribute(const char* name, Coord&) const;
    boolean value_is_on(const String& name) const;
    boolean value_is_on(const char* name) const;
private:
    friend class StyleRep;

    StyleRep* rep_;
private:
    /* not implemented */
    Style& operator =(const Style&);
};

#include <InterViews/_leave.h>

#endif
