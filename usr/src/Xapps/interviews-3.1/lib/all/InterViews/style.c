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

#include <InterViews/action.h>
#include <InterViews/brush.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <OS/file.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/table.h>
#include <OS/string.h>
#include <OS/ustring.h>
#include <ctype.h>

declarePtrList(StyleList,Style)
implementPtrList(StyleList,Style)

declarePtrList(UniqueStringList,UniqueString)
implementPtrList(UniqueStringList,UniqueString)

struct StyleAttribute {
private:
    friend class Style;
    friend class StyleRep;

    String* name_;
    UniqueStringList* path_;
    String* value_;
    int priority_;
    Macro* observers_;
    long index_;
};

declarePtrList(StyleAttributeList,StyleAttribute)
implementPtrList(StyleAttributeList,StyleAttribute)

struct StyleAttributeTableEntry {
    StyleAttributeList** entries_;
    long avail_;
    long used_;
};

inline unsigned long key_to_hash(UniqueString& s) { return s.hash(); }

declareTable(StyleAttributeTable,UniqueString,StyleAttributeTableEntry*)
implementTable(StyleAttributeTable,UniqueString,StyleAttributeTableEntry*)

class StyleRep {
private:
    friend class Style;

    UniqueString* name_;
    UniqueStringList* aliases_;
    Style* parent_;
    StyleAttributeTable* table_;
    StyleAttributeList* list_;
    StyleList* children_;
    Macro* observers_;
    boolean modified_;

    StyleRep(UniqueString*);
    ~StyleRep();

    void clear_info();
    void modify();
    void update();

    StyleAttribute* add_attribute(
	const String& name, const String& value, int priority
    );
    UniqueStringList* parse_name(String&, int& priority);
    String* parse_value(const String&);
    int find_separator(const String&);
    int match_name(const UniqueString&);
    boolean same_path(const UniqueStringList&, const UniqueStringList&);
    void delete_path(UniqueStringList*);
    void delete_attribute(StyleAttribute*);

    String strip(const String&);
    void missing_colon(const String&);
    void bad_property_name(const String&);
    void bad_property_value(const String&);

    StyleAttributeTableEntry* find_entry(const UniqueString&);
    boolean wildcard_match(
	const StyleAttributeTableEntry&, const StyleList&, String& value
    );
    boolean wildcard_match_name(
	const UniqueString& name, const StyleAttributeTableEntry&,
	const StyleList&, long s_index, String& value
    );
    int StyleRep::finish_match(
	const StyleList&, long s_index, const UniqueStringList&, long p_index
    );
};

/*
 * Scan an attribute value, replace \<char> as follows:
 *
 *	\<newline>	nothing
 *	\n		newline
 *	\<backslash>	\
 */

class ValueString : public String {
public:
    ValueString(char*, int len);
    virtual ~ValueString();

    virtual boolean null_terminated() const;
};

ValueString::ValueString(char* str, int len) : String(str, len) { }
ValueString::~ValueString() {
    char* s = (char*)string();
    delete s;
}

boolean ValueString::null_terminated() const { return true; }

String* StyleRep::parse_value(const String& v) {
    if (v.index('\\') == -1) {
	return new NullTerminatedString(v);
    }
    const char* src = v.string();
    int len = v.length();
    const char* src_end = src + len;
    char* dst_start = new char[len + 1];
    char* dst = dst_start;
    for (; src < src_end; src++) {
	if (*src == '\\') {
	    ++src;
	    switch (*src) {
	    case '\n':
		/* eliminate */
		break;
	    case 'n':
		*dst++ = '\n';
		break;
	    case '\\':
		*dst++ = *src;
		break;
	    default:
		*dst++ = '\\';
		*dst++ = *src;
		break;
	    }
	} else {
	    *dst++ = *src;
	}
    }
    *dst = '\0';
    return new ValueString(dst_start, dst - dst_start);
}

Style::Style() {
    rep_ = new StyleRep(nil);
}

Style::Style(const String& name) {
    rep_ = new StyleRep(new UniqueString(name));
}

Style::Style(Style* p) {
    rep_ = new StyleRep(nil);
    p->append(this);
}

Style::Style(const String& name, Style* p) {
    rep_ = new StyleRep(new UniqueString(name));
    p->append(this);
}

Style::Style(const Style& style) {
    StyleRep& s = *style.rep_;
    rep_ = new StyleRep(s.name_ == nil ? nil : new UniqueString(*s.name_));
    if (s.parent_ != nil) {
	s.parent_->append(this);
    }
    s.update();

    long n = style.alias_count();
    long i;
    for (i = n - 1; i >= 0; i--) {
	alias(*style.alias(i));
    }

    n = style.children();
    for (i = 0; i < n; i++) {
	append(style.child(i));
    }

    n = style.attribute_count();
    for (i = 0; i < n; i++) {
	String name, value;
	if (style.attribute(i, name, value)) {
	    attribute(name, value);
	}
    }

    rep_->modify();
}

Style::~Style() {
    Style* p = rep_->parent_;
    if (p != nil) {
	p->remove(this);
    }
    delete rep_;
}

StyleRep::StyleRep(UniqueString* s) {
    name_ = s;
    aliases_ = nil;
    parent_ = nil;
    table_ = nil;
    list_ = nil;
    children_ = nil;
    observers_ = nil;
    modified_ = true;
}

StyleRep::~StyleRep() {
    clear_info();
    delete name_;
    StyleAttributeTable* t = table_;
    if (t != nil) {
	for (TableIterator(StyleAttributeTable) i(*t); i.more(); i.next()) {
	    StyleAttributeTableEntry* e = i.cur_value();
	    for (unsigned long j = 0; j < e->used_; j++) {
		StyleAttributeList* a = e->entries_[j];
		if (a != nil) {
		    for (
			ListItr(StyleAttributeList) k(*a); k.more(); k.next()
		    ) {
			delete_attribute(k.cur());
		    }
		    delete a;
		}
	    }
	    delete e->entries_;
	    delete e;
	}
	delete t;
    }
    delete list_;
    delete_path(aliases_);
    if (children_ != nil) {
	for (ListItr(StyleList) i(*children_); i.more(); i.next()) {
	    Style* s = i.cur();
	    s->rep_->parent_ = nil;
	}
	delete children_;
    }
}

const String* Style::name() const { return rep_->name_; }

void Style::name(const String& str) {
    StyleRep& s = *rep_;
    delete s.name_;
    s.name_ = new UniqueString(str);
}

void Style::alias(const String& name) {
    StyleRep& s = *rep_;
    if (s.aliases_ == nil) {
	s.aliases_ = new UniqueStringList(5);
    }
    s.aliases_->prepend(new UniqueString(name));
    s.modify();
}

long Style::alias_count() const {
    StyleRep& s = *rep_;
    return s.aliases_ == nil ? 0 : s.aliases_->count();
}

const String* Style::alias(long i) const {
    UniqueStringList* list = rep_->aliases_;
    if (list == nil || i < 0 || i >= list->count()) {
	return nil;
    }
    return list->item(i);
}

void Style::name(const char* s) { name(String(s)); }
void Style::alias(const char* s) { alias(String(s)); }

Style* Style::parent() const { return rep_->parent_; }

/*
 * Add a child style.  Implicitly remove the child from its current parent,
 * if it has one, and set its parent to this.  Because reparenting may change
 * (cached) attributes, we must mark the child modified.
 */

void Style::append(Style* style) {
    Style* p = style->parent();
    if (p == this) {
	return;
    }
    if (p != nil) {
	p->remove(style);
    }
    StyleRep& s = *rep_;
    if (s.children_ == nil) {
	s.children_ = new StyleList(5);
    }
    s.children_->append(style);
    Resource::ref(this);
    style->rep_->parent_ = this;
    style->rep_->modify();
}

/*
 * Remove a child style.  Do nothing if the given style isn't really a child.
 */

void Style::remove(Style* style) {
    StyleList* list = rep_->children_;
    if (list != nil) {
	for (ListUpdater(StyleList) i(*list); i.more(); i.next()) {
	    if (i.cur() == style) {
		i.remove_cur();
		style->rep_->parent_ = nil;
		Resource::unref(this);
		break;
	    }
	}
    }
}

/*
 * Return the number of children styles.
 */

long Style::children() const {
    StyleList* list = rep_->children_;
    return list == nil ? 0 : list->count();
}

/*
 * Return a particular child.
 */

Style* Style::child(long i) const {
    StyleList* list = rep_->children_;
    if (list != nil && i >= 0 && i < list->count()) {
	return list->item(i);
    }
    return nil;
}

/*
 * Put a <name, value> pair on the attribute list.
 */

void Style::attribute(const String& name, const String& value, int priority) {
    rep_->add_attribute(name, value, priority);
}

StyleAttribute* StyleRep::add_attribute(
    const String& name, const String& value, int priority
) {
    String str(name);
    int p = priority;
    UniqueStringList* path = parse_name(str, p);
    if (path == nil) {
	/* irrelevant attribute: A*B where A doesn't match */
	return nil;
    }

    if (table_ == nil) {
	table_ = new StyleAttributeTable(50);
    }

    UniqueString u(str);
    StyleAttributeTableEntry* e = find_entry(u);
    if (e == nil) {
	e = new StyleAttributeTableEntry;
	e->entries_ = new StyleAttributeList*[3];
	e->avail_ = 3;
	e->used_ = 0;
	for (long i = 0; i < e->avail_; i++) {
	    e->entries_[i] = nil;
	}
	table_->insert(u, e);
    }

    long n = path->count();
    if (e->avail_ <= n) {
	long new_avail = n + 5;
	StyleAttributeList** new_list = new StyleAttributeList*[new_avail];
	for (long i = 0; i < e->avail_; i++) {
	    new_list[i] = e->entries_[i];
	}
	for (i = e->avail_; i < new_avail; i++) {
	    new_list[i] = nil;
	}
	delete e->entries_;
	e->entries_ = new_list;
	e->avail_ = new_avail;
    }
    if (e->entries_[n] == nil) {
	e->entries_[n] = new StyleAttributeList;
    }
    e->used_ = Math::max(e->used_, n + 1);
    StyleAttributeList& list = *e->entries_[n];
    for (ListItr(StyleAttributeList) i(list); i.more(); i.next()) {
	StyleAttribute* a = i.cur();
	if (same_path(*a->path_, *path)) {
	    if (p >= a->priority_) {
		delete a->value_;
		a->value_ = parse_value(value);
		a->priority_ = p;
		if (a->observers_ != nil) {
		    a->observers_->execute();
		}
		modify();
	    }
	    delete_path(path);
	    return a;
	}
    }
    StyleAttribute* a = new StyleAttribute;
    a->name_ = new CopyString(name);
    a->path_ = path;
    a->value_ = parse_value(value);
    a->priority_ = p;
    a->observers_ = nil;
    list.append(a);
    if (list_ == nil) {
	list_ = new StyleAttributeList;
    }
    a->index_ = list_->count();
    list_->append(a);
    modify();
    return a;
}

/*
 * Parse a name of the form *A*B*C into the list of names A, B, C.
 * Strip the first name (e.g., A) if it matches the style's name
 * or an alias.
 */

UniqueStringList* StyleRep::parse_name(String& s, int& priority) {
    boolean leading_star = false;
    if (s[0] == '*') {
	leading_star = true;
	s.set_to_right(1);
    }
    UniqueStringList* list = new UniqueStringList;
    boolean first = true;
    for (int i = find_separator(s); i != -1; i = find_separator(s)) {
	UniqueString name(s.left(i));
	if (first) {
	    first = false;
	    int q = match_name(name);
	    if (q != 0) {
		priority += (q == 1) ? 2 : 1;
		s.set_to_right(i + 1);
		continue;
	    } else if (!leading_star) {
		delete_path(list);
		return nil;
	    }
	}
	list->append(new UniqueString(name));
	s.set_to_right(i + 1);
    }
    return list;
}

/*
 * Return the index of the next separator ("*" or ".") in the string.
 * If no separator is present, return -1.
 */

int StyleRep::find_separator(const String& s) {
    int n = s.length();
    for (int i = 0; i < n; i++) {
	char c = s[i];
	if (c == '*' || c == '.') {
	    return i;
	}
    }
    return -1;
}

/*
 * Check to see if a given name matches the style's name
 * or any of its aliases.
 *
 * Return value:
 *     0 - no match
 *     1 - name match
 *     2 and up - index of alias match plus 2
 */

int StyleRep::match_name(const UniqueString& name) {
    int match = 0;
    if (name_ != nil && name == *name_) {
	match = 1;
    } else if (aliases_ != nil) {
	int possible_match = 2;
	for (ListItr(UniqueStringList) i(*aliases_); i.more(); i.next()) {
	    if (name == *i.cur()) {
		match = possible_match;
		break;
	    }
	    ++possible_match;
	}
    }
    return match;
}

/*
 * Compare to lists of strings.
 */

boolean StyleRep::same_path(
    const UniqueStringList& p1, const UniqueStringList& p2
) {
    if (p1.count() != p2.count()) {
	return false;
    }
    ListItr(UniqueStringList) i1(p1);
    ListItr(UniqueStringList) i2(p2);
    for (; i1.more(); i1.next(), i2.next()) {
	if (*i1.cur() != *i2.cur()) {
	    return false;
	}
    }
    return true;
}

void StyleRep::delete_path(UniqueStringList* list) {
    if (list != nil) {
	for (ListItr(UniqueStringList) i(*list); i.more(); i.next()) {
	    UniqueString* s = i.cur();
	    delete s;
	}
	delete list;
    }
}

/*
 * Clear out any cached information about this style.
 */

void StyleRep::clear_info() { }

void StyleRep::modify() {
    modified_ = true;
    if (observers_ != nil) {
	observers_->execute();
    }
    if (children_ != nil) {
	for (ListItr(StyleList) i(*children_); i.more(); i.next()) {
	    i.cur()->rep_->modify();
	}
    }
}

void StyleRep::update() {
    if (!modified_) {
	return;
    }
    clear_info();
    if (parent_ != nil) {
	parent_->rep_->update();
    }
    modified_ = false;
}

void Style::remove_attribute(const String& name) {
    StyleRep& s = *rep_;
    s.update();
    if (s.table_ == nil) {
	return;
    }
    String tail(name);
    int priority = 0;
    UniqueStringList* path = s.parse_name(tail, priority);
    if (path == nil) {
	return;
    }
    UniqueString u(tail);
    StyleAttributeTableEntry* e = s.find_entry(u);
    long p = path->count();
    if (e != nil && e->used_ > p) {
	StyleAttributeList* a = e->entries_[p];
	if (a != nil) {
	    for (ListUpdater(StyleAttributeList) i(*a); i.more(); i.next()) {
		StyleAttribute* attr = i.cur();
		if (s.same_path(*attr->path_, *path)) {
		    s.delete_attribute(attr);
		    i.remove_cur();
		    break;
		}
	    }
	    if (a->count() == 0) {
		delete a;
		e->entries_[p] = nil;
	    }
	}
    }
    s.delete_path(path);
}

void StyleRep::delete_attribute(StyleAttribute* a) {
    delete a->name_;
    list_->remove(a->index_);
    long n = list_->count();
    for (long i = a->index_; i < n; i++) {
	StyleAttribute* attr = list_->item(i);
	attr->index_ -= 1;
    }
    delete_path(a->path_);
    delete a->value_;
    delete a->observers_;
    delete a;
}

/*
 * Return number of attributes.
 */

long Style::attribute_count() const {
    StyleAttributeList* list = rep_->list_;
    return list == nil ? 0 : list->count();
}

/*
 * Return an attribute name and value for a given index.
 */

boolean Style::attribute(long i, String& name, String& value) const {
    StyleAttributeList* list = rep_->list_;
    if (list == nil || i < 0 || i >= list->count()) {
	return false;
    }
    StyleAttribute& a = *list->item(i);
    name = *a.name_;
    value = *a.value_;
    return true;
}

/*
 * Convenient short-hand
 */

void Style::attribute(const char* name, const char* value, int priority) {
    attribute(String(name), String(value), priority);
}

void Style::remove_attribute(const char* name) {
    remove_attribute(String(name));
}

void Style::load_file(const String& filename, int priority) {
    InputFile* f = InputFile::open(filename);
    if (f == nil) {
	return;
    }
    const char* start;
    int len = f->read(start);
    if (len > 0) {
	load_list(String(start, len), priority);
    }
    f->close();
    delete f;
}

void Style::load_list(const String& str, int priority) {
    const char* p = str.string();
    const char* q = p + str.length();
    const char* start = p;
    for (; p < q; p++) {
	if (*p == '\n') {
	    if (p > start && *(p-1) != '\\') {
		load_property(String(start, p - start), priority);
		start = p + 1;
	    }
	}
    }
}

void Style::load_property(const String& prop, int priority) {
    StyleRep& s = *rep_;
    String p(s.strip(prop));
    if (p.length() == 0 || p[0] == '!') {
	return;
    }
    int colon = p.index(':');
    if (colon < 0) {
	s.missing_colon(p);
    } else {
	String name(s.strip(p.left(colon)));
	String value(s.strip(p.right(colon + 1)));
	if (name.length() <= 0) {
	    s.bad_property_name(name);
	} else if (value.length() <= 0) {
	    s.bad_property_value(value);
	} else {
	    attribute(name, value, priority);
	}
    }
}

String StyleRep::strip(const String& s) {
    int i = 0;
    int len = s.length();
    for (i = 0; i < len && isspace(s[i]); i++);
    int j = len - 1;
    for (; j >= 0 && isspace(s[j]); j--);
    return s.substr(i, j - i + 1);
}

/*
 * Errors are nops for now.
 */

void StyleRep::missing_colon(const String&) { }
void StyleRep::bad_property_name(const String&) { }
void StyleRep::bad_property_value(const String&) { }

void Style::add_trigger(const String& name, Action* action) {
    String v("undefined");
    StyleAttribute* a = rep_->add_attribute(name, v, -1000);
    if (a != nil) {
	if (a->observers_ == nil) {
	    a->observers_ = new Macro;
	}
	a->observers_->append(action);
    }
}

/*
 * If the action parameter is nil, remove all triggers associated
 * with the name.
 */

void Style::remove_trigger(const String& name, Action* action) {
    String v("undefined");
    StyleAttribute* a = rep_->add_attribute(name, v, -1000);
    if (a != nil) {
	Macro* m = a->observers_;
	if (action == nil) {
	    delete m;
	    a->observers_ = nil;
	} else {
	    MacroIndex mcount = m->count();
	    for (MacroIndex i = 0; i < mcount; i++) {
		if (m->action(i) == action) {
		    m->remove(i);
		    break;
		}
	    }
	}
    }
}

void Style::add_trigger_any(Action* action) {
    StyleRep& s = *rep_;
    if (s.observers_ == nil) {
	s.observers_ = new Macro;
    }
    s.observers_->append(action);
}

void Style::remove_trigger_any(Action* action) {
    StyleRep& s = *rep_;
    Macro* m = s.observers_;
    MacroIndex mcount = m->count();
    for (MacroIndex i = 0; i < mcount; i++) {
	if (m->action(i) == action) {
	    m->remove(i);
	    break;
	}
    }
}

void Style::add_trigger(const char* name, Action* a) {
    add_trigger(String(name), a);
}

void Style::remove_trigger(const char* name, Action* a) {
    remove_trigger(String(name), a);
}

/*
 * Find the value bound to a given name, if any.
 */

boolean Style::find_attribute(const String& name, String& value) const {
    StyleRep* s = rep_;
    s->update();
    UniqueString uname(name);
    StyleAttributeTableEntry* e = s->find_entry(uname);
    if (e != nil) {
	StyleAttributeList* list = e->entries_[0];
	if (list != nil && list->count() != 0) {
	    value = *list->item(0)->value_;
	    return true;
	}
    }

    StyleList sl(20);
    Style* this_style = (Style*)this;
    sl.prepend(this_style);
    for (Style* style = s->parent_; style != nil; style = s->parent_) {
	s = style->rep_;
	e = s->find_entry(uname);
	if (e != nil) {
	    if (e->used_ > 0 && s->wildcard_match(*e, sl, value)) {
		return true;
	    }
	    StyleAttributeList* list = e->entries_[0];
	    if (list != nil) {
		value = *list->item(0)->value_;
		return true;
	    }
	}
	sl.prepend(style);
    }
    return false;
}

StyleAttributeTableEntry* StyleRep::find_entry(const UniqueString& name) {
    StyleAttributeTableEntry* e;
    if (table_ != nil) {
	/* avoid && to workaround cfront bug */
	if (table_->find(e, name)) {
	    return e;
	}
    }
    return nil;
}

/*
 * Check if the given table entry contains a match for the
 * given list of styles and if so copy the value.
 *
 * We start from the end of the style list so that we can find
 * the closest match.
 */

boolean StyleRep::wildcard_match(
    const StyleAttributeTableEntry& e, const StyleList& sl, String& value
) {
    long n = sl.count();
    for (long i = n - 1; i >= 0; i--) {
	StyleRep& s = *sl.item(i)->rep_;
	if (s.name_ != nil && wildcard_match_name(*s.name_, e, sl, i, value)) {
	    return true;
	}
	UniqueStringList* list = s.aliases_;
	if (list != nil) {
	    for (ListItr(UniqueStringList) a(*list); a.more(); a.next()) {
		if (wildcard_match_name(*a.cur(), e, sl, i, value)) {
		    return true;
		}
	    }
	}
    }
    return false;
}

boolean StyleRep::wildcard_match_name(
    const UniqueString& name, const StyleAttributeTableEntry& e,
    const StyleList& sl, long s_index, String& value
) {
    long n = Math::min(s_index + 1, e.used_ - 1);
    for (long i = n; i >= 1; i--) {
	StyleAttributeList* list = e.entries_[i];
	if (list != nil) {
	    boolean found_match = false;
	    int best_match = 0;
	    for (ListItr(StyleAttributeList) a(*list); a.more(); a.next()) {
		StyleAttribute& attr = *a.cur();
		const UniqueStringList& path = *attr.path_;
		if (name == *path.item(i - 1)) {
		    if (i == 1) {
			value = *attr.value_;
			return true;
		    } else if (s_index != 0) {
			int new_match = finish_match(sl, s_index-1, path, i-2);
			if (new_match > best_match) {
			    found_match = true;
			    best_match = new_match;
			    value = *attr.value_;
			}
		    }
		}
	    }
	    if (found_match) {
		return true;
	    }
	}
    }
    return false;
}

int StyleRep::finish_match(
    const StyleList& sl, long s_index,
    const UniqueStringList& path, long p_index
) {
    int matched = 0;
    long s_cur = s_index;
    long p_cur = p_index;
    while (p_cur >= 0 && s_cur >= 0) {
	StyleRep& s = *sl.item(s_cur)->rep_;
	int m = s.match_name(*path.item(p_cur));
	if (m != 0) {
	    --p_cur;
	    matched += m;
	}
	--s_cur;
    }
    return matched;
}

/*
 * Short-hand
 */

boolean Style::find_attribute(const char* name, String& value) const {
    return find_attribute(String(name), value);
}

boolean Style::find_attribute(const String& name, long& value) const {
    String v;
    return find_attribute(name, v) && v.convert(value);
}

boolean Style::find_attribute(const char* name, long& value) const {
    return find_attribute(String(name), value);
}

boolean Style::find_attribute(const String& name, double& value) const {
    String v;
    return find_attribute(name, v) && v.convert(value);
}

boolean Style::find_attribute(const char* name, double& value) const {
    return find_attribute(String(name), value);
}

boolean Style::find_attribute(const String& name, Coord& value) const {
    String v;
    if (!find_attribute(name, v)) {
	return false;
    }
    String units(v);
    Coord pts = 1.0;
    const char* p = v.string();
    const char* end = p + v.length();
    if (p < end && (*p == '-' || *p == '+')) {
	++p;
    }
    boolean dot = false;
    for (; p < end; p++) {
	if (!dot && *p == '.') {
	    dot = true;
	} else if (!isspace(*p) && !isdigit(*p)) {
	    int i = p - v.string();
	    units.set_to_right(i);
	    if (units == "mm") {
		pts = 72.0 / 25.4;
	    } else if (units == "cm") {
		pts = 72.0 / 2.54;
	    } else if (units == "in") {
		pts = 72.0;
	    } else if (units != "pt") {
		return false;
	    }
	    v.set_to_left(i);
	    break;
	}
    }
    if (v.convert(value)) {
	value *= pts;
	return true;
    }
    return false;
}

boolean Style::find_attribute(const char* name, Coord& value) const {
    return find_attribute(String(name), value);
}

boolean Style::value_is_on(const String& s) const {
    String v;
    if (!find_attribute(s, v)) {
	return false;
    }
    return v.case_insensitive_equal("on") || v.case_insensitive_equal("true");
}

boolean Style::value_is_on(const char* s) const {
    return value_is_on(String(s));
}
