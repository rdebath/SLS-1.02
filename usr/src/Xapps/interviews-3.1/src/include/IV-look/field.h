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
 * FieldEditor -- simple editor for text fields
 */

#ifndef ivlook_field_h
#define ivlook_field_h

#include <InterViews/input.h>
#include <InterViews/resource.h>

class FieldEditor;
class FieldEditorImpl;
class String;
class Style;
class WidgetKit;

class FieldEditorAction : public Resource {
protected:
    FieldEditorAction();
    virtual ~FieldEditorAction();
public:
    virtual void accept(FieldEditor*);
    virtual void cancel(FieldEditor*);
};

#if defined(__STDC__) || defined(__ANSI_CPP__)
#define __FieldEditorCallback(T) T##_FieldEditorCallback
#define FieldEditorCallback(T) __FieldEditorCallback(T)
#define __FieldEditorMemberFunction(T) T##_FieldEditorMemberFunction
#define FieldEditorMemberFunction(T) __FieldEditorMemberFunction(T)
#else
#define __FieldEditorCallback(T) T/**/_FieldEditorCallback
#define FieldEditorCallback(T) __FieldEditorCallback(T)
#define __FieldEditorMemberFunction(T) T/**/_FieldEditorMemberFunction
#define FieldEditorMemberFunction(T) __FieldEditorMemberFunction(T)
#endif

#define declareFieldEditorCallback(T) \
typedef void (T::*FieldEditorMemberFunction(T))(FieldEditor*); \
class FieldEditorCallback(T) : public FieldEditorAction { \
public: \
    FieldEditorCallback(T)( \
	T*, FieldEditorMemberFunction(T) accept, \
	FieldEditorMemberFunction(T) cancel \
    ); \
    virtual ~FieldEditorCallback(T)(); \
\
    virtual void accept(FieldEditor*); \
    virtual void cancel(FieldEditor*); \
private: \
    T* obj_; \
    FieldEditorMemberFunction(T) accept_; \
    FieldEditorMemberFunction(T) cancel_; \
};

#define implementFieldEditorCallback(T) \
FieldEditorCallback(T)::FieldEditorCallback(T)( \
    T* obj, FieldEditorMemberFunction(T) accept, \
    FieldEditorMemberFunction(T) cancel \
) { \
    obj_ = obj; \
    accept_ = accept; \
    cancel_ = cancel; \
} \
\
FieldEditorCallback(T)::~FieldEditorCallback(T)() { } \
\
void FieldEditorCallback(T)::accept(FieldEditor* f) { (obj_->*accept_)(f); } \
void FieldEditorCallback(T)::cancel(FieldEditor* f) { (obj_->*cancel_)(f); }

class FieldEditor : public InputHandler {
public:
    FieldEditor(
	const String& sample, WidgetKit*, Style*, FieldEditorAction* = nil
    );
    virtual ~FieldEditor();

    virtual void undraw();

    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);
    virtual void keystroke(const Event&);
    virtual InputHandler* focus_in();
    virtual void focus_out();

    virtual void field(const char*);
    virtual void field(const String&);

    virtual void select(int pos);
    virtual void select(int left, int right);

    virtual void edit();
    virtual void edit(const char*, int left, int right);
    virtual void edit(const String&, int left, int right);

    virtual const String* text() const;
private:
    FieldEditorImpl* impl_;
};

#endif
