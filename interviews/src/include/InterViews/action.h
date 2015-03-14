/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
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
 * Action - something to do
 */

#ifndef iv_action_h
#define iv_action_h

#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class Action : public Resource {
protected:
    Action();
    virtual ~Action();
public:
    virtual void execute() = 0;
};

/*
 * Macro - list of actions
 */

typedef long MacroIndex;

class MacroActionList;

class Macro : public Action {
public:
    Macro(Action* = nil, Action* = nil, Action* = nil, Action* = nil);
    virtual ~Macro();

    virtual void prepend(Action*);
    virtual void append(Action*);
    virtual void insert(MacroIndex, Action*);
    virtual void remove(MacroIndex);

    virtual MacroIndex count() const;
    virtual Action* action(MacroIndex) const;

    virtual void execute();
private:
    MacroActionList* list_;
};

/*
 * Action denoted by an object and member function to call on the object.
 */

#if defined(__STDC__) || defined(__ANSI_CPP__)
#define __ActionCallback(T) T##_ActionCallback
#define ActionCallback(T) __ActionCallback(T)
#define __ActionMemberFunction(T) T##_ActionMemberFunction
#define ActionMemberFunction(T) __ActionMemberFunction(T)
#else
#define __ActionCallback(T) T/**/_ActionCallback
#define ActionCallback(T) __ActionCallback(T)
#define __ActionMemberFunction(T) T/**/_ActionMemberFunction
#define ActionMemberFunction(T) __ActionMemberFunction(T)
#endif

#define declareActionCallback(T) \
typedef void (T::*ActionMemberFunction(T))(); \
class ActionCallback(T) : public Action { \
public: \
    ActionCallback(T)(T*, ActionMemberFunction(T)); \
\
    virtual void execute(); \
private: \
    T* obj_; \
    ActionMemberFunction(T) func_; \
};

#define implementActionCallback(T) \
ActionCallback(T)::ActionCallback(T)(T* obj, ActionMemberFunction(T) func) { \
    obj_ = obj; \
    func_ = func; \
} \
\
void ActionCallback(T)::execute() { (obj_->*func_)(); }

#include <InterViews/_leave.h>

#endif
