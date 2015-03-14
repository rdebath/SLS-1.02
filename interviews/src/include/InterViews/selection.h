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

#ifndef iv_selection_h
#define iv_selection_h

#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class Display;
class SelectionHandler;
class SelectionManagerRep;
class String;

class SelectionManager : public Resource {
public:
    SelectionManager(Display*, const char*);
    SelectionManager(Display*, const String&);
    virtual ~SelectionManager();

    virtual void own(
        SelectionHandler* convert,
        SelectionHandler* lose = nil,
        SelectionHandler* done = nil
    );
    virtual void put_value(const void*, int length, int format = 8);
    virtual void retrieve(
        const String& type, SelectionHandler* ok, SelectionHandler* fail = nil
    );
    virtual void get_value(String*& type, void*&, int&, int& format);

    SelectionManagerRep* rep() const;
private:
    SelectionManagerRep* rep_;
};

class SelectionHandler : public Resource {
protected:
    SelectionHandler();    
public:
    virtual ~SelectionHandler();

    virtual void handle(SelectionManager*) = 0;
};

#if defined(__STDC__) || defined(__ANSI_CPP__)
#define __SelectionCallback(T) T##_SelectionCallback
#define SelectionCallback(T) __SelectionCallback(T)
#define __SelectionMemberFunction(T) T##_SelectionMemberFunction
#define SelectionMemberFunction(T) __SelectionMemberFunction(T)
#else
#define __SelectionCallback(T) T/**/_SelectionCallback
#define SelectionCallback(T) __SelectionCallback(T)
#define __SelectionMemberFunction(T) T/**/_SelectionMemberFunction
#define SelectionMemberFunction(T) __SelectionMemberFunction(T)
#endif

#define declareSelectionCallback(T) \
typedef void (T::*SelectionMemberFunction(T))(SelectionManager*); \
class SelectionCallback(T) : public SelectionHandler { \
public: \
    SelectionCallback(T)(T*, SelectionMemberFunction(T)); \
\
    virtual void handle(SelectionManager*); \
private: \
    T* obj_; \
    SelectionMemberFunction(T) func_; \
};

#define implementSelectionCallback(T) \
SelectionCallback(T)::SelectionCallback(T)( \
    T* obj, SelectionMemberFunction(T) func \
) { \
    obj_ = obj; \
    func_ = func; \
} \
\
void SelectionCallback(T)::handle(SelectionManager* s) { (obj_->*func_)(s); }

#endif
