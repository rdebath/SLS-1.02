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
 * FileChooser -- select a file
 */

#ifndef ivlook_fchooser_h
#define ivlook_fchooser_h

#include <InterViews/dialog.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class FileChooser;
class FileChooserImpl;
class String;
class WidgetKit;

class FileChooserAction : public Resource {
protected:
    FileChooserAction();
    virtual ~FileChooserAction();
public:
    virtual void execute(FileChooser*, boolean accept);
};

#if defined(__STDC__) || defined(__ANSI_CPP__)
#define FileChooserCallback(T) T##_FileChooserCallback
#define FileChooserMemberFunction(T) T##_FileChooserMemberFunction
#else
#define FileChooserCallback(T) T/**/_FileChooserCallback
#define FileChooserMemberFunction(T) T/**/_FileChooserMemberFunction
#endif

#define declareFileChooserCallback(T) \
typedef void (T::*FileChooserMemberFunction(T))(FileChooser*, boolean); \
class FileChooserCallback(T) : public FileChooserAction { \
public: \
    FileChooserCallback(T)(T*, FileChooserMemberFunction(T)); \
    virtual ~FileChooserCallback(T)(); \
\
    virtual void execute(FileChooser*, boolean accept); \
private: \
    T* obj_; \
    FileChooserMemberFunction(T) func_; \
};

#define implementFileChooserCallback(T) \
FileChooserCallback(T)::FileChooserCallback(T)( \
    T* obj, FileChooserMemberFunction(T) func \
) { \
    obj_ = obj; \
    func_ = func; \
} \
\
FileChooserCallback(T)::~FileChooserCallback(T)() { } \
\
void FileChooserCallback(T)::execute(FileChooser* f, boolean accept) { \
    FileChooserMemberFunction(T) pf = func_; \
    (obj_->*pf)(f, accept); \
}

class FileChooser : public Dialog {
public:
    FileChooser(
	const String& dir, WidgetKit*, Style*, FileChooserAction* = nil
    );
    virtual ~FileChooser();

    virtual const String* selected() const;
    virtual void reread();
    virtual void dismiss(boolean);
private:
    FileChooserImpl* impl_;
};

#include <InterViews/_leave.h>

#endif
