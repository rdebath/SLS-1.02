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
 * Application
 */

#ifndef Application_h
#define Application_h

#include <InterViews/boolean.h>

class ApplicationViewerInfo;
class ApplicationViewerInfo_List;
class Document;
class DocumentViewer;
class Session;

const int Confirmed = 1;
const int NotConfirmed = 2;
const int Cancelled = 3;

class Application {
public:
    Application ();
    virtual ~Application ();

    virtual const char* choose (
        DocumentViewer*, const char* prompt, const char* filter
    );
    virtual const char* ask (
        DocumentViewer*, const char* prompt, const char* initial
    );
    virtual int confirm (DocumentViewer*, const char* prompt);
    virtual void report (DocumentViewer*, const char* prompt);
    virtual void complain (DocumentViewer*, const char* prompt);

    virtual void open (DocumentViewer*);
    virtual void close (DocumentViewer*);

    virtual Document* read (const char* name);
    virtual void write (Document*, const char* name);

     virtual boolean file_path (
        const char* name, const char* ext, const char* pathlist, char* path
    );

    virtual boolean command (const char* command);
protected:
    Session* _session;
    ApplicationViewerInfo_List* _viewer;
private:
    void raise_for_post(ApplicationViewerInfo&);
};

#endif
