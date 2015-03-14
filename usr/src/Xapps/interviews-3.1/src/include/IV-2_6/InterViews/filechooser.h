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
 * FileChooser - a StringChooser for choosing files from a directory.
 */

#ifndef ivlook2_6_filechooser_h
#define ivlook2_6_filechooser_h

#include <IV-2_6/InterViews/strchooser.h>

#include <IV-2_6/_enter.h>

class Button;
class FileBrowser;
class MarginFrame;

class FileChooser : public StringChooser {
public:
    FileChooser(
        const char* title = "Please select a file:",
        const char* subtitle = "",
        const char* dir = "~",
        int rows = 10, int cols = 24,
        const char* acceptLabel = " Open ",
        Alignment = Center
    );
    FileChooser(
        const char* name, const char* title, const char* subtitle,
        const char* dir, int, int, const char* acceptLabel, Alignment
    );
    virtual ~FileChooser();

    void SetTitle(const char*);
    void SetSubtitle(const char*);
    void SelectFile();

    virtual boolean Accept();
    virtual void Update();
protected:
    FileChooser(ButtonState*, Alignment = Center);
    FileChooser(ButtonState*, const char* dir, int rows, int cols, Alignment);
    void Init(const char*, const char*);
    Interactor* Interior(const char* acptlbl);
    Interactor* AddScroller(Interactor*);
    FileBrowser* browser();

    virtual void UpdateEditor();
    virtual void UpdateBrowser();
protected:
    MarginFrame* title, *subtitle;
};

inline FileBrowser* FileChooser::browser () { return (FileBrowser*) _browser; }

#include <IV-2_6/_leave.h>

#endif
