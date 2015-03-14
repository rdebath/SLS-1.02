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
 * FileBrowser - a StringBrowser that displays file names.
 */

#ifndef ivlook2_6_filebrowser_h
#define ivlook2_6_filebrowser_h

#include <IV-2_6/InterViews/strbrowser.h>

#include <IV-2_6/_enter.h>

class FBDirectory;
class Regexp;

class FileBrowser : public StringBrowser {
public:
    FileBrowser(
        ButtonState*, const char* dir = "~", int rows = 10, int cols = 24,
        boolean uniqueSel = true, int highlight = Reversed,
        const char* done = SBDone
    );
    FileBrowser(
        const char* name, ButtonState*, const char* = "~", int = 10,
	int = 24, boolean = true, int = Reversed, const char* = SBDone
    );
    virtual ~FileBrowser();

    boolean IsADirectory(const char*);
    boolean SetDirectory(const char*);
    const char* GetDirectory();
    const char* ValidDirectories(const char*);
    const char* Normalize(const char*);
    const char* Path(int index);

    void RereadDirectory();
    void SetTextFilter(const char*);
    void SetDirectoryTextFilter(const char*);
    void SetModeFilter(int);
    void SetDirectoryModeFilter(int);

    virtual void Update();
protected:
    void UpdateStrings();
    virtual boolean Acceptable(const char*);
private:
    void Init(const char*);

    FBDirectory* dir;
    char* lastpath;
    Regexp* regexp;
    Regexp* directory_regexp;
    int mode;
    int directory_mode;
};

inline const char* FileBrowser::GetDirectory () { return lastpath; }

#include <IV-2_6/_leave.h>

#endif
