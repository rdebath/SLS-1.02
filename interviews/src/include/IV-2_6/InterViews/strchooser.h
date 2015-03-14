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
 * StringChooser - a dialog box that provides a keyboard and mouse interface
 * to selecting from a list of strings.
 */

#ifndef ivlook2_6_strchooser_h
#define ivlook2_6_strchooser_h

#include <IV-2_6/InterViews/dialog.h>

#include <IV-2_6/_enter.h>

class StringBrowser;
class StringEditor;

class StringChooser : public Dialog {
public:
    StringChooser(
        ButtonState*, int rows, int cols, const char* sample, Alignment =Center
    );
    StringChooser(
        const char* name,
        ButtonState*, int, int, const char*, Alignment = Center
    );
    virtual ~StringChooser();

    void Select(int = -1);
    void Select(int left, int right);
    void SelectMessage();
    void Message(const char*);

    virtual const char* Choice();
    virtual boolean Accept();
    virtual void Handle(Event&);
protected:
    StringChooser(ButtonState*, Alignment = Center);
    void Init(StringEditor*, StringBrowser*);
    void Forward(Event&);

    virtual void SwitchFocus();
    virtual boolean CanFocus(Interactor*);
    virtual void HandleFocus();
    virtual void UpdateEditor();
    virtual void UpdateBrowser();
protected:
    StringEditor* _sedit;
    StringBrowser* _browser;
    Interactor* _focus;
};

#include <IV-2_6/_leave.h>

#endif
