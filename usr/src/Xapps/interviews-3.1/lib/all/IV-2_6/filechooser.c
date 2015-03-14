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

#include <InterViews/event.h>
#include <IV-2_6/InterViews/adjuster.h>
#include <IV-2_6/InterViews/border.h>
#include <IV-2_6/InterViews/button.h>
#include <IV-2_6/InterViews/filebrowser.h>
#include <IV-2_6/InterViews/filechooser.h>
#include <IV-2_6/InterViews/frame.h>
#include <IV-2_6/InterViews/scrollbar.h>
#include <IV-2_6/InterViews/streditor.h>
#include <IV-2_6/InterViews/box.h>
#include <IV-2_6/InterViews/glue.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/message.h>
#include <OS/math.h>
#include <string.h>

#include <IV-2_6/_enter.h>

FileChooser::FileChooser(
    const char* t, const char* subt, const char* d, 
    int r, int c, const char* acceptLabel, Alignment a
) : StringChooser(new ButtonState, a) {
    FileBrowser* fb = new FileBrowser(state, d, r, c);
    StringChooser::Init(new StringEditor(state, fb->GetDirectory()), fb);
    Init(t, subt);
    Insert(Interior(acceptLabel));
}

FileChooser::FileChooser(
    const char* name, const char* t, const char* subt, const char* d, 
    int r, int c, const char* acceptLabel, Alignment a
) : StringChooser(new ButtonState, a) {
    SetInstance(name);
    FileBrowser* fb = new FileBrowser(state, d, r, c);
    StringChooser::Init(new StringEditor(state, fb->GetDirectory()), fb);
    Init(t, subt);
    Insert(Interior(acceptLabel));
}

FileChooser::FileChooser(
    ButtonState* bs, const char* d, int r, int c, Alignment a
) : StringChooser(bs, a) {
    FileBrowser* fb = new FileBrowser(state, d, r, c);
    StringChooser::Init(new StringEditor(state, fb->GetDirectory()), fb);
}    

FileChooser::FileChooser(
    ButtonState* bs, Alignment a
) : StringChooser(bs, a) { }

FileChooser::~FileChooser() { }

void FileChooser::Init(const char* t, const char* subt) {
    if (*t == '\0') {
        title = new MarginFrame(new VGlue(0, 0));
    } else {
        title = new MarginFrame(new class Message(t));
    }
    if (*subt == '\0') {
        subtitle = new MarginFrame(new VGlue(0, 0));
    } else {
        subtitle = new MarginFrame(new class Message(subt));
    }
}

void FileChooser::SelectFile() {
    const char* path = _sedit->Text();
    int left = strlen(browser()->ValidDirectories(path));
    int right = strlen(path);

    Select(left, right);
}

void FileChooser::Update () {
    _browser->Update();
}

void FileChooser::UpdateEditor() {
    int index = browser()->Selection();

    if (index >= 0) {
        _sedit->Message(browser()->Path(index));
        browser()->UnselectAll();
    } else {
        _sedit->Message(browser()->Normalize(_sedit->Text()));
    }
    SelectFile();
}

void FileChooser::UpdateBrowser() {
    browser()->SetDirectory(Choice());
}

boolean FileChooser::Accept() {
    boolean accepted, dirSelected;

    do {
        accepted = StringChooser::Accept();
        dirSelected = browser()->IsADirectory(Choice());
    } while (accepted && dirSelected);

    return accepted;
}

static void ChangeMsg (const char* name, MarginFrame* frame) {
    Interactor* msg;

    if (*name == '\0') {
        msg = new VGlue(0, 0);
    } else {
        msg = new Message(name);
    }
    frame->Insert(msg);
    frame->Change(msg);
}

void FileChooser::SetTitle(const char* name) {
    ChangeMsg(name, title);
}

void FileChooser::SetSubtitle(const char* name) {
    ChangeMsg(name, subtitle);
}

Interactor* FileChooser::AddScroller(Interactor* i) {
    return new HBox(
        new MarginFrame(i, 2),
        new VBorder,
	new VScrollBar(i)
    );
}

Interactor* FileChooser::Interior(const char* acptLbl) {
    const int space = Math::round(.5*cm);
    VBox* titleblock = new VBox(
        new HBox(title, new HGlue),
        new HBox(subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space, 0),
            new Frame(new MarginFrame(_sedit, 2)),
            new VGlue(space, 0),
            new Frame(AddScroller(browser())),
            new VGlue(space, 0),
            new HBox(
                new VGlue(space, 0),
                new HGlue,
                new PushButton("Cancel", state, '\007'),
                new HGlue(space, 0),
                new PushButton(acptLbl, state, '\r')
            )
        ), space, space/2, 0
    );
}
