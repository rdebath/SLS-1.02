/*
 * Copyright (c) 1989 Stanford University
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
 * IClass implementation.
 */

#include "classbuffer.h"
#include "classeditor.h"
#include "classinfo.h"
#include "dialogs.h"
#include "globals.h"
#include "iclass.h"

#include <InterViews/adjuster.h>
#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/button.h>
#include <InterViews/compeditor.h>
#include <InterViews/filebrowser.h>
#include <InterViews/font.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/menu.h>
#include <InterViews/message.h>
#include <InterViews/regexp.h>
#include <InterViews/painter.h>
#include <InterViews/scrollbar.h>
#include <InterViews/sensor.h>
#include <InterViews/strbrowser.h>
#include <InterViews/streditor.h>
#include <InterViews/textbuffer.h>
#include <InterViews/world.h>
#include <OS/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

/*****************************************************************************/

static const float fspace = .375;                // space in cm

static const char* NONE = "<none>";

static const int BUFFERSIZE = 100;
static const int FILENAMESIZE = 100;
static const int MINTEXTSIZE = 10000;

#define BSRCH_LBL "^R"
#define BSRCH_CODE '\022'

#define FSRCH_LBL "^S"
#define FSRCH_CODE '\023'

#define GOTO_LBL "^N"
#define GOTO_CODE '\016'

#define QUIT_LBL "^Q"
#define QUIT_CODE '\021'

#define SCAN_LBL "^V"
#define SCAN_CODE '\026'

#define CLEAR_LBL "^C"
#define CLEAR_CODE '\003'

/*****************************************************************************/

class Command : public MenuItem {
public:
    Command(const char* lbl, const char* klbl, char kcode, IClass*);

    virtual void Do();
private:
    IClass* _iclass;
    char _kcode;
};

Command::Command (
    const char* lbl, const char* klbl, char kcode, IClass* s
) : MenuItem((Interactor*) nil) {
    Insert(
       new HBox(
            new Message(lbl, Left, 2, hfil),
            new Message("   ", Center, 2, hfil),
            new Message(klbl, Right, 2, hfil)
        )
    );
    _iclass = s;
    _kcode = kcode;
}

void Command::Do () { _iclass->Command(_kcode); }

struct MenuInfo {
    const char* _lbl;
    const char* _klbl;
    char _kcode;
};

/*****************************************************************************/

IClass::IClass (ClassBuffer* cb) {
    SetClassName("IClass");
    Init(cb);
    Visit("");
    _editor->Edit(_text);
}

IClass::~IClass () {
    delete _cbuf;
    delete _buf;
    delete _text;
    delete _lastFile;
    _state->Detach(this);
    Unref(_state);

    if (_scanner != nil) delete _scanner;
    if (_fwdSearch != nil) delete _fwdSearch;
    if (_bwdSearch != nil) delete _bwdSearch;
    if (_goto != nil) delete _goto;
    if (_completions != nil) delete _completions;
}

void IClass::Init (ClassBuffer* cb) {
    _cbuf = cb;
    _state = new ButtonState;
    _state->Attach(this);
    _curClass = new CompletionEditor(_state, "");
    _focus = _curClass;
    _classes = new StringBrowser(_state, 5, 24);
    _parents = new StringBrowser(_state, 2, 24);
    _children = new StringBrowser(_state, 5, 24);
    _fileIndic = new MarginFrame(new Message("testing"));
    _editor = new ClassEditor(_state, 24, 80, 8, Reversed);
    _editor->SetScrollAlignment(TopLeft);

    _scanner = new FileDialog("Scan through files/directories:", ".");
    _fwdSearch = nil;
    _bwdSearch = nil;
    _goto = nil;

    _buf = nil;
    _text = nil;
    _lastFile = nil;
    _completions = nil;

    input = new Sensor;
    input->Catch(KeyEvent);

    Insert(Interior());
    UpdateClassesBrowser();
}

void IClass::Visit (const char* filename) {
    if (_lastFile == nil || strcmp(_lastFile, filename) != 0) {
        class Message* msg = new Message(_scanner->FullPath(filename));
        _fileIndic->Insert(msg);
        _fileIndic->Change(msg);

        delete _lastFile;
        _lastFile = strnew(filename);

        delete _buf;
        delete _text;

        FILE* f = nil;

        if (strlen(filename) != 0) {
	    /* cast to work around bug in prototype on some systems */
            f = fopen((char*)filename, "r");
        }

        if (f != nil) {
            struct stat filestats;
            stat(filename, &filestats);
            _bufsize = max(round(filestats.st_size * 1.2), MINTEXTSIZE);
            _buf = new char[_bufsize];
            char* b = _buf;
            int remaining = _bufsize;
            while (remaining > 1 && fgets(b, remaining, f) != nil) {
                int l = strlen(b);
                remaining -= l;
                b += l;
            }
            fclose(f);
            _text = new TextBuffer(_buf, b-_buf, _bufsize);

        } else {
            _bufsize = MINTEXTSIZE;
            _buf = new char[_bufsize];
            _text = new TextBuffer(_buf, 0, _bufsize);
        }
    }
}

void IClass::Run () {
    Event e;
    int value;

    do {
        Read(e);
	if (e.target == nil) {
	    /* probably quit from window manager */
	    break;
	}
        Handle(e);

        _state->GetValue(value);
    } while (value != QUIT_CODE);
}

void IClass::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.len > 0) {
        char c = e.keystring[0];

        if (!Command(c)) {
            _focus->Handle(e);
        }

    } else if (e.target != this && e.eventType != KeyEvent) {
        boolean focusable =
            e.target == _classes || e.target == _parents ||
            e.target == _children || e.target == _editor;

        if (focusable && e.button == LEFTMOUSE) {
            UnselectCurClass();
            UnselectBrowsers();
            _focus = e.target;
        }
        e.target->Handle(e);
    }
}

boolean IClass::Command (char c) {
    boolean executed = true;

    switch (c) {
        case BSRCH_CODE:         BackwardSearchCmd(); break;
        case FSRCH_CODE:         ForwardSearchCmd(); break;
        case GOTO_CODE:          GotoCmd(); break;
        case QUIT_CODE:          QuitCmd(); break;
        case SCAN_CODE:          ScanCmd(); break;
        case CLEAR_CODE:         ClearCmd(); break;
        default:                 executed = false; break;
    }
    return executed;
}

boolean IClass::ForwardSearch (const char* string) {
    Regexp re(string);
    boolean successful = false;

    if (
        _text->ForwardSearch(&re, _editor->Dot()) >= 0
        || _text->ForwardSearch(&re, _text->BeginningOfText()) >= 0
    ) {
        _editor->Select(re.EndOfMatch(), re.BeginningOfMatch());
        successful = true;
    }
    return successful;
}

boolean IClass::BackwardSearch (const char* string) {
    Regexp re(string);
    boolean successful = false;

    if (
        _text->BackwardSearch(&re, _editor->Mark()) >= 0
        || _text->BackwardSearch(&re, _text->EndOfText()) >= 0
    ) {
        _editor->Select(re.EndOfMatch(), re.BeginningOfMatch());
        successful = true;
    }
    return successful;
}

void IClass::UnselectBrowsers () {
    int index;

    if ((index = _classes->Selection()) >= 0) {
        _classes->UnselectAll();

    } else if ((index = _parents->Selection()) >= 0) {
        _parents->UnselectAll();

    } else if ((index = _children->Selection()) >= 0) {
        _children->UnselectAll();
    }
}

void IClass::Update () {
    int value;
    _state->GetValue(value);

    if (value == '\r') {
        UpdateCurClass();
        UpdateParentBrowser();
        UpdateChildBrowser();

    } else if (value == '\t' || value == '\033') {
        SelectCurClass();
        _focus = _curClass;

    } else if (value == '\007') {
        Complain();
    }

    if (value != QUIT_CODE) {
        UnselectBrowsers();
        _state->SetValue('\0');
    }
}

void IClass::UpdateCurClass () {
    int index;

    if ((index = _classes->Selection()) >= 0) {
        UpdateCurClass(_classes->String(index));

    } else if ((index = _parents->Selection()) >= 0) {
        UpdateCurClass(_parents->String(index));

    } else if ((index = _children->Selection()) >= 0) {
        UpdateCurClass(_children->String(index));

    } else {
        UpdateCurClass(_curClass->Text());
    }
}

void IClass::UpdateCurClass (const char* string) {
    if (strcmp(string, NONE) != 0) {
        if (strcmp(string, _curClass->Text()) != 0) {
            _curClass->Message(string);
        }
        ClassInfo* info = _cbuf->Info(string);

        if (info == nil) {
            SelectCurClass();
            Complain();

        } else {
            Visit(info->Path());
            int line = info->LineNumber();
	    _editor->Edit(_text, _text->LineIndex(line));
            ForwardSearch(info->Name());
            _focus = _editor;
        }
    }
}

void IClass::UpdateClassesBrowser () {
    const char* className;
    int i = 0;

    _classes->Clear();

    int count = 0;
    for (;;) {
        className = _cbuf->Class(i++);
        if (className == nil) {
            break;
        } else {
            _classes->Append(className);
            ++count;
        }
    }
    if (_completions != nil) {
        delete _completions;
    }
    _completions = new const char*[count];
    for (i = 0; i < count; ++i) {
        _completions[i] = _cbuf->Class(i);
    }
    _curClass->Completions(_completions, count);
}

void IClass::UpdateParentBrowser () {
    const char* parent;
    int i = 0;

    _parents->Clear();

    for (;;) {
        parent = _cbuf->Parent(_curClass->Text(), i++);

        if (parent == nil) {
            if (i == 1) {
                _parents->Append(NONE);
            }
            break;
        } else {
            _parents->Append(parent);
        }
    }
}

void IClass::UpdateChildBrowser () {
    const char* child;
    int i = 0;

    _children->Clear();

    for (;;) {
        child = _cbuf->Child(_curClass->Text(), i++);
        if (child == nil) {
            if (i == 1) {
                _children->Append(NONE);
            }
            break;
        } else {
            _children->Append(child);
        }
    }
}

void IClass::SelectCurClass () {
    _curClass->Select(0, strlen(_curClass->Text()));
}

void IClass::UnselectCurClass () {
    _curClass->Select(strlen(_curClass->Text()));
}

Interactor* IClass::Interior () {
    int space = round(fspace*cm);
    const char* showButton = GetAttribute("showButton");
    Interactor* selector = SelectedClass();

    if (showButton != nil && strcmp(showButton, "true") == 0) {
	selector = new HBox(
	    selector,
	    new HGlue(space, space, 0),
	    new PushButton(" Show ", _state, '\r')
	);
    }

    return new VBox(
        new HBox(Commands(), new HGlue),
        new HBorder,
        new MarginFrame(
            new VBox(
                new HBox(
                    Classes(),
                    new HGlue(space, space, 0),
                    new VBox(
			selector,
                        new VGlue(space, space, 0),
                        Parents(),
                        new VGlue(space, space, 0),
                        Children()
                    )
                ),
                new VGlue(space, space, 0),
                Editor()
            ), space, space, 0
        )
    );
}

static MenuInfo fileMenu[] = {
    { "Scan Files/Directories...", SCAN_LBL, SCAN_CODE },
    { "Clear Classes", CLEAR_LBL, CLEAR_CODE },
    { "Quit", QUIT_LBL, QUIT_CODE },
    { nil }
};

static MenuInfo searchMenu[] = {
    { "Forward Search...", FSRCH_LBL, FSRCH_CODE },
    { "Backward Search...", BSRCH_LBL, BSRCH_CODE },
    { "Go to Line...", GOTO_LBL, GOTO_CODE },
    { nil }
};

static PulldownMenu* MakePulldown(
    const char* name, MenuInfo* item, IClass* ic
) {
    PulldownMenu* menu = new PulldownMenu(
	new Message(name, Left, round(.1*cm))
    );

    for (MenuInfo* i = item; i->_lbl != nil; i++) {
        menu->Include(new Command(i->_lbl, i->_klbl, i->_kcode, ic));
    }
    return menu;
}

Interactor* IClass::Commands () {
    MenuBar* menuBar = new MenuBar;

    menuBar->Include(MakePulldown("File", fileMenu, this));
    menuBar->Include(MakePulldown("Search", searchMenu, this));

    return menuBar;
}

Interactor* IClass::Classes () {
    return new VBox(
        new HBox(
            new Message("classes"),
            new HGlue
        ),
        new Frame(
            AddScroller(_classes)
        )
    );
}

Interactor* IClass::SelectedClass () {
    return new VBox(
        new HBox(
            new Message("selected class"),
            new HGlue
        ),
        new Frame(new MarginFrame(_curClass, 2))
    );            
}

Interactor* IClass::Parents () {
    return new VBox(
        new HBox(
            new Message("parents"),
            new HGlue
        ),
        new Frame(AddScroller(_parents))
    );
}

Interactor* IClass::Children () {
    return new VBox(
        new HBox(
            new Message("children"),
            new HGlue
        ),
        new Frame(AddScroller(_children))
    );
}

Interactor* IClass::Editor () {
    HBox* fileBox = new HBox(
        new Message("file: "),
        _fileIndic,
        new HGlue
    );
    fileBox->Propagate(false);

    return new VBox(
        fileBox,
        new Frame(AddScroller(_editor))
    );
}

Interactor* IClass::AddScroller (Interactor* i) {
    return new HBox(
        new MarginFrame(i, 2),
        new VBorder,
	new VScrollBar(i)
    );
}

void IClass::InsertDialog (Interactor* dialog) {
    World* world = GetWorld();

    Coord x, y;
    Align(Center, 0, 0, x, y);
    GetRelative(x, y, world);

    world->InsertTransient(dialog, this, x, y, Center);
}

void IClass::RemoveDialog (Interactor* dialog) {
    GetWorld()->Remove(dialog);
}

void IClass::Complain (const char* msg) {
    if (msg == nil) {
        fprintf(stderr, "%c", '\007');

    } else {
        AcknowledgeDialog complaint(msg);

        InsertDialog(&complaint);
        complaint.Acknowledge();
        RemoveDialog(&complaint);
    }
}

void IClass::ScanCmd () {
    InsertDialog(_scanner);

    if (_scanner->Accept()) {
        FileBrowser* browser = _scanner->GetBrowser();

        for (int i = 0; i < browser->Selections(); ++i) {
            int index = browser->Selection(i);
            _cbuf->Search(browser->Path(index));
        }
    }
    RemoveDialog(_scanner);
    UpdateClassesBrowser();
}

void IClass::ClearCmd () {
    ConfirmDialog dialog("Really clear all classes?");
    InsertDialog(&dialog);
    boolean accepted = dialog.Confirm() == 'y';
    RemoveDialog(&dialog);

    if (accepted) {
        World* world = GetWorld();
        const char* recursive = world->GetAttribute("recursive");
        const char* verbose = world->GetAttribute("verbose");
        const char* CPlusPlusFiles = world->GetAttribute("CPlusPlusFiles");

        delete _cbuf;
        _cbuf = new ClassBuffer(
            strcmp(recursive, "true") == 0, strcmp(verbose, "true") == 0,
            strcmp(CPlusPlusFiles, "true") == 0
        );

        UpdateClassesBrowser();
        UpdateParentBrowser();
        UpdateChildBrowser();
    }
}

void IClass::QuitCmd () { _state->SetValue(QUIT_CODE); }

void IClass::ForwardSearchCmd () {
    if (_fwdSearch == nil) {
        _fwdSearch = new StringDialog("Search forward for: ", round(3*inches));
    }

    InsertDialog(_fwdSearch);
    
    if (_fwdSearch->Accept()) {
        if (ForwardSearch(_fwdSearch->String())) {
            _editor->ScrollToSelection();
        } else {
            Complain();
        }
    }
    RemoveDialog(_fwdSearch);
}

void IClass::BackwardSearchCmd () {
    if (_bwdSearch == nil) {
        _bwdSearch = new StringDialog("Search backward for:", round(3*inches));
    }

    InsertDialog(_bwdSearch);
    
    if (_bwdSearch->Accept()) {
        if (BackwardSearch(_bwdSearch->String())) {
            _editor->ScrollToSelection();
        } else {
            Complain();
        }
    }
    RemoveDialog(_bwdSearch);
}

void IClass::GotoCmd () {
    if (_goto == nil) {
        int width = output->GetFont()->Width("9999999");
        _goto = new StringDialog("Go to line:", width);
    }

    InsertDialog(_goto);
    
    if (_goto->Accept()) {
        const char* string = _goto->String();
        int line = atoi(string);
        _editor->Select(_text->LineIndex(line-1));
        _editor->ScrollToSelection();
    }
    RemoveDialog(_goto);
}
