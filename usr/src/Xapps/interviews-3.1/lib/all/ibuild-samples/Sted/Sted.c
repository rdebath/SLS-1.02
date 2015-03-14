#include <InterViews/texteditor.h>
#include "Sted.h"
#include <IV-2_6/_enter.h>

#include <InterViews/event.h>
#include <InterViews/filebrowser.h>
#include <InterViews/regexp.h>
#include <InterViews/streditor.h>
#include <InterViews/textbuffer.h>
#include <InterViews/world.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "OpenDialog.h"
#include "SaveDialog.h"
#include "SearchDialog.h"

Sted::Sted(const char* name) : Sted_core(name) {
    _mgr = new FileManager;
    editor->Edit(_mgr->GetTextBuffer());
}

void Sted::Handle (Event& e) {
    if (e.eventType == KeyEvent) {
        if (e.len != 0) {
            char c = e.keystring[0];

            switch (c) {
            case '\010':
            case '\177':
                if (editor->Dot() != editor->Mark()) {
                    editor->DeleteSelection();
                } else {
                    editor->DeleteText(-1);
                }
                break;
            case '\015':
                InsertChar('\n');
                break;
            default:
                if (!iscntrl(c)) {
                    InsertChar(c);
                }
                break;
            }
        }
    } else if (e.eventType == DownEvent) {
        GetRelative(e.x, e.y, editor);
        editor->Select(editor->Locate(e.x, e.y));
        do {
            editor->ScrollToView(e.x, e.y);
            editor->SelectMore(editor->Locate(e.x, e.y));
            Poll(e);
            GetRelative(e.x, e.y, editor);
        } while (e.leftmouse);
    }
}

void Sted::InsertChar (char c) {
    editor->DeleteSelection();
    editor->InsertText(&c, 1);
    editor->ScrollToSelection();
}    

void Sted::InsertDialog (Interactor* dialog) {
    World* world = GetWorld();

    Coord x, y;
    Align(Center, 0, 0, x, y);
    GetRelative(x, y, world);

    world->InsertTransient(dialog, this, x, y, Center);
}

void Sted::RemoveDialog (Interactor* dialog) {
    GetWorld()->Remove(dialog);
}

void Sted::New() {
    editor->SelectAll();
    editor->DeleteSelection();
}

void Sted::Open() {
    static OpenDialog* dialog;
    if (dialog == nil) dialog = new OpenDialog("openDialog");
    InsertDialog(dialog);

    FileBrowser* fb = dialog->GetFileBrowser();
    boolean accepted = dialog->Accept();

    RemoveDialog(dialog);

    if (accepted && fb->Selections() != 0) {
        _mgr->Open(fb->String(fb->Selection()));
        editor->Edit(_mgr->GetTextBuffer());
    }
}

void Sted::Save() {
    static SaveDialog* dialog;
    if (dialog == nil) dialog = new SaveDialog("saveDialog");
    InsertDialog(dialog);
    boolean accepted = dialog->Accept();
    RemoveDialog(dialog);

    if (accepted) {
        const char* name = dialog->GetStringEditor()->Text();
        _mgr->Save(name);
    }
}

void Sted::Quit() {
    exit(0);    
}

void Sted::Cut() {
    Copy();
    editor->DeleteSelection();
}

void Sted::Copy() {
    _mgr->Copy(editor->Dot(), editor->Mark());
}

void Sted::Paste() {
    editor->DeleteSelection();
    editor->InsertText(_mgr->Clipboard(), strlen(_mgr->Clipboard()));
    editor->ScrollToSelection();
}

void Sted::Search() {
    static SearchDialog* dialog;
    if (dialog == nil) dialog = new SearchDialog("searchDialog");
    InsertDialog(dialog);
    boolean accept = dialog->Accept();
    RemoveDialog(dialog);

    if (accept) {
        int beg = editor->Mark(), end;

        if (_mgr->FwdSearch(dialog->GetStringEditor()->Text(), beg, end)) {
            editor->Select(beg, end);
        } 
            
        editor->ScrollToSelection();
    }
}

/*****************************************************************************/

static const int MINTEXTSIZE = 10000;

FileManager::FileManager () {
    _buffer = new char[MINTEXTSIZE];
    _text = new TextBuffer(_buffer, 0, MINTEXTSIZE);
    _clipboard = nil;
}

void FileManager::Open (const char* filename) {
    delete _buffer;
    delete _text;
    FILE* f = fopen(filename, "r");

    if (f != nil) {
        struct stat filestats;
        stat(filename, &filestats);
        _size = max(round(filestats.st_size * 10), MINTEXTSIZE);
        _buffer = new char[_size];
        char* b = _buffer;
        int remaining = _size;

        while (remaining > 1 && fgets(b, remaining, f) != nil) {
            int l = strlen(b);
            remaining -= l;
            b += l;
        }
        fclose(f);
        _text = new TextBuffer(_buffer, b-_buffer, _size);

    } else {
        _size = MINTEXTSIZE;
        _buffer = new char[_size];
        _text = new TextBuffer(_buffer, 0, _size);
    }
}

void FileManager::Save (const char* name) {
    FILE* f = fopen(name, "w");

    if (f != nil) {
        for (int i = 0; i < _text->Length(); ++i) {
            putc(_text->Char(i), f);
        }
    }

    fclose(f);
}

void FileManager::Copy (int beg, int end) {
    if (beg > end) {
        int tmp = beg;
        beg = end;
        end = tmp;
    }
    int sel = end - beg;

    if (sel != 0) {
        delete _clipboard;
        _clipboard = new char[sel+1];
        strncpy(_clipboard, _text->Text(beg, end), sel);
        _clipboard[sel] = '\0';
    }
}

boolean FileManager::FwdSearch (const char* string, int& beg, int& end) {
    Regexp re(string);

    boolean found =
        _text->ForwardSearch(&re, beg) >= 0 ||
        _text->ForwardSearch(&re, _text->BeginningOfText()) >= 0;

    if (found) {
        beg = re.BeginningOfMatch();
        end = re.EndOfMatch();
    }
    return found;
}

boolean FileManager::BwdSearch (const char* string, int& beg, int& end) {
    Regexp re(string);

    boolean found =
        _text->BackwardSearch(&re, beg) >= 0 ||
        _text->BackwardSearch(&re, _text->EndOfText()) >= 0;

    if (found) {
        beg = re.BeginningOfMatch();
        end = re.EndOfMatch();
    }
    return found;
}
