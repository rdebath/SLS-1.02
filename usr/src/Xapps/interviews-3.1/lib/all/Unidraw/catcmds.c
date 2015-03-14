/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Implementation of cataloging commands.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/dialogs.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/psview.h>

#include <IV-look/dialogs.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>

#include <osfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

static void UpdateCompNameVars () {
    Iterator i;

    for (unidraw->First(i); !unidraw->Done(i); unidraw->Next(i)) {
        Editor* ed = unidraw->GetEditor(i);
        CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
        if (compNameVar != nil) compNameVar->UpdateName();
    }
}

static boolean Writable (Component* comp) {
    Catalog* catalog = unidraw->GetCatalog();
    const char* name = catalog->GetName(comp);
    return name == nil || (catalog->Exists(name) && catalog->Writable(name));
}

static boolean OnlyOneEditorOf (Component* c) {
    Component* comp = c->GetRoot();
    Iterator i;
    int count = 0;

    for (unidraw->First(i); !unidraw->Done(i) && count < 2; unidraw->Next(i)) {
        Component* test_comp = unidraw->GetEditor(i)->GetComponent();

        if (test_comp != nil && test_comp->GetRoot() == comp) {
            ++count;
        }
    }
    return count == 1;
}

static boolean ReadyToClose (Editor* ed) {
    ModifStatusVar* mv = (ModifStatusVar*) ed->GetState("ModifStatusVar");

    if (mv != nil && Writable(mv->GetComponent()) && mv->GetModifStatus()) {
        ConfirmDialog dialog("Save changes?");

        ed->InsertDialog(&dialog);
        char resp = dialog.Confirm();
        ed->RemoveDialog(&dialog);

        if (resp == '\007') {
            return false;                       // confirm dialog aborted

        } else if (resp == 'y') {
            SaveCompCmd saveComp(ed);
            saveComp.Execute();

            if (mv->GetModifStatus()) {
                return false;                   // save dialog was aborted
            }
        }
    }
    return true;
}

/*****************************************************************************/

ClassId NewCompCmd::GetClassId () { return NEWCOMP_CMD; }

boolean NewCompCmd::IsA (ClassId id) {
    return NEWCOMP_CMD == id || Command::IsA(id);
}

NewCompCmd::NewCompCmd (ControlInfo* c, Component* p) : Command(c) { 
    prototype_ = p; 
}

NewCompCmd::NewCompCmd (Editor* ed, Component* p) : Command(ed) { 
    prototype_ = p; 
}

NewCompCmd::~NewCompCmd () { delete prototype_; }

Command* NewCompCmd::Copy () {
    Command* copy = new NewCompCmd(CopyControlInfo(), prototype_->Copy());
    InitCopy(copy);
    return copy;
}

void NewCompCmd::Execute () {
    Editor* ed = GetEditor();
    Component* orig = ed->GetComponent();
    Component* comp = prototype_->Copy();
    CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
    ModifStatusVar* modifVar = (ModifStatusVar*)ed->GetState("ModifStatusVar");

    if (OnlyOneEditorOf(orig) && !ReadyToClose(ed)) {
        return;
    }

    if (compNameVar != nil) compNameVar->SetComponent(comp);
    if (modifVar != nil) modifVar->SetComponent(comp);

    ed->SetComponent(comp);
    ed->Update();

    if (orig != nil && unidraw->FindAny(orig) == nil) {
        Component* root = orig->GetRoot();
        delete root;
    }
}

boolean NewCompCmd::Reversible () { return false; }

void NewCompCmd::Read (istream& in) {
    Command::Read(in);
    prototype_ = unidraw->GetCatalog()->ReadComponent(in);
}

void NewCompCmd::Write (ostream& out) {
    Command::Write(out); 
    unidraw->GetCatalog()->WriteComponent(prototype_, out);
}

/*****************************************************************************/

ClassId RevertCmd::GetClassId () { return REVERT_CMD; }

boolean RevertCmd::IsA (ClassId id) {
    return REVERT_CMD == id || Command::IsA(id);
}

RevertCmd::RevertCmd (ControlInfo* c) : Command(c) { }
RevertCmd::RevertCmd (Editor* ed) : Command(ed) { }

Command* RevertCmd::Copy () {
    Command* copy = new RevertCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void RevertCmd::Execute () {
    Editor* ed = GetEditor();
    Component* comp = ed->GetComponent();
    Catalog* catalog = unidraw->GetCatalog();
    const char* name = catalog->GetName(comp);
    ModifStatusVar* mv = (ModifStatusVar*) ed->GetState("ModifStatusVar");

    if (name != nil && (mv == nil || mv->GetModifStatus())) {
        char buf[CHARBUFSIZE];
        strcpy(buf, name);

        ConfirmDialog dialog("Really revert to last version saved?");
        ed->InsertDialog(&dialog);
        char confirmation = dialog.Confirm();
        ed->RemoveDialog(&dialog);

        if (confirmation == 'y') {
            Component* orig = comp;
            catalog->Forget(orig);

            if (unidraw->GetCatalog()->Retrieve(buf, comp)) {
                ed->SetComponent(comp);
                unidraw->CloseDependents(orig);
                unidraw->Update();

                CompNameVar* cv = (CompNameVar*) ed->GetState("CompNameVar");

                if (cv != nil) cv->SetComponent(comp);
                if (mv != nil) mv->SetComponent(comp);

                Component* root = orig->GetRoot();
                delete root;

            } else {
                ConfirmDialog dialog(
                    "Couldn't revert! (File nonexistent?)", "Save changes?"
                );
                ed->InsertDialog(&dialog);
                char confirmation = dialog.Confirm();
                ed->RemoveDialog(&dialog);

                UpdateCompNameVars();
                if (mv != nil) mv->Notify();

                if (confirmation == 'y') {
                    SaveCompAsCmd saveCompAs(ed);
                    saveCompAs.Execute();
                }
            }
        }
    }
}

boolean RevertCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId ViewCompCmd::GetClassId () { return VIEWCOMP_CMD; }

boolean ViewCompCmd::IsA (ClassId id) {
    return VIEWCOMP_CMD == id || Command::IsA(id);
}

ViewCompCmd::ViewCompCmd (ControlInfo* c, FileChooser* fc) : Command(c) {
    chooser_ = fc;
    Resource::ref(chooser_);
}

ViewCompCmd::ViewCompCmd (Editor* ed, FileChooser* fc) : Command(ed) {
    chooser_ = fc;
    Resource::ref(chooser_);
}

ViewCompCmd::~ViewCompCmd () {
    Resource::unref(chooser_);
}

Command* ViewCompCmd::Copy () {
    Command* copy = new ViewCompCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void ViewCompCmd::Execute () {
    Editor* ed = GetEditor();

    if (OnlyOneEditorOf(ed->GetComponent()) && !ReadyToClose(ed)) {
        return;
    }

    Style* style;
    boolean reset_caption = false;
    if (chooser_ == nil) {
	style = new Style(Session::instance()->style());
	chooser_ = DialogKit::instance()->file_chooser(".", style);
	Resource::ref(chooser_);
	char buf[CHARBUFSIZE];
	const char* domain = unidraw->GetCatalog()->GetAttribute("domain");
	domain = (domain == nil) ? "component" : domain;
	sprintf(buf, "Select a %s to open:", domain);
	style->attribute("caption", "");
	style->attribute("subcaption", buf);
    } else {
	style = chooser_->style();
    }
    while (chooser_->post_for(ed->GetWindow())) {
        const String* s = chooser_->selected();
	NullTerminatedString ns(*s);
	const char* name = ns.string();
        Catalog* catalog = unidraw->GetCatalog();
        GraphicComp* comp;

        if (catalog->Retrieve(name, (Component*&) comp)) {
	    ModifStatusVar* modif = (ModifStatusVar*) ed->GetState(
		"ModifStatusVar"
	    );
            Component* orig = ed->GetComponent();
            ed->SetComponent(comp);
            unidraw->Update();

            StateVar* sv = ed->GetState("CompNameVar");
            CompNameVar* cnv = (CompNameVar*) sv;

            if (cnv != nil) cnv->SetComponent(comp);
            if (modif != nil) modif->SetComponent(comp);

            if (orig != nil && unidraw->FindAny(orig) == nil) {
                Component* root = orig->GetRoot();
                delete root;
            }
	    break;
        } else {
	    style->attribute("caption", "Open failed!");
	    reset_caption = true;
        }
    }
    if (reset_caption) {
	style->attribute("caption", "");
    }
}

boolean ViewCompCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId SaveCompCmd::GetClassId () { return SAVECOMP_CMD; }

boolean SaveCompCmd::IsA (ClassId id) {
    return SAVECOMP_CMD == id || Command::IsA(id);
}

SaveCompCmd::SaveCompCmd (ControlInfo* c) : Command(c) { }
SaveCompCmd::SaveCompCmd (Editor* ed) : Command(ed) { }

Command* SaveCompCmd::Copy () {
    Command* copy = new SaveCompCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void SaveCompCmd::Execute () {
    Editor* ed = GetEditor();
    ModifStatusVar* modifVar = (ModifStatusVar*)ed->GetState("ModifStatusVar");
    CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
    const char* name = (compNameVar == nil) ? nil : compNameVar->GetName();

    if (name == nil) {
        SaveCompAsCmd saveCompAs(ed);
        saveCompAs.Execute();

    } else if (modifVar == nil || modifVar->GetModifStatus()) {
        Catalog* catalog = unidraw->GetCatalog();
        Component* comp;

        if (catalog->Retrieve(name, comp) && catalog->Save(comp, name)) {
            if (modifVar != nil) modifVar->SetModifStatus(false);
            unidraw->ClearHistory(comp);

        } else {
            char title[CHARBUFSIZE];
            const char* reason = !Writable(comp) ? "(File not writable.)" : "";
            sprintf(title, "Couldn't save! %s", reason);

            char subtitle[CHARBUFSIZE];
            const char* domain = unidraw->GetCatalog()->GetAttribute("domain");
            domain = (domain == nil) ? "component" : domain;
            sprintf(subtitle, "Save this %s as:", domain);

	    Style* s = new Style(Session::instance()->style());
	    s->attribute("caption", title);
	    s->attribute("subcaption", subtitle);
	    s->attribute("open", "Save");
	    /* BUG: style s is never used!!!! */
            SaveCompAsCmd saveCompAs(ed);
            saveCompAs.Execute();
        }
    }
}

boolean SaveCompCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId SaveCompAsCmd::GetClassId () { return SAVECOMPAS_CMD; }

boolean SaveCompAsCmd::IsA (ClassId id) {
    return SAVECOMPAS_CMD == id || Command::IsA(id);
}

SaveCompAsCmd::SaveCompAsCmd (ControlInfo* c, FileChooser* fc) : Command(c) {
    chooser_ = fc;
    Resource::ref(chooser_);
}

SaveCompAsCmd::SaveCompAsCmd (Editor* ed, FileChooser* fc) : Command(ed) {
    chooser_ = fc;
    Resource::ref(chooser_);
}

SaveCompAsCmd::~SaveCompAsCmd () {
    Resource::unref(chooser_);
}

Command* SaveCompAsCmd::Copy () {
    Command* copy = new SaveCompAsCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void SaveCompAsCmd::Execute () {
    Editor* ed = GetEditor();

    char buf[CHARBUFSIZE];
    const char* domain = unidraw->GetCatalog()->GetAttribute("domain");
    domain = (domain == nil) ? "component" : domain;
    sprintf(buf, "Save this %s as:", domain);

    boolean reset_caption = false;
    Style* style = new Style(Session::instance()->style());
    style->attribute("subcaption", buf);
    style->attribute("open", "Save");
    if (chooser_ == nil) {
	chooser_ = DialogKit::instance()->file_chooser(".", style);
	Resource::ref(chooser_);
    }
    while (chooser_->post_for(ed->GetWindow())) {
	const String* str = chooser_->selected();
	NullTerminatedString ns(*str);
        const char* name = ns.string();
        Catalog* catalog = unidraw->GetCatalog();
        boolean ok = true;

        if (catalog->Exists(name) && catalog->Writable(name)) {
            char buf[CHARBUFSIZE];
            sprintf(buf, "\"%s\" already exists.", name);
            ConfirmDialog dialog(buf, "Overwrite?");
            ed->InsertDialog(&dialog);
            char confirmation = dialog.Confirm();
            ed->RemoveDialog(&dialog);

            if (confirmation == 'n') {
                ok = false;
            } else if (confirmation != 'y') {
		break;
            }
        }
        if (ok) {
            CompNameVar* cnv = (CompNameVar*) ed->GetState("CompNameVar");
            const char* oldname = (cnv == nil) ? nil : cnv->GetName();
            Component* comp = ed->GetComponent();

            if (catalog->Exists(name) && !catalog->Writable(name)) {
		style->attribute(
		    "caption", "Couldn't save! (File not writable.)"
		);
            } else {
                if (oldname == nil) {
                    comp = comp->GetRoot();
                } else {
                    catalog->Retrieve(oldname, comp);
                    catalog->Forget(comp);
                }

                StateVar* sv = ed->GetState("ModifStatusVar");
                ModifStatusVar* mv = (ModifStatusVar*) sv;

                if (catalog->Save(comp, name)) {
                    if (mv != nil) mv->SetModifStatus(false);
                    unidraw->ClearHistory(comp);
                    UpdateCompNameVars();
		    break;
                } else {
                    if (mv != nil) mv->Notify();
                    UpdateCompNameVars();
		    style->attribute("caption", "Couldn't save!");
		    reset_caption = true;
                }
            } 
        }
    }
    if (reset_caption) {
	style->attribute("caption", "");
    }
}

boolean SaveCompAsCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId PrintCmd::GetClassId () { return PRINT_CMD; }

boolean PrintCmd::IsA (ClassId id) {
    return PRINT_CMD == id || Command::IsA(id);
}

PrintCmd::PrintCmd (ControlInfo* c, PrintDialog* pd) : Command(c) {
    _dialog = pd;
}

PrintCmd::PrintCmd (Editor* ed, PrintDialog* pd) : Command(ed) {
    _dialog = pd;
}

PrintCmd::~PrintCmd () { delete _dialog; }

Command* PrintCmd::Copy () {
    Command* copy = new PrintCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PrintCmd::Execute () {
    GraphicComp* comps = GetGraphicComp();
    boolean ok;

    if (_dialog == nil) {
        _dialog = new PrintDialog;
    }

    do {
        _editor->InsertDialog(_dialog);
        boolean accepted = _dialog->Accept();
        _editor->RemoveDialog(_dialog);

        if (!accepted) {
            break;
        }

        filebuf fbuf;
        char* tmpfilename;

        if (_dialog->ToPrinter()) {
            tmpfilename = tmpnam(nil);
            ok = fbuf.open(tmpfilename, output) != 0;
        } else {
            ok = fbuf.open((char*) _dialog->Choice(), output) != 0;
        }

        if (ok) {
            ostream out(&fbuf);
            ExternView* ev = (ExternView*) comps->Create(POSTSCRIPT_VIEW);
            comps->Attach(ev);
            ev->Update();
            ok = ev->Emit(out);
	    out.flush();
            delete ev;

            if (_dialog->ToPrinter()) {
                ok = print(_dialog->Choice(), tmpfilename) == 0;
            }
        }
        if (!ok) {
            _dialog->SetTitle("Couldn't print!");
        }
    } while (!ok);

    _dialog->SetTitle("");
}

int PrintCmd::print (const char* print_cmd, const char* file) {
    char cmd[CHARBUFSIZE];
    sprintf(cmd, "%s %s", print_cmd, file);
    return system(cmd);
}

boolean PrintCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId QuitCmd::GetClassId () { return QUIT_CMD; }
boolean QuitCmd::IsA (ClassId id) { return QUIT_CMD == id || Command::IsA(id);}
QuitCmd::QuitCmd (ControlInfo* c) : Command(c) { }
QuitCmd::QuitCmd (Editor* ed) : Command(ed) { }

Command* QuitCmd::Copy () {
    Command* copy = new QuitCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void QuitCmd::Execute () {
    Editor* ed = GetEditor();
    
    if (ReadyToClose(ed)) {
        Component* comp = ed->GetComponent();

        if (comp == nil) {
            unidraw->Close(ed);
        } else {
            unidraw->CloseDependents(comp->GetRoot());
        }
        Iterator i;

        for (;;) {
            unidraw->First(i);

            if (unidraw->Done(i)) {
                break;
            }

            ed = unidraw->GetEditor(i);

            if (ReadyToClose(ed)) {
                comp = ed->GetComponent();

                if (comp == nil) {
                    unidraw->Close(ed);
                } else {
                    unidraw->CloseDependents(comp->GetRoot());
                }
            } else {
                return;
            }
        }
        unidraw->Quit();
    }
}

boolean QuitCmd::Reversible () { return false; }
