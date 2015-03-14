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
 * DialogManager
 */

#include "DialogMgr.h"
#include "dialogs.h"

#include <OS/list.h>
#include <string.h>

class ChooserInfo {
public:
    char* _prompt;
    Chooser* _chooser;
};

class AskerInfo {
public:
    char* _prompt;
    Asker* _asker;
};

class ConfirmerInfo {
public:
    char* _prompt;
    Confirmer* _confirmer;
};

class ReporterInfo {
public:
    char* _prompt;
    Reporter* _reporter;
};

declareList(ChooserInfo_List,ChooserInfo)
implementList(ChooserInfo_List,ChooserInfo)

declareList(AskerInfo_List,AskerInfo)
implementList(AskerInfo_List,AskerInfo)

declareList(ConfirmerInfo_List,ConfirmerInfo)
implementList(ConfirmerInfo_List,ConfirmerInfo)

declareList(ReporterInfo_List,ReporterInfo)
implementList(ReporterInfo_List,ReporterInfo)

DialogManager::DialogManager () {
    _chooser = new ChooserInfo_List();
    _asker = new AskerInfo_List();
    _confirmer = new ConfirmerInfo_List();
    _reporter = new ReporterInfo_List();
}

DialogManager::~DialogManager () {
    while (_chooser->count() > 0) {
        ChooserInfo& info = _chooser->item_ref(0);
        delete info._prompt;
        delete info._chooser;
        _chooser->remove(0);
    }
    delete _chooser;
    while (_asker->count() > 0) {
        AskerInfo& info = _asker->item_ref(0);
        delete info._prompt;
        delete info._asker;
        _asker->remove(0);
    }
    delete _asker;
    while (_confirmer->count() > 0) {
        ConfirmerInfo& info = _confirmer->item_ref(0);
        delete info._prompt;
        delete info._confirmer;
        _confirmer->remove(0);
    }
    delete _confirmer;
    while (_reporter->count() > 0) {
        ReporterInfo& info = _reporter->item_ref(0);
        delete info._prompt;
        delete info._reporter;
        _reporter->remove(0);
    }
    delete _reporter;
}

const char* DialogManager::choose (
    Window* window, const char* prompt, const char* filter
) {
    long count = _chooser->count();
    for (long i = 0; i < count; ++i) {
        ChooserInfo& info = _chooser->item_ref(i);
        if (strcmp(info._prompt, prompt) == 0) {
            break;
        }
    }
    if (i == count) {
        ChooserInfo info;
        info._prompt = strcpy(new char[strlen(prompt) + 1], prompt);
        info._chooser = new Chooser("choose", prompt);
        _chooser->append(info);
    }
    ChooserInfo& info = _chooser->item_ref(i);
    return info._chooser->post(window, filter);
}

const char* DialogManager::ask (
    Window* window, const char* prompt, const char* initial
) {
    long count = _asker->count();
    for (long i = 0; i < count; ++i) {
        AskerInfo& info = _asker->item_ref(i);
        if (strcmp(info._prompt, prompt) == 0) {
            break;
        }
    }
    if (i == count) {
        AskerInfo info;
        info._prompt = strcpy(new char[strlen(prompt) + 1], prompt);
        info._asker = new Asker("ask", prompt);
        _asker->append(info);
    }
    AskerInfo& info = _asker->item_ref(i);
    return info._asker->post(window, initial);
}

int DialogManager::confirm (Window* window, const char* prompt) {
    long count = _confirmer->count();
    for (long i = 0; i < count; ++i) {
        ConfirmerInfo& info = _confirmer->item_ref(i);
        if (strcmp(info._prompt, prompt) == 0) {
            break;
        }
    }
    if (i == count) {
        ConfirmerInfo info;
        info._prompt = strcpy(new char[strlen(prompt) + 1], prompt);
        info._confirmer = new Confirmer("confirm", prompt);
        _confirmer->append(info);
    }
    ConfirmerInfo& info = _confirmer->item_ref(i);
    return info._confirmer->post(window);
}

void DialogManager::report (Window* window, const char* prompt) {
    long count = _reporter->count();
    for (long i = 0; i < count; ++i) {
        ReporterInfo& info = _reporter->item_ref(i);
        if (strcmp(info._prompt, prompt) == 0) {
            break;
        }
    }
    if (i == count) {
        ReporterInfo info;
        info._prompt = strcpy(new char[strlen(prompt) + 1], prompt);
        info._reporter = new Reporter("report", prompt);
        _reporter->append(info);
    }
    ReporterInfo& info = _reporter->item_ref(i);
    info._reporter->post(window);
}
