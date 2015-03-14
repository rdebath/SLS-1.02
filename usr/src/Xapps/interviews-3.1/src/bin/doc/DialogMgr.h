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

#ifndef DialogManager_h
#define DialogManager_h

#include <InterViews/boolean.h>

class ChooserInfo_List;
class AskerInfo_List;
class ConfirmerInfo_List;
class ReporterInfo_List;
class Window;

class DialogManager {
public:
    DialogManager ();
    virtual ~DialogManager ();

    virtual const char* choose (Window*, const char* prompt, const char* filt);
    virtual const char* ask (Window*, const char* prompt, const char* init);
    virtual int confirm (Window*, const char* prompt);
    virtual void report (Window*, const char* prompt);
private:
    ChooserInfo_List* _chooser;
    AskerInfo_List* _asker;
    ConfirmerInfo_List* _confirmer;
    ReporterInfo_List* _reporter;
};

#endif
