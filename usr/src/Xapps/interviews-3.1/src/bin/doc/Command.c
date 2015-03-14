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
 * Command -- acts on an ItemView through a DocumentViewer
 */

#include "Command.h"

#include "DocViewer.h"
#include "ItemView.h"

#include <string.h>

Command::Command (
    DocumentViewer* viewer, const char* command
) {
    _viewer = viewer;
    _command = strcpy(new char[strlen(command)+1], command);
}

Command::~Command () {
    _viewer = nil;
    delete _command;
}

void Command::execute () {
    if (_viewer != nil) {
        _viewer->ref();
        ItemView* view = _viewer->focus();
        if (view != nil) {
            boolean pending_repair = view->command(_command);
            if (pending_repair) {
                view->repair();
            }
            _viewer->update();
        }
        _viewer->unref();
    }
}
