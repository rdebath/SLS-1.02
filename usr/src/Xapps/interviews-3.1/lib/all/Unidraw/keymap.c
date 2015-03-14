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
 * Keymap implementation.
 */

#include <Unidraw/ctrlinfo.h>
#include <Unidraw/keymap.h>
#include <Unidraw/uctrl.h>

#include <string.h>

/*****************************************************************************/

KeyMap::KeyMap () { Init(); }

KeyMap::KeyMap (UControl* c) {
    Init();
    _ctrl = c;
}

KeyMap::KeyMap (KeyMap* k) {
    Init();
    _submap = k;
}

KeyMap::~KeyMap () {
    if (_next != nil) {
        delete _next;
    }
}

void KeyMap::Init () {
    _next = nil;
    _submap = nil;
    _ctrl = nil;
}

void KeyMap::Register (UControl* c) {
    KeyMap* newMap = new KeyMap(c);
    newMap->_next = _next;
    _next = newMap;
}

void KeyMap::Register (KeyMap* k) {
    KeyMap* newMap = new KeyMap(k);
    newMap->_next = _next;
    _next = newMap;
}

void KeyMap::Unregister (UControl* c) {
    KeyMap* cur = this;
    KeyMap* prev = nil;
    
    while (cur != nil) {
	if (cur->_ctrl == c) {
	    if (prev != nil) {
		prev->_next = cur->_next;
	    }
	    cur->_next = nil;
	    delete cur;
	    return;
	} else {
	    prev = cur;
	    cur = cur->_next;
	}
    }
}

void KeyMap::Unregister (KeyMap* k) {
    KeyMap* cur = this;
    KeyMap* prev = nil;
    
    while (cur != nil) {
	if (cur->_submap == k) {
	    if (prev != nil) {
		prev->_next = cur->_next;
	    }
	    cur->_next = nil;
	    delete cur;
	    return;
	} else {
	    prev = cur;
	    cur = cur->_next;
	}
    }
}

void KeyMap::Execute (const char* keyCode) { 
    if (*keyCode != '\0') {
        KeyMap* cur = this;

        while (cur != nil) {
            KeyMap* testk = cur->_submap;
            UControl* testc = cur->_ctrl;

            if (testk != nil) {
                testk->Execute(keyCode);

            } else if (testc != nil) {
                const char* target = testc->GetControlInfo()->GetKeyCode();

                if (strcmp(target, keyCode) == 0) {
                    testc->Do();
                    return;
                }
            }
            cur = cur->_next;
        }
    }
}
