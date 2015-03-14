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
 * Implementation of global definitions.
 */

#include "globals.h"
#include <InterViews/enter-scope.h>
#include <OS/memory.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

char* strnew (const char* s) {
    char* dup = new char[strlen(s) + 1];
    strcpy(dup, s);
    return dup;
}

char* strnnew (const char* s, int len) {
    char* dup = new char[len + 1];
    strncpy(dup, s, len);
    dup[len] = '\0';
    return dup;
}

void BufCheck (const void**& buf, int& bufsize, int count, int index) {
    void** newbuf;

    if (index >= bufsize) {
        int length = count*sizeof(void*);
        bufsize = (index+1) * 2;
        newbuf = new void*[bufsize];
        Memory::copy(buf, newbuf, length);
        delete buf;
        buf = (const void**)newbuf;
    }
}

void BufInsert (
    const void* s, int index, const void**& buf, int& bufsize, int& count
) {
    const void** spot;
    index = (index < 0) ? count : index;

    if (index < count) {
        BufCheck(buf, bufsize, count, count+1);
        spot = &buf[index];
        Memory::copy(spot, spot+1, (count - index)*sizeof(void*));

    } else {
        BufCheck(buf, bufsize, count, index);
        spot = &buf[index];
    }
    *spot = s;
    ++count;
}

void BufRemove (int index, const void** buf, int& count) {
    if (index < --count) {
        const void** spot = &buf[index];
        Memory::copy(spot+1, spot, (count - index)*sizeof(void*));
    }
}

int BufFind (
    int index, 
    const void** srcbuf, int srccount, 
    const void** dstbuf, int dstcount
) {
    if (0 <= index && index < srccount) {
        const void* s = srcbuf[index];

        if (s != nil) {
            for (int i = 0; i < dstcount; ++i) {
                if (dstbuf[i] == s) {
                    return i;
                }
            }
        }
    }
    return -1;
}
