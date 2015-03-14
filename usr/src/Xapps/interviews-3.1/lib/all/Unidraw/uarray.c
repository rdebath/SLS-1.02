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
 * UArray implementation.
 */

#include <Unidraw/uarray.h>

#include <OS/memory.h>
#include <OS/math.h>

/*****************************************************************************/

UArray::UArray (int defaultSize) {
    _bufsize = defaultSize;
    _buf = new void*[_bufsize];
    _count = 0;
}

UArray::~UArray () { delete _buf; }

void UArray::Check (int index) {
    void** newbuf;

    if (index >= _bufsize) {
        _bufsize = (index+1) * 2;
        newbuf = new void*[_bufsize];
        Memory::copy(_buf, newbuf, _count*sizeof(void*));
        delete _buf;
        _buf = newbuf;
    }
}

void UArray::Insert (void* v, int index) {
    void** spot;
    index = (index < 0) ? _count : index;

    if (index < _count) {
        Check(_count+1);
        spot = &_buf[index];
        Memory::copy(spot, spot+1, (_count - index)*sizeof(void*));

    } else {
        Check(index);
        spot = &_buf[index];
    }
    *spot = v;
    ++_count;
}

void UArray::Remove (int index) {
    if (0 <= index && index < _count) {
        --_count;
        void** spot = &_buf[index];
        Memory::copy(spot+1, spot, (_count - index)*sizeof(void*));
    }
}

int UArray::Index (void* v) {
    for (int i = 0; i < _count; ++i) {
        if (_buf[i] == v) {
            return i;
        }
    }
    return -1;
}

void*& UArray::operator[] (int index) {
    Check(index);
    _count = Math::max(_count, index+1);
    return _buf[index];
}
