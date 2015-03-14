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
 * EditorInfo - manages persistent information contained in an editor.
 */

#ifndef unidraw_editorinfo_h
#define unidraw_editorinfo_h

#include <InterViews/enter-scope.h>
#include <Unidraw/umap.h>

#include <InterViews/_enter.h>

class EditorInfo : public UMap {
public:
    EditorInfo();
    virtual ~EditorInfo();

    void Register(const char* name, const char* info = "");
    void UnregisterName(const char* name);
    void UnregisterInfo(const char* info);
    boolean Registered(const char* name);

    const char* GetName(const char* info);
    const char* GetName(int index);
    const char* GetInfo(const char* name);
    const char* GetInfo(int index);
private:
    virtual UMapElem* FindId(void*);
    virtual UMapElem* FindTag(void*);
};

#include <InterViews/_leave.h>

#endif
