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
 * ControlInfo - manages persistent information contained in a Control.
 */

#ifndef unidraw_ctrlinfo_h
#define unidraw_ctrlinfo_h

#include <InterViews/enter-scope.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

class GraphicComp;

class ControlInfo {
public:
    ControlInfo(
        GraphicComp*, const char* = "", const char* = "", void* = nil
    );
    ControlInfo(
        const char*, const char* = "", const char* = "", void* = nil
    );
    virtual ~ControlInfo();

    void SetLabel(GraphicComp*);
    void SetLabel(const char*);
    void SetKeyLabel(const char*);
    void SetKeyCode(const char*);
    void SetOwner(void*);

    GraphicComp* GetLabel();
    const char* GetKeyLabel();
    const char* GetKeyCode();
    void* GetOwner();

    virtual ControlInfo* Copy();
private:
    void Init(const char*, const char*, void*);
private:
    GraphicComp* _label;
    char* _keyLabel;
    char* _keyCode;
    void* _owner;
};

inline void ControlInfo::SetOwner (void* o) { _owner = o; }

inline GraphicComp* ControlInfo::GetLabel () { return _label; }
inline const char* ControlInfo::GetKeyLabel () { return _keyLabel; }
inline const char* ControlInfo::GetKeyCode () { return _keyCode; }
inline void* ControlInfo::GetOwner() { return _owner; }

#include <IV-2_6/_leave.h>

#endif
