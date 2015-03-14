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
 * StateVarView subclasses.
 */

#ifndef unidraw_stateviews_h
#define unidraw_stateviews_h

#include <Unidraw/stateview.h>

#include <IV-2_6/_enter.h>

class BrushVar;
class ColorVar;
class FontVar;
class GravityVar;
class MagnifVar;
class ModifStatusVar;
class NameVar;
class PatternVar;
class PSBrush;
class PSColor;
class PSPattern;
class PSFont;
class TextInteractor;

class NameVarView : public StateVarView {
public:
    NameVarView(NameVar*, Alignment = Center, const char* sample = nil);
protected:
    virtual void Init();
    virtual boolean Stale();
};

class FileNameVarView : public StateVarView {
public:
    FileNameVarView(
        NameVar*, Alignment = Center,
        boolean relative = true, const char* sample = nil
    );
protected:
    virtual void Init();
protected:
    boolean _relative;
};

class CompNameVarView : public StateVarView {
public:
    CompNameVarView(
        CompNameVar*, Alignment = Center,
        boolean relative = true, const char* sample = nil
    );
protected:
    virtual void Init();
protected:
    boolean _relative;
};

class ModifStatusVarView : public StateVarView {
public:
    ModifStatusVarView(ModifStatusVar*, Alignment = Center);
protected:
    virtual void Init();
    virtual boolean Stale();
    virtual void Reconfig();
    boolean WriteProtected();
protected:
    boolean _prevVal, _prevProt;
};

class MagnifVarView : public StateVarView {
public:
    MagnifVarView(MagnifVar*, Alignment = Center);
protected:
    virtual void Init();
    virtual boolean Stale();
protected:
    float _prevVal;
};

class GravityVarView : public StateVarView {
public:
    GravityVarView(GravityVar*, Alignment = Center);
protected:
    virtual void Init();
    virtual boolean Stale();
protected:
    boolean _prevVal;
};

class FontVarView : public StateVarView {
public:
    FontVarView(FontVar*, Alignment = Center, const char* sample = nil);
protected:
    virtual void Init();
    virtual boolean Stale();
protected:
    PSFont* _prevVal;
};

class BrushVarView : public StateVarView {
public:
    BrushVarView(BrushVar*, ColorVar* = nil);
    virtual ~BrushVarView();
protected:
    virtual void Init();
    virtual boolean Stale();
protected:
    PSBrush* _prevVal;
    PSColor* _prevFg, *_prevBg;
    ColorVar* _colorSubj;
};

class PatternVarView : public StateVarView {
public:
    PatternVarView(PatternVar*, ColorVar* = nil);
    virtual ~PatternVarView();
protected:
    virtual void Init();
    virtual boolean Stale();
protected:
    PSPattern* _prevVal;
    PSColor* _prevFg, *_prevBg;
    ColorVar* _colorSubj;
};

#include <IV-2_6/_leave.h>

#endif
