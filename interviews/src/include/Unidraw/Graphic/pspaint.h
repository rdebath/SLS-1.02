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
 * Persistent paint subclasses that incorporate additional information 
 * needed to generate idraw-compatible PostScript.
 */

#ifndef unidraw_graphic_pspaint_h
#define unidraw_graphic_pspaint_h

#include <IV-2_6/InterViews/paint.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

static const int patternWidth = 16;
static const int patternHeight = 16;

class PSBrush : public Brush {
public:
    PSBrush();
    PSBrush(int, int);

    int GetLinePattern();
    int Width();

    boolean None();
    const int* GetDashPattern();
    int GetDashPatternSize();
    int GetDashOffset();
protected:
    void CalcDashPat(int);
private:
    boolean _none;
    int _linepat;
    int _dashpat[patternWidth];	// stores dash pattern
    int _dashpatsize;		// stores number of defined elements in dashpat
    int _dashoffset;		// stores dash pattern's offset
};

inline boolean PSBrush::None () { return _none; }
inline int PSBrush::GetLinePattern () { return _linepat; }
inline const int* PSBrush::GetDashPattern () { return _dashpat; }
inline int PSBrush::GetDashPatternSize () { return _dashpatsize; }
inline int PSBrush::GetDashOffset () { return _dashoffset; }

class PSColor : public Color {
public:
    PSColor(ColorIntensity, ColorIntensity, ColorIntensity, const char*);
    virtual ~PSColor();

    const char* GetName();
    void GetIntensities(ColorIntensity&, ColorIntensity&, ColorIntensity&);
private:
    char* _name;			// stores name passed into constructor
    ColorIntensity _r, _g, _b;		// stores intensities passed to ctor
};

inline const char* PSColor::GetName () { return _name; }

class PSFont : public Font {
public:
    PSFont(const char*, const char*, const char*);
    virtual ~PSFont();

    const char* GetName();
    const char* GetPrintFont();
    const char* GetPrintSize();
    const char* GetPrintFontAndSize();

    int GetLineHt();
private:
    char* _name;                // name used by window system
    char* _printfont;		// name used by printer
    char* _printsize;		// scale used by printer
    char* _printfontandsize;	// name and size separated by a blank
    int _lineHt;        	// printsize converted to int
};

inline const char* PSFont::GetName () { return _name ? _name : "fixed"; }
inline const char* PSFont::GetPrintFont () { return _printfont; }
inline const char* PSFont::GetPrintSize () { return _printsize; }
inline const char* PSFont::GetPrintFontAndSize () { return _printfontandsize; }
inline int PSFont::GetLineHt () { return _lineHt; }

class PSPattern : public Pattern {
public:
    PSPattern();
    PSPattern(int, float);
    PSPattern(const int* pattern, int);

    boolean None();
    float GetGrayLevel();
    const int* GetData();
    int GetSize();
private:
    boolean _none;
    int _data[patternHeight];
    float _graylevel;		// stores gray level for grayscale patterns
    int _size;			// stores pat's orig size (4x4, 8x8, or 16x16)
};

inline boolean PSPattern::None () { return _none; }
inline float PSPattern::GetGrayLevel () { return _graylevel; }
inline const int* PSPattern::GetData () { return _data; }
inline int PSPattern::GetSize () { return _size; }

#include <IV-2_6/_leave.h>

#endif
