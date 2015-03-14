/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * A frame surrounds another interactor.
 */

#ifndef ivlook2_6_frame_h
#define ivlook2_6_frame_h

#include <IV-2_6/InterViews/scene.h>

#include <IV-2_6/_enter.h>

class Frame : public MonoScene {
public:
    Frame(Interactor* = nil, int width = 1);
    Frame(const char*, Interactor* = nil, int width = 1);
    virtual ~Frame();
protected:
    int left:8, bottom:8, right:8, top:8;
    
    Frame(Interactor*, int, int, int, int);
    Frame(const char*, Interactor*, int, int, int, int);

    virtual void Reconfig();
    virtual void Resize();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    void Init(Interactor*, int, int, int, int);
};

class ShowFrame : public Frame {
public:
    ShowFrame(Interactor* i = nil, int width = 1) : Frame(i, width) { Init(); }
    ShowFrame(
	const char* name,
	Interactor* i = nil, int width = 1
    ) : Frame(name, i, width) { Init(); }
    virtual ~ShowFrame();

    virtual void Handle(Event&);
    virtual void HandleInput(Event&);
    virtual void InsideFrame(boolean);
protected:
    ShowFrame(
	Interactor* i, int l, int b, int r, int h
    ) : Frame(i, l, b, r, h) { Init(); }
    ShowFrame(
	const char* name,
	Interactor* i, int l, int b, int r, int h
    ) : Frame(name, i, l, b, r, h) { Init(); }
private:
    void Init();
};

class Banner;

class TitleFrame : public ShowFrame {
public:
    TitleFrame(Banner*, Interactor*, int width = 1);
    TitleFrame(const char*, Banner*, Interactor*, int width = 1);
    virtual ~TitleFrame();

    virtual void InsideFrame(boolean);
protected:
    Banner* banner;

    virtual Interactor* Wrap(Interactor*);
private:
    void Init(Banner*, Interactor*);
};

class BorderFrame : public ShowFrame {
public:
    BorderFrame(Interactor* = nil, int width = 1);
    BorderFrame(const char*, Interactor* = nil, int width = 1);
    virtual ~BorderFrame();

    virtual void InsideFrame(boolean);
protected:
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    boolean normal;

    void Init();
};

class ShadowFrame : public Frame {
public:
    ShadowFrame(Interactor* = nil, int h = 1, int v = 1);
    ShadowFrame(const char*, Interactor* = nil, int h = 1, int v = 1);
    virtual ~ShadowFrame();
protected:
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    void Init(Interactor*, int h, int v);
};

class MarginFrame : public Frame {
public:
    MarginFrame(Interactor* = nil, int margin = 0);
    MarginFrame(const char*, Interactor* = nil, int margin = 0);
    MarginFrame(Interactor*, int margin, int shrink, int stretch);
    MarginFrame(const char*, Interactor*, int margin, int shrink, int stretch);
    MarginFrame(Interactor*, int hmargin, int vmargin);
    MarginFrame(const char*, Interactor*, int hmargin, int vmargin);
    MarginFrame(
	Interactor*,
        int hmargin, int hshrink, int hstretch,
        int vmargin, int vshrink, int vstretch
    );
    MarginFrame(
	const char*, Interactor*,
        int hmargin, int hshrink, int hstretch,
        int vmargin, int vshrink, int vstretch
    );
    virtual ~MarginFrame();
protected:
    virtual void Reconfig();
    virtual void Resize();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
protected:
    int hmargin, hshrink, hstretch;
    int vmargin, vshrink, vstretch;
private:
    void Init(int, int, int, int, int, int);
};

#include <IV-2_6/_leave.h>

#endif
