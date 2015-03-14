/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
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
 * digital clockface class
 */

#ifndef dface_h
#define dface_h

struct CharPoints {
    int count;
    Coord x[12];
    Coord y[12];
};

enum AMPMMODE { BLANK, AM, PM };

class DFace : public Interactor {
public:
    DFace(
	boolean showDate, boolean showBorder, boolean showTime,
	TMode timeMode, int width = 0, int height = 0
    );
    ~DFace();

    virtual void Reconfig();
    virtual void Resize();
    virtual void Draw();
    virtual void Handle(Event&);
    void Run();
protected:
    void DrawFace();
    void DrawColon();
    void DrawAMPM(Painter*);
    void DrawDate();
    void DrawBorder();
    void Redraw(Coord left, Coord bottom, Coord right, Coord top);
    void RedrawList(int, Coord[], Coord[], Coord[], Coord[]);
    void Set(char* today, int hours, int minutes);
    void Tick(long, long);
private:
    Clock* clock;
    TMode mode;				/* civil or military */
    AMPMMODE AMPMmode;
    SegPoints colon[2];			/* colon shape data */
    CharPoints A, P, M;			/* character shape data */
    Painter* invertor;			/* for highlights and erasing */
    boolean showDate;			/* visibility of date */
    boolean showBorder;			/* visibility of date/time line */
    boolean showTime;			/* visibility of time */
    boolean selected;			/* highlight date if true */
    boolean done;			/* program terminator */

    Digit* ht, * hu, * mt, * mu;

    struct {				/* date string */
	int len;
	char text[50];
    } date;

    class IOHandler* tick;
};

#endif
