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

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include "dclock.h"
#include "dface.h"
#include "digit.h"
#include "clocktime.h"
#include <string.h>

declareIOCallback(DFace)
implementIOCallback(DFace)

static const int FadeDelay = 10000;

void DFace::DrawFace () {
    output->ClearRect(canvas, 0, 0, xmax, ymax);
}

void DFace::DrawColon () {
    output->FillPolygon(canvas, colon[0].x, colon[0].y, colon[0].count);
    output->FillPolygon(canvas, colon[1].x, colon[1].y, colon[1].count);
}

void DFace::DrawAMPM (Painter *painter) {
    if (canvas != nil) {
	if (AMPMmode != BLANK) {
	    if (AMPMmode == AM) {
		painter->FillPolygon(canvas, A.x, A.y, A.count);
	    } else {
		painter->FillPolygon(canvas, P.x, P.y, P.count);
	    }
	    painter->FillPolygon(canvas, M.x, M.y, M.count);
	}
    }
}

void DFace::DrawDate () {
    if (showDate && date.len != 0) {
	const Font* f = output->GetFont();
	Coord dateYPos = ymax - f->Height();
	Coord dateXPos = 2;

	int availWidth = xmax - dateXPos - 2;
	int dayWidth = f->Width( date.text, 3 );
	int dayDateWidth = f->Width( date.text, 10 );
	int wholeWidth = f->Width( date.text, date.len );

	output->ClearRect(canvas, 0, ymax - f->Height(), xmax, ymax);
	if (wholeWidth < availWidth) {
	    output->Text(canvas, date.text, date.len, dateXPos, dateYPos);
	} else if (dayDateWidth < availWidth) {
	    output->Text(canvas, date.text, 10, dateXPos, dateYPos);
	} else if (dayWidth < availWidth) {
	    output->Text(canvas, date.text, 3, dateXPos, dateYPos);
	}
    }
}

void DFace::DrawBorder () {
    if (showDate && showTime) {
	int ypos = ymax - output->GetFont()->Height() - 2;
	output->Line(canvas, 0, ypos, xmax, ypos);
    }
}

void DFace::Tick (long /* sec */, long /* usec */) {
    int nextTick = clock->NextTick();
    if (nextTick <= 0) {
	int h, m, s;
	char date[50];
	clock->GetTime(date, h, m, s);
	Set(date, h, m);
	nextTick = clock->NextTick();
    }
    /*
     * The +1 below is an extra second to make sure the timeout
     * is after the next minute.
     */
    Dispatcher::instance().startTimer(nextTick + 1, 0, tick);
}

void DFace::Set (char *today, int hours, int minutes) {
    int h = hours;
    if (mode==CIVIL) {
	if (hours > 12) {
	    h -= 12;
	} else if ( hours == 0 ) {
	    h = 12;				// midnight is 12:xx
	}
    }

    unsigned long fade = FadeDelay * (1 << (min(4,max(0,FadeRate))) );
    Event e;
    boolean done_fading = false;
    while (showTime && !done_fading && !done) {
	if (Read(0, fade, e)) {
	    e.target->Handle(e);
	    if (done) {
		return;
	    }
	}
	done_fading = true;
	if (mode == CIVIL && h < 10) {
	    done_fading &= ht->Set(-1);		// blank digit
	} else {
	    done_fading &= ht->Set(h/10);
	}
	done_fading &= hu->Set(h%10);
	done_fading &= mt->Set(minutes/10);
	done_fading &= mu->Set(minutes%10);
    }

    if (showTime && mode==CIVIL) {
	AMPMMODE newAMPM = (hours >= 12) ? PM : AM;

	if (AMPMmode == BLANK) {
	    AMPMmode = newAMPM;
	    DrawAMPM(output);
	} else if (AMPMmode != newAMPM) {
	    DrawAMPM(invertor);				// erase old
	    AMPMmode = newAMPM;
	    DrawAMPM(output);				// draw new
	}
    }
    if (showDate && strcmp(date.text, today) != 0) {
	strcpy(date.text, today);
	date.len = strlen(today);
	DrawDate();
	if (showBorder) {
	    DrawBorder();
	}
    }
}

DFace::DFace (
    boolean showDate, boolean showBorder, boolean showTime,
    TMode timeMode, int width, int height
) {
    clock = new Clock();
    mode = timeMode;
    AMPMmode = BLANK;
    A.count = AData.count;
    P.count = PData.count;
    M.count = MData.count;
    colon[0].count = ColonData[0].count;
    colon[1].count = ColonData[1].count;
    this->showDate = showDate;
    this->showBorder = showBorder;
    this->showTime = showTime;
    ht = new Digit(HTx, ALLy);
    hu = new Digit(HUx, ALLy);
    mt = new Digit(MTx, ALLy);
    mu = new Digit(MUx, ALLy);
    selected = false;
    done = false;
    date.len = 0;
    shape->Rect(width, height);
    shape->Rigid(hfil, hfil, vfil, vfil);
    invertor = nil;
    input = new Sensor;
    input->Catch(KeyEvent);
    tick = new IOCallback(DFace)(this, &DFace::Tick);
}

void DFace::Reconfig () {
    Unref(invertor);
    invertor = new Painter(output);
    invertor->Reference();
    invertor->SetColors(invertor->GetBgColor(), invertor->GetFgColor());
    ht->Reconfig(output);
    hu->Reconfig(output);
    mt->Reconfig(output);
    mu->Reconfig(output);
}

DFace::~DFace () {
    delete clock;
    delete ht;
    delete hu;
    delete mt;
    delete mu;
    Unref(invertor);
}

void DFace::Resize () {
    int i;

    int w = xmax;
    int h = ymax;
    if (showDate) {
	// adjust vertical size for date
	h -= output->GetFont()->Height();
    }
    // resize colon
    for (i = 0; i < colon[0].count; i++) {
	colon[0].x[i] = Coord(ColonData[0].x[i] * w );
	colon[0].y[i] = Coord(ColonData[0].y[i] * h );
	colon[1].x[i] = Coord(ColonData[1].x[i] * w );
	colon[1].y[i] = Coord(ColonData[1].y[i] * h );
    }
    // resize AM/PM
    for (i = 0; i < 12; i++) {
	A.x[i] = Coord(AData.x[i] * w);
	A.y[i] = Coord(AData.y[i] * h);
	P.x[i] = Coord(PData.x[i] * w);
	P.y[i] = Coord(PData.y[i] * h);
	M.x[i] = Coord(MData.x[i] * w);
	M.y[i] = Coord(MData.y[i] * h);
    }
    if (showTime) {
	ht->Resize(canvas, h);
	hu->Resize(canvas, h);
	mt->Resize(canvas, h);
	mu->Resize(canvas, h);
    }
}

void DFace::Redraw (Coord left, Coord bottom, Coord right, Coord top) {
    output->Clip(canvas, left, bottom, right, top);
    Draw();
    output->NoClip();
}

void DFace::RedrawList (int, Coord[], Coord[], Coord[], Coord[]) {
    Redraw(0, 0, xmax, ymax);
}

void DFace::Draw () {
    DrawFace();
    if (showDate) {
	DrawDate();
    }
    if (showTime) {
	DrawColon();
	DrawAMPM(output);
	ht->Redraw();
	hu->Redraw();
	mt->Redraw();
	mu->Redraw();
    }
    if (showBorder) {
	DrawBorder();
    }
}

void DFace::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.len > 0 && e.keystring[0] == 'q') {
	done = true;
	GetWorld()->quit();
    }
}

void DFace::Run () {
    Tick(0, 0);
    if (!done) {
	GetWorld()->run();
    }
}
