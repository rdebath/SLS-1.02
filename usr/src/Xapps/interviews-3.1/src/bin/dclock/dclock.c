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
 * dclock - digital clock
 */

#include "dclock.h"
#include "dface.h"
#include <stdio.h>
#include <stdlib.h>

DFace *TheClock;
World *world;

static void DoArgs(int argc, char *argv[]);

int main (int argc, char * argv[]) {
    world = new World("Dclock", argc, argv);
    DoArgs(argc, argv);
    InitData();
    switch (CreateMode) {
	case SWEPT:
	    TheClock = new DFace(
		ShowDate, ShowBorder, ShowTime, TimeMode
	    );
	    world->InsertApplication(TheClock);
	    break;
	case PLACED:
	    TheClock = new DFace(
		ShowDate, ShowBorder, ShowTime, TimeMode, Width, Height
	    );
	    world->InsertApplication(TheClock);
	    break;
	case DEFINED:
	    TheClock = new DFace(
		ShowDate, ShowBorder, ShowTime, TimeMode, Width, Height
	    );
	    world->InsertApplication(TheClock, XPos, YPos);
	    break;
    }
    TheClock->Run();
    return 0;
}

static void DoArgs( int argc, char * argv[] ) {
    int i, j, p1, p2;
    char* curarg;

    for (i = 1; i < argc; i++) {
	curarg = argv[i];
	if (sscanf(curarg, "size=%d,%d", &p1, &p2) == 2) {
	    if (CreateMode != DEFINED) {
		CreateMode = PLACED;			// size specified
	    }
	    Width = p1;
	    Height = p2;
	} else if (sscanf(curarg, "pos=%d,%d", &p1, &p2) == 2) {
	    CreateMode = DEFINED;			// position specified
	    XPos = p1;
	    YPos = p2;
	} else if (sscanf(curarg, "-s%d", &p1) == 1) {
	    SlantPC = p1;
	} else if (sscanf(curarg, "-t%d", &p1) == 1) {
	    ThickPC = p1;
	} else if (sscanf(curarg, "-f%d", &p1) == 1) {
	    FadeRate = p1;
	} else if (curarg[0] == '-') {
	    for (j = 1; curarg[j] != '\0'; j++) {
		switch (curarg[j]) {
		    case 'c':
			TimeMode = CIVIL;		// the default
			break;
		    case 'm':
			TimeMode = MIL;			// 24 hour
			break;
		    case 'j':
			JohnsFlag = true;		// tail on 9
			break;
		    case 'd':
			ShowDate = false;
			break;
		    case 's':
			SlantPC = 0;
			break;
		    case 'f':
			FadeRate = 0;
			break;
		    case 'b':				//show date/time border
			ShowBorder = true;
			break;
		    case 'T':
			ShowTime = false;		// date only
			break;
		    default:
			fprintf(stderr,
			    "%s: unrecognized flag '%c'\n", argv[0], curarg[j]
			);
			break;
		}
	    }
	} else {
	    fprintf(stderr, "%s: unexpected argument '%s'\n", argv[0], curarg);
	    fprintf(stderr,
		"usage: %s [-cmjidbT] [-s#] [-t#] [-f#] [pos=#,#] [size=#,#]\n",
		argv[0]
	    );
	    exit(1);
	}
    }
}
