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
 * Keycode equivalents for controls.
 */

#ifndef unidraw_kybd_h
#define unidraw_kybd_h

#include <Unidraw/enter-scope.h>

/*
 * components
 */
extern const char* KLBL_LINE, *CODE_LINE;
extern const char* KLBL_ELLIPSE, *CODE_ELLIPSE;
extern const char* KLBL_RECT, *CODE_RECT;
extern const char* KLBL_POLY,  *CODE_POLY;
extern const char* KLBL_MULTILINE, *CODE_MULTILINE;
extern const char* KLBL_SPLINE, *CODE_SPLINE;
extern const char* KLBL_CSPLINE, *CODE_CSPLINE;
extern const char* KLBL_TEXT, *CODE_TEXT;
extern const char* KLBL_PIN, *CODE_PIN;
extern const char* KLBL_SLOT, *CODE_SLOT;
extern const char* KLBL_PAD, *CODE_PAD;
extern const char* KLBL_LINK, *CODE_LINK;

/*
 * commands
 */

extern const char* KLBL_NEWCOMP, *CODE_NEWCOMP;                   /* catalog */
extern const char* KLBL_REVERT, *CODE_REVERT;
extern const char* KLBL_VIEWCOMP, *CODE_VIEWCOMP;
extern const char* KLBL_SAVECOMP, *CODE_SAVECOMP;
extern const char* KLBL_SAVECOMPAS, *CODE_SAVECOMPAS;
extern const char* KLBL_PRINT, *CODE_PRINT;
extern const char* KLBL_IMPORT, *CODE_IMPORT;
extern const char* KLBL_QUIT, *CODE_QUIT;

extern const char* KLBL_UNDO, *CODE_UNDO;			     /* edit */
extern const char* KLBL_REDO, *CODE_REDO;
extern const char* KLBL_CUT, *CODE_CUT;
extern const char* KLBL_COPY, *CODE_COPY;
extern const char* KLBL_PASTE, *CODE_PASTE;
extern const char* KLBL_DUP, *CODE_DUP;
extern const char* KLBL_DEL, *CODE_DEL;
extern const char* KLBL_SLCTALL, *CODE_SLCTALL;
extern const char* KLBL_HFLIP, *CODE_HFLIP;
extern const char* KLBL_VFLIP, *CODE_VFLIP;
extern const char* KLBL_CW90, *CODE_CW90;
extern const char* KLBL_CCW90, *CODE_CCW90;

extern const char* KLBL_GROUP, *CODE_GROUP;		        /* structure */
extern const char* KLBL_UNGROUP, *CODE_UNGROUP;
extern const char* KLBL_SHOWSTRUCT, *CODE_SHOWSTRUCT;
extern const char* KLBL_FRONT, *CODE_FRONT;
extern const char* KLBL_BACK, *CODE_BACK;
extern const char* KLBL_INSTANCE, *CODE_INSTANCE;

extern const char* KLBL_ALGNLEFT, *CODE_ALGNLEFT;		    /* align */
extern const char* KLBL_ALGNRIGHT, *CODE_ALGNRIGHT;
extern const char* KLBL_ALGNTOP, *CODE_ALGNTOP;
extern const char* KLBL_ALGNBOT, *CODE_ALGNBOT;
extern const char* KLBL_ALGNHCTR, *CODE_ALGNHCTR;
extern const char* KLBL_ALGNVCTR, *CODE_ALGNVCTR;
extern const char* KLBL_ALGNCTR, *CODE_ALGNCTR;
extern const char* KLBL_ABUTLEFT, *CODE_ABUTLEFT;
extern const char* KLBL_ABUTRIGHT, *CODE_ABUTRIGHT;
extern const char* KLBL_ABUTUP, *CODE_ABUTUP;
extern const char* KLBL_ABUTDOWN, *CODE_ABUTDOWN;
extern const char* KLBL_ALGNTOGRID, *CODE_ALGNTOGRID;

extern const char* KLBL_NORMSIZE, *CODE_NORMSIZE;		     /* view */
extern const char* KLBL_REDTOFIT, *CODE_REDTOFIT;
extern const char* KLBL_CENTER, *CODE_CENTER;
extern const char* KLBL_GRID, *CODE_GRID;
extern const char* KLBL_GRIDSPC, *CODE_GRIDSPC;
extern const char* KLBL_GRAVITY, *CODE_GRAVITY;
extern const char* KLBL_ORIENTATION, *CODE_ORIENTATION;
extern const char* KLBL_CLOSEEDITOR, *CODE_CLOSEEDITOR;

/*
 * tools
 */
extern const char* KLBL_SELECT, *CODE_SELECT;
extern const char* KLBL_MOVE, *CODE_MOVE;
extern const char* KLBL_CONNECT, *CODE_CONNECT;
extern const char* KLBL_DETACH, *CODE_DETACH;
extern const char* KLBL_SCALE, *CODE_SCALE;
extern const char* KLBL_STRETCH, *CODE_STRETCH;
extern const char* KLBL_ROTATE, *CODE_ROTATE;
extern const char* KLBL_RESHAPE, *CODE_RESHAPE;
extern const char* KLBL_MAGNIFY, *CODE_MAGNIFY;

#endif
