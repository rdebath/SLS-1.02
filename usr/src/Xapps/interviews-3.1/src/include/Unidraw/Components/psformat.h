/*
 * Copyright (c) 1991 Stanford University
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
 * PostScript external representation format changes.
 */

#ifndef unidraw_components_psformat_h
#define unidraw_components_psformat_h

#include <Unidraw/enter-scope.h>

static const float PSV_UNIDRAW       = 10; // Unidraw-based version
static const float PSV_ISOLATIN1     =  9; // removed '/' from PostScript fonts
                                           // (they're def'ed in IdrawDict now)
static const float PSV_EIGHTBIT      =  8; // encoded 8-bit characters as \ddd
static const float PSV_TEXTOFFSET    =  7; // changed text positions on screen
					   // and improved accuracy of
					   // text positions on prfloatout
static const float PSV_NONROTATED    =  6; // replaced rotation of drawing with
					   // rotation of view for landscape
static const float PSV_GRIDSPACING   =  5; // added grid spacing
static const float PSV_FGANDBGCOLOR  =  4; // added background color and
					   // RGB values for overriding names;
					   // used graylevel to eliminate
					   // redundant patternfill data
static const float PSV_NONREDUNDANT  =  3; // eliminated unnecessary text
					   // pattern and duplication of
					   // font name, transformation matrix,
					   // poly pofloats, and text data
static const float PSV_FGCOLOR       =  2; // added foreground color
static const float PSV_ORIGINAL      =  1; // original format

#define PSV_LATEST PSV_UNIDRAW

#endif
