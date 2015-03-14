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
 * Common definitions for all of InterViews.
 */

#ifndef iv2_6_alignment_h
#define iv2_6_alignment_h

#include <InterViews/enter-scope.h>

#include <IV-2_6/_enter.h>

/*
 * Alignment needs to be unsigned so that it can be stored
 * as a bit field.
 */

typedef unsigned Alignment;

/*
 * Use the concrete type (unsigned) instead of Alignment
 * to get around cfront 2.1 incorrect warning.
 */
static const unsigned TopLeft = 0;
static const unsigned TopCenter = 1;
static const unsigned TopRight = 2;
static const unsigned CenterLeft = 3;
static const unsigned Center = 4;
static const unsigned CenterRight = 5;
static const unsigned BottomLeft = 6;
static const unsigned BottomCenter = 7;
static const unsigned BottomRight = 8;
static const unsigned Left = 9;
static const unsigned Right = 10;
static const unsigned Top = 11;
static const unsigned Bottom = 12;
static const unsigned HorizCenter = 13;
static const unsigned VertCenter = 14;

#include <IV-2_6/_leave.h>

#endif
