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
 * Unique idraw class identifiers
 */

#ifndef idclasses_h
#define idclasses_h

#include <Unidraw/classes.h>

#define ABOUT_CMD                1001
#define ARROWLINE_COMP           1002
#define ARROWMULTILINE_COMP      1003
#define ARROWSPLINE_COMP         1004
#define ARROW_CMD                1005
#define ARROW_VAR                1006
#define IDRAW_COMP               1007
#define IGRIDSPACING_CMD         1008
#define NEWVIEW_CMD              1009
#define OPEN_CMD                 1010
#define PRECISEMOVE_CMD          1011
#define PRECISEROTATE_CMD        1012
#define PRECISESCALE_CMD         1013

#define ARROWLINE_VIEW      Combine(ARROWLINE_COMP, COMPONENT_VIEW)
#define ARROWMULTILINE_VIEW Combine(ARROWMULTILINE_COMP, COMPONENT_VIEW)
#define ARROWSPLINE_VIEW    Combine(ARROWSPLINE_COMP, COMPONENT_VIEW)
#define IDRAW_VIEW          Combine(IDRAW_COMP, COMPONENT_VIEW)
#define PS_ARROWLINE        Combine(ARROWLINE_COMP, POSTSCRIPT_VIEW)
#define PS_ARROWMULTILINE   Combine(ARROWMULTILINE_COMP, POSTSCRIPT_VIEW)
#define PS_ARROWSPLINE      Combine(ARROWSPLINE_COMP, POSTSCRIPT_VIEW)
#define PS_IDRAW            Combine(IDRAW_COMP, POSTSCRIPT_VIEW)

#endif



