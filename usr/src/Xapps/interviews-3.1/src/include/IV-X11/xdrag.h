/*
 * Copyright (c) 1992 Redwood Design Automation
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the name of
 * Redwood Design Automation may not be used in any advertising or publicity
 * relating to the software without the specific, prior written permission of
 * Redwood Design Automation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL REDWOOD DESIGN AUTOMATION BE LIABLE FOR ANY SPECIAL,
 * INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT
 * ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#ifndef iv_xdrag_h
#define iv_xdrag_h

#include <InterViews/_enter.h>

class XDrag {
public:
    // these are used by xevent to detect and interpret a drag client message.
    static boolean isDrag(const XEvent&);
    static void locate(const XEvent&, int& x, int& y);
};

#include <InterViews/_leave.h>

#endif
