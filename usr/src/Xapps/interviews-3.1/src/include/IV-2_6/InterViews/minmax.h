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

#ifndef iv2_6_minmax_h
#define iv2_6_minmax_h

#include <InterViews/boolean.h>

#define declare_2(T) \
inline T min(T a, T b) { return a < b ? a : b; } \
inline T max(T a, T b) { return a > b ? a : b; } \

declare_2(int)
declare_2(unsigned)
declare_2(float)
declare_2(double)

/*
 * Compiler isn't smart enough to figure out how to do 4-way min inline.
 */

#define declare_4(T) \
inline T min(T a, T b, T c, T d) { \
    T r1 = min(a, b), r2 = min(c, d); \
    return min(r1, r2); \
} \
\
inline T max(T a, T b, T c, T d) { \
    T r1 = max(a, b), r2 = max(c, d); \
    return max(r1, r2); \
}

declare_4(int)
declare_4(float)
declare_4(double)

inline int round(double x) { return x > 0 ? int(x+0.5) : -int(-x+0.5); }

inline boolean equal(float x, float y, float e) {
    return x - y < e && y - x < e;
}

#endif
