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
 * A pattern is a bit array describing where to fill.
 */

#ifndef iv_pattern_h
#define iv_pattern_h

#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class PatternRep;

class Pattern : public Resource {
public:
    enum {
	solid = 0xffff,
	clear = 0,
	lightgray = 0x8020,
	gray = 0xa5a5,
	darkgray = 0xfafa
    };

    Pattern();
    Pattern(const char*, unsigned int width, unsigned int height);
    Pattern(int);		/* 4 x 4 */
    Pattern(const int*);	/* 16 x 16 */
    virtual ~Pattern();

    PatternRep* rep() const;
private:
    PatternRep* rep_;

    void init(const char*, unsigned int width, unsigned int height);
};

inline PatternRep* Pattern::rep() const { return rep_; }

#include <InterViews/_leave.h>

#endif
