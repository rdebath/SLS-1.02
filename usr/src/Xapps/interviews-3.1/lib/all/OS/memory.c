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

#include <OS/memory.h>
#if defined(sun) && OSMajorVersion >= 5
#include <memory.h>
#else
/* would that these lived in a standard place ... */
extern "C" {
    extern void bcopy(const void*, void*, int);
    extern int bcmp(const void*, const void*, int);
    extern void bzero(void*, int);
}
#endif

void Memory::copy(const void* from, void* to, unsigned int nbytes) {
#if defined(sun) && OSMajorVersion >= 5
    memcpy(to, from, nbytes);
#else
    bcopy(from, to, nbytes);
#endif
}

int Memory::compare(const void* b1, const void* b2, unsigned int nbytes) {
#if defined(sun) && OSMajorVersion >= 5
    return memcmp(b1, b2, nbytes) != 0;
#else
    return bcmp(b1, b2, nbytes);
#endif
}

void Memory::zero(void* b, unsigned int nbytes) {
#if defined(sun) && OSMajorVersion >= 5
    memset(b, 0, nbytes);
#else
    bzero(b, nbytes);
#endif
}
