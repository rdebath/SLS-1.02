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
 * PPM_Image - read a Raster from a file containing postscript from ppmtops
 */

#include "PPM_Image.h"

#include <InterViews/image.h>
#include <InterViews/raster.h>

#include <string.h>

static int hexmap[] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

static int gethex (FILE* file) {
    int c;
    while ((c = getc(file)) == ' ' || c == '\n') { }
    return (hexmap[c] << 4) + hexmap[getc(file)];
}

PPM_Image::PPM_Image(FILE* file) {
    char line[1000];
    do {
        fgets(line, 1000, file);
    } while (strcmp(line, "gsave\n") != 0);

    fgets(line, 1000, file);                    // translate
    fgets(line, 1000, file);                    // scale
    fgets(line, 1000, file);                    // scale
    fgets(line, 1000, file);                    // sizes
    int w, h, d;
    sscanf(line, "%d %d %d", &w, &h, &d);
    fgets(line, 1000, file);                    // [ ... ]
    fgets(line, 1000, file);                    // { ... }
    fgets(line, 1000, file);                    // false 3
    fgets(line, 1000, file);                    // colorimage

    Raster* raster = new Raster(w, h);
    
    for (int row = h - 1; row >= 0; --row) {
        for (int column = 0; column < w; ++column) {
            int red = gethex(file);
            int green = gethex(file);
            int blue = gethex(file);
            raster->poke(
                column, row,
                float(red)/0xff, float(green)/0xff, float(blue)/0xff, 1.0
            );
        }
    }
    raster->flush();
    body(new Image(raster));
}

PPM_Image::~PPM_Image() { }
