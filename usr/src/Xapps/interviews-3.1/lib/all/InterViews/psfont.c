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
 * PSFont - use PostScript font metrics
 */

#include <InterViews/psfont.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Users can also override this by setting the PSFONTDIR environment variable.
 */
#ifndef ps_metrics_dir
#define ps_metrics_dir "/usr/lib/ps"
#endif

class PSFontImpl {
private:
    friend class PSFont;

    char* name;
    char* encoding;
    Coord size;
    Coord widths[256];

    static char* psfile(const char* name);
};

PSFont::PSFont(
    const char* psname, Coord size, const char* name, float scale
) : Font(name, scale) {
    PSFontImpl* p = new PSFontImpl;
    impl_ = p;
    p->name = nil;
    p->encoding = nil;
    p->size = size;
    char* metrics_file = PSFontImpl::psfile(psname);
    FILE* file = fopen(metrics_file, "r");
    if (file != nil) {
	p->name = new char[256];
	p->encoding = new char[256];

	char line[256];
	int c;
	int w;
	while (fgets(line, 255, file) != NULL) {
	    if (sscanf(line, "FullName %[a-zA-Z ]", p->name) == 1) {
		;
	    } else if (sscanf(line, "EncodingScheme %s", p->encoding) == 1) {
		;
	    } else if (sscanf(line, "C %d ; WX %d ;", &c, &w) == 2) {
		if (c != -1) {
		    p->widths[c] = float(w) / 1000 * p->size;
		}
	    }
	}
	fclose(file);
    }
    delete metrics_file;
}

PSFont::~PSFont() {
    delete impl_->name;
    delete impl_->encoding;
    delete impl_;
}

const char* PSFont::name() const { return impl_->name; }
const char* PSFont::encoding() const { return impl_->encoding; }
Coord PSFont::size() const { return impl_->size; }
Coord PSFont::width(long c) const { return impl_->widths[c]; }
Coord PSFont::width(const char* s, int n) const { return Font::width(s, n); }

boolean PSFont::exists(const char* psname) {
    char* metrics_file = PSFontImpl::psfile(psname);
    FILE* f = fopen(metrics_file, "r");
    delete metrics_file;
    if (f == nil) {
	return false;
    }
    fclose(f);
    return true;
}

char* PSFontImpl::psfile(const char* name) {
    const char* dir = getenv("PSFONTDIR");
    if (dir == nil) {
	dir = ps_metrics_dir;
    }
    char* metrics_file = new char[
	strlen(dir) + strlen("/") + strlen(name) + strlen(".afm") + 1
    ];
    sprintf(metrics_file, "%s/%s.afm", dir, name);
    return metrics_file;
}
