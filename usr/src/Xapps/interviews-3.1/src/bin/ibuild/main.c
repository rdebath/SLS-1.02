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
 * User interface builder main program.
 */

#include "ibcatalog.h"
#include "ibcreator.h"
#include "ibed.h"

#include <Unidraw/catalog.h>
#include <Unidraw/unidraw.h>
#include <InterViews/world.h>
#include <stream.h>

/*****************************************************************************/

static PropertyData properties[] = {
    { "*domain",	"user interface" },
    { "*history",	"20" },
    { "*initialborder",  "2" },
    { "*initialbrush",  "1" },
    { "*initialfgcolor","1" },
    { "*initialbgcolor","10" },
    { "*initialfont",   "4" },
    { "*initialpattern","3" },
    { "*font1",         "*-courier-medium-r-*-80-*    Courier 8" },
    { "*font2",         "*-courier-medium-r-*-100-*   Courier 10" },
    { "*font3",         "*-courier-bold-r-*-120-*     Courier-Bold 12" },
    { "*font4",         "*-helvetica-medium-r-*-120-* Helvetica 12" },
    { "*font5",         "*-helvetica-medium-r-*-140-* Helvetica 14" },
    { "*font6",         "*-helvetica-bold-r-*-140-*   Helvetica-Bold 14" },
    { "*font7",         "*-helvetica-medium-o-*-140-* Helvetica-Oblique 14"},
    { "*font8",         "*-times-medium-r-*-120-*     Times-Roman 12" },
    { "*font9",         "*-times-medium-r-*-140-*     Times-Roman 14" },
    { "*font10",        "*-times-bold-r-*-140-*       Times-Bold 14" },
    { "*font11",        "*-times-medium-i-*-140-*     Times-Italic 14" },
    { "*border1",        "ffff 1" },
    { "*border2",        "ffff 2" },
    { "*border3",        "ffff 3" },
    { "*border4",        "ffff 4" },
    { "*border5",        "ffff 5" },
    { "*brush1",	"none" },
    { "*brush2",	"ffff 0" },
    { "*brush3",	"ffff 1" },
    { "*brush4",	"ffff 2" },
    { "*brush5",	"ffff 3" },
    { "*brush6",	"fff0 0" },
    { "*brush7",	"fff0 1" },
    { "*brush8",	"fff0 2" },
    { "*brush9",	"fff0 3" },
    { "*pattern1",	"none" },
    { "*pattern2",	"0.0" },
    { "*pattern3",	"1.0" },
    { "*pattern4",	"0.75" },
    { "*pattern5",	"0.5" },
    { "*pattern6",	"0.25" },
    { "*pattern7",	"1248" },
    { "*pattern8",	"8421" },
    { "*pattern9",	"f000" },
    { "*pattern10",	"8888" },
    { "*pattern11",	"f888" },
    { "*pattern12",	"8525" },
    { "*pattern13",	"cc33" },
    { "*pattern14",	"7bed" },
    { "*fgcolor1",      "Black 0 0 0" },
    { "*fgcolor2",      "Brown 42240 10752 10752" },
    { "*fgcolor3",      "Red 65535 0 0" },
    { "*fgcolor4",      "Orange 65535 42405 0" },
    { "*fgcolor5",      "Yellow 65535 65535 0" },
    { "*fgcolor6",      "Green 0 65535 0" },
    { "*fgcolor7",      "Blue 0 0 65535" },
    { "*fgcolor8",      "Indigo 48896 0 65280" },
    { "*fgcolor9",      "Violet 20224 12032 20224" },
    { "*fgcolor10",     "White 65535 65535 65535" },
    { "*fgcolor11",     "LtGray 50000 50000 50000" },
    { "*fgcolor12",     "DkGray 33000 33000 33000" },
    { "*bgcolor1",      "Black 0 0 0" },
    { "*bgcolor2",      "Brown 42240 10752 10752" },
    { "*bgcolor3",      "Red 65535 0 0" },
    { "*bgcolor4",      "Orange 65535 42405 0" },
    { "*bgcolor5",      "Yellow 65535 65535 0" },
    { "*bgcolor6",      "Green 0 65535 0" },
    { "*bgcolor7",      "Blue 0 0 65535" },
    { "*bgcolor8",      "Indigo 48896 0 65280" },
    { "*bgcolor9",      "Violet 20224 12032 20224" },
    { "*bgcolor10",     "White 65535 65535 65535" },
    { "*bgcolor11",     "LtGray 50000 50000 50000" },
    { "*bgcolor12",     "DkGray 33000 33000 33000" },
    { nil }
};

static OptionDesc options[] = {
    { nil }
};

/*****************************************************************************/

int main (int argc, char** argv) {
    IBCreator creator;
    int exit_status = 0;
    Unidraw *unidraw = new Unidraw(
        new IBuildCatalog(
            "ibuild", &creator, 1.1
        ), argc, argv, options, properties
    );

    if (argc > 2) {
	cerr << "Usage: ibuild [file]" << "\n";
	exit_status = 1;

    } else {
	const char* initial_file = (argc == 2) ? argv[1] : nil;
	IBEditor* ed = new IBEditor(initial_file);

	unidraw->Open(ed);
	unidraw->Run();
    }

    delete unidraw;
    return exit_status;
}
