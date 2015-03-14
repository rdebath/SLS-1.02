/*
 * Copyright (c) 1989 Stanford University
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
 * InterViews class browser main program.
 */

#include "classbuffer.h"
#include "iclass.h"

#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <InterViews/world.h>

#include <string.h>

/*****************************************************************************/

static PropertyData properties[] = {
    { "*Message*font",              "*helvetica-bold-r-normal--10*" },
    { "*Dialog*Message*font",       "*helvetica-bold-r-normal--12*" },
    { "*path*font",                 "*helvetica-bold-r-normal--12*" },
    { "*PulldownMenu*Message*font", "*helvetica-bold-r-normal--12*" },
    { "*PushButton*font",           "*helvetica-bold-r-normal--12*" },
    { "*StringBrowser*font",        "*helvetica-bold-r-normal--10*" },
    { "*dirBrowser*font",           "*helvetica-bold-r-normal--10*" },
    { "*StringEditor*font",         "*helvetica-bold-r-normal--10*" },
    { "*recursive",                 "false" },
    { "*verbose",                   "false" },
    { "*CPlusPlusFiles",            "false" },
    { "*showButton",                "true" },
    { "*dirBrowser*singleClick",    "on" },
    { nil }
};

static OptionDesc options[] = {
    { "-r", "*recursive", OptionValueImplicit, "true" },
    { "-v", "*verbose", OptionValueImplicit, "true" },
    { "-c", "*CPlusPlusFiles", OptionValueImplicit, "true" },
    { nil }
};

/*****************************************************************************/

int main (int argc, char** argv) {
    World world("iclass", argc, argv, options, properties);
    const char* recursive = world.GetAttribute("recursive");
    const char* verbose = world.GetAttribute("verbose");
    const char* CPlusPlusFiles = world.GetAttribute("CPlusPlusFiles");
    ClassBuffer* buffer = new ClassBuffer(
        strcmp(recursive, "true") == 0, strcmp(verbose, "true") == 0,
	strcmp(CPlusPlusFiles, "true") == 0
    );

    for (int i = 1; i < argc; ++i) {
        buffer->Search(argv[i]);
    }

    IClass* iclass = new IClass(buffer);
    ApplicationWindow* window = new ApplicationWindow(iclass);
    Style* s = new Style(Session::instance()->style());
    s->attribute("name", "InterViews class browser");
    s->attribute("iconName", "iclass");
    window->style(s);
    window->map();
    iclass->Run();
    return 0;
}
