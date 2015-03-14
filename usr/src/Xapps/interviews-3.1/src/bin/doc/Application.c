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
 * Application
 */

#include "Application.h"

#include "Document.h"
#include "DocViewer.h"
#include "DialogMgr.h"

#include "properties.h"

#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <OS/list.h>
#include <OS/string.h>
#include <OS/types.h>

#include <fstream.h>
#include <strstream.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

static int DEFAULT_SIZE_HINT = 1000;
static const char* MAGIC = "%Doc-";

class ApplicationViewerInfo {
public:
    DocumentViewer* _viewer;
    DialogManager* _dialogs;
    char* _window_name;
    char* _icon_name;
};

declareList(ApplicationViewerInfo_List,ApplicationViewerInfo)
implementList(ApplicationViewerInfo_List,ApplicationViewerInfo)

static int viewer_info(
    DocumentViewer* viewer, ApplicationViewerInfo_List* viewers
) {
    long count = viewers->count();
    for (long i = 0; i < count; ++i) {
        ApplicationViewerInfo& info = viewers->item_ref(i);
        if (info._viewer == viewer) {
            return i;
        }
    }
    return -1;
}

Application::Application () {
    _session = Session::instance();
    _viewer = new ApplicationViewerInfo_List;
}

Application::~Application () {
    delete _viewer;
}

const char* Application::choose (
    DocumentViewer* viewer, const char* prompt, const char* filter
) {
    ApplicationViewerInfo& info = _viewer->item_ref(
	viewer_info(viewer, _viewer)
    );
    raise_for_post(info);
    return info._dialogs->choose(info._viewer, prompt, filter);
}

const char* Application::ask (
    DocumentViewer* viewer, const char* prompt, const char* initial
) {
    ApplicationViewerInfo& info = _viewer->item_ref(
	viewer_info(viewer, _viewer)
    );
    raise_for_post(info);
    return info._dialogs->ask(info._viewer, prompt, initial);
}

int Application::confirm (DocumentViewer* viewer, const char* prompt) {
    ApplicationViewerInfo& info = _viewer->item_ref(
	viewer_info(viewer, _viewer)
    );
    raise_for_post(info);
    return info._dialogs->confirm(info._viewer, prompt);
}

void Application::report (DocumentViewer* viewer, const char* prompt) {
    ApplicationViewerInfo& info = _viewer->item_ref(
	viewer_info(viewer, _viewer)
    );
    raise_for_post(info);
    info._dialogs->report(info._viewer, prompt);
}

void Application::complain (DocumentViewer* viewer, const char* prompt) {
    boolean ring = true;
    String complaint_mode;
    if (_session->style()->find_attribute(COMPLAINT_MODE, complaint_mode)) {
	if (complaint_mode == "report") {
	    report(viewer, prompt);
	    ring = false;
	} else if (complaint_mode == "ignore") {
	    ring = false;
	}
    }
    if (ring) {
	_session->default_display()->ring_bell(10);
    }
}

void Application::open (DocumentViewer* viewer) {
    long index = viewer_info(viewer, _viewer);
    if (index < 0) {
        index = _viewer->count();
        ApplicationViewerInfo info;
        info._dialogs = new DialogManager();
        info._viewer = viewer;
        info._viewer->ref();
        info._viewer->map();
        info._window_name = nil;
        info._icon_name = nil;
        _viewer->append(info);
    }
    ApplicationViewerInfo& info = _viewer->item_ref(index);
    const char* name = viewer->document()->name();
    if (info._window_name == nil || strcmp(info._window_name, name) != 0) {
        delete info._window_name;
        delete info._icon_name;
        if (name != nil) {
            info._window_name = strcpy(new char[strlen(name) + 1], name);
            const char* icon_name = strrchr(name, '/');
            if (icon_name != nil) {
                icon_name += 1;
            } else {
                icon_name = name;
            }
            info._icon_name = strcpy(new char[strlen(name) + 1], name);
	    Style* s = info._viewer->style();
	    if (s == nil) {
		s = new Style(Session::instance()->style());
		info._viewer->style(s);
	    }
	    s->attribute("name", info._window_name);
	    s->attribute("iconName", info._icon_name);
        } else {
            info._window_name = nil;
            info._icon_name = nil;
        }
    }
}

void Application::close (DocumentViewer* viewer) {
    int index = viewer_info(viewer, _viewer);
    if (index >= 0) {
        ApplicationViewerInfo& info = _viewer->item_ref(index);
        info._viewer->unmap();
        info._viewer->unref();
        delete info._dialogs;
        delete info._window_name;
        delete info._icon_name;
        _viewer->remove(index);
	if (_viewer->count() == 0) {
	    _session->quit();
	}
    }
}

Document* Application::read (const char* file_name) {
    Document* document = nil;
    String doc_version;
    _session->style()->find_attribute(VERSION, doc_version);
    struct stat filestats;
    if (strlen(file_name) > 0 && stat(file_name, &filestats) == 0) {
        ifstream in(file_name);
        char buffer[256];
        in.getline(buffer, 256);
        in.seekg(0);
        char* version = strrchr(buffer, MAGIC[0]);
        int l = strlen(MAGIC);
        if (
            version == nil
            || (
                strncmp(version, MAGIC, l) == 0 &&
		doc_version == (version + l)
            )
        ) {
            document = new Document(this, int(filestats.st_size));
            document->read(in);
        }
    } else {
        document = new Document(this, DEFAULT_SIZE_HINT);
	NullTerminatedString doc_version_0(doc_version);
        strstream empty;
        empty << MAGIC;
        empty << doc_version_0.string();
        empty << "\n";
        empty << "\\documentstyle{";
	String default_style;
	_session->style()->find_attribute(DEFAULT_STYLE, default_style);
	NullTerminatedString default_style_0(default_style);
        empty << default_style_0.string();
        empty << "}\n";
        empty.seekg(0);
        document->read(empty);
    }
    if (document != nil) {
        document->touch(false);
        document->notify();
    }
    return document;
}

void Application::write (Document* document, const char* name) {
    String doc_version;
    _session->style()->find_attribute(VERSION, doc_version);
    NullTerminatedString doc_version_0(doc_version);
    ofstream out(name);
    out << MAGIC;
    out << doc_version_0.string();
    out << "\n";
    document->write(out);
    document->touch(false);
    document->notify();
}

boolean Application::file_path (
    const char* name, const char* extension, const char* pathlist, char* path
) {
    struct stat filestats;
    char filename[256];
    if (name[0] == '~' && name[1] == '/') {
        strcpy(filename, getenv("HOME"));
        strcat(filename, name+1);
    } else {
        strcpy(filename, name);
    }
    if (extension != nil && strrchr(filename, '.') <= strrchr(filename, '/')) {
        strcat(filename, ".");
        strcat(filename, extension);
    }
    if (filename[0] == '/') {
        strcpy(path, filename);
        return stat(path, &filestats) ==  0;
    } else {
        char path_list[256];
        strcpy(path_list, pathlist);
        char* p = strtok(path_list, ":");
        while (p != nil) {
            strcpy(path, p);
            strcat(path, "/");
            strcat(path, filename);
            if (stat(path, &filestats) == 0) {
                return true;
            } else {
                p = strtok(nil, ":");
            }
        }
        return false;
    }
}

boolean Application::command (const char* command) {
    if (strncmp(command, "application", 11) == 0) {
        const char* keyword = command + 12;
        if (strcmp(keyword, "quit") == 0) {
            long count = _viewer->count();
            for (long i = count-1; i >= 0; --i) {
                ApplicationViewerInfo& info = _viewer->item_ref(i);
                info._viewer->ref();
                info._viewer->command("viewer close");
                info._viewer->unref();
            }
            return true;
        }
    }
    return false;
}

void Application::raise_for_post(ApplicationViewerInfo& info) {
    if (_session->style()->value_is_on(AUTORAISE_ON_POST)) {
        info._viewer->raise();
    }
}
