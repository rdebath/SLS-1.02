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
 * Directory object implementation.
 */

#include "direct.h"
#include "globals.h"
#include <OS/directory.h>
#include <OS/string.h>
#include <OS/types.h>
#include <stddef.h>
#include <osfcn.h>
#include <pwd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>

#ifdef __DECCXX
extern "C" {
    extern uid_t getuid();
    extern struct passwd* getpwuid(uid_t);
}
#endif

#undef File
#undef Directory
#define SysDir _lib_os(Directory)

class DirectoryRep {
private:
    friend class Directory;

    SysDir* dir;
};

static const int MAX_PATH_LENGTH = 256;

/*****************************************************************************/

Directory::Directory (const char* name) {
    rep_ = new DirectoryRep;
    rep_->dir = nil;
    LoadDirectory(name);
}

Directory::~Directory () {
    delete rep_->dir;
    delete rep_;
}

const char* Directory::RealPath (const char* path) {
    const char* realpath;

    if (*path == '\0') {
        realpath = "./";
    } else {
        realpath = InterpTilde(InterpSlashSlash(path));
    }
    return realpath;
}

boolean Directory::LoadDirectory (const char* name) {
    char buf[MAX_PATH_LENGTH+2];
    const char* path = buf;

    strcpy(buf, ValidDirectories(RealPath(name)));
    return Reset(buf);
}

int Directory::Index (const char* name) {
    _lib_os(String) s(name);
    return rep_->dir == nil ? -1 : rep_->dir->index(s);
}

boolean Directory::Reset (char* path) {
    SysDir* d = SysDir::open(_lib_os(String)(path));
    if (d != nil) {
	delete rep_->dir;
	rep_->dir = d;
	return true;
    }
    return false;
}

boolean Directory::IsADirectory (const char* path) {
    struct stat filestats;
    stat(path, &filestats);
    return filestats.st_mode & S_IFDIR;
}

int Directory::Count() {
    return rep_->dir == nil ? 0 : rep_->dir->count();
}

const char* Directory::File(int index) {
    if (rep_->dir == nil) {
	return nil;
    }
    const _lib_os(String)* s = rep_->dir->name(index);
    if (s == nil) {
	return nil;
    }
    return s->string();
}

const char* Directory::Home (const char* name) {
    struct passwd* pw =
	(name == nil) ? getpwuid(getuid()) : getpwnam(name);
    return (pw == nil) ? nil : pw->pw_dir;
}

inline boolean DotSlash (const char* path) {
    return 
        path[0] != '\0' && path[0] == '.' &&
        (path[1] == '/' || path[1] == '\0');
}

inline boolean DotDotSlash (const char* path) {
    return 
        path[0] != '\0' && path[1] != '\0' &&
        path[0] == '.' && path[1] == '.' &&
        (path[2] == '/' || path[2] == '\0');
}

const char* Directory::Normalize (const char* path) {
    static char newpath[MAX_PATH_LENGTH+1];
    const char* buf;

    buf = InterpSlashSlash(path);
    buf = ElimDot(buf);
    buf = ElimDotDot(buf);
    buf = InterpTilde(buf);

    if (*buf == '\0') {
        strcpy(newpath, "./");

    } else if (!DotSlash(buf) && !DotDotSlash(buf) && *buf != '/') {
        strcpy(newpath, "./");
        strcat(newpath, buf);

    } else {
        strcpy(newpath, buf);
    }

    if (IsADirectory(newpath) && newpath[strlen(newpath)-1] != '/') {
        strcat(newpath, "/");
    }
    return newpath;
}

const char* Directory::ValidDirectories (const char* path) {
    static char buf[MAX_PATH_LENGTH+1];
    strcpy(buf, path);
    int i = strlen(path);

    while (!IsADirectory(RealPath(buf)) && i >= 0) {
        for (--i; buf[i] != '/' && i >= 0; --i);
        buf[i+1] = '\0';
    }
    return buf;
}

const char* Directory::InterpSlashSlash (const char* path) {
    for (int i = strlen(path)-1; i > 0; --i) {
        if (path[i] == '/' && path[i-1] == '/') {
            return &path[i];
        }
    }
    return path;
}

const char* Directory::ElimDot (const char* path) {
    static char newpath[MAX_PATH_LENGTH+1];
    const char* src;
    char* dest = newpath;

    for (src = path; src < &path[strlen(path)]; ++src) {
        if (!DotSlash(src)) {
            *dest++ = *src;

        } else if (*(dest-1) == '/') {
            ++src;

        } else {
            *dest++ = *src;
        }            
    }
    *dest = '\0';
    return newpath;
}

static boolean CollapsedDotDotSlash (char* path, char*& start) {
    if (path == start || *(start-1) != '/') {
        return false;

    } else if (path == start-1 && *path == '/') {
        return true;

    } else if (path == start-2) {               // NB: won't handle '//' right
        start = path;
        return *start != '.';

    } else if (path < start-2 && !DotDotSlash(start-3)) {
        for (start -= 2; path <= start; --start) {
            if (*start == '/') {
                ++start;
                return true;
            }
        }
        start = path;
        return true;
    }
    return false;
}

const char* Directory::ElimDotDot (const char* path) {
    static char newpath[MAX_PATH_LENGTH+1];
    const char* src;
    char* dest = newpath;

    for (src = path; src < &path[strlen(path)]; ++src) {
        if (DotDotSlash(src) && CollapsedDotDotSlash(newpath, dest)) {
            src += 2;
        } else {
            *dest++ = *src;
        }
    }
    *dest = '\0';
    return newpath;
}

const char* Directory::InterpTilde (const char* path) {
    static char realpath[MAX_PATH_LENGTH+1];
    const char* beg = strrchr(path, '~');
    boolean validTilde = beg != nil && (beg == path || *(beg-1) == '/');

    if (validTilde) {
        const char* end = strchr(beg, '/');
        int length = (end == nil) ? strlen(beg) : end - beg;
        const char* expandedTilde = ExpandTilde(beg, length);

        if (expandedTilde == nil) {
            validTilde = false;
        } else {
            strcpy(realpath, expandedTilde);
            if (end != nil) {
                strcat(realpath, end);
            }
        }
    }
    return validTilde ? realpath : path;
}

const char* Directory::ExpandTilde (const char* tildeName, int length) {
    const char* name = nil;

    if (length > 1) {
        static char buf[MAX_PATH_LENGTH+1];
        strncpy(buf, tildeName+1, length-1);
        buf[length-1] = '\0';
        name = buf;
    }
    return Home(name);
}        
