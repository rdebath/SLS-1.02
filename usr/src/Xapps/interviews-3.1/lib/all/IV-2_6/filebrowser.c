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
 * FileBrowser implementation.
 */

#include <InterViews/regexp.h>
#include <IV-2_6/InterViews/filebrowser.h>
#include <IV-2_6/InterViews/perspective.h>
#include <OS/directory.h>
#include <OS/memory.h>
#include <OS/string.h>
#include <OS/types.h>
#include <pwd.h>
#include <stdlib.h>
#include <string.h>
#if defined(mips) && defined(ultrix)
#undef NULL
#endif
#include <sys/param.h>
#include <sys/stat.h>

/* no standard place for this */
extern "C" {
    extern uid_t getuid();
#ifdef __DECCXX
    extern struct passwd* getpwuid(uid_t);
#endif
}

#include <IV-2_6/_enter.h>

static const unsigned int max_filename_length = 256;

class FBDirectory {       
public:
    FBDirectory(const char* name);
    ~FBDirectory();

    boolean LoadDirectory(const char*);
    const char* Normalize(const char*);
    const char* ValidDirectories(const char*);

    int Index(const char*);
    const char* File(unsigned int index);
    int Count();

    boolean IsADirectory(const char*);
private:
    const char* Home(const char* = nil);
    const char* ElimDot(const char*);
    const char* ElimDotDot(const char*);
    const char* InterpSlashSlash(const char*);
    const char* InterpTilde(const char*);
    const char* ExpandTilde(const char*, int);
    const char* RealPath(const char*);

    boolean Reset(const char*);
private:
    Directory* dir;
};

int FBDirectory::Count() {
    return dir == nil ? 0 : dir->count();
}

const char* FBDirectory::File(unsigned int i) {
    return dir == nil ? nil : dir->name(i)->string();
}

static inline char* fb_strdup(const char* s) {
    char* dup = new char[strlen(s) + 1];
    strcpy(dup, s);
    return dup;
}

FBDirectory::FBDirectory(const char* name) {
    dir = nil;
    LoadDirectory(name);
}

FBDirectory::~FBDirectory() {
    delete dir;
}

const char* FBDirectory::RealPath(const char* path) {
    const char* realpath;

    if (*path == '\0') {
        realpath = "./";
    } else {
        realpath = InterpTilde(InterpSlashSlash(path));
    }
    return realpath;
}

boolean FBDirectory::LoadDirectory(const char* name) {
    char buf[max_filename_length+2];
    const char* path = buf;

    strcpy(buf, ValidDirectories(RealPath(name)));
    return Reset(buf);
}

int FBDirectory::Index(const char* name) {
    if (dir == nil) {
	return -1;
    }
    return dir->index(name);
}

boolean FBDirectory::Reset(const char* path) {
    Directory* d = Directory::open(path);
    if (d == nil) {
	return false;
    }
    delete dir;
    dir = d;
    return true;
}

boolean FBDirectory::IsADirectory(const char* path) {
    struct stat st;
    return stat(path, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR;
}

const char* FBDirectory::Home(const char* name) {
    /* cast to workaround DEC C++ prototype bug */
    struct passwd* pw =
        (name == nil) ? getpwuid(getuid()) : getpwnam((char*)name);
    return (pw == nil) ? nil : pw->pw_dir;
}

inline boolean DotSlash(const char* path) {
    return 
        path[0] != '\0' && path[0] == '.' &&
        (path[1] == '/' || path[1] == '\0');
}

inline boolean DotDotSlash(const char* path) {
    return 
        path[0] != '\0' && path[1] != '\0' &&
        path[0] == '.' && path[1] == '.' &&
        (path[2] == '/' || path[2] == '\0');
}

const char* FBDirectory::Normalize(const char* path) {
    static char newpath[MAXPATHLEN+1];
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

    } else if (IsADirectory(buf) && buf[strlen(buf)-1] != '/') {
        strcpy(newpath, buf);
        strcat(newpath, "/");

    } else {
        strcpy(newpath, buf);
    }
    return newpath;
}

const char* FBDirectory::ValidDirectories(const char* path) {
    static char buf[MAXPATHLEN+1];
    strcpy(buf, path);
    int i = strlen(path);

    while (!IsADirectory(RealPath(buf)) && i >= 0) {
        for (--i; buf[i] != '/' && i >= 0; --i);
        buf[i+1] = '\0';
    }
    return buf;
}

const char* FBDirectory::InterpSlashSlash(const char* path) {
    for (int i = strlen(path)-1; i > 0; --i) {
        if (path[i] == '/' && path[i-1] == '/') {
            return &path[i];
        }
    }
    return path;
}

const char* FBDirectory::ElimDot(const char* path) {
    static char newpath[MAXPATHLEN+1];
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

static boolean CollapsedDotDotSlash(const char* path, const char*& start) {
    if (path == start || *(start-1) != '/') {
        return false;

    } else if (path == start-1 && *path == '/') {
        return true;

    } else if (path == start-2) {       /* NB: won't handle '//' right */
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

const char* FBDirectory::ElimDotDot(const char* path) {
    static char newpath[MAXPATHLEN+1];
    const char* src;
    char* dest = newpath;

    for (src = path; src < &path[strlen(path)]; ++src) {
        if (DotDotSlash(src) &&
	    CollapsedDotDotSlash(newpath, (const char*&)dest)
	) {
            src += 2;
        } else {
            *dest++ = *src;
        }
    }
    *dest = '\0';
    return newpath;
}

const char* FBDirectory::InterpTilde(const char* path) {
    static char realpath[MAXPATHLEN+1];
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

const char* FBDirectory::ExpandTilde(const char* tildeName, int length) {
    const char* name = nil;

    if (length > 1) {
        static char buf[max_filename_length+1];
        strncpy(buf, tildeName+1, length-1);
        buf[length-1] = '\0';
        name = buf;
    }
    return Home(name);
}        

/* class FileBrowser */

FileBrowser::FileBrowser(
    ButtonState* bs, const char* dir, int r, int c,
    boolean u, int h, const char* d
) : StringBrowser(bs, r, c, u, h, d) {
    Init(dir);
    UpdateStrings();
}

FileBrowser::FileBrowser(
    const char* name, ButtonState* bs, const char* dir, int r, int c,
    boolean u, int h, const char* d
) : StringBrowser(name, bs, r, c, u, h, d) {
    Init(dir);
    UpdateStrings();
}

void FileBrowser::Init(const char* d) {
    dir = new FBDirectory(d);
    lastpath = fb_strdup(ValidDirectories(Normalize(d)));
    regexp = nil;
    directory_regexp = nil;
    mode = 0;
    directory_mode = 0;
}

FileBrowser::~FileBrowser() {
    delete dir;
    delete lastpath;
    delete regexp;
    delete directory_regexp;
}

static const char* Concat(const char* path, const char* file) {
    static char buf[MAXPATHLEN+1];

    strcpy(buf, path);
    if (path[strlen(path)-1] != '/') {
        strcat(buf, "/");
    }
    return strcat(buf, file);
}

boolean FileBrowser::IsADirectory(const char* path) {
    return dir->IsADirectory(Normalize(path));
}

boolean FileBrowser::SetDirectory(const char* path) {
    boolean successful = true;
    path = ValidDirectories(path);
    const char* normpath = Normalize(path);

    if (strcmp(normpath, lastpath) != 0) {
        char* newnormpath = fb_strdup(normpath);
        successful = dir->LoadDirectory(newnormpath);

        if (successful) {
            delete lastpath;
            lastpath = newnormpath;
            UpdateStrings();
        } else {
            delete newnormpath;
        }
    }
    return successful;
}

const char* FileBrowser::ValidDirectories(const char* path) {
    return dir->ValidDirectories(path);
}

const char* FileBrowser::Normalize(const char* path) {
    return dir->Normalize(path);
}

const char* FileBrowser::Path(int index) {
    const char* s = StringBrowser::String(index);

    return (s == nil ) ? nil : Normalize(Concat(lastpath, s));
}

boolean FileBrowser::Acceptable(const char* name) {
    boolean dir = IsADirectory(name);
    int m = dir ? directory_mode : mode;
    Regexp* r = dir ? directory_regexp : regexp;
    boolean mode_ok, name_ok;

    if (m != 0) {
	struct stat st;
	mode_ok = stat((char*)name, &st) == 0 && (st.st_mode & m) != 0;
    } else {
	mode_ok = true;
    }

    if (r != nil) {
	name_ok = r->Match(name, strlen(name), 0) >= 0;
    } else {
	name_ok = true;
    }

    return mode_ok && name_ok;
}

void FileBrowser::Update() {
    Perspective orig_p = *perspective;
    RereadDirectory();
    UpdateStrings();
    Adjust(orig_p);
}

void FileBrowser::UpdateStrings() {
    Clear();

    for (int i = 0; i < dir->Count(); i++) {
	const char* name = dir->File(i);

	if (Acceptable(Concat(lastpath, name))) {
	    if (dir->IsADirectory(Concat(lastpath, name))) {
		char buf[MAXPATHLEN+1];
		strcpy(buf, name);
		strcat(buf, "/");
		Append(buf);
	    } else {
		Append(name);
	    }
	}
    }
}

void FileBrowser::RereadDirectory() {
    dir->LoadDirectory(lastpath);
}

void FileBrowser::SetTextFilter(const char* r) {
    delete regexp;
    if (r != nil) {
	regexp = new Regexp(r);
    } else {
	regexp = nil;
    }
}

void FileBrowser::SetDirectoryTextFilter(const char* r) {
    delete directory_regexp;
    if (r != nil) {
	directory_regexp = new Regexp(r);
    } else {
	directory_regexp = nil;
    }
}

void FileBrowser::SetModeFilter(int value) {
    mode = value;
}

void FileBrowser::SetDirectoryModeFilter(int value) {
    directory_mode = value;
}
