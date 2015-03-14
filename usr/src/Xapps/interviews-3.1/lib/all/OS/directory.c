/*
 * Copyright (c) 1991 Stanford University
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

#include <OS/directory.h>
#include <OS/memory.h>
#include <OS/string.h>

/*
 * BSD tends to have things in <sys/dir.h>, System V uses <dirent.h>.
 * So far as I can tell, POSIX went with <dirent.h>.  Ultrix <dirent.h>
 * includes <sys/dir.h>, which is silly because <sys/dir.h>
 * needs <sys/types.h>.
 */
#include <OS/types.h>
#ifdef apollo
#include <sys/dir.h>
#include <osfcn.h>
#else
#include <dirent.h>
#endif
#include <pwd.h>

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

/*
 * These hide in mysterious places on various systems.
 * For now, it seems safest just to declare them explicitly.
 */

extern "C" {
    extern uid_t getuid();
    extern void qsort(
	void*, size_t, size_t, int (*) (const void*, const void*)
    );
#ifdef __DECCXX
    extern struct passwd* getpwent();
    extern struct passwd* getpwnam(const char*);
    extern struct passwd* getpwuid(uid_t);
#endif
}

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif

/*
 * Buffer size for internal path name computation.
 * The path stuff should really be reimplemented
 * with variable-length strings.
 */

static const int path_buffer_size = 1024 + 1;

class DirectoryEntry {
public:
    const String& name() const;
private:
    friend class Directory;
    friend class DirectoryImpl;

    String* name_;
    struct stat* info_;
};

inline const String& DirectoryEntry::name() const { return *name_; }

class DirectoryImpl {
private:
    friend class Directory;

    DirectoryImpl(DIR*, String*);
    ~DirectoryImpl();

    String* name_;
    DIR* dir_;
    DirectoryEntry* entries_;
    int count_;
    int used_;
    boolean filled_;

    static unsigned int overflows_;

    DirectoryImpl& filled();
    void do_fill();

    static boolean dot_slash(const char*);
    static boolean dot_dot_slash(const char*);
    static const char* home(const char*);
    static const char* eliminate_dot(const char*);
    static boolean collapsed_dot_dot_slash(char*, char*& start);
    static const char* eliminate_dot_dot(const char*);
    static const char* interpret_slash_slash(const char*);
    static const char* interpret_tilde(const char*);
    static const char* expand_tilde(const char*, int);
    static const char* real_path(const char*);
    static boolean ifdir(const char*);
};

unsigned int DirectoryImpl::overflows_ = 0;

Directory::Directory() {
    impl_ = nil;
}

Directory::~Directory() {
    close();
    delete impl_;
}

Directory* Directory::current() {
    return open(".");
}

Directory* Directory::open(const String& name) {
    String* s = canonical(name);
    /* cast is to workaround bug in some opendir prototypes */
    DIR* dir = opendir((char*)s->string());
    if (dir == nil) {
	delete s;
	return nil;
    }
    Directory* d = new Directory;
    d->impl_ = new DirectoryImpl(dir, s);
    return d;
}

void Directory::close() {
    DirectoryImpl& d = *impl_;
    if (d.dir_ != nil) {
	closedir(d.dir_);
	DirectoryEntry* end = &d.entries_[d.used_];
	for (DirectoryEntry* e = &d.entries_[0]; e < end; e++) {
	    delete e->name_;
	    delete e->info_;
	}
	delete d.entries_;
	d.dir_ = nil;
    }
}

const String* Directory::path() const {
    DirectoryImpl& d = *impl_;
    return d.name_;
}

int Directory::count() const {
    DirectoryImpl& d = impl_->filled();
    return d.used_;
}

const String* Directory::name(int i) const {
    DirectoryImpl& d = impl_->filled();
    if (i < 0 || i >= d.count_) {
	/* raise exception -- out of range */
	return nil;
    }
    return d.entries_[i].name_;
}

int Directory::index(const String& name) const {
    NullTerminatedString ns(name);
    const char* s = ns.string();
    DirectoryImpl& d = impl_->filled();
    int i = 0, j = d.used_ - 1;
    while (i <= j) {
	int k = (i + j) / 2;
	int cmp = strcmp(s, d.entries_[k].name_->string());
	if (cmp == 0) {
	    return k;
	}
	if (cmp > 0) {
	    i = k + 1;
	} else {
	    j = k - 1;
	}
    }
    return -1;
}

boolean Directory::is_directory(int i) const {
    DirectoryImpl& d = impl_->filled();
    if (i < 0 || i >= d.count_) {
	/* raise exception -- out of range */
	return false;
    }
    DirectoryEntry& e = d.entries_[i];
    if (e.info_ == nil) {
	e.info_ = new (struct stat);
	char* tmp = new char[d.name_->length() + e.name_->length() + 2];
	sprintf(tmp, "%s/%s", d.name_->string(), e.name_->string());
	stat(tmp, e.info_);
	delete tmp;
    }
    return S_ISDIR(e.info_->st_mode);
}

inline boolean DirectoryImpl::dot_slash(const char* path) {
    return path[0] == '.' && (path[1] == '/' || path[1] == '\0');
}

inline boolean DirectoryImpl::dot_dot_slash(const char* path) {
    return (path[0] == '.' && path[1] == '.' &&
	(path[2] == '/' || path[2] == '\0')
    );
}

String* Directory::canonical(const String& name) {
    NullTerminatedString ns(name);
    const char* path = ns.string();
    static char newpath[path_buffer_size];
    const char* s = DirectoryImpl::interpret_slash_slash(path);
    s = DirectoryImpl::eliminate_dot(s);
    s = DirectoryImpl::eliminate_dot_dot(s);
    s = DirectoryImpl::interpret_tilde(s);
    if (s[0] == '\0') {
	sprintf(newpath, "./");
    } else if (!DirectoryImpl::dot_slash(s) &&
	!DirectoryImpl::dot_dot_slash(s) && s[0] != '/'
    ) {
	sprintf(newpath, "./%s", s);
    } else if (DirectoryImpl::ifdir(s) && s[strlen(s) - 1] != '/') {
	sprintf(newpath, "%s/", s);
    } else {
	sprintf(newpath, "%s", s);
    }
    return new CopyString(newpath);
}

boolean Directory::match(const String& name, const String& pattern) {
    const char* s = name.string();
    const char* end_s = s + name.length();
    const char* p = pattern.string();
    const char* end_p = p + pattern.length();
    for (; p < end_p; p++, s++) {
	if (*p == '*') {
	    const char* pp = p + 1;
	    if (pp == end_p) {
		return true;
	    }
	    for (; s < end_s && *s != *pp; s++);
	    p = pp;
	} else if (s == end_s || *p != *s) {
	    return false;
	}
    }
    return s == end_s;
}

/** class DirectoryImpl **/

DirectoryImpl::DirectoryImpl(DIR* d, String* name) {
    dir_ = d;
    entries_ = nil;
    count_ = 0;
    used_ = 0;
    filled_ = false;
    name_ = name;
}

DirectoryImpl::~DirectoryImpl() {
    delete name_;
}

DirectoryImpl& DirectoryImpl::filled() {
    if (!filled_) {
	do_fill();
	filled_ = true;
    }
    return *this;
}

static int compare_entries(const void* k1, const void* k2) {
    DirectoryEntry* e1 = (DirectoryEntry*)k1;
    DirectoryEntry* e2 = (DirectoryEntry*)k2;
    return strcmp(e1->name().string(), e2->name().string());
}

void DirectoryImpl::do_fill() {
#ifdef apollo
    for (struct direct* d = readdir(dir_); d != nil; d = readdir(dir_)) {
#else
    for (struct dirent* d = readdir(dir_); d != nil; d = readdir(dir_)) {
#endif
	if (used_ >= count_) {
	    ++overflows_;
	    int new_count = count_ + 50*overflows_;
	    DirectoryEntry* new_entries = new DirectoryEntry[new_count];
	    Memory::copy(
		entries_, new_entries, count_ * sizeof(DirectoryEntry)
	    );
	    delete entries_;
	    entries_ = new_entries;
	    count_ = new_count;
	}
	DirectoryEntry& e = entries_[used_];
	e.name_ = new CopyString(d->d_name);
	e.info_ = nil;
	++used_;
    }
    qsort(entries_, used_, sizeof(DirectoryEntry), &compare_entries);
}

const char* DirectoryImpl::home(const char* name) {
    struct passwd* pw;
    if (name == nil) {
	pw = getpwuid(getuid());
    } else {
	pw = getpwnam(name);
    }
    return (pw == nil) ? nil : pw->pw_dir;
}

const char* DirectoryImpl::eliminate_dot(const char* path) {
    static char newpath[path_buffer_size];
    const char* src;
    char* dest = newpath;

    const char* end = &path[strlen(path)];
    for (src = path; src < end; src++) {
	if (dot_slash(src) && dest > newpath && *(dest - 1) == '/') {
	    src++;
	} else {
	    *dest++ = *src;
	}
    }
    *dest = '\0';
    return newpath;
}

boolean DirectoryImpl::collapsed_dot_dot_slash(char* path, char*& start) {
    if (path == start || *(start - 1) != '/') {
	return false;
    }
    if (path == start - 1 && *path == '/') {
	return true;
    }
    if (path == start - 2) {	/* doesn't handle double-slash correctly */
	start = path;
	return *start != '.';
    }
    if (path < start - 2 && !dot_dot_slash(start - 3)) {
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

const char* DirectoryImpl::eliminate_dot_dot(const char* path) {
    static char newpath[path_buffer_size];
    const char* src;
    char* dest = newpath;

    const char* end = &path[strlen(path)];
    for (src = path; src < end; src++) {
	if (dot_dot_slash(src) && collapsed_dot_dot_slash(newpath, dest)) {
	    src += 2;
	} else {
	    *dest++ = *src;
	}
    }
    *dest = '\0';
    return newpath;
}

const char* DirectoryImpl::interpret_slash_slash(const char* path) {
    for (int i = strlen(path) - 1; i > 0; --i) {
	if (path[i] == '/' && path[i - 1] == '/') {
	    return &path[i];
	}
    }
    return path;
}

const char* DirectoryImpl::interpret_tilde(const char* path) {
    static char realpath[path_buffer_size];
    const char* beg = strrchr(path, '~');
    boolean valid = (beg != nil && (beg == path || *(beg - 1) == '/'));
    if (valid) {
	const char* end = strchr(beg, '/');
	int length = (end == nil) ? strlen(beg) : (end - beg);
	const char* expanded = expand_tilde(beg, length);
	if (expanded == nil) {
	    valid = false;
	} else {
	    strcpy(realpath, expanded);
	    if (end != nil) {
		strcat(realpath, end);
	    }
	}
    }
    return valid ? realpath : path;
}

const char* DirectoryImpl::expand_tilde(const char* tilde, int length) {
    const char* name = nil;
    if (length > 1) {
	static char buf[path_buffer_size];
	strncpy(buf, tilde + 1, length - 1);
	buf[length - 1] = '\0';
	name = buf;
    }
    return home(name);
}

const char* DirectoryImpl::real_path(const char* path) {
    const char* realpath;
    if (*path == '\0') {
	realpath = "./";
    } else {
	realpath = interpret_tilde(interpret_slash_slash(path));
    }
    return realpath;
}

boolean DirectoryImpl::ifdir(const char* path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}
