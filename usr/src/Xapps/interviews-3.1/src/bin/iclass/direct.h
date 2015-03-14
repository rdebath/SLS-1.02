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
 * Directory - a class interface to Unix directory operations.
 */

#ifndef direct_h
#define direct_h

#include <InterViews/boolean.h>

class DirectoryRep;

class Directory {       
public:
    Directory(const char* name);
    ~Directory();

    boolean LoadDirectory(const char*);
    const char* Normalize(const char*);
    const char* ValidDirectories(const char*);

    int Index(const char*);
    const char* File(int index);
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

    boolean Reset(char*);
private:
    DirectoryRep* rep_;
};

#endif
