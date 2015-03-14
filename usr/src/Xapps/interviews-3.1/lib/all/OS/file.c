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

#include <OS/file.h>
#include <OS/string.h>
#include <OS/types.h>
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>

#ifdef sgi
#include <sys/mman.h>
#endif

/* no standard place for these */
extern "C" {
    extern int close(int);
#if defined(sgi)
    extern int read(int, void*, unsigned int);
#endif
#if defined(sun) && !defined(__SYSENT_H)
    extern int read(int, void*, unsigned int);
#endif
#if defined(AIXV3)
    extern int read(int, char*, unsigned int);
#endif
#if defined(apollo)
    extern long read(int, void*, unsigned int);
#endif
#if defined(__DECCXX)
    extern int read(int, void*, unsigned int);
#endif
}

#ifdef __GNUC__
#include <unistd.h>
#endif

class FileInfo {
public:
    CopyString* name_;
    int fd_;
    char* map_;
    struct stat info_;
    off_t pos_;
    char* buf_;
    unsigned int limit_;

    FileInfo(CopyString*, int fd);
};

FileInfo::FileInfo(CopyString* s, int fd) {
    name_ = s;
    fd_ = fd;
    pos_ = 0;
    limit_ = 0;
    map_ = nil;
    buf_ = nil;
}

File::File(FileInfo* i) {
    assert(i != nil);
    rep_ = i;
}

File::~File() {
    close();
    delete rep_->name_;
    delete rep_;
}

const String* File::name() const {
    return rep_->name_;
}

long File::length() const {
    return rep_->info_.st_size;
}

void File::close() {
    FileInfo* i = rep_;
    if (i->fd_ >= 0) {
	if (i->map_ != nil) {
#ifdef sgi
	    munmap(i->map_, int(i->info_.st_size));
#endif
	}
	if (i->buf_ != nil) {
	    delete i->buf_;
	}
	::close(i->fd_);
	i->fd_ = -1;
    }
}

void File::limit(unsigned int buffersize) {
    rep_->limit_ = buffersize;
}

FileInfo* File::rep() const { return rep_; }

/* class InputFile */

InputFile::InputFile(FileInfo* i) : File(i) { }
InputFile::~InputFile() { }

InputFile* InputFile::open(const String& name) {
    CopyString* s = new CopyString(name);
    /* cast to workaround DEC C++ prototype bug */
    int fd = ::open((char*)s->string(), O_RDONLY);
    if (fd < 0) {
	delete s;
	return nil;
    }
    FileInfo* i = new FileInfo(s, fd);
    if (fstat(fd, &i->info_) < 0) {
	delete s;
	delete i;
	return nil;
    }
    return new InputFile(i);
}

int InputFile::read(const char*& start) {
    FileInfo* i = rep();
    int len = (int)(i->info_.st_size);
    if (i->pos_ >= len) {
	return 0;
    }
    if (i->limit_ != 0 && len > i->limit_) {
	len = (int)(i->limit_);
    }
#ifdef sgi
    i->map_ = (char*)mmap(0, len, PROT_READ, MAP_PRIVATE, i->fd_, i->pos_);
    if ((int)(i->map_) == -1) {
	return -1;
    }
    start = i->map_;
#else
    if (i->buf_ == nil) {
	i->buf_ = new char[len];
    }
    start = i->buf_;
    len = ::read(i->fd_, i->buf_, len);
#endif
    i->pos_ += len;
    return len;
}

/* class StdInput */

StdInput::StdInput() : InputFile(new FileInfo(new CopyString("-stdin"), 0)) { }
StdInput::~StdInput() { }

long StdInput::length() const { return -1; }

int StdInput::read(const char*& start) {
    FileInfo* i = rep();
    if (i->buf_ == nil) {
	if (i->limit_ == 0) {
	    i->limit_ = BUFSIZ;
	}
	i->buf_ = new char[i->limit_];
    }
    int nbytes = ::read(i->fd_, (char*)i->buf_, i->limit_);
    if (nbytes > 0) {
	start = (const char*)(i->buf_);
    }
    return nbytes;
}
