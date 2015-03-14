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

#ifndef dp_iostreamb_h
#define dp_iostreamb_h

#include <Dispatch/enter-scope.h>
#include <iostream.h>

// Modify ios to store extra state information for binary I/O.

class iosb : public virtual ios {
public:
    iosb();
    ~iosb();

    boolean binary() const;
    void binary(boolean);
    boolean swapped() const;
    void swapped(boolean);
protected:
    boolean _binary;		// are my peer and I performing binary I/O?
    boolean _swapped;		// does my peer have a swapped endian?
};

// Modify istream to extract unformatted data where possible for
// faster I/O throughput and to discard delimiters automatically.

class istreamb : public virtual iosb, public istream {
protected:
    istreamb();
public:
    istreamb(streambuf*);
    ~istreamb();

    istreamb& operator>>(char&);
    istreamb& operator>>(unsigned char&);
    istreamb& operator>>(short&);
    istreamb& operator>>(unsigned short&);
    istreamb& operator>>(int&);
    istreamb& operator>>(unsigned int&);
    istreamb& operator>>(long&);
    istreamb& operator>>(unsigned long&);
    istreamb& operator>>(float&);
    istreamb& operator>>(double&);
    istreamb& operator>>(char*);
    istreamb& operator>>(unsigned char*);

    istreamb& operator>>(istream& (*)(istream&));
    istreamb& operator>>(ios& (*)(ios&));
};

// Modify ostream to insert unformatted data where possible for faster
// I/O throughput and to delimit formatted data automatically.

class ostreamb : public virtual iosb, public ostream {
protected:
    ostreamb();
public:
    ostreamb(streambuf*);
    ~ostreamb();

    ostreamb& operator<<(char);
    ostreamb& operator<<(unsigned char);
    ostreamb& operator<<(short);
    ostreamb& operator<<(unsigned short);
    ostreamb& operator<<(int);
    ostreamb& operator<<(unsigned int);
    ostreamb& operator<<(long);
    ostreamb& operator<<(unsigned long);
    ostreamb& operator<<(float);
    ostreamb& operator<<(double);
    ostreamb& operator<<(const char*);
    ostreamb& operator<<(const unsigned char*);

    ostreamb& operator<<(ostream& (*)(ostream&));
    ostreamb& operator<<(ios& (*)(ios&));
protected:
    void fixwidth();
};

// Replace iostream with a stream that inserts and extracts
// unformatted data where possible for faster I/O throughput and
// delimits formatted data automatically.

class iostreamb : public istreamb, public ostreamb {
protected:
    iostreamb();
public:
    iostreamb(streambuf*);
    ~iostreamb();

    void negotiate(boolean binary);
};

#endif
