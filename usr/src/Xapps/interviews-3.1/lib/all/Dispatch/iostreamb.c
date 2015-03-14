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

#include <Dispatch/iostreamb.h>

// Make sure that these assumptions about the sizes of integral types
// hold for your machine.  If your machine does not have 32-bit and
// 16-bit integers, then you cannot use binary I/O because other
// machines will expect the integers to be these sizes.  If your
// machine does not have 8-bit characters, then I don't know how
// you're going to communicate with other machines!

typedef long		INT32;
typedef short		INT16;

typedef unsigned long	UINT32;
typedef unsigned short	UINT16;

// Assume the peer machine has the same endian architecture but
// disable binary I/O by default anyway.  The two machines should
// always negotiate these two options before enabling binary I/O.

iosb::iosb() :
    _binary(false),
    _swapped(false) {}

iosb::~iosb() {}

// Get and set iosb's extra state information.  Verify that the
// assumptions about sizes of integral types hold before enabling
// binary I/O; if they do not hold, then refuse to enable binary I/O.

boolean iosb::binary() const {
    return _binary;
}

void iosb::binary(boolean binary) {
    if (binary) {
	// don't use consts; cfront has wrong idea of their values
	UINT32 max32 = ~0;
	UINT16 max16 = ~0;

	if (max32 != 0xffffffff || max16 != 0xffff) {
	    binary = false;
	}
    }
    _binary = binary;
}

boolean iosb::swapped() const {
    return _swapped;
}

void iosb::swapped(boolean swapped) {
    _swapped = swapped;
}

// Provide a constructor for derived classes to use and a constructor
// for public use.

istreamb::istreamb() {}

istreamb::istreamb(streambuf* b) {
    init(b);
}

istreamb::~istreamb() {}

// Redefine these functions to extract binary data where possible for
// faster I/O throughput and to discard delimiters automatically.
// Binary integral values occupy the length implied by their type and
// already fit this machine's endian architecture.  Formatted integral
// and floating point values end in a blank character that is
// extracted.  Strings end in a null character that is extracted.

istreamb& istreamb::operator>>(char& c) {
    get(c);			// assume c is 8 bits long on all machines
    return *this;
}

istreamb& istreamb::operator>>(unsigned char& uc) {
    get(uc);			// assume uc is 8 bits long on all machines
    return *this;
}

istreamb& istreamb::operator>>(short& s) {
    if (_binary) {
	INT16 value;
	read((char*)&value, sizeof(value));
	if (good()) {
	    s = (short)value;
	    if (s != value) {
		setstate(ios::failbit); // overflow: value won't fit in short
	    }
	}
    } else {
	istream::operator>>(s);
	get();
    }
    return *this;
}

istreamb& istreamb::operator>>(unsigned short& us) {
    if (_binary) {
	UINT16 value;
	read((char*)&value, sizeof(value));
	if (good()) {
	    us = (unsigned short)value;
	    if (us != value) {
		setstate(ios::failbit); // overflow: value won't fit in ushort
	    }
	}
    } else {
	istream::operator>>(us);
	get();
    }
    return *this;
}

istreamb& istreamb::operator>>(int& i) {
    if (_binary) {
	INT32 value;
	read((char*)&value, sizeof(value));
	if (good()) {
	    i = (int)value;
	    if (i != value) {
		setstate(ios::failbit); // overflow: value won't fit in int
	    }
	}
    } else {
	istream::operator>>(i);
	get();
    }
    return *this;
}

istreamb& istreamb::operator>>(unsigned int& ui) {
    if (_binary) {
	UINT32 value;
	read((char*)&value, sizeof(value));
	if (good()) {
	    ui = (unsigned int)value;
	    if (ui != value) {
		setstate(ios::failbit); // overflow: value won't fit in uint
	    }
	}
    } else {
	istream::operator>>(ui);
	get();
    }
    return *this;
}

istreamb& istreamb::operator>>(long& l) {
    if (_binary) {
	INT32 value;
	read((char*)&value, sizeof(value));
	if (good()) {
	    l = (long)value;
	    if (l != value) {
		setstate(ios::failbit); // overflow: value won't fit in long
	    }
	}
    } else {
	istream::operator>>(l);
	get();
    }
    return *this;
}

istreamb& istreamb::operator>>(unsigned long& ul) {
    if (_binary) {
	UINT32 value;
	read((char*)&value, sizeof(value));
	if (good()) {
	    ul = (unsigned long)value;
	    if (ul != value) {
		setstate(ios::failbit); // overflow: value won't fit in ulong
	    }
	}
    } else {
	istream::operator>>(ul);
	get();
    }
    return *this;
}

istreamb& istreamb::operator>>(float& f) {
    istream::operator>>(f);
    get();
    return *this;
}

istreamb& istreamb::operator>>(double& d) {
    istream::operator>>(d);
    get();
    return *this;
}

istreamb& istreamb::operator>>(char* p) {
    const int MAXINT = (int)(((unsigned)-1) >> 1);
    const int w = width(0);
    getline(p, w ? w : MAXINT, '\0');
    if (w && gcount() == w - 1) {
	setstate(ios::failbit);
    }
    return *this;
}

istreamb& istreamb::operator>>(unsigned char* up) {
    const int MAXINT = (int)(((unsigned)-1) >> 1);
    const int w = width(0);
    getline(up, w ? w : MAXINT, '\0');
    if (w && gcount() == w - 1) {
	setstate(ios::failbit);
    }
    return *this;
}

// Redefine the rest of the overloaded operator>> functions that we
// want to keep in the derived class.

istreamb& istreamb::operator>>(istream& (*f)(istream&)) {
    istream::operator>>(f);
    return *this;
}

istreamb& istreamb::operator>>(ios& (*f)(ios&)) {
    istream::operator>>(f);
    return *this;
}

// Provide a constructor for derived classes to use and a constructor
// for public use.

ostreamb::ostreamb() {}

ostreamb::ostreamb(streambuf* b) {
    init(b);
}

ostreamb::~ostreamb() {}

// Decrement the width by one to take into account the delimiter
// character that all the inserters insert after a formatted value.

inline void ostreamb::fixwidth() {
    register int w = width();
    if (w) {
	width(w - 1);
    }
}

// Redefine these functions to insert binary data where possible for
// faster I/O throughput and to delimit formatted data automatically.
// Binary integral values will occupy the length implied by their type
// and fit the peer's endian architecture.  Formatted integral and
// floating point values will end in a blank character.  Strings will
// end in a null character.  You need not, and should not, insert
// explicit delimiters.

ostreamb& ostreamb::operator<<(char c) {
    put(c);			// assume c is 8 bits long on all machines
    return *this;
}

ostreamb& ostreamb::operator<<(unsigned char uc) {
    put(uc);			// assume uc is 8 bits long on all machines
    return *this;
}

ostreamb& ostreamb::operator<<(short s) {
    if (_binary) {
	INT16 value = (INT16)s;
	if (value != s) {
	    setstate(ios::failbit); // overflow: number won't fit in 16 bits
	}
	else if (_swapped) {
	    INT16 copy = value;
	    ((char*)&value)[0] = ((char*)&copy)[1];
	    ((char*)&value)[1] = ((char*)&copy)[0];
	}
	width(0);
	write((char*)&value, sizeof(value));
    }
    else {
	fixwidth();
	ostream::operator<<(s);
	put(' ');
    }
    return *this;
}

ostreamb& ostreamb::operator<<(unsigned short us) {
    if (_binary) {
	UINT16 value = (UINT16)us;
	if (value != us) {
	    setstate(ios::failbit); // number won't fit
	}
	else if (_swapped) {
	    UINT16 copy = value;
	    ((char*)&value)[0] = ((char*)&copy)[1];
	    ((char*)&value)[1] = ((char*)&copy)[0];
	}
	width(0);
	write((char*)&value, sizeof(value));
    }
    else {
	fixwidth();
	ostream::operator<<(us);
	put(' ');
    }
    return *this;
}

ostreamb& ostreamb::operator<<(int i) {
    return *this << (long)i;
}

ostreamb& ostreamb::operator<<(unsigned int ui) {
    return *this << (unsigned long)ui;
}

ostreamb& ostreamb::operator<<(long l) {
    if (_binary) {
	INT32 value = (INT32)l;
	if (value != l) {
	    setstate(ios::failbit); // overflow: number won't fit in 32 bits
	}
	else if (_swapped) {
	    INT32 copy = value;
	    ((char*)&value)[0] = ((char*)&copy)[3];
	    ((char*)&value)[1] = ((char*)&copy)[2];
	    ((char*)&value)[2] = ((char*)&copy)[1];
	    ((char*)&value)[3] = ((char*)&copy)[0];
	}
	width(0);
	write((char*)&value, sizeof(value));
    }
    else {
	fixwidth();
	ostream::operator<<(l);
	put(' ');
    }
    return *this;
}

ostreamb& ostreamb::operator<<(unsigned long ul) {
    if (_binary) {
	UINT32 value = (UINT32)ul;
	if (value != ul) {
	    setstate(ios::failbit); // overflow: number won't fit in 32 bits
	}
	else if (_swapped) {
	    UINT32 copy = value;
	    ((char*)&value)[0] = ((char*)&copy)[3];
	    ((char*)&value)[1] = ((char*)&copy)[2];
	    ((char*)&value)[2] = ((char*)&copy)[1];
	    ((char*)&value)[3] = ((char*)&copy)[0];
	}
	width(0);
	write((char*)&value, sizeof(value));
    }
    else {
	fixwidth();
	ostream::operator<<(ul);
	put(' ');
    }
    return *this;
}

ostreamb& ostreamb::operator<<(float f) {
    fixwidth();
    ostream::operator<<(f);
    put(' ');
    return *this;
}

ostreamb& ostreamb::operator<<(double d) {
    fixwidth();
    ostream::operator<<(d);
    put(' ');
    return *this;
}

ostreamb& ostreamb::operator<<(const char* p) {
    fixwidth();
    ostream::operator<<(p);
    put('\0');
    return *this;
}

ostreamb& ostreamb::operator<<(const unsigned char* up) {
    fixwidth();
    ostream::operator<<((const char*)up); // 2.0 ostream omitted unsigned char*
    put('\0');
    return *this;
}

// Redefine the rest of the overloaded operator<< functions that we
// want to keep in the derived class.

ostreamb& ostreamb::operator<<(ostream& (*f)(ostream&)) {
    ostream::operator<<(f);
    return *this;
}

ostreamb& ostreamb::operator<<(ios& (*f)(ios&)) {
    ostream::operator<<(f);
    return *this;
}

// Provide a constructor for derived classes to use and a constructor
// for public use.

iostreamb::iostreamb() {}

iostreamb::iostreamb(streambuf* b) {
    init(b);
}

iostreamb::~iostreamb() {}

// Negotiate the option of binary I/O with the remote iostreamb.  If
// binary I/O is set, compare endians to enable swapping if necessary.
// Ordinarily we don't need an explicit flush when we're switching
// from insertion to extraction, but we do here because the first
// underflow may read both format and remoteEndian, thus preventing
// underflow from being called again and flushing localEndian.  Tying
// the stream to itself won't work either because 1) ipfx won't call
// flush if remoteEndian is already available, and 2) opfx will make
// the stream flush itself before every insertion because it doesn't
// check for streams tied to themselves.  Sigh....

void iostreamb::negotiate(boolean b) {
    if (!good()) {
	return;
    }

    binary(b);
    char format = binary() ? 'T' : 'F';

    *this << format; flush();
    *this >> format;

    if (format != 'T' && format != 'F') {
	setstate(ios::badbit);
    }
    else if (format == 'F') {
	binary(false);
    }

    if (binary()) {
	int indian = 1;
	char localEndian = (*(char*)&indian) ? 'l' : 'B';
	char remoteEndian = localEndian;

	*this << localEndian; flush();
	*this >> remoteEndian;

	if (remoteEndian != 'l' && remoteEndian != 'B') {
	    setstate(ios::badbit);
	}
	else if (remoteEndian != localEndian) {
	    swapped(true);
	}
    }
}
