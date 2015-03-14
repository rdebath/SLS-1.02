/* ms_real.h - define real and complex number formats and macros */
/* Copyright (C) 1990, 1991 Andreas Gustafsson */

/* Three potentially different abstract data types are used to
represent real numbers: "double", "real", and "netreal".  "double" is
what the client program uses internally and is supposed to be easy to
use from C and accurate but not necessarily fast nor portable (as in
having a standardized binary representation that is compatible between
machines).  "real" is what the computation server uses internally.
Additions, multiplications, and magnitude comparisions of "real"
values are supposed to be fast; generality and portability are
secondary.  Finally, the "netreal" type is the format in which the
client and the server communicate real numbers over the network.
"netreal" numbers must be in a well-defined, portable format that is
easily converted to and from the other formats.

The "netreal" format in the current protocol version is a 32-bit two's
complement fixed-point number with a 7 bit integer part and 25 bit
fractional part, transmitted in network byte order.  The "double" type
is the native C "double" on the client machine.  The "real" type is
currently the same as "netreal" on 680x0, Vax, 80386, and MIPS
processors, and a native C "double" on all other machines. */

#ifndef _ms_real_h
#define _ms_real_h

/* Sony defines mc68020 but not mc68000; fix it */
#ifdef mc68020
#ifndef mc68000
#define mc68000
#endif
#endif

/* Currently fixed-point arithmetic is supported only with GCC on */
/* 680x0, Vax, i386, and MIPS processors */
#ifdef __GNUC__
#ifdef mc68000
#define REAL_FIXED
#endif
#ifdef vax
#define REAL_FIXED
#endif
#ifdef i386
#define REAL_FIXED
#endif
#ifdef mips
/* Fixed point is supported for MIPS processors, but at least on a */
/* DECStation it is slower than floating point */
/* #define REAL_FIXED */ 
#endif
#endif

/* All others machines use doubles */
#ifndef REAL_FIXED
#define REAL_DOUBLE	/* use floating-point arithmetic */
#endif

/* definitions for fixed-point numbers */
typedef long fixed;
#define ALLBITS 32
#define LEFTBITS 7
#define RIGHTBITS (ALLBITS-LEFTBITS)
#define one_fixed() (1<<RIGHTBITS)

/* conversion between fixed-point numbers and doubles */
#define double_to_fixed(x) ((real)((x) * ((double)one_fixed())))
#define fixed_to_double(x) ((double)(x) * (1/(double)one_fixed()))

/* how to implement the necessary real number primitives using doubles */
#ifdef REAL_DOUBLE
typedef double real;
#define add_real(x,y) ((x)+(y))
#define sub_real(x,y) ((x)-(y))
#define mul_real(x,y) ((x)*(y))
#define mul_real_int(x,i) ((x)*(double)(i))
#define gteq_real(x,y) ((x)>=(y))
#define twice_mul_real(x,y) ((x)*(y)*2.0)
#define zero_real() (0.0)
#define four_real() (4.0)
#define int_to_real(x) ((double)(x))
#define double_to_real(x) (x)
#define fixed_to_real(x) fixed_to_double(x)
#endif

/* how to implement the necessary real number primitives using */
/* fixed-point numbers */
#ifdef REAL_FIXED
typedef fixed real;
#define add_real(x,y) ((x)+(y))
#define sub_real(x,y) ((x)-(y))
#define mul_real(x,y) fracmult(x,y)
#define mul_real_int(x,i) ((x)*(long)(i))
#define gteq_real(x,y) ((x)>=(y))
#define twice_mul_real(x,y) fracmult2(x,y)
#define zero_real() (0L)
#define four_real() (one_fixed()*4L)
#define int_to_real(x) ((x) << RIGHTBITS)
#define double_to_real(x) double_to_fixed(x)
#define fixed_to_real(x) (x)
#endif

/* these defines reflect the fact that the "net" type is currently "fixed" */
#define netreal fixed
#define double_to_net(x) double_to_fixed(x)
#define net_to_real(x) fixed_to_real(x)

typedef struct
{ real re; 
  real im;
} complex;

typedef struct
{ netreal re; 
  netreal im;
} netcomplex;

#endif /* _ms_real_h */
