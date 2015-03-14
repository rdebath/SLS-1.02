/* ms_real.c - machine-dependent real number manipulation */

/*  This file is part of MandelSpawn, a parallel Mandelbrot program for
    the X window system.

    Copyright (C) 1990 Andreas Gustafsson

    MandelSpawn is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License, version 1,
    as published by the Free Software Foundation.

    MandelSpawn is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License,
    version 1, along with this program; if not, write to the Free 
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef REAL_FIXED
#ifdef lint /* lint doesn't like asm's; fake it */
static inline unsigned long fracmult(x,y) unsigned long x,y;
{ return(x*y);
}
static inline unsigned long fracmult2(x,y) unsigned long x,y;
{ return(2*x*y);
}
#else

#ifdef mc68000
static inline unsigned long fracmult(x,y) unsigned long x,y;
{ unsigned long high;
  asm("mulsl %3,%1,%0" : "=d" (x), "=d" (high): "0" (x), "d" (y));
  return((high<<LEFTBITS) | (x>>RIGHTBITS));
}

static inline unsigned long fracmult2(x,y) unsigned long x,y;
{ unsigned long high;
  asm("mulsl %3,%1,%0" : "=d" (x), "=d" (high): "0" (x), "d" (y));
  return((high<<(LEFTBITS+1)) | (x>>(RIGHTBITS-1)));
}
#endif /* mc68000 */

#ifdef vax  /* VAX code due to Jussi Maki, thanks */
typedef struct 
{ unsigned int i0;
  unsigned int i1;
} int64;

static inline unsigned long fracmult(x,y) unsigned long x,y;
{ int64 r;
  asm("emul %1,%2,$0,%0" : "=g" (r) : "g" (x), "g" (y));
  return((r.i1<<LEFTBITS) | (r.i0>>RIGHTBITS));
}

static inline unsigned long fracmult2(x,y) unsigned long x,y;
{ int64 r;
  asm("emul %1,%2,$0,%0" : "=g" (r) : "g" (x), "g" (y));
  return((r.i1<<LEFTBITS+1) | (r.i0>>RIGHTBITS-1));
}
#endif /* vax */

#ifdef i386
/* i386 assembly syntax tested with Mach/i386 */

static inline long fracmult(x,y) long x,y;
{ long high;
  asm("imull %3" : "=a" (x), "=d" (high) : "0" (x), "rm" (y));
  asm("shrdl %4,%1,%2" : "=a" (x), "=d" (high) : "0" (x), "1" (high),
        "i" (RIGHTBITS));
  return(x);
}

static inline long fracmult2(x,y) long x,y;
{ long high;
  asm("imull %3" : "=a" (x), "=d" (high) : "0" (x), "rm" (y));
  asm("shrdl %4,%1,%2" : "=a" (x), "=d" (high) : "0" (x), "1" (high),
        "i" (RIGHTBITS-1));
  return(x);
}
#endif /* i386 */

#ifdef mips
/* This is slower than it needs to be because mflo/mfhi are accessed */
/* immediately after the multiply; if GCC would know about the mflo/mfhi */
/* it could be optimized to avoid at least part of the interlocks. */
static inline long fracmult(x,y) register long x,y;
{ register long high;
  register unsigned long low;
  asm("mult %2,%3; mflo %0; mfhi %1" : "=r" (low), "=r" (high) :
      "r" (x), "r" (y));
  return((high<<LEFTBITS) | (low>>RIGHTBITS));
}

static inline long fracmult2(x,y) register long x,y;
{ register long high;
  register unsigned long low;
  asm("mult %2,%3; mflo %0; mfhi %1" : "=r" (low), "=r" (high) :
      "r" (x), "r" (y));
  return((high<<LEFTBITS+1) | (low>>RIGHTBITS-1));
}
#endif /* not mips */
#endif /* not lint */
#endif /* REAL_FIXED */
