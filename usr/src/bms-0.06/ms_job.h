/* ms_job.h - "end-to-end" data structures (known to the client */
/* and the slave but not to the workforce handler or the Mama widget) */
/* Copyright (C) 1990, 1991 Andreas Gustafsson */

#ifndef _ms_job_h
#define _ms_job_h

#include "ms_real.h" /* for "netcomplex" declaration */

/* options for the "flags" field */
#define MS_OPT_INTERIOR	0x0100	/* show speed of convergence in set interior */
#define MS_OPT_GUESS	0x0200	/* reserved for "guessing" algorithm */

/* this is the part of a calculation request that is common to all */
/* requests generated for this exposure event */
struct static_job_info
{ short julia;		/* fractal type: 0 for Mandelbrot, 1 for Julia set */
  short flags;		/* flags and options */
  netcomplex corner;	/* Corner C for Mandelbrot, C for Julia */
  netcomplex z0;	/* Z0=0 for Mandelbrot, corner Z for julia */
  netcomplex delta;	/* dC (Mandelbrot) / dZ (Julia) */
  long iteration_limit;
};

/* This is the information that changes from request to request */

/* This contains the same information as an XRectangle, but we don't want */
/* any X-specific information in the slave */
typedef struct
{ short x;
  short y;
  short height;
  short width;
} ms_rectangle;

/* this is what the .data field of a WhipMessage really looks like */
/* (but Mama doesn't know this) */
/* All the fields are stored in network byte order */
typedef struct
{ struct static_job_info j;	/* corner, delta, iterations... */
  ms_rectangle s;		/* area to calculate */
} ms_job;

#endif /* _ms_job_h */
