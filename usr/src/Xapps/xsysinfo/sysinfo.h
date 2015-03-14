/*
 * sysinfo.h		- get kernel info
 *
 */

/* 
 * Written by Gabor Herr <herr@iti.informatik.th-darmstadt.de>.
 *
 * Copyright (c) 1992, 1993 by Gabor Herr, all rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear in
 * supporting documentation, and that may name is not used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. I make no representations
 * about the suitability of this software for any purpose. It is
 * provided "as is" without express or implied warranty.
 */

/* $Id: sysinfo.h,v 1.3 1993/01/24 17:58:53 gabor Exp $ */
 
#ifndef SYSINFO_INCLUDED
#define SYSINFO_INCLUDED

#define MAX_SWAPFILES 8

struct meminfo {
  int total;
  int used;
  int cache;
  int free;
  int shared;
};

extern void get_meminfo( int *, struct meminfo * );
extern double get_load();
extern double get_idle();
extern int sysinfo_init( int );

#endif
