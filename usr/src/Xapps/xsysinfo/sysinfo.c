/*
 * sysinfo.c		- get kernel info
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

/* $Id: sysinfo.c,v 1.2 1993/01/24 17:47:43 gabor Exp $ */
 
#include "sysinfo.h"
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>

static int meminfo_fd;
static int idle_fd;
static int loadavg_fd;

static char buffer[1024];

static void reread( int fd, char *message )
{
  if ( lseek( fd, 0L, 0 ) == 0 &&
       read( fd, buffer, sizeof(buffer) - 1 ) > 0 )
    return;
       
  perror(message);
  exit(1);
}

void get_meminfo( int *swapdevs, struct meminfo *result )
{
  int i;
  char *bufptr;
  int res;

  reread( meminfo_fd, "get_meminfo:");

  bufptr = strchr( buffer, '\n' ) + 1;
  sscanf( bufptr, "Mem: %d %d %d %d %d",
  	&result[0].total,
  	&result[0].used,
  	&result[0].free,
  	&result[0].shared,
  	&result[0].cache );
  
  for ( i = 1; i < MAX_SWAPFILES; i++ ) {

    bufptr = strchr( bufptr, '\n' ) + 1;

    if ( *bufptr == '\0' ||
	 (res = sscanf( bufptr, "Swap: %d %d %d",
  		&result[i].total,
  		&result[i].used,
  		&result[i].free )) != 3 )
	break; 

  }

  *swapdevs = i - 1;  
}

double get_load( void )
{
  double load;

  reread( loadavg_fd, "get_load:");

  sscanf( buffer, "%lf", &load );

  return load;
}

double get_idle()
{
  static int last_idle  = 0;
  static int last_ticks = 0;
  int idle;
  int ticks;
  int idle_diff;
  int ticks_diff;

  reread( idle_fd, "get_idle:");

  sscanf( buffer, "%d %d", &idle, &ticks );

  idle_diff  = idle  - last_idle;
  ticks_diff = ticks - last_ticks;

  last_idle  = idle;
  last_ticks = ticks; 

  if (idle_diff > 0 && ticks_diff > 0 )
    return (double) idle_diff / ticks_diff;
  else
    return 0.0; 
}

int sysinfo_init( int no_idle )
{
  if ((meminfo_fd = open("/proc/meminfo",O_RDONLY)) < 0) {
    perror("/proc/meminfo");
    return 1;
  }
  if ((loadavg_fd = open("/proc/loadavg",O_RDONLY)) < 0) {
    perror("/proc/loadavg");
    return 1;
  }

  if (no_idle) return 0;
  
  if ((idle_fd = open("/proc/idle",O_RDONLY)) < 0) {
    perror("/proc/idle");
    return 1;
  }
  return 0;
}

