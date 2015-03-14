/*
  write.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_write_h
#define in_libfax_write_h 1

/*
  Prototypes:
*/

int nwrite(
#ifdef _PROTO
     int fd,
     char *buf,
     int bufsize
#endif
);

/* VARARGS */
int fdprintf();

#endif
