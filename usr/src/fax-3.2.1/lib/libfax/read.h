/*
  read.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_read_h
#define in_libfax_read_h 1

/*
  Prototypes:
*/

int tread(
#ifdef _PROTO
     int fd,
     char *buf,
     int bufsize,
     int timeout
#endif
);

int pread(
#ifdef _PROTO
     int fd,
     char *buf,
     int bufsize
#endif
);

int fdgets(
#ifdef _PROTO
     int fd,
     char *buf,
     int bufsize
#endif
);

int tfdgets(
#ifdef _PROTO
     int fd,
     char *buf,
     int bufsize,
     int timeout
#endif
);

int wait_for_char(
#ifdef _PROTO
     int fd,
     char c_expect,
     int timeout
#endif
);

int wait_for_string(
#ifdef _PROTO
     int fd,
     char *s_expect,
     int timeout
#endif
);

#endif
