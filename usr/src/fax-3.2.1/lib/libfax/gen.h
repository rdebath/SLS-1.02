/*
  gen.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_gen_h
#define in_libfax_gen_h 1

/*
  Prototypes:
*/

int faxmodem_open(
#ifdef _PROTO
     FaxModem *f,
     char *filename
#endif
);

int faxmodem_close(
#ifdef _PROTO
     FaxModem *f
#endif
);

int faxmodem_sync(
#ifdef _PROTO
     FaxModem *f,
     int total_tries
#endif
);

int faxmodem_hangup(
#ifdef _PROTO
     FaxModem *f
#endif
);

int faxmodem_bit_reverse(
#ifdef _PROTO
     FaxModem *f,
     int code
#endif
);

int faxmodem_byte_align(
#ifdef _PROTO
     FaxModem *f,
     int code
#endif
);

int faxmodem_set_capabilities(
#ifdef _PROTO
     FaxModem *f,
     int vr,
     int br			      
#endif
);

#endif
