/*
  send.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_send_h
#define in_libfax_send_h 1

/*
  Prototypes:
*/

int faxmodem_initiate_call(
#ifdef _PROTO
     FaxModem *f,
     char *dialstring
#endif
);

int faxmodem_send_page(
#ifdef _PROTO
     FaxModem *f,
     int fd,
     int last_page,
     int tries
#endif
);

#endif
