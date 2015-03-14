/*
  recv.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_recv_h
#define in_libfax_recv_h 1

/*
  Prototypes:
*/

int faxmodem_answer(
#ifdef _PROTO
     FaxModem *f
#endif
);

recv_code faxmodem_start_recv(
#ifdef _PROTO
     FaxModem *f
#endif
);

int faxmodem_recv_page(
#ifdef _PROTO
     FaxModem *f,
     int fd
#endif
);

#endif
