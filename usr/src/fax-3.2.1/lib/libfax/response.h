/*
  response.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_response_h
#define in_libfax_response_h 1

/*
  Prototypes:
*/

int get_modem_response(
#ifdef _PROTO
     FaxModem *f,
     int timeout
#endif
);

void init_modem_response(
#ifdef _PROTO
     FaxModem *f
#endif
);

void faxmodem_print_status(
#ifdef _PROTO
     FaxModem *f
#endif
);

#endif
