/*
  ttyio.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_tty_h
#define in_libfax_tty_h 1

typedef enum {
    FC_OUTPUT_ON,	/* allow modem to send ^S to stop computer */
    FC_INPUT_ON,	/* allow computer to send ^S to stop modem */
    FC_BOTH_ON,		/* both input and output flow control on   */
    FC_BOTH_OFF,	/* no flow control on			   */
} fc_state;

/*
  Prototypes:
*/

int tty_fc(
#ifdef _PROTO
     int fd,
     fc_state state
#endif
);

int tty_open(
#ifdef _PROTO
     char *filename
#endif
);

int tty_close(
#ifdef _PROTO
     int fd
#endif
);

#endif
