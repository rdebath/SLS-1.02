/*
  msgs.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_msgs_h
#define in_libfax_msgs_h 1

/*
  Prototypes:
*/

char *hayes_result_msg(
#ifdef _PROTO
     int result
#endif
);

char *faxmodem_result_msg(
#ifdef _PROTO
     FaxModem *f
#endif
);

void faxmodem_print_id_strings(
#ifdef _PROTO
     FaxModem *f,
     FILE *fp
#endif
);

char *t30_vr_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_br_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_wd_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_ln_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_df_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_ec_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_bf_string(
#ifdef _PROTO
     T30params *p
#endif
);

char *t30_st_string(
#ifdef _PROTO
     T30params *p
#endif
);

#endif
