/*
  log.h

  (c) Copyright 1991 by David M. Siegel.
      All rights reserved.

  %W% %G% %U%
*/

#ifndef in_libfax_log_h
#define in_libfax_log_h 1

#include <syslog.h>

/* log levels */
#define L_EMERG   LOG_EMERG,__FILE__,__LINE__	/* system is unusabled	*/
#define L_ALERT	  LOG_ALERT,__FILE__,__LINE__	/* action must be taken	*/
#define L_CRIT	  LOG_CRIT,__FILE__,__LINE__	/* critical condition	*/
#define L_ERR	  LOG_ERR,__FILE__,__LINE__	/* error condition	*/
#define L_WARNING LOG_WARNING,__FILE__,__LINE__	/* warning condition	*/
#define L_NOTICE  LOG_NOTICE,__FILE__,__LINE__	/* normal but signif	*/
#define L_INFO    LOG_INFO,__FILE__,__LINE__	/* informational	*/
#define L_DEBUG   LOG_DEBUG,__FILE__,__LINE__	/* debug-level message	*/

/*
  Prototypes:
*/

/*VARARGS*/
int log();

void log_set_level(
#ifdef _PROTO
    int
#endif
);

void log_enable_syslog(
#ifdef _PROTO
     int facility
#endif
);

#endif
