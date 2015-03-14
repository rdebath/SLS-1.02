/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef INClogmsgh
#define INClogmsgh 1

#include <syslog.h>

#define MAX_LOG_LEVEL	LOG_DEBUG

typedef struct _log {
    int mask[MAX_LOG_LEVEL+1];
    int type;
    int style;
    FILE *fp;
} LOG;

/* log styles */
#define L_STYLE_TIME	001			/* include time/date 	*/
#define L_STYLE_LINE	002			/* include file/line	*/
#define L_STYLE_ALL	(L_STYLE_TIME|L_STYLE_LINE)

/* log masks */
#define L_MASK_ALL	-1			/* all catagories	*/

/* log types */
#define L_TYPE_FILE	0	/* log to a file	*/
#define L_TYPE_FP	1	/* log to an fp		*/
#define L_TYPE_SYSLOG	2	/* log to syslog	*/

/* log levels */
#define L_EMERG   LOG_EMERG,__FILE__,__LINE__	/* system is unusabled	*/
#define L_ALERT	  LOG_ALERT,__FILE__,__LINE__	/* action must be taken	*/
#define L_CRIT	  LOG_CRIT,__FILE__,__LINE__	/* critical condition	*/
#define L_ERR	  LOG_ERR,__FILE__,__LINE__	/* error condition	*/
#define L_WARNING LOG_WARNING,__FILE__,__LINE__	/* warning condition	*/
#define L_NOTICE  LOG_NOTICE,__FILE__,__LINE__	/* normal but signif	*/
#define L_INFO    LOG_INFO,__FILE__,__LINE__	/* informational	*/
#define L_DEBUG   LOG_DEBUG,__FILE__,__LINE__	/* debug-level message	*/

/* used to parse a symbolic mask list */
typedef struct {
    char *name;
    int  bit;
} LOGMASK_ALIST;

/*
  Prototypes:
*/

/*VARARGS*/
int logmsg();

/*VARARGS*/
int notice();

int log_set_limit(
#ifdef _PROTO
    int rate
#endif
);

int log_reset_limits(
#ifdef _PROTO
    void
#endif		     
);

int log_set_mask(
#ifdef _PROTO
    LOG *log, 
    int level,
    int mask
#endif
);

int log_set_level(
#ifdef _PROTO
    LOG *log, 
    int level
#endif
);

LOG *log_open_syslog(
#ifdef _PROTO
    char *ident, 
    int logopt,
    int facility, 
    int level, 
    int mask,
    int style
#endif
);

LOG *log_open_fp(
#ifdef _PROTO
    FILE *fp,
    int level,
    int mask,
    int style
#endif
);

LOG *log_open_file(
#ifdef _PROTO
    char *filename,
    int level,
    int mask,
    int style
#endif
);

int log_close(
#ifdef _PROTO
    LOG *log
#endif
);

int log_parse_mask(
#ifdef _PROTO
    char *mask_str, 
    LOGMASK_ALIST *alist,
    int size,
    int *maskp
#endif
);

int log_open_network(
#ifdef _PROTO
    int dispatch_priority,
    char *service
#endif
);

int log_connect(
#ifdef _PROTO
    char *host,
    char *service,
    int level
#endif
);

#endif
