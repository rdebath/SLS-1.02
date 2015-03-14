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

#include <stdio.h>
#include <time.h>
#include <syslog.h>
#include <errno.h>
#include <varargs.h>
#include <strings.h>

#include "alloc.h"
#include "bool.h"
#include "list.h"
#include "hash.h"
#include "logmsg.h"

extern char *sys_errlist[];
extern int sys_nerr;

static LIST *logs = NULL;

struct logrecord {
    long last;
    long ok;
    int count;
    int shutdown;
};

#define HTSIZE		   1024
#define LIMIT_RESTART_WAIT (60*10)

static HTTABLE *limit_ht;
static int limit_rate;

#define LIMIT_OK	0
#define LIMIT_SHUTDOWN	1
#define LIMIT_OFF	2

static int
limit_check(file, line)
char *file;
int line;
{
    char key[BUFSIZ];
    struct logrecord *r;
    long curtime = time(0);

    if (limit_ht == NULL) {
	if ((limit_ht = htinit(HTSIZE, 0)) == NULL) {
	    fprintf(stderr, "can't init logmsg hashtable\n");
	    return (LIMIT_OK);
	}
    }

    if (file == NULL)
      return (LIMIT_OK);

    sprintf(key, "%s%d", file, line);
    
    if ((r = (struct logrecord *)htgetdata(key, limit_ht)) == NULL) {
	if ((r = salloc(1, struct logrecord)) == NULL) {
	    fprintf(stderr, "can't allocate logrecord struct\n");
	    return (LIMIT_OK);
	}
	htadd(key, limit_ht, (char *)r);
    }

    r->count++;

    if (r->shutdown) {
	if (r->last == curtime)
	  return (LIMIT_OFF);
	else {
	    r->last = curtime;
	    if (r->count/(curtime - r->ok) > limit_rate) {
		r->ok = curtime;
		r->count = 1;
		return (LIMIT_OFF);
	    } else {
		if ((curtime - r->ok) >= (long)LIMIT_RESTART_WAIT) {
		    r->shutdown = FALSE;
		    r->count = 1;
		    return (LIMIT_OK);
		} else {
		    r->count = 1;
		    return (LIMIT_OFF);
		}
	    }
	}
    }

    if (r->last == curtime) {
	if (r->count > limit_rate) {
	    r->shutdown = TRUE;
	    r->ok = curtime;
	    return (LIMIT_SHUTDOWN);
	}
    } else {
	r->last = curtime;
	r->count = 1;
    }

    return (LIMIT_OK);
}

/*
  We break this routine out from below to avoid performing
  this scan if we're not using the format string.  That is,
  the loglevel is to low to force the printout.
*/
static void
fix_format(format_in, format_out, len, errno_save, file, line)
char *format_in;
char *format_out;
int len;
int errno_save;
char *file;
int line;
{
    char *f = format_in;
    char *b = format_out;
    char *b_end = &format_out[len];
    char c;

    while ((c = *f++) != '\0' && c != '\n' && b < b_end) {
	if (c != '%') {
	    *b++ = c;
	    continue;
	}
	c = *f++;
	switch (c) {
	  case 'm':
	    if ((unsigned)errno_save > sys_nerr)
	      sprintf(b, "error %d", errno_save);
	    else
	      strcpy(b, sys_errlist[errno_save]);
	    b += strlen(b);
	    break;
	  case 'F':
	    sprintf(b, "%s", file);
	    b += strlen(b);
	    break;
	  case 'L':
	    sprintf(b, "%d", line);
	    b += strlen(b);
	    break;
	  default:
	    *b++ = '%';
	    *b++ = c;
	    break;
	}
    }

    *b++ = '\n';
    *b = '\0';
}

/*VARARGS*/
int
logmsg(va_alist)
va_dcl
{
    va_list ap;
    int level;
    char *format;
    NODE *node = NULL;
    LOG *log;
    char buf[BUFSIZ];
    char formatbuf[BUFSIZ];
    char *file;
    int line;
    int mask;
    int limit_stat;
    int do_fix = TRUE;
    int errno_save;

    errno_save = errno;

    va_start(ap);

    level = va_arg(ap, int);
    file = va_arg(ap, char *);
    line = va_arg(ap, int);
    mask = va_arg(ap, int);
    format = va_arg(ap, char *);

    if ((file != NULL) && (limit_rate > 0))
      limit_stat = limit_check(file, line);
    else
      limit_stat = LIMIT_OK;

    if (limit_stat == LIMIT_OFF)
      return (0);


    if (logs == NULL) {
	if (do_fix) {
	    fix_format(format, formatbuf, sizeof(formatbuf),
		       errno_save, file, line);
	    do_fix = FALSE;
	}
	vfprintf(stderr, formatbuf, ap);
	return (0);
    }

    if (level < 0)
      level = 0;
    if (level > MAX_LOG_LEVEL)
      level = MAX_LOG_LEVEL;

    while ((log = (LOG *)list_next(logs, &node)) != NULL) {
	if ((mask == 0) || mask & log->mask[level]) {
	    if (log->style & L_STYLE_TIME) {
		struct tm *tm;
		long t = time(0);
		tm = localtime(&t);
		sprintf(buf, "%d/%d/%d %02d:%02d:%02d",
			tm->tm_mon+1, tm->tm_mday, tm->tm_year,
			tm->tm_hour, tm->tm_min, tm->tm_sec);
	    } else
	      buf[0] = '\0';

	    if ((log->style & L_STYLE_LINE) && (file != NULL)) {
		char *ptr = rindex(file, '.');
		if (ptr != NULL)
		  *ptr = '\0';
		if (strlen(buf) > 0)
		  strcat(buf, " ");
		sprintf(&buf[strlen(buf)], "(%s:%d): ", file, line);
	    } else {
		if (strlen(buf) > 0)
		  strcat(buf, ": ");
	    }

	    if (limit_stat == LIMIT_SHUTDOWN)
	      strcat(buf, "EXCESSIVE MESSAGES: ");

	    if (do_fix) {
		fix_format(format, formatbuf, sizeof(formatbuf),
			   errno_save, file, line);
		do_fix = FALSE;
	    }
	    vsprintf(&buf[strlen(buf)], formatbuf, ap);

	    if (log->type != L_TYPE_SYSLOG)
	      fprintf(log->fp, "%s", buf);
	    else
	      syslog(level, "%s", buf);
	}
    }

    va_end(ap);

    return (0);
}

int
notice(va_alist)
va_dcl
{
    va_list ap;
    char *format;
    char buf[BUFSIZ];

    va_start(ap);
    format = va_arg(ap, char *);
    va_end(ap);

    vsprintf(buf, format, ap);

    logmsg(LOG_INFO, NULL, 0, 0, "%s", buf);

    return (0);
}

int
log_set_limit(rate)
int rate;
{
    limit_rate = rate;

    return (0);
}

static int
reset_limits(r)
struct logrecord *r;
{
    r->shutdown = FALSE;

    return (0);
}

int
log_reset_limits()
{
    if (limit_ht != NULL)
      htmap(limit_ht, reset_limits, 0);
    
    return (0);
}

int
log_set_mask(log, level, mask)
LOG *log;
int level;
int mask;
{
    int i;

    if (level < 0)
      level = 0;

    if (level > MAX_LOG_LEVEL)
      level = MAX_LOG_LEVEL;

    for (i = 0; i <= level; i++)
     log->mask[i] |= mask;

    for (i = level+1; i <= MAX_LOG_LEVEL; i++)
      log->mask[i] &= ~mask;
    
    return (0);
}

int
log_set_level(log, level)
LOG *log;
int level;
{
    return (log_set_mask(log, level, -1));
}

static LOG *
add_log(level, mask, type, style)
int level;
int mask;
int type;
int style;
{
    LOG *log;

    if (logs == NULL)
      if ((logs = list_make(NULL, NULL)) == NULL)
	return (NULL);

    if ((log = salloc(1, LOG)) == NULL)
      return (NULL);

    log->type = type;
    log->style = style;

    log_set_mask(log, level, mask);

    list_add(logs, (char *)log);

    return (log);
}

LOG *
log_open_syslog(ident, logopt, facility, level, mask, style)
char *ident;
int logopt;
int facility;
int level;
int mask;
int style;
{
    LOG *log;

    if ((log = add_log(level, mask, L_TYPE_SYSLOG, style)) == NULL)
      return (NULL);

    openlog(ident, logopt, facility);

    return (log);
}

LOG *
log_open_fp(fp, level, mask, style)
FILE *fp;
int level;
int mask;
int style;
{
    LOG *log;

    if ((log = add_log(level, mask, L_TYPE_FP, style)) == NULL)
      return (NULL);
    
    log->fp = fp;

    return (log);
}

LOG *
log_open_file(filename, level, mask, style)
char *filename;
int level;
int mask;
int style;
{
    FILE *fp;
    LOG *log;

    if ((fp = fopen(filename, "a")) == NULL)
      return (NULL);

    if ((log = add_log(level, mask, L_TYPE_FILE, style)) == NULL)
      return (NULL);

    log->fp = fp;

    return (log);
}

int
log_close(log)
LOG *log;
{
    if (log->type == L_TYPE_FILE)
      fclose(log->fp);
    
    list_delete(logs, (char *)log);

    return (0);
}

int
log_parse_mask(mask_str, alist, size, maskp)
char *mask_str;
LOGMASK_ALIST *alist;
int size;
int *maskp;
{
    char name[BUFSIZ];
    char *namep = name;
    char *ptr = mask_str;
    int i;

    *maskp = 0;

    for (;;) {
	/*
	  Mask lists have symbolic mask names separated by commas.
	*/
	if ((*ptr == ',') || (*ptr == '\0')) {
	    /*
	      Process the last entry we found by searching the mask
	      alist for a match, and oring in the bit pattern.
	    */
	    *namep = '\0';
	    for (i=0; i<size; i++) {
		if (strcmp(name, alist[i].name) == 0) {
		    *maskp |= alist[i].bit;
		    goto ok;
		}
	    }
	    return (-1);
	  ok:
	    namep = name;
	} else
	  *(namep++) = *ptr;
	
	if (*ptr == '\0')
	  return (0);
	
	ptr++;
    }
}

#ifdef DEBUG
log_test_open()
{
    log_open_fp(stderr, LOG_DEBUG, L_MASK_ALL, L_STYLE_ALL);
}

log_test()
{
    logmsg(L_EMERG, 1, "test message");
}

log_test_other()
{
    logmsg(L_EMERG, 1, "test other message");
}
#endif DEBUG
