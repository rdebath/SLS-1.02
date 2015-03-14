/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : strftime.c
 *  Author    : A.Robbins & I.Lea
 *  Created   : 01-02-91
 *  Updated   : 29-09-92
 *  Notes     : Relatively quick-and-dirty implemenation of ANSI library 
 *              routine for System V Unix systems.
 *              It's written in old-style C for maximal portability.
 *  Example   : time (&secs);
 *              tm = localtime (&secs);
 *              num = strftime (buf, sizeof (buf), "%a %d-%m-%y %H:%M:%S", tm);
 *  Copyright : (c) Copyright 1991-92 by Arnold Robbins & Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include "tin.h"

#ifdef SYSV
extern int daylight;
#endif

#define SYSV_EXT	1	/* stuff in System V ascftime routine */

/* 
 * strftime --- produce formatted time 
 */

#ifndef __STDC__
size_t
my_strftime(s, maxsize, format, timeptr)
char *s;
size_t maxsize;
char *format;
struct tm *timeptr;
#else
size_t
my_strftime(char *s, size_t maxsize, char *format, struct tm *timeptr)
#endif
{
	char *endp = s + maxsize;
	char *start = s;
	char tbuf[100];
	int i;

	/* 
	 * various tables, useful in North America 
	 */
	static char *days_a[] = {
		"Sun", "Mon", "Tue", "Wed",
		"Thu", "Fri", "Sat",
	};
	static char *days_l[] = {
		"Sunday", "Monday", "Tuesday", "Wednesday",
		"Thursday", "Friday", "Saturday",
	};
	static char *months_a[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	};
	static char *months_l[] = {
		"January", "February", "March", "April",
		"May", "June", "July", "August", "September",
		"October", "November", "December",
	};
	static char *ampm[] = { "AM", "PM", };

	if (s == NULL || format == NULL || timeptr == NULL || maxsize == 0)
		return 0;

	if (strchr(format, '%') == NULL && strlen(format) + 1 >= maxsize)
		return 0;

#ifndef DONT_HAVE_TZSET
	tzset();
#endif

	for (; *format && s < endp - 1; format++) {
		tbuf[0] = '\0';
		if (*format != '%') {
			*s++ = *format;
			continue;
		}
		switch (*++format) {
		case '\0':
			*s++ = '%';
			goto out;

		case '%':
			*s++ = '%';
			continue;

		case 'a':	/* abbreviated weekday name */
			strcpy(tbuf, days_a[timeptr->tm_wday]);
			break;

		case 'A':	/* full weekday name */
			strcpy(tbuf, days_l[timeptr->tm_wday]);
			break;

#ifdef SYSV_EXT
		case 'h':	/* abbreviated month name */
#endif
		case 'b':	/* abbreviated month name */
			strcpy(tbuf, months_a[timeptr->tm_mon]);
			break;

		case 'B':	/* full month name */
			strcpy(tbuf, months_l[timeptr->tm_mon]);
			break;

		case 'c':	/* appropriate date and time representation */
			sprintf(tbuf, "%s %s %2d %02d:%02d:%02d %d",
				days_a[timeptr->tm_wday],
				months_a[timeptr->tm_mon],
				timeptr->tm_mday,
				timeptr->tm_hour,
				timeptr->tm_min,
				timeptr->tm_sec,
				timeptr->tm_year + 1900);
			break;

		case 'd':	/* day of the month, 01 - 31 */
			sprintf(tbuf, "%02d", timeptr->tm_mday);
			break;

		case 'H':	/* hour, 24-hour clock, 00 - 23 */
			sprintf(tbuf, "%02d", timeptr->tm_hour);
			break;

		case 'I':	/* hour, 12-hour clock, 01 - 12 */
			i = timeptr->tm_hour;
			if (i == 0)
				i = 12;
			else if (i > 12)
				i -= 12;
			sprintf(tbuf, "%02d", i);
			break;

		case 'j':	/* day of the year, 001 - 366 */
			sprintf(tbuf, "%03d", timeptr->tm_yday + 1);
			break;

		case 'm':	/* month, 01 - 12 */
			sprintf(tbuf, "%02d", timeptr->tm_mon + 1);
			break;

		case 'M':	/* minute, 00 - 59 */
			sprintf(tbuf, "%02d", timeptr->tm_min);
			break;

		case 'p':	/* am or pm based on 12-hour clock */
			if (timeptr->tm_hour < 12)
				strcpy(tbuf, ampm[0]);
			else
				strcpy(tbuf, ampm[1]);
			break;

		case 'S':	/* second, 00 - 61 */
			sprintf(tbuf, "%02d", timeptr->tm_sec);
			break;

		case 'w':	/* weekday, Sunday == 0, 0 - 6 */
			sprintf(tbuf, "%d", timeptr->tm_wday);
			break;

		case 'x':	/* appropriate date representation */
			sprintf(tbuf, "%s %s %2d %d",
				days_a[timeptr->tm_wday],
				months_a[timeptr->tm_mon],
				timeptr->tm_mday,
				timeptr->tm_year + 1900);
			break;

		case 'X':	/* appropriate time representation */
			sprintf(tbuf, "%02d:%02d:%02d",
				timeptr->tm_hour,
				timeptr->tm_min,
				timeptr->tm_sec);
			break;

		case 'y':	/* year without a century, 00 - 99 */
			i = timeptr->tm_year % 100;
			sprintf(tbuf, "%d", i);
			break;

		case 'Y':	/* year with century */
			sprintf(tbuf, "%d", 1900 + timeptr->tm_year);
			break;

#ifdef SYSV_EXT
		case 'n':	/* same as \n */
			tbuf[0] = '\n';
			tbuf[1] = '\0';
			break;

		case 't':	/* same as \t */
			tbuf[0] = '\t';
			tbuf[1] = '\0';
			break;

		case 'D':	/* date as %m/%d/%y */
			my_strftime(tbuf, sizeof tbuf, "%m/%d/%y", timeptr);
			break;

		case 'e':	/* day of month, blank padded */
			sprintf(tbuf, "%2d", timeptr->tm_mday);
			break;

		case 'r':	/* time as %I:%M:%S %p */
			my_strftime(tbuf, sizeof tbuf, "%I:%M:%S %p", timeptr);
			break;

		case 'R':	/* time as %H:%M */
			my_strftime(tbuf, sizeof tbuf, "%H:%M", timeptr);
			break;

		case 'T':	/* time as %H:%M:%S */
			my_strftime(tbuf, sizeof tbuf, "%H:%M:%S", timeptr);
			break;
#endif

		default:
			tbuf[0] = '%';
			tbuf[1] = *format;
			tbuf[2] = '\0';
			break;
		}
		i = strlen(tbuf);
		if (i)
			if (s + i < endp - 1) {
				strcpy(s, tbuf);
				s += i;
			} else
				return 0;
	}
out:
	if (s < endp && *format == '\0') {
		*s = '\0';
		return (s - start);
	} else
		return 0;
}
