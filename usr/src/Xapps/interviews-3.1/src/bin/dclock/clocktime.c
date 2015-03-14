/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * time related functions for digital clock
 */

#include "clocktime.h"
#include <string.h>

#ifdef __DECCXX
extern "C" int gettimeofday(struct timeval*, struct timezone*);
#endif

Clock::Clock () {
#if defined(sun) && OSMajorVersion >= 5
    gettimeofday(&gmt);
#else
    gettimeofday(&gmt, 0);
#endif
    nextMinute = gmt.tv_sec;
}

int Clock::NextTick () {
#if defined(sun) && OSMajorVersion >= 5
    gettimeofday(&gmt);
#else
    gettimeofday(&gmt, 0);
#endif
    return nextMinute - gmt.tv_sec;
}

void Clock::GetTime (char* date, int& h, int& m, int& s) {
    struct tm local;

#if defined(hpux) || defined(AIXV3)
    local = * localtime((time_t*) &gmt.tv_sec);
#else
#ifdef __DECCXX
    local = * localtime((time_t*)&gmt.tv_sec);
#else
    local = * localtime(&gmt.tv_sec);
#endif
#endif
    h = local.tm_hour;
    m = local.tm_min;
    s = local.tm_sec;
    char ds[26];
    strcpy(ds, asctime(&local));
    strncpy(date, ds, 10);		/* day, month, day of month */
    date[10] = '\0';
    strncat(date, ds+19, 5);		/* year */
    date[15] = '\0';
    nextMinute = gmt.tv_sec + (60 - s);
}
