/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * util.c
 *   Feb. 6, 1990: Larry Medwin
 *   gas: Copyright 1990 Larry Medwin: @(#)util.c	1.2 2/9/90
 */

#include "xgas.h"

/*
 * V EQUILIBRIUM
 * What is magnitude of molecule velocity at wall temperature?
 *   velocity is in mm/microsec
 */
float vEquilibrium( temperature)
    float temperature;
{
    return (float) (1.0e-5 * sqrt( (double)(2.0 * KB * temperature / MASS)));
}

/* CHANGE TEMPERATURE OF ONE CHAMBER */
void changeTemp( w, chamber, sliderpos)	/* ARGSUSED */
    Widget w;
    Box *chamber;
    float *sliderpos;
{
    Arg wargs[1];
    char str[10];
    float chamberTmp = MAXTEMP * (1.0 - *sliderpos);

    /* Make new temperature string */
    chamber->temperature = chamberTmp;
    sprintf( str, "%.1f K", chamberTmp);

    /* Tell the widget */
    XtSetArg( wargs[0], XtNlabel, str);
    XtSetValues( chamber->display, wargs, 1);
}

/*
 *  FRAND -- random number routine
 *  Return a floating point number n such that n >= min and n < max
 *    if (min == max), return min
 */
float frand(min, max)
    float min, max;
{
    float n;

    n = (rand() & 0x7fff) / 32768.0;
/*    n = ((float)rand()) / 32768.0;*/
    return n * (max - min) + min;
}

/*
 * ERROR handler
 *   This is a good place to put a breakpoint.
 */
error( message, time)
    char *message;
    float time;
{
    printf("Error at time = %6.3f msec: %s\n", 1.0e-3 * time, message);
    exit(-1);
}

/*
 * QUIT_CALLBACK
 *   is adapted from Douglas A. Young's
 */
void quit_callback(w, client_data, call_data) /* ARGSUSED */
     Widget     w;
     caddr_t    client_data;
     caddr_t    call_data;
{
   exit(0);
}
