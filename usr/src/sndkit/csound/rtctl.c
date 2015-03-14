#include "cs.h"			/*			     rtctl.c	*/
#include "window.h"
#include "rtctl.h"		/* real-time control units		*/
				/* 26aug90 dpwe				*/

extern void (*mkxyFn)();	/* pointer to xyinput window creator */
extern void (*rdxyFn)();	/* pointer to xyinput window creator */

xyinset(p)
 register XYIN	*p;
{
    float	f;

    if ((p->timcount = ekr * *p->iprd) <= 0)
        initerror("illegal iprd");
    if(*p->iymin > *p->iymax)           	/* swap if wrong order */
	{  f = *p->iymin; *p->iymin = *p->iymax; *p->iymax = f; }
    if(*p->iymin == *p->iymax)	/* force some room (why?) */
	{
	*p->iymax = *p->iymin + 1.0;    	/* say.. */
	*p->iymin -= 1.0;
	}
    if(*p->iyinit < *p->iymin) 		*p->iyinit = *p->iymin;
    else if(*p->iyinit > *p->iymax) 	*p->iyinit = *p->iymax;

    if(*p->ixmin > *p->ixmax)   	/* swap if wrong order */
	{  f = *p->ixmin; *p->ixmin = *p->ixmax; *p->ixmax = f; }
    if(*p->ixmin == *p->ixmax)  	/* force some room (why?) */
	{
	*p->ixmax = *p->ixmin + 1.0;    	/* say.. */
	*p->ixmin -= 1.0;
	}
    if(*p->ixinit < *p->ixmin) 		*p->ixinit = *p->ixmin;
    else if(*p->ixinit > *p->ixmax) 	*p->ixinit = *p->ixmax;

    (*mkxyFn)(&p->w,(*p->ixinit - *p->ixmin)/(*p->ixmax - *p->ixmin), 
	           (*p->iyinit - *p->iymin)/(*p->iymax - *p->iymin) );

    p->countdown = 1;           /* init counter to run xyin on first call */
    }

xyin(p)
 register XYIN *p;
{
    if (!(--p->countdown)) {                  /* at each countdown   */
        p->countdown = p->timcount;           /*   reset counter &   */
	(*rdxyFn)(&p->w);                      /*   read cursor postn */
	*(p->kxrslt) = *p->ixmin + p->w.x * (*p->ixmax - *p->ixmin);
	*(p->kyrslt) = *p->iymin + (1.0 - p->w.y) * (*p->iymax - *p->iymin);
	/*  printf("x: %5.2f   y:%5.2f \n", *p->kxrslt, *p->kyrslt); */
    }
}
