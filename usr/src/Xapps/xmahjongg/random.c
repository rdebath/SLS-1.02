/*
 ******************************************************************************
 *									      *
 *	Copyright (c) 1990 by Jeff S. Young.  All rights reserved under the   *
 *	copyright laws of the United States.			      	      *
 *									      *
 ******************************************************************************
 */

#include <stdio.h>
#include <sys/time.h>

#define	POLY_SIZE	31
#define	TAP_1		0
#define	TAP_2		3
#define	TAP_3		POLY_SIZE

long	poly[POLY_SIZE+1];
long	*p1 = &poly[TAP_1];
long	*p2 = &poly[TAP_2];
long	*p3 = &poly[TAP_3];

long random_init(x)
int x;
{
	int i;

	poly[0] = x;
	for (i = 1; i < POLY_SIZE; i++) {
		poly[i] = 3234846615*poly[i-1] + 47027;
	};

	p1 = &poly[TAP_1];
	p2 = &poly[TAP_2];
	p3 = &poly[TAP_3];

	for (i = 0; i < 2*POLY_SIZE; i++) {
		random();
	};

	return(0L);
}

long random_next()
{
	long x;

	*p3 = (*p2 + *p1);
	x = (*p3 >> 1) & 0x7fffffff;

	if (p3 == &poly[POLY_SIZE]) {
		p3 = poly;
		p2++;
		p1++;
	} else if (p2 == &poly[POLY_SIZE]) {
		p3++;
		p2 = poly;
		p1++;
	} else if (p1 == &poly[POLY_SIZE]) {
		p3++;
		p2++;
		p1 = poly;
	} else {
		p3++;
		p2++;
		p1++;
	};

	return(x);
}
