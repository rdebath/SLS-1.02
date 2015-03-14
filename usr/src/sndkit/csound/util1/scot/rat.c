/* The following routines operate on the RAT data type. */

#include "data.h"

/* gcd of two long integers */

long gcd(a, b)
 long a, b;
{
	long x;
	if (a < b) {
		x = a;
		a = b;
		b = x;
	}
	if (b == 0)
		return(1L);
	while (x = a % b) {
		a = b;
		b = x;
	}
	return (b);
}

rassn(a, b)		       /* a = b */
 register RAT *a, *b;
{
	a->num = b->num;
	a->den = b->den;
}

radd(a, b)		       /* a += b */
 register RAT *a, *b;
{
	long x, d, n;
	x = gcd (a->den, b->den);
	d = b->den / x;
	n = (a->num * d) + (b->num * a->den / x);
	d *= a->den;
	x = gcd (n > 0 ? n : -n, d);
	a->num = n / x;
	a->den = d / x;
}

rsub(a,b)			/* a -= b */
 register RAT *a, *b;
{
	b->num = - (b->num);
	radd (a, b);
	b->num = - (b->num);
}

rmul(a,b)		       /* a *= b */
 register RAT *a, *b;
{
	long g;
	a->num *= b->num;
	a->den *= b->den;
	g = gcd(a->num, a->den);
	a->num /= g;
	a->den /= g;
}

rdiv(a, i)		       /* a /= b, b integer */
 register RAT *a;
 int i;
{
	long g; 	/* for gcd */
	a->den *= i;
	g = gcd(a->num, a->den);
	a->num /= g;
	a->den /= g;
}

/* returns 1 if a > b, -1 if a < b, 0 if a == b */

int rcmp(a, b)
 register RAT *a, *b;
{
	RAT x;
	x.num = - (b->num);
	x.den = b->den;
	radd(&x, a);
	if (x.num > 0)
		return (1);
	if (x.num < 0)
		return (-1);
	return (0);
}


/* like rcmp(a,b) where b is integer */

int ricmp(r, i)
 register RAT *r;
 int i;
{
	RAT tmp;
	rint (&tmp, i);
	return (rcmp (r, &tmp));
}


/* converts RAT to a double precision number */

double rval(a)
 register RAT *a;
{
	return ( ((double)(a->num)) / ((double)(a->den)) );
}


/* assign integer value to RAT */

rint(r, i)
 register RAT *r;
 int i;
{
	r->num = i;
	r->den = 1;
}
