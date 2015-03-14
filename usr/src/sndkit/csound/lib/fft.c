/***********************************************************************\
*	fft.c								*
*   Fast Fourier Transform C library - 					*
*   Based on RECrandell's. With all declarations			*
*   dpwe 22jan90							*
*   08apr90 With experimental FFT2torl - converse of FFT2real		*
\***********************************************************************/

/*  Routines include:
    FFT2raw : Radix-2 FFT, in-place, with yet-scrambled result.
    FFT2    : Radix-2 FFT, in-place and in-order.
    FFTreal : Radix-2 FFT, with real data assumed,
	      in-place and in-order (this routine is the fastest of the lot).
    FFTarb  : Arbitrary-radix FFT, in-order but not in-place.
    FFT2dimensional : Image FFT, for real data, easily modified for other
	      purposes.
	
    To call an FFT, one must first assign the complex exponential factors:
    A call such as
	e = AssignBasis(NULL, size)
    will set up the complex *e to be the array of cos, sin pairs corresponding
    to total number of complex data = size.   This call allocates the
    (cos, sin) array memory for you.  If you already have such memory
    allocated, pass the allocated pointer instead of NULL.
 */

#include <stdio.h>
#include <math.h>
#ifdef THINK_C
#include <stdlib.h>
#endif
/* #ifdef CSOUND
/* #include "cs.h"   /* for mmalloc, mcalloc prototypes */
/* #else  */
#define mmalloc(a) malloc(a)
/* #endif */
#include <fft.h>

LNODE	*lroot = NULL;	/* root of look up table list */

int scancomplexdata(x)
    complex x[];
{   int n=0;
    while(scanf("%f %f\n",&x[n].re, &x[n].im) != -1) ++n;
    return(n);
}

void putcomplexdata(x, n)
    complex x[];
    int n;
{   int j;
    for(j=0;j<n;j++) printf("%f %f\n",x[j].re, x[j].im);
}

void ShowCpx(x,siz,s)
    complex x[];
    int siz;
    char *s;
    {
    int i;

    printf("%s \n",s);
    for(i=0; i<siz; ++i)
	printf(" %6.2f",x[i].re);
    printf("\n");
    for(i=0; i<siz; ++i)
	printf(" %6.2f",x[i].im);
    printf("\n");
    }

int PureReal(x, n)
/* Query whether the data is pure real. */
    complex x[];
    int n;
{   register int m;
    for(m=0;m<n;m++) {
	if(x[m].im!=0.0) return(0);
    }
    return(1);
}

int IsPowerOfTwo(n) int n;
/* Query whether n is a pure power of 2 */
{
    while(n>1) {
	if(n%2) break;
	n >>= 1;
    }
    return(n==1);
}

complex *FindTable(n)	/* search our list of existing LUT's */
    int    n;		/* set up globals expn and lutSize too */
    {
    LNODE *plnode;
    complex *ex = NULL;

/*    lutSize = 0; expn = NULL;	/* globals */
    plnode = lroot;
    for(plnode = lroot; plnode != NULL && plnode->size != n; 
		plnode = plnode->next)
	;
    if(plnode != NULL)
	ex = plnode->table;
/*	{ expn = plnode->table; lutSize = plnode->size; } */
    return(ex);
    }

complex *AssignBasis(ex, n)
    complex ex[];
    int n;
    {
    register int j;
    register float a=0, inc=2*PI/n;
    LNODE    *plnode;
    complex  *expn;

    if(expn=FindTable(n))	
	return(expn);
    if(ex != NULL) expn = ex;
      else
	{ 
	expn = (complex *) mmalloc((long)n*sizeof(complex));
	if(expn==NULL) return(NULL);
	}
    for(j=0;j<n;j++) 
	{
	expn[j].re = cos(a);
	expn[j].im = -sin(a);
	a += inc;
	}
    plnode = lroot;
    lroot = (LNODE *) mmalloc((long)sizeof(LNODE));
    lroot->next = plnode;
    lroot->size = n;
    lroot->table = expn;
/*    lutSize = n;		/* set the global */
    return(expn);
}

void reverseDig(x, n, skip)
    complex x[];
    int n,skip;
{   register int i,j,k, jj, ii;
    complex tmp;
    for(i=0,j=0;i<n-1;i++) {
	if(i<j) {
	jj = j*skip; ii = i*skip;
	tmp = x[jj];
	x[jj]=x[ii];
	x[ii]=tmp;
	}
	k = n/2;
	while(k<=j) {
	    j -= k;
	    k>>=1;
	}
	j += k;
    }
}

void FFT2dimensional(x, w, h, ex)
/* Perform 2D FFT on image of width w, height h (both powers of 2).
   IMAGE IS ASSUMED PURE-REAL.	If not, the FFT2real call should be just FFT2.
 */
    complex *x;
    int w, h;
    complex *ex;
{
    register int j;
    for(j=0;j<h;j++) FFT2real(x+j*w, w, 1, ex);
    for(j=0;j<w;j++) FFT2(x+j, h, w, ex);
}

void FFT2torl(x, n, skip, scale, ex)
/* Performs FFT on data assumed complex conjugate to give purely
 * real result
 */
    complex x[];
    int n,skip;
    float   scale;	   /* (frigged for ugens7.c) */
    complex ex[];	   /* pass in lookup table */
{   register int half = n>>1, quarter = half>>1, m, mm;

    if(!quarter) return;
    Reals(x,n,skip,-1,ex);
    ConjScale(x, half+1, 2.0 * scale);
    FFT2raw(x, half, 2, skip, ex);
    reverseDig(x, half, skip);
    for(mm= half-1;mm>=0;mm--) {
	m = mm*skip;
	x[m<<1].re = x[m].re;
	x[(m<<1)+skip].re = -x[m].im;	    /* need to conjugate result for true ifft */
	x[m<<1].im = x[(m<<1)+skip].im = 0.0;
    }
}

void ConjScale(x, len, scale)
/* Conjugate and scale complex data (e.g. prior to IFFT by FFT)
 */
 complex x[];
 int len;
 float	 scale;
    {
    float miscale = -scale;

    while(len--)
	{
	x->re *= scale;
	x->im *= miscale;
	x++;
	}
    }

void FFT2real(x, n, skip,ex)
/* Perform real FFT, data arrange as {re, 0, re, 0, re, 0...};
 * leaving full complex result in x.
 */
    complex x[];
    int n,skip;
    complex ex[];	/* lookup table */
{   register int half = n>>1, quarter = half>>1, m, mm;

    if(!quarter) return;
    for(mm=0;mm<half;mm++) {
	m = mm*skip;
	x[m].re = x[m<<1].re;
	x[m].im = x[(m<<1)+skip].re;
    }
    FFT2raw(x, half, 2, skip, ex);
    reverseDig(x, half, skip);
    Reals(x,n,skip,1,ex);
}

void Reals(x,n,skip,sign,ex) /* sign is 1 for FFT2real, -1 for torl */
    complex x[];
    int n,skip,sign;
    complex ex[];	/* lookup table passed in */
{   register int half = n>>1, quarter = half>>1, m, mm;
    register float tmp;
    complex a,b,s,t;

    half *= skip; n *= skip;
    if(sign ==1)    x[half]= x[0];	/* only for Yc to Xr */
    for(mm=0;mm<=quarter;mm++) {
	m = mm*skip;
	s.re = (1.0+ex[mm].im)/2;
	s.im = (-(float)sign)*ex[mm].re/2;
	t.re = 1.0-s.re;
	t.im = -s.im;
	a = x[m];
	b = x[half-m];
	b.im = -b.im;
	
	tmp = a.re;
	a.re = (a.re*s.re - a.im*s.im);
	a.im = (tmp*s.im + a.im*s.re);
	
	tmp = b.re;
	b.re = (b.re*t.re - b.im*t.im);
	b.im = (tmp*t.im + b.im*t.re);

	b.re += a.re;
	b.im += a.im;
	
	a = x[m];
	a.im = -a.im;
	x[m] = b;
	if(m) {
	    b.im = -b.im;
	    x[n-m] = b;
	}
	b = x[half-m];

	tmp = a.re;
	a.re = (a.re*t.re + a.im*t.im);
	a.im = (-tmp*t.im + a.im*t.re);
	
	tmp = b.re;
	b.re = (b.re*s.re + b.im*s.im);
	b.im = (-tmp*s.im + b.im*s.re);

	b.re += a.re;
	b.im += a.im;

	x[half-m] = b;
	if(m) {
	    b.im = -b.im;
	    x[half+m] = b;
	}
    }
}

void FFT2(x, n, skip,ex)
/* Perform FFT for n = a power of 2.
   The relevant data are the complex numbers x[0], x[skip], x[2*skip], ...
 */
    complex x[];
    int n,skip;
    complex ex[];
{   FFT2raw(x, n, 1, skip,ex);
    reverseDig(x, n, skip);
}

void FFT2raw(x, n, dilate, skip, ex)
/* Data is x,
   data size is n,
   dilate means: library global expn is the (cos, -j sin) array, EXCEPT for
	effective data size n/dilate,
   skip is the offset of each successive data term, as in "FFT2" above.
 */
    complex x[];
    int n,dilate,skip;
    complex ex[];	/* lookup table */
{   register int j, m=1, p, q, i, k, n2=n, n1 ;
    register float c, s, rtmp, itmp;

    while(m<n) {
	n1 = n2;
	n2 >>= 1;
	for(j=0, q=0; j<n2; j++) {
	    c = ex[q].re;
	    s = ex[q].im;
	    q += m*dilate;
	    for(k=j;k<n;k+=n1) {
		p = (k + n2)*skip;
		i = k*skip;
		rtmp = x[i].re - x[p].re;
		x[i].re += x[p].re;
		itmp = x[i].im - x[p].im;
		x[i].im += x[p].im;
		x[p].re = (c*rtmp - s*itmp);	
		x[p].im = (c*itmp + s*rtmp);
	    }
	}
    m <<= 1;
    }
}

void FFTarb(data, result, n, ex)
/* Compute FFT for arbitrary radix, with limitation  n <= 1024.
 */
    complex data[], result[], ex[];	/* ex is lookup table */
    int n;
{
int s, ctr, p, p0, i, j, a, b, c, v, k, m;
float	x,y,z;
int sum, car, q, arg;
int	aa[10], pr[10], cc[10], jj[1024];
complex t[1024];

    /* Next, get the prime factors of n */
    m = n;
    v = 0;
    j = 2;
    while(m!=1) {
	   while (m%j==0) {
	   m /= j;
	   ++v;
	   pr[v] = j;
	   }
	j += 2;
    if(j==4) j=3;
    }
    /* pr[] is now the array of prime factors of n, with v relevant elements */

    /* Next, re-order the array in reverse-complement binary */
    cc[0] = 1;
    for(i=1;i<v;i++) cc[i] = cc[i - 1] * pr[i];
    for(m=1;m<10;m++) aa[m] = 0;
    jj[0] = 0;	
    /* jj array will be the input order when xr, xi are read */
    for(i=1; i<n; i++) {
	    j = v;
	   car = 1;
	   while(car) {
	    aa[j] += car;
	    car = aa[j]/pr[j];
	    aa[j] %=  pr[j];
	    --j;
	    }
	   sum = 0;
	   for(q=0;q<v;q++) sum += aa[q + 1] * cc[q];
	   jj[sum] = i;
   }	

    /* Next, read in the data */
    for(i=0;i<n;i++) {
       result[jj[i]].re = data[i].re;
       result[jj[i]].im = data[i].im;
     }
	c = v;
    a = 1;
	b = 1;
    while(c) {
	a *= pr[c];
	for(k=0;k<n;k++) {
		    arg = a * (k/a) + k%b;
		    p0 = (k * n) / a;
		    p = 0;
		    x = 0;
		    y = 0;
		for(q=0;q<pr[c];q++) {
	    x += (result[arg].re * ex[p].re - result[arg].im * ex[p].im);
	    y += (result[arg].re * ex[p].im + result[arg].im * ex[p].re);
		      p = (p + p0) % n;
		  arg += b;
		}
		t[k].re = x;
		t[k].im = y;
	}
	    for(k=0;k<n;k++) result[k] = t[k];
	   --c;
	   b = a;
	}
}

void DFT(data,result, n, ex)
/* Perform direct Discrete Fourier Transform. */
    complex data[], result[], ex[];
    int n;
{   int j,k,m;
    float arg, s, c;

    for(j=0;j<n;j++) {
	result[j].re = 0; result[j].im = 0;
	for(k=0;k<n;k++) {
	    m = (j*k)%n;
	    c = ex[m].re; s = ex[m].im;
	    result[j].re += (data[k].re * c - data[k].im * s);
	    result[j].im += (data[k].re * s + data[k].im * c);
	}
    }
}

#ifdef MAIN
	
static TestArith()
/* Lets just see what happens if you do these things ..*/
    {
    float   a,b,c,d;

    a = -10000;
    b = (1/2);
    c = (a*b);
    printf("Result .. and what it should have been\n");
    printf("%f %f\n",c,a/2);
    }

#define tDLEN 16

static TestReals()
/* test out our 'reals' function */
    {
    complex data[tDLEN],datb[tDLEN];
    complex *e;
    int i,len;

    len = tDLEN /2;
    printf("Hello dan.\n");
    srand((int)time(NULL));
    e = AssignBasis(NULL,tDLEN);
    for(i = 0; i<tDLEN; i++)
	{
	datb[i].re = data[i].re = -7+(15&rand());   /* (i-1)? 0 : 1;	/* i+1; */
	datb[i].im = data[i].im = 0;		    /* (tDLEN - i); */
	}
    ShowCpx(data, tDLEN, "Start data");
    FFT2real(data, tDLEN, 1, e);	
    ShowCpx(data, tDLEN, "Transform");
    data[tDLEN/2].re = 0; data[tDLEN/2].im = 0;
    for(i=tDLEN/2+1; i<tDLEN; ++i)
	{
	data[i].re = 0.0; data[i].im = 0.0;
	}
	/* conjugate 2nd half of data should make no difference to FFT2torl */
    FFT2torl(data, tDLEN, 1, 1.0/(float)tDLEN,e); /* */
    ShowCpx(data, tDLEN, "Final result");

    }

main()
    {
    TestReals();
    }

#endif

