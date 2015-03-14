/***********************************************************************\
*	fft.h								*
*   Fast Fourier Transform C library - header file			*
*   Based on RECrandell's						*
*   dpwe 22jan90							*
*   08apr90 With experimental FFT2torl - converse of FFT2real		*
\***********************************************************************/

/*  
    To call an FFT, one must first assign the complex exponential factors:
    A call such as
	e = AssignBasis(NULL, size)
    will set up the complex *e to be the array of cos, sin pairs corresponding
    to total number of complex data = size.   This call allocates the
    (cos, sin) array memory for you.  If you already have such memory
    allocated, pass the allocated pointer instead of NULL.
 */

/*** how can it be so hard to get PI *?***/
#ifndef PI
#ifdef M_PI
#define PI M_PI
#else
#define PI 3.1415926535
#endif
#endif

/*** yukky ansi bodge ***/
#ifndef FLOATARG
#ifdef __STDC__
#define FLOATARG double /* some prototype checkers get funny about promotion */
#else
#define FLOATARG double
#endif /* def __STDC__ */
#endif /* def FLOATARG */

typedef struct {
    float re, im;
    } complex;

typedef struct lnode
    {
    struct lnode *next;
    int		 size;
    complex	 *table;
    } LNODE;

#ifdef __STDC__

int scancomplexdata(complex *);
void putcomplexdata(complex *, int);
void ShowCpx(complex *, int, char *);
int PureReal(complex *, int);
int IsPowerOfTwo(int);
complex *FindTable(int);	/* search our list of existing LUT's */
complex *AssignBasis(complex *, int);
void reverseDig(complex *, int, int);
void FFT2dimensional(complex *, int, int, complex *);
void FFT2torl(complex *, int, int, FLOATARG, complex *);
void ConjScale(complex *, int, FLOATARG);
void FFT2real(complex *, int, int, complex *);
void Reals(complex *, int, int, int, complex *);
void FFT2(complex *, int, int, complex *);
void FFT2raw(complex *, int, int, int, complex *);
void FFTarb(complex *, complex *, int, complex *);
void DFT(complex *, complex *, int, complex *);

#else

int scancomplexdata();
void putcomplexdata();
void ShowCpx();
int PureReal();
int IsPowerOfTwo();
complex *FindTable();	/* search our list of existing LUT's */
complex *AssignBasis();
void reverseDig();
void FFT2dimensional();
void FFT2torl();
void ConjScale();
void FFT2real();
void Reals();
void FFT2();
void FFT2raw();
void FFTarb();
void DFT();

#endif
