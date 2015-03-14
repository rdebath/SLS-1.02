/****************************************************************/
/*  dsputil.c							*/
/* DSP utility functions for Csound - dispfft and pvoc		*/
/* 20apr90 dpwe							*/
/****************************************************************/

#include <stdio.h>
#include <math.h>
#ifdef THINK_C
#include <stdlib.h>
#endif
/* #ifdef CSOUND
/* #include "cs.h"   /* for mmalloc, mcalloc prototypes */
/* #else */
#define mmalloc(a) malloc(a)
/* #endif */
#include <dsputil.h>

/* Do we do the whole buffer, or just indep vals? */
#define someof(s)	(1+s/2)
    /* (change to just (s) to work on whole buffer) */

void CopySamps(sce,dst,size)	/* just move samples */
    float	*sce;
    float	*dst;
    int 	size;
    {
    ++size;
    while(--size)
	*dst++ = *sce++;
    }

/* Allocate a cleared buffer */
float *MakeBuf(size)
    int     size;
    {
    float *res,*p;
    int	  i;
    
    res = (float *) mmalloc((long)size * sizeof(float));
    p = res;
    for(i=0; i<size; ++i)
    	*p++ = 0.0;
	return(res);
    }

/* Write window coefs into buffer, don't malloc */
void FillHalfWin(wBuf,size, max, hannq)
    float	*wBuf;	
    int		size;
    FLOATARG	max;
    int 	hannq;		/* 1 => hanning window else hamming */
    {
    float	a,b;
    int		i;

    if(hannq)
	{	a = .50;	b = .50;	}
    else
	{	a = .54;	b = .46;	}
		
    if (wBuf!= NULL)
	{			/* NB: size/2 + 1 long - just indep terms */
	size /= 2; 		/* to fix scaling */
	for(i=0; i<=size;++i)
	    wBuf[i] = max * (a-b*cos(PI*(float)i/(float)size ) );
    	}
    return;
    }
    
/* Make 1st half of hamming window for those as desire */
float *MakeHalfWin(size, max, hannq)
    int		size;		/* Effective window size (altho only s/2+1 alloc'd */
    FLOATARG	max;
    int 	hannq;		/* 1 => hanning window else hamming */
    {
    float 	*wBuf;
    float	a,b;
    int		i;

    wBuf = (float *) mmalloc((long)(size/2+1) * sizeof(float));
				/* NB: size/2 + 1 long - just indep terms */
    FillHalfWin(wBuf,size,max,hannq);
    return(wBuf);
    }
    
void UnpackReals(dst,size)		/* expand out size to re,0,re,0 etc */
    float	*dst;
    int 	size;
    {
    float	*d2;

    d2 = dst + 2*size - 1;	/* start at ends .. */
    dst += size - 1;
    ++size;
    while(--size)		/* .. & copy backwards */
	{
	*d2-- = 0.0;
	*d2-- = *dst--;
	}
    }

void PackReals(buffer,size)	/* pack re,im,re,im into re,re */
    float *buffer;
    int size;
    {
    float	*b2 = buffer;

    ++size;
    while(--size)
	{
	*b2++ = *buffer++;
	++buffer;
	}
    }

/* Convert Real & Imaginary spectra into Amplitude & Phase */
void Rect2Polar(buffer,size)
    float *buffer;
    int size;
    {
    int 	i;
    float	*real,*imag;
    float	re,im;
    float	mag, pha;

    real = buffer;	imag = buffer+1;
    for(i = 0; i< someof(size); ++i)
	{
	re = real[2*i];
	im = imag[2*i];
	real[2*i] = mag = sqrt(re*re+im*im);
	imag[2*i] = (mag==0.0)?0.0:atan2(im,re);
	} 
    }


void Lin2DB(buffer, size)	/* packed buffer i.e. reals, not complex */
    float *buffer;
    int size;
    {
    ++size;
    while(--size)
	*buffer++ = /* 20.0*log10 */ 8.68589*log(*buffer);
    }

void DB2Lin(buffer, size)	/* packed buffer i.e. reals, not complex */
    float *buffer;
    int size;
    {
    ++size;
    while(--size)
	*buffer++ = exp( /* 1/20.0*log10 */ .1151292*(*buffer) );
    }

void Polar2Rect(buffer,size)
    float *buffer;
    int size;	    /* reverse above */
    {
    int		    i;
    float   *magn,*phas;
    float   mag, pha;

    magn = buffer;  phas = buffer+1;
    for(i = 0; i< someof(size); ++i)
	{
	mag = magn[2*i];
	pha = phas[2*i];
	magn[2*i] = mag*cos(pha);
	phas[2*i] = mag*sin(pha);
	}
    }

float maskPhs(phs)	/* do it inline instead! */
    FLOATARG   phs;
    {
    while(phs > PI)
	{
/*	putchar('-'); fflush(stdout);	*/
	phs -= 2*PI;
	}
    while(phs < -PI)
	{
/*	putchar('+'); fflush(stdout);	*/
	phs += 2*PI;
	}
    return(phs);
    }

void UnwrapPhase(buf,size,oldPh)
    float *buf;
    int size;
    float *oldPh;
    {
    int     i;
    float   *pha;
    float   p;

    pha = buf + 1;
    for(i=0; i<someof(size); ++i)
	{
	p = pha[2*i];
	p -= oldPh[i];		/* find change since last frame */
	p = maskPhs(p);
	oldPh[i] = pha[2*i];	/* hold actual phase for next diffce */
	pha[2*i] = p;		/* .. but write back phase change */
	}
    }

void RewrapPhase(buf,size,oldPh)
    float   *buf;
    int		    size;
    float   *oldPh;
    {
    int		    i;
    float   *pha;
    register float  p,q,r;

    /* Phase angle was properly scaled when it came out of frqspace */
    /* .. so just add it */
    pha = buf + 1;
    q = PI; r = 2*q;
    for(i=0; i<someof(size); ++i)
	{
	p = (pha[2*i]+oldPh[i]);
	if(p > q)	p -= r;
	else if(p < -q) p += r;
	oldPh[i] = pha[2*i] = p;
	}
    }

/* Convert a batch of phase differences to actual target freqs */
void PhaseToFrq(buf,size,incr,sampRate)
    float   *buf;
    int     size;
    FLOATARG   incr;
    FLOATARG   sampRate;
    {
    int     i;
    float   *pha;
    float   srOn2pi, binMidFrq, frqPerBin;
    float   expectedDphas,eDphIncr;

    pha = buf + 1;
    srOn2pi = sampRate/(2*PI*incr);
    frqPerBin = sampRate/((float)size);
    binMidFrq = 0;
    /* Of course, you get some phase shift with spot-on frq coz time shift */
    expectedDphas = 0;
    eDphIncr = 2.0*PI*incr/((float)size);
    for(i=0; i<someof(size); ++i)
	{
	pha[2*i] = maskPhs(pha[2*i]-expectedDphas);
	pha[2*i] *= srOn2pi;
	pha[2*i] += binMidFrq;

	expectedDphas += eDphIncr;
	binMidFrq += frqPerBin;
	}
    /* Doesn't deal with 'phases' of DC & fs/2 any different */
    }

/* Undo a pile of frequencies back into phase differences */
void FrqToPhase(buf,size,incr,sampRate,fixUp)
    float   *buf;
    int		    size;
    FLOATARG   incr;
    FLOATARG   sampRate;
    FLOATARG   fixUp;	    /* the fixup phase shift ... ? */
    {
    float   *pha;
    float   twoPiOnSr, binMidFrq, frqPerBin;
    float   expectedDphas,eDphIncr;
    register float  p,q;
    register int    j,i;

    pha = buf + 1;
    q = PI;
    twoPiOnSr = 2.*((float)incr)/sampRate;  /* was *pi too */
    frqPerBin = sampRate/((float)size);
    binMidFrq = 0;
    /* Of course, you get some phase shift with spot-on frq coz time shift */
    expectedDphas = 0;
    eDphIncr = 2.*((incr)/((float)size) + fixUp);   /* was *pi too */
    for(i=0; i<someof(size); ++i)
	{
	p = pha[2*i];
	p -= binMidFrq;
	p *= twoPiOnSr;
	p += expectedDphas;
/*	j = (int)p;
 *	j = (j + (j>0)? 1:-1)&-2;   /* j = (-3<p<-1)-2 (-1<p<1)0  (1<p<3)2 */
	j = (((int)p)+1)&-2;

/* slow but it does the right thing !! */
/*	if(p>0)
	    j = 2*((int)((p+1)/2));
	else
	    j = -2*((int)((1-p)/2)); */
	if(j)	p -= (float)j;

	pha[2*i] = q*p;
	expectedDphas += eDphIncr;
	if(expectedDphas > 2.)	expectedDphas -= 2.;
	else if(expectedDphas < 2) expectedDphas += 2.;
	binMidFrq += frqPerBin;
	}
	/* Doesn't deal with 'phases' of DC & fs/2 any different */
    }

/* Unpack stored mag/pha data into buffer */
void FetchIn(inp, buf, fsize, pos)
    float   *inp;	/* pointer to input data */
    float   *buf;	/* where to put our nice mag/pha pairs */
    int     fsize;	/* frame size we're working with */
    FLOATARG   pos;	/* fractional frame we want */
    {
    int     j;
    float   *frm0,*frm1;
    long    base;
    float   frac;

    /***** WITHOUT INFO ON WHERE LAST FRAME IS, MAY 'INTERP' BEYOND IT ****/
    base = (long)pos;		    /* index of basis frame of interpolation */
    frac = ((float)(pos - (float)base));
    /* & how close to get to next */
    frm0 = inp + ((long)fsize+2L)*base;
    frm1 = frm0 + ((long)fsize+2L);	    /* addresses of both frames */
	if(frac != 0.0)		/* I must have two cases to avoid poss seg violations */
		{			/* and failed computes, else may interp beyond valid data */
	    for(j=0; j<(fsize/2 + 1); ++j)  /* i.e. mag/pha for just over 1/2 */
			{		/* Interpolate both magnitude and phase */
			buf[2*j] = frm0[2*j] + frac*(frm1[2*j]-frm0[2*j]);
			buf[2*j+1] = frm0[2*j+1] + frac*(frm1[2*j+1]-frm0[2*j+1]);
			}
		}
	else
		{	/* frac is 0.0 i.e. just copy the source frame */
	    for(j=0; j<(fsize/2 + 1); ++j)
			{		/* no need to interpolate */
			buf[2*j] = frm0[2*j];
			buf[2*j+1] = frm0[2*j+1];
			}
		}
    if(someof(fsize) == fsize)	    /* i.e. if dealing whole frames.. */
		{
		for(j=(fsize/2 + 1); j<fsize; ++j)	/* .. rest is mirrored .. */
		    {
		    buf[2*j] = buf[2*(fsize-j)];
		    buf[2*j+1] = -buf[2*(fsize-j)+1];	/* ..with imag part conj */
		    }
		}
    }

/* Fill out the dependent 2nd half of iFFT data; scale down & opt conjgt */
void FillFFTnConj(buf,size,scale,conj)
    float   *buf;
    int     size;		/* full length of FFT ie 2^n */
    FLOATARG   scale;		/* can apply a scale factor.. */
    int     conj;		/* flag to conjugate at same time */
    {
    float   miscale;		/* scaling for poss. conj part */
    float   *mag,*pha;
    int     j;
    int     hasiz = 1 + size/2; /* the indep values */

    if(scale == 0.0)	scale = 1.0;
    if(conj)
	miscale = -scale;
    else
	miscale = scale;
    mag = buf;	    pha = buf+1;
    for(j=0; j<hasiz; ++j)	    /* i.e. mag/pha for just over 1/2 */
	{
	mag[2*j] *= scale;
	pha[2*j] *= miscale;
	}
    for(j=hasiz; j<size; ++j)		/* .. the rest is mirrored .. */
	{
	mag[2*j] = mag[2*(size-j)];	/* For the symmetry extension, */
	pha[2*j] = -pha[2*(size-j)];	/*  conjugate of 1st 1/2 rgdls */
	}
    }

void ApplyHalfWin(buf,win,len)	/* Window only store 1st half, is symmetric */
    float   *buf;
    float   *win;
    int     len;
    {
    int     j, lenOn2 = (len/2);

    for(j=0; j<len; ++j)
	buf[j] *= win[lenOn2 - abs(lenOn2 -j)];
    }	

/* Overlap (some of) new data window with stored previous data in circular buffer */
void addToCircBuf(sce, dst, dstStart, numToDo, circBufSize)
    float   *sce, *dst;     /* linear source and circular destination */
    int     dstStart;	    /* Current starting point index in circular dst */
    int     numToDo;	    /* How many points to add ( <= circBufSize ) */
    int     circBufSize;    /* Size of circ buf i.e. dst[0..circBufSize-1] */
    {
    int     i;
    int     breakPoint;     /* how many points to add before having to wrap */

    breakPoint = circBufSize-dstStart;	/* i.e. if we start at (dIndx = lim -2) */
    if(numToDo > breakPoint)		/*   we will do 2 in 1st loop, rest in 2nd. */
	{
	for(i=0; i<breakPoint; ++i)
	    dst[dstStart+i] += sce[i];
	dstStart -= circBufSize;
	for(i=breakPoint; i<numToDo; ++i)
	    dst[dstStart+i] += sce[i];
	}
    else				/* All fits without wraparound */
	for(i=0; i<numToDo; ++i)
	    dst[dstStart+i] += sce[i];
    return;
    }

/* Write from a circular buffer into a linear output buffer CLEARING DATA */
void writeClrFromCircBuf(sce, dst, sceStart, numToDo, circBufSize)
    float   *sce, *dst;     /* Circular source and linear destination */
    int     sceStart;	    /* Current starting point index in circular sce */
    int     numToDo;	    /* How many points to write ( <= circBufSize ) */
    int     circBufSize;    /* Size of circ buf i.e. sce[0..circBufSize-1] */
    {
    int     i;
    int     breakPoint;     /* how many points to add before having to wrap */

    breakPoint = circBufSize-sceStart;	/* i.e. if we start at (Indx = lim -2)	*/
    if(numToDo > breakPoint)		/*  we will do 2 in 1st loop, rest in 2nd. */
	{
	for(i=0; i<breakPoint; ++i)
	    {
	    dst[i] = sce[sceStart+i];
	    sce[sceStart+i] = 0.0;
	    }
	sceStart -= circBufSize;
	for(i=breakPoint; i<numToDo; ++i)
	    {
	    dst[i] = sce[sceStart+i];
	    sce[sceStart+i] = 0.0;
	    }
	}
    else				/* All fits without wraparound */
	for(i=0; i<numToDo; ++i)
	    {
	    dst[i] = sce[sceStart+i];
	    sce[sceStart+i] = 0.0;
	    }
    return;
    }

/* Add source array to dest array, results in dest */
void	FixAndAdd(samplSce, shortDest, size)
    float   *samplSce;
    short   *shortDest;
    int     size;
    {
    int i;
    for (i = 0; i < size; i++)
	shortDest[i] += (short)samplSce[i];
    }

/* Rules to convert between samples and frames, given frSiz & frIncr */
long NumFrames(dataSmps,frSiz,frInc)
    long    dataSmps;
    int     frSiz;
    int     frInc;
    {
    return( 1L + (dataSmps - (long)frSiz)/(long)frInc );
    }

long NumSampls(frames, frSiz, frIncr)
    long    frames;
    int     frSiz;
    int     frIncr;
    {
    return(((long)frSiz)+((long)frIncr)*(frames-1L));
    }

/********************************************************************/
/*  udsample.c	    -	    from dsampip.c			    */
/*  Performs sample rate conversion by interpolated FIR LPF approx  */
/*  VAX, CSOUND VERSION						    */
/*  1700 07feb90 taken from rational-only version		    */
/*  1620 06dec89 started dpwe					    */
/********************************************************************/

/* (ugens7.h) #define	SPDS (8)    /* How many sinc lobes to go out */
/* Static function prototypes - moved to top of file */
static float *sncTab = NULL;	/* point to our sin(x)/x lookup table */

void UDSample(inSnd, stindex, outSnd, inLen, outLen, fex)
    float   *inSnd;
    FLOATARG   stindex;
    float   *outSnd;
    int     inLen;
    int     outLen;
    FLOATARG   fex;
/*  Perform the sample rate conversion:
    inSnd   is the existing sample to be converted
    outSnd  is a pointer to the (pre-allocated) new soundspace
    inLen   is the number of points in the input sequence
    outLen  is the number of output sample points.  e.g inLen/fex
    fex is the factor by which frequencies are increased
    1/fex = lex, the factor by which the output will be longer
    i.e. if the input sample is at 12kHz and we want to produce an
    8kHz sample, we will want it to be 8/12ths as many samples, so
    lex will be 0.75
 */
    {
    int     in2out;
    int     i,j,x;
    float   a;
    float   phasePerInStep, fracInStep;
    float   realInStep, stepInStep;
    int     nrstInStep;
    float   posPhase, negPhase;
    float   lex = 1.0/fex;
    int     nrst;
    float   frac;

    phasePerInStep = ((lex>1)? 1.0 : lex)* (float)SPTS;
    /* If we are upsampling, LPF is at input frq => sinc pd matches */
    /*	downsamp => lpf at output rate; input steps at some fraction */
    in2out = (int)( ((float)SPDS) * ( (fex<1)? 1.0 : fex ) );
    /* number of input points contributing to each op: depends on LPF */
    realInStep = stindex;	    stepInStep = fex;
    for(i = 0; i<outLen; ++i)		    /* output sample loop */
	{				    /* i = lex*nrstIp, so .. */
	nrstInStep = (int)realInStep;		    /* imm. prec actual sample */
	fracInStep = realInStep-(float)nrstInStep;  /* Fractional part */
	negPhase = phasePerInStep * fracInStep;
	posPhase = -negPhase;
	/* cum. sinc arguments for +ve & -ve going spans into input */
	nrst = (int)negPhase;	frac = negPhase - (float)nrst;
	a = (sncTab[nrst]+frac*(sncTab[nrst+1]-sncTab[nrst]))*(float)inSnd[nrstInStep];
	for(j=1; j<in2out; ++j)		    /* inner FIR convolution loop */
	    {
	    posPhase += phasePerInStep;
	    negPhase += phasePerInStep;
	    if( (x = nrstInStep-j)>=0 )
	    nrst = (int)negPhase;   frac = negPhase - (float)nrst;
	    a += (sncTab[nrst]+frac*(sncTab[nrst+1]-sncTab[nrst]))
		    * (float)inSnd[x];
	    if( (x = nrstInStep+j)<inLen )
	    nrst = (int)posPhase;   frac = posPhase - (float)nrst;
	    a += (sncTab[nrst]+frac*(sncTab[nrst+1]-sncTab[nrst]))
		    * (float)inSnd[x];
	    }
	outSnd[i] = (float)a;
	realInStep += stepInStep;
	}
    }

void    FloatAndCopy(sce,dst,size)
    short  *sce;
    float  *dst;
    int    size;
    {
    while(size--)
	*dst++ = (float)*sce++;
    }

/* Copy converted frame to the output data */
void	WriteOut(sce,pdst,fsize)
    float *sce;
    float **pdst;
    int   fsize;	/* the frame size - but we may not copy them all! */
    {
    int     j;

    for(j=0; j<(2*(fsize/2 + 1)); ++j)	    /* i.e. mg/ph for just over 1/2 */
	*(*pdst)++ = sce[j];		    /* pointer updated for next time */
    }	

/*--------------------------------------------------------------------*/
/*---------------------------- sinc module ---------------------------*/
/*--------------------------------------------------------------------*/

/* (ugens7.h) #define SPTS (16) /* How many points in each lobe */

void MakeSinc()		/* initialise our static sinc table */
    {
    int     i;
    int     stLen = SPDS*SPTS;	/* sinc table is SPDS/2 periods of sinc */
    float   theta   = 0.0;	/* theta (sinc arg) reaches pi in SPTS */
    float   dtheta  = SBW*PI/(float)SPTS;   /* SBW lowers cutoff to redc ali */
    float   phi     = 0.0;	/* phi (hamm arg) reaches pi at max ext */
    float   dphi    = PI/(float)(SPDS*SPTS);


    if(sncTab == NULL)
	sncTab = (float *)mmalloc((long)(stLen+1) * sizeof(float));
    /* (stLen+1 to include final zero; better for interpolation etc) */
/*    printf("Make sinc : pts = %d, table = %lx \n",stLen,sncTab);   */
    sncTab[0] =  1.0;
    for(i=1; i<=stLen; ++i) /* build table of sin x / x */
	{
	theta += dtheta;
	phi   += dphi;
	sncTab[i] = sin(theta)/theta * (.54 + .46*cos(phi));
	/* hamming window on top of sinc */
	}
    }

void DestroySinc()	/* relase the lookup table */
    {
    free(sncTab);
    }

float SincIntp(index)
    FLOATARG index;
/* Calculate the sinc of the 'index' value by interpolating the table */
/* <index> is scaled s.t. 1.0 is first zero crossing */
/* ! No checks ! */
    {
    int     nrst;
    float   frac,scaledUp;

    scaledUp = index * SPTS;
    nrst = (int)scaledUp;
    frac = scaledUp - (float)nrst;
    return(sncTab[nrst] + frac*(sncTab[nrst+1]-sncTab[nrst]) );
    }	

/****************************************/
/** prewarp.c module			*/
/****************************************/

/* spectral envelope detection: this is a very crude peak picking algorithm
	which is used to detect and pre-warp the spectral envelope so that
	pitch transposition can be performed without altering timbre.
	The basic idea is to disallow large negative slopes between
	successive values of magnitude vs. frequency. */

#ifndef NULL
#define NULL 0x0L
#endif


static	float	*env = (float *)NULL;	/* Scratch buffer to hold 'envelope' */

void PreWarpSpec(spec, size, warpFactor)
    float   *spec;	/* spectrum as magnitude,phase */
    int     size;	/* full frame size, tho' we only use n/2+1 */
    FLOATARG   warpFactor; /* How much pitches are being multd by */
    {
    float   eps,slope;
    float   mag, lastmag, nextmag, pkOld;
    int     pkcnt, i, j;

    if(env==(float *)NULL)
	env = (float *)mmalloc((long)size * sizeof(float));
    /*!! Better hope <size> in first call is as big as it gets !! */
    eps = -64. / size;		    /* for spectral envelope estimation */
    lastmag = *spec;
    mag = spec[2*1];
    pkOld = lastmag;
    *env = pkOld;
    pkcnt = 1;

    for (i = 1; i < someof(size); i++)	/* step thru spectrum */
	{
	if (i < someof(size)-1)
	    nextmag = spec[2*(i+1)];
	else nextmag = 0.;

	if (pkOld != 0.)
	    slope =
		((float) (mag - pkOld)/(pkOld * pkcnt));
	else
	    slope = -10.;

	/* look for peaks */
	if ((mag>=lastmag)&&(mag>nextmag)&&(slope>eps))
	    {
	    env[i] = mag;
	    pkcnt--;
	    for (j = 1; j <= pkcnt; j++)
		{
		env[i - pkcnt + j - 1]
		    = pkOld * (1. + slope * j);
		}
	    pkOld = mag;
	    pkcnt = 1;
	    }
	else
	    pkcnt++;		    /* not a peak */

	lastmag = mag;
	mag = nextmag;
	}

    if (pkcnt > 1)		    /* get final peak */
	{
	mag = spec[2*(size/2)];
	slope = ((float) (mag - pkOld) / pkcnt);
	env[size/2] = mag;
	pkcnt--;
	for (j = 1; j <= pkcnt; j++)
	    {
	    env[size/2 - pkcnt + j - 1] = pkOld + slope * j;
	    }
	}

    for (i = 0; i < someof(size); i++)	/* warp spectral env.*/
	{
	j = ((float) i * warpFactor);
	mag = spec[2*i];
	if ((j < someof(size)) && (env[i] != 0.))
	    spec[2*i] *= env[j]/env[i];
	else
	    spec[2*i] = 0.;
/*	printf("I<%d>J<%d>S<%.0f>E<%.0f>F<%.0f>T<%0.f>",
	    i, j, mag, env[i], env[j], spec[2*i]);  */
	}
    }


