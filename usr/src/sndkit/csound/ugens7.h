/*      					UGENS7.H    */

#define     PVFRAMSIZE	    1024		/* i.e. max FFT point size */
#define     PVFFTSIZE	    (2*PVFRAMSIZE)	/* 2x for real + imag */
#define     PVDATASIZE	    (1+PVFRAMSIZE/2)	/* Need 1/2 channels + mid */
#define     PVFRDASIZE	    (2*PVDATASIZE)	/* as above, but Re + Im */

/* PVDATASIZE reflects the fact that for n point _real_ time data, the fourier
 *  transform will only have n degrees of freedom, although it has 2n values
 *  (n bins x {re,im} or {mag,phase}). This constraint is reflected by the top
 *  n/2-1 bins (bins n/2+1 .. n-1) being complex conjugates of bins 1..n/2-1.
 *  Bins 0 and n/2 do not have conjugate images, but they ARE always real,
 *  so only contribute one degree of freedom each.  So the number of degrees of
 *  freedom in the complex FFT is {re,im}*(n/2 - 1) +2 = n , as expected.
 *  Thus we only need to store and process these independent values.  However,
 *  for convenience, and because our analysis system records the phase of
 *  bins 0 and n/2 as 0 or pi rather than making the magnitude negative, we
 *  allow these 2 bins to have imaginary components too, so that FFT frames are
 *  stored as Magnitude & phase for bins 0..n/2 = 2*(n/2 + 1) or n+2 values.
 *  These are the n+2 channels interleaved in the PVOC analysis file, and
 *  then stored and processed wherever you see PVDATA/FRDA (frame data) */

#define     pvfrsiz(p)	    (p->frSiz)
#define     pvffsiz(p)	    (2* p->frSiz)
#define     pvdasiz(p)	    (1 + (p->frSiz)/2)	/* as above, based on	*/
#define     pvfdsiz(p)	    (2 + p->frSiz)	/*  ACTUAL frameSize in use */

typedef struct {
    OPDS    h;
    float   *rslt, *ktimpnt, *kfmod, *ispecwp, *ifilno;
    MEMFIL	*mfp;
    long    kcnt;
    long    baseFr, maxFr, frSiz, prFlg, opBpos;
    /* base Frame (in frameData0) and maximum frame on file, ptr to fr, size */
    float   frPktim, frPrtim, scale, asr, *frPtr, lastPex, *plut;
    /* asr is analysis sample rate */
    /* fft frames per k-time (equals phase change expansion factor) */

    float   lastPhase[PVDATASIZE];	/* Keep track of cum. phase */
    float   fftBuf[PVFFTSIZE];		/* FFT works on Real & Imag */
    float   dsBuf[PVFFTSIZE];		/* Output of downsampling may be 2x */
    float   outBuf[PVFFTSIZE];		/* Output buffer over win length */
    float   window[PVDATASIZE];		/* Store 1/2 window */
    float   sncTab[SPDS*SPTS + 1];	/* sinx/x lookup for d/samp */
} PVOC;

