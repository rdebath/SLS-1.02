#include "cs.h"		/*			UGENS7.C	*/
#include <math.h>
#include <dsputil.h>
#include <fft.h>
#include <pvoc.h>
#include "ugens7.h"
#include "soundio.h"

#define WLN   1		/* time window is WLN*2*ksmps long */

/*  #define OPWLEN	size	*/
#define OPWLEN (2*WLN*ksmps)	/* manifest used for final time wdw */

extern	float	esr, ekr, sicvt, kicvt, pi, pid100, dv32768;
extern	int	ksmps;
extern	char	errmsg[];
extern  int     odebug;

static	float	fzero = 0, fone = 1;
static	int	pdebug = 0;
static	int	dchan = 6;	/* which channel to examine on debug */

pvset(p)
    register PVOC *p;
    {
    int      i;
    char     pvfilnam[16], *cp;
    float    *pvp;
    MEMFIL   *mfp, *ldmemfile();
    PVSTRUCT *pvh;
    int     frInc, chans, size;	/* THESE SHOULD BE SAVED IN PVOC STRUCT */

    sprintf(pvfilnam,"pvoc.%d", (int)*p->ifilno); /* build fname */
    if ((mfp = p->mfp) == NULL
      || strcmp(mfp->filename, pvfilnam) != 0) /* if file not already readin */
	if ( (mfp = ldmemfile(pvfilnam)) == NULL)
	    dies("cannot open %s",pvfilnam);
    pvh = (PVSTRUCT *)mfp->beginp;
    if (pvh->magic != PVMAGIC) {
	sprintf(errmsg,"%s not a PVOC file (magic %ld)", 
		pvfilnam, pvh->magic );
	goto pverr;
	}
    p->frSiz = pvh->frameSize;
    frInc    = pvh->frameIncr;
    chans    = pvh->channels;
    if ((p->asr = pvh->samplingRate) != esr) { /* & chk the data */
	sprintf(errmsg,"%s''s srate = %8.0f, orch's srate = %8.0f",
		pvfilnam, p->asr, esr);
	warning(errmsg);
	}
    if (pvh->format != PVFLOAT) {
	sprintf(errmsg,"unsupported PVOC data format %d in %s",
		pvh->format, pvfilnam);
	goto pverr;
	}
    if (p->frSiz > PVFRAMSIZE) {
	sprintf(errmsg,"PVOC frame %d bigger than %d in %s",
		p->frSiz, PVFRAMSIZE, pvfilnam);
	goto pverr;
	}
    if (p->frSiz < PVFRAMSIZE/8) {
	sprintf(errmsg,"PVOC frame %d seems too small in %s",
		p->frSiz, pvfilnam);
	goto pverr;
	}
    if (chans != 1) {
	sprintf(errmsg,"%d chans (not 1) in PVOC file %s",
	        chans, pvfilnam);
	goto pverr;
	}
    /* Check that pv->frSiz is a power of two too ? */
    p->frPtr = (float *) ((char *)pvh+pvh->headBsize);
    p->baseFr = 0;  /* point to first data frame */
    p->maxFr = -1 + ( pvh->dataBsize / (chans * (p->frSiz+2) * sizeof(float) ) );
    /* highest possible frame index */
    p->frPktim = ((float)ksmps)/((float)frInc);
    /* factor by which to mult expand phase diffs (ratio of samp spacings) */
    p->frPrtim = esr/((float)frInc);
    /* factor by which to mulitply 'real' time index to get frame index */
    size = pvfrsiz(p);		/* size used in def of OPWLEN ? */
/*  p->scale = 4.*((float)ksmps)/((float)pvfrsiz(p)*(float)pvfrsiz(p)); */
    p->scale = 2.*((float)ksmps)/((float)OPWLEN*(float)pvfrsiz(p));
    /* 2*incr/OPWLEN scales down for win ovlp, windo'd 1ce (but 2ce?) */
    /* 1/frSiz is the required scale down before (i)FFT */
    p->prFlg = 1;    /* true */
    p->opBpos = 0;
    p->lastPex = 1.0;	    /* needs to know last pitchexp to update phase */
    /* Set up time window */
    for (i=0; i < pvdasiz(p); ++i)   /* or maybe pvdasiz(p) */
	{
     /* p->window[i] = (0.54-0.46*cos(2.0*pi*(float)i/(float)(pvfrsiz(p)))); */
	p->lastPhase[i] = 0.;
	}
    for (i=0; i < OPWLEN/2+1; ++i)    /* time window is OPWLEN long */
        p->window[i] = (0.54-0.46*cos(2*pi*(float)i/(float)OPWLEN));
    /* NB : HAMMING */
    for(i=0; i< pvfrsiz(p); ++i)
        p->outBuf[i] = 0.0;
    MakeSinc( /* p->sncTab */ );  	/* sinctab is same for all instances */
    p->plut = (float *)AssignBasis(NULL, pvfrsiz(p));    /* SET UP NONET FFT */

    return;

pverr:	initerror(errmsg);
    }

pvoc(p)
    register PVOC *p;
    {
	     int    n, nsmps = ksmps;
    register float  *samp;
    register float  *ar = p->rslt;
	     float  frIndx;
	     float  *buf = p->fftBuf;
	     float  *buf2 = p->dsBuf;
	     float  *plut = p->plut;
	     int    size = pvfrsiz(p);
	     int    buf2Size, outlen;
	     int    circBufSize = PVFFTSIZE;
	     int    specwp = (int)*p->ispecwp;   /* spectral warping flag */
	     float  pex;

    if (pdebug) { printf("<%7.4f>",*p->ktimpnt); fflush(stdout); }
    pex = *p->kfmod;
    outlen = (int)(((float)size)/pex);
    /* use outlen to check window/krate/transpose combinations */
    if (outlen>PVFFTSIZE)  /* Maximum transposition down is one octave */
	{		    /* ..so we won't run into buf2Size problems */
	perferror("PVOC transpose too low");
	return;
	}
    if (outlen<2*ksmps)    /* minimum post-squeeze windowlength */
	{
	perferror("PVOC transpose too high");
	return;
	}
    buf2Size = OPWLEN;     /* always window to same length after DS */
    if ((frIndx = *p->ktimpnt * p->frPrtim) < 0)
	{
	perferror("PVOC timpnt < 0");
	return;
	}
    if (frIndx > p->maxFr)  /* not past last one */
	{
	frIndx = p->maxFr;
	if (p->prFlg)
	    {
	    p->prFlg = 0;   /* false */
	    warning("PVOC ktimpnt truncated to last frame");
	    }
	}
    FetchIn(p->frPtr,buf,size,frIndx);
/*    if(frIndx >= p->maxFr)
    	printf("Fetched %8.1f %6.1f %8.1f %6.1f %8.1f %6.1f %8.1f %6.1f\n",
    			buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);  /* */
    FrqToPhase(buf, size, pex*(float)ksmps, p->asr,
	   /*a0.0*/(float)(.5 * ( (pex / p->lastPex) - 1) ));
    /* Offset the phase to align centres of stretched windows, not starts */
    RewrapPhase(buf,size,p->lastPhase);
/**/    if( specwp == 0 || (p->prFlg)++ == -(int)specwp) /* ?screws up when prFlg used */
        { /* specwp=0 => normal; specwp = -n => just nth frame */
	if(specwp<0) printf("PVOC debug : one frame gets through \n");	/*	*/
    if (specwp>0)
        PreWarpSpec(buf, size, pex);    /*	    */
    Polar2Rect(buf,size);
/*    if(frIndx >= p->maxFr)
    	printf("Rected %8.1f %6.1f %8.1f %6.1f %8.1f %6.1f %8.1f %6.1f\n",
    			buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]); /* */
    buf[1] = 0; buf[size+1] = 0;	/* kill spurious imag at dc & fs/2 */
    FFT2torl((complex *)buf,size,1,/*a pex*/ p->scale, (complex *)plut);
    /* CALL TO NONET FFT */
    PackReals(buf, size);
/*    if(frIndx >= p->maxFr)
    	printf("IFFTed %8.1f %6.1f %8.1f %6.1f %8.1f %6.1f %8.1f %6.1f\n",
    			buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]); /* */
/*a    ApplyHalfWin(buf, p->window, size);	/* */
    if (pex != 1.0)
	UDSample(buf,(.5*((float)size - pex*(float)buf2Size))/*a*/,buf2, size, buf2Size, pex);
    else
	CopySamps(buf+(int)(.5*((float)size - pex*(float)buf2Size))/*a*/,buf2,buf2Size);
/*a*/    if(specwp>=0) ApplyHalfWin(buf2, p->window, buf2Size);	/* */
/**/      }
      else
        for(n = 0; n<buf2Size; ++n)
          buf2[n] = 0.;		/*	*/
    addToCircBuf(buf2, p->outBuf, p->opBpos, ksmps, circBufSize);
    writeClrFromCircBuf(p->outBuf, ar, p->opBpos, ksmps, circBufSize);
    p->opBpos += ksmps;
    if(p->opBpos > circBufSize)     p->opBpos -= circBufSize;
    addToCircBuf(buf2+ksmps,p->outBuf,p->opBpos,buf2Size-ksmps,circBufSize);
    p->lastPex = pex;	     /* needs to know last pitchexp to update phase */
    }


