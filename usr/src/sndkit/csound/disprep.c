#include "cs.h"		/*				       		DISPREP.C	*/
#include <math.h>
#include "window.h"
#include "disprep.h"
#include <fft.h>
#include <dsputil.h>

static	float	*fftcoefs = NULL;     /* malloc for fourier coefs, mag or db */
static  float   fzero = 0.0;
extern  long    kcounter;

printv(p)
 register PRINTV *p;
{
register int	nargs = p->INCOUNT;
register char	**txtp = p->ORTXT.inlist->arg;
register float	**valp = p->iargs;

	printf("instr %d:", p->h.insdshead->insno);
	do printf("  %s = %5.3f", *txtp++, **valp++);
	while (--nargs);
	printf("\n");
}

dspset(p)
 register DSPLAY *p;
{
register long	npts, size;
register char	*auxp;

	if (p->h.optext->t.pftype == 'k')
	    npts = *p->iprd * ekr;
	else npts = *p->iprd * esr;
	if (npts <= 0) {
	    initerror("illegal iprd");
	    return;
	}
	size = npts * sizeof(float);
	if ((auxp = p->auxch.auxp) == NULL || npts != p->npts) {
	    auxalloc(size,&p->auxch);
	    auxp = p->auxch.auxp;
	    p->npts = npts;
	}
	p->nxtp = (float *) auxp;
	sprintf(strmsg,"instr %d, signal %s:",
	    p->h.insdshead->insno,p->h.optext->t.inlist->arg[0]);
	dispset(&p->dwindow,(float *)auxp,npts,strmsg,(int)*p->iwtflg,"display");
}

kdsplay(p)
 DSPLAY *p;
{
register float	*fp = p->nxtp;

	*fp++ = *p->signal;
	if (fp >= (float *) p->auxch.endp) {
	    fp = (float *) p->auxch.auxp;
	    display(&p->dwindow);
	}
	p->nxtp = fp;
}

dsplay(p)
 DSPLAY *p;
{
register float	*fp = p->nxtp, *sp = p->signal, *endp = (float *) p->auxch.endp;
register int	nsmps = ksmps;

	do {
	    *fp++ = *sp++;
	    if (fp >= endp) {
		fp = (float *) p->auxch.auxp;
		display(&p->dwindow);
	    }
	} while (--nsmps);
	p->nxtp = fp;
}

fftset(p)               /* fftset, dspfft -- calc Fast Fourier Transform of collected	*/
 register DSPFFT *p;    /*      samples and displays coefficients (mag or db)		*/
{
 register int	window_size, step_size, hanning;

	window_size = *p->inpts;
	if (window_size > WINDMAX)  initerror("too many points requested");
	if (window_size < WINDMIN)  initerror("too few points requested");
	if (!IsPowerOfTwo(window_size))
	    initerror("window size must be power of two");
	if ((step_size = *p->iprd * esr) <= 0)
	    initerror("illegal iprd");
	if (inerrcnt)
	    return;
	hanning = *p->ihann;
	p->dbout   = *p->idbout;
        p->overlap = window_size - step_size;
        if (window_size != p->windsize
	    || hanning != p->hanning) {              /* if windowing has changed:  */
	  long auxsiz;
	  float *hWin;
	  p->windsize = window_size;                   /* set new parameter values */
	  p->hanning = hanning;
	  p->bufp    = p->sampbuf;
	  p->endp    = p->bufp + window_size;
	  p->overN    = 1./(*p->inpts);
	  p->ncoefs  = window_size >>1;
	  auxsiz = (window_size/2 + 1) * sizeof(float);    /* size for half window */
	  auxalloc((long)auxsiz, &p->auxch);               /*  alloc or realloc */
	  hWin = (float *) p->auxch.auxp;
	  FillHalfWin(hWin, window_size, 1.0, hanning); /* fill with proper values */
	  p->fftlut = (float *)AssignBasis(NULL,window_size);  /* fft lookup table */
	  if (fftcoefs == NULL)		   /* room for WINDMAX*2 floats (fft size) */
	    fftcoefs = (float *) mmalloc((long)WINDMAX * 2 * sizeof(float));
	  sprintf(strmsg,"instr %d, signal %s, fft (%s):",p->h.insdshead->insno,
		p->h.optext->t.inlist->arg[0],p->dbout ? "db" : "mag");
	  dispset(&p->dwindow,fftcoefs,(long)p->ncoefs,strmsg,(int)*p->iwtflg,"fft");
	}
}

static d_fft(sce, dst, size, lut, hWin, dbq)	/* perform an FFT as reqd below */
  float	*sce;	/* input array - pure packed real */
  float	*dst; 	/* output array - packed magnitude, only half-length */
  int 	size;	/* number of points in input */
  float	*lut;	/* look up table for FFT - already set up */
  float	*hWin;	/* hanning window lookup table */
  int 	dbq;	/* flag: 1-> convert output into db */
{
	CopySamps(sce,dst,size);	/* copy into scratch buffer */
	ApplyHalfWin(dst,hWin,size);
	UnpackReals(dst,size);		/* expand out size to re,0,re,0 etc */
	FFT2real((complex *)dst,size,1,(complex *)lut);	/* perform the FFT */
	Rect2Polar(dst, size);
	PackReals(dst, size);
	if(dbq)
	    Lin2DB(dst, size);
}

dspfft(p)
 register DSPFFT *p;
{
 register float *sigp = p->signal, *bufp = p->bufp, *endp = p->endp;
 register int   nsmps = ksmps;

	do {
	    if (bufp < p->sampbuf) {    	/* skip any spare samples */
		bufp++; sigp++;
	    }
	    else {				/* then start collecting  */
		*bufp++ = *sigp++;
		if (bufp >= endp) {     	/* when full, do fft:     */
		    register float *tp, *tplim;
		    float *hWin = (float *) p->auxch.auxp;
		    d_fft(p->sampbuf,fftcoefs,p->windsize,p->fftlut,hWin,p->dbout);
		    tp = fftcoefs;
		    tplim = tp + p->ncoefs;
		    do  *tp *= p->overN;		/* scale 1/N */
		    while (++tp < tplim);
		    display(&p->dwindow);               /* & display */
		    if (p->overlap > 0) {
			bufp = p->sampbuf;
			tp   = endp - p->overlap;
			do *bufp++ = *tp++;
			while(tp < endp);
		    }
		    else bufp = p->sampbuf + p->overlap;
		}
	    }
	}
	while (--nsmps);
	p->bufp = bufp;
}

static void DOWNset(downdp, npts)
 DOWNDAT *downdp;
 long    npts;
{
  register char *auxp;
  register long nbytes = npts * sizeof(float);

        if ((auxp = downdp->auxch.auxp) == NULL
	 || nbytes != downdp->auxch.size)
	    auxalloc(nbytes, &downdp->auxch);
        downdp->npts = npts;
}
	     
static void SPECset(specdp, npts)
 SPECDAT *specdp;
 long    npts;
{
  register char *auxp;
  register long nbytes = npts * sizeof(float);

        if ((auxp = specdp->auxch.auxp) == NULL
	 || nbytes != specdp->auxch.size)
	    auxalloc(nbytes, &specdp->auxch);
        specdp->npts = npts;
}
	     
octdwnset(p)
 OCTDOWN *p;
{
	int	n, nocts, nsamps;
	float   *fltp;
	DOWNDAT *dwnp = p->dsig;
	OCTDAT  *octp;

	if ((p->disprd = ekr * *p->idisprd) < 0)  p->disprd = 0;   /* pos prd = display */
	nocts = *p->iocts;
	nsamps = *p->isamps;
	if (nocts != dwnp->nocts || nsamps != dwnp->nsamps         /* if params changed */
	 || !nocts || !nsamps || p->disprd && !p->dwindow.windid) {
	    long	totsamps, totsize;
	    double	hicps,oct,onept=1.021975,logtwo=0.693147;  /*   must alloc anew */

	    if (nocts <= 0 || nocts > MAXOCTS)	initerror("illegal iocts");
	    if (nsamps <= 0)          	        initerror("illegal isamps");
	    if (inerrcnt) return;
	    hicps = esr * 0.375;                        /* top freq is 3/4 pi/2 ...   */
	    printf("octdown: high cps of %f\n",hicps);
	    oct = log(hicps / onept) / logtwo;          /* ... octcps()  (see aops.c) */
	    oct = ((int)(oct*12.0 + 0.5)) / 12.0;       /*     semitone round to A440 */
	    hicps = pow((double)2., oct) * onept;       /*     cpsoct()               */
	    printf("\t  retuned to %f\n",hicps);
	    dwnp->hifrq = hicps;
	    dwnp->lofrq = hicps / (1L << nocts);
	    dwnp->nsamps = nsamps;
	    dwnp->nocts = nocts;
	    totsamps = (long)nsamps * nocts;            /* calc totsamps reqd,        */
	    DOWNset(dwnp, totsamps);                    /* auxalloc in DOWNDAT struct */
	    fltp = (float *) dwnp->auxch.auxp;               /*  & distrib to octdata */
	    for (n=nocts,octp=dwnp->octdata+(nocts-1); n--; octp--) {
	        octp->begp = fltp;      fltp += nsamps;      /*        (lo oct first) */
		octp->endp = fltp;                     
	    }
	    if (p->disprd) {                                 /* if display requested, */
	        totsize = totsamps * sizeof(float);          /*  alloc an equiv local */
		auxalloc((long)totsize, &p->auxch);          /*  linear output window */
	        dispset(&p->dwindow, (float *)p->auxch.auxp, (long)totsamps,
			"octdown buffers:", 0, "octdown");
	    }
	}
	for (octp=dwnp->octdata; nocts--; octp++) {    /* reset all oct params, &     */
	    octp->curp = octp->begp;
	    for (fltp=octp->feedback,n=6; n--; )
	        *fltp++ = fzero;
	    octp->scount = 0;
	}
	p->countdown = p->disprd;                      /* prime the display countdown */
}

static void linocts(dwnp, bufp)        /* linearize octdown data into one buffer */
 DOWNDAT *dwnp;                        /* presumes correct buffer alloc'd in set */
 register float  *bufp;
{
register float  *curp, *endp;
register int    wrap;
register OCTDAT *octp;
	 int    nocts;
	 float  *begp;

	nocts = dwnp->nocts;
	octp = dwnp->octdata + nocts;
	while (nocts--) {
	    octp--;                            /* for each octave (low to high) */
	    begp = octp->begp;
	    curp = octp->curp;
	    endp = octp->endp;
	    wrap = curp - begp;
	    while (curp < endp)                     /*   copy circbuf to linbuf */
	        *bufp++ = *curp++;
	    for (curp=begp; wrap--; )
	        *bufp++ = *curp++;
	}
}

octdown(p)
 OCTDOWN *p;
{
        float   *sigp = p->signal, SIG, yt1, yt2;
	int     nocts, nsmps = ksmps;
	DOWNDAT *dwnp = p->dsig;
	OCTDAT  *octp;
static float bicoefs[] = { -0.2674054, 0.7491305, 0.7160484, 0.0496285, 0.7160484,
			    0.0505247, 0.3514850, 0.5257536, 0.3505025, 0.5257536,
			    0.3661840, 0.0837990, 0.3867783, 0.6764264, 0.3867783 };
	do {
	    SIG = *sigp++;                            /* for each source sample:     */
	    octp = dwnp->octdata;                     /*   align onto top octave     */
	    nocts = dwnp->nocts;
	    do {                                      /*   then for each oct:        */
	        register float *coefp,*ytp,*curp;
		register int   nfilt;
	        curp = octp->curp;
		*curp++ = SIG;                            /*  write samp to cur buf  */
		if (curp >= octp->endp)
		    curp = octp->begp;                    /*    & modulo the pointer */
		octp->curp = curp;
		if (!(--nocts))  break;                   /*  if lastoct, break      */
		coefp = bicoefs;  ytp = octp->feedback;
		for (nfilt = 3; nfilt--; ) {              /*  apply triple biquad:   */
		    yt2 = *ytp++; yt1 = *ytp--;                 /* get prev feedback */
		    SIG -= (*coefp++ * yt1 + *coefp++ * yt2);   /* apply recurs filt */
		    *ytp++ = yt1; *ytp++ = SIG;                 /* stor nxt feedback */
		    SIG *= *coefp++;
		    SIG += (*coefp++ * yt1 + *coefp++ * yt2);   /* apply forwrd filt */
		}
	    } while (!(++octp->scount & 01) && octp++);  /* send alt samps to nxtoct */
	} while (--nsmps);

	if (p->disprd)                                /* if displays requested,      */
	    if (!(--p->countdown)) {                  /*    on countdown             */
	        linocts(dwnp, p->auxch.auxp);         /*      linearize the oct bufs */
	        display(&p->dwindow);                 /*      & display              */
		p->countdown = p->disprd;
	    }
}

static char *getstrdbout(dbcode)
 int dbcode;
{
static char *outstring[] = {"mag", "db", "mag squared"};
        switch (dbcode) {
	case 0:
	case 1:
	case 2:  return(outstring[dbcode]);
	default: return("unknown dbcode");
	}
}

nocdfset(p)	/* noctdft - calcs disc Fourier transform of oct-downsampled data */
 NOCTDFT *p;	/* outputs coefs (mag, db or mag2) of log freq within each octave */
{
	int	nfreqs, hanning, nocts, ncoefs;
	float   Q, *fltp;
	OCTDAT  *octp;
	DOWNDAT *downp = p->dsig;
	SPECDAT *specp = p->wsig;

	p->timcount = ekr * *p->iprd;
	nfreqs = *p->ifrqs;
	Q = *p->iq;
	hanning = (*p->ihann) ? 1 : 0;
	if ((p->dbout = *p->idbout) && p->dbout != 1 && p->dbout != 2) {
	    sprintf(errmsg, "noctdft: unknown dbout code of %d", p->dbout);
	    initerror(errmsg);
	}
	nocts = downp->nocts;
	ncoefs = nocts * nfreqs;
	if (nfreqs != p->nfreqs || Q != p->curq             /* if anything changed */
	     || p->timcount <= 0 || Q <= 0.
	     || hanning != p->hanning
	     || ncoefs != p->ncoefs) {                      /*     make new tables */
	    double	basfrq, curfrq, frqmlt, Qfactor;
	    double	theta, a, windamp, onedws, pidws;
	    float	*sinp, *cosp;
	    int         n, k, sumk, windsiz, *wsizp, nsamps;
	    long	auxsiz;

	    fprintf(stderr,"noctdft: %s window, %s out, making tables ...\n",
		    (hanning) ? "hanning":"hamming", getstrdbout(p->dbout));
	    if (p->timcount <= 0)                initerror("illegal iprd");
	    if (nfreqs <= 0 || nfreqs > MAXFRQS) initerror("illegal ifrqs");
	    if (Q <= fzero)                      initerror("illegal Q value");
	    if (inerrcnt)     return;
	    nsamps = downp->nsamps;
	    p->nfreqs = nfreqs;
	    p->curq = Q;
	    p->hanning = hanning;
	    p->ncoefs = ncoefs;
	    basfrq = downp->hifrq / 2.0 * tpidsr;          /* octave below retuned top */
	    frqmlt = pow((double)2.0,(double)1./nfreqs);    /* nfreq interval mult */
	    Qfactor = 6.2831854 * Q;
	    curfrq = basfrq;
	    for (sumk=0,wsizp=p->winlen,n=nfreqs; n--; ) {
	        *wsizp++ = k = Qfactor/curfrq + 0.5;         /* calc window sizes  */
		sumk += k;                                   /*   and find total   */
	/*	printf("frq %f, k = %d\n",curfrq,k);  */
		curfrq *= frqmlt;
	    }
	    if ((windsiz = *(p->winlen)) > nsamps) {        /* chk longest windsiz */
	        sprintf(errmsg,"Q %4.1f needs %d samples, octdown has just %d",
			Q, windsiz, nsamps);
		initerror(errmsg);
		return;
	    }
	    else printf("noctdft: Q %4.1f uses %d of %d samps per octdown\n",
			Q, windsiz, nsamps);
	    auxsiz = (nsamps + 2*sumk) * sizeof(float);        /* calc local space reqd */
	    auxalloc((long)auxsiz, &p->auxch);                 /*     & alloc auxspace  */
	    fltp = (float *) p->auxch.auxp;
	    p->linbufp = fltp;  	fltp += nsamps;    /* linbuf must handle nsamps */
	    p->sinp = sinp = fltp;	fltp += sumk;
	    p->cosp = cosp = fltp;                         /* cos gets rem sumk  */
	    wsizp = p->winlen;
	    for (curfrq=basfrq,n=nfreqs; n--; ) {      	    /* now fill tables */
	        windsiz = *wsizp++;
		onedws = 1.0 / windsiz;
		pidws = 3.1415927 / windsiz;
		for (k=0; k<windsiz; k++) {     	    /*   with sines    */
		    a = sin(k * pidws);
		    windamp = a * a;            	    /*   times hanning */
		    if (!hanning)
		        windamp = 0.08 + 0.92 * windamp;    /*   or hamming    */
		    windamp *= onedws;                      /*   scaled        */
		    theta = k * curfrq;
		    *sinp++ = windamp * sin(theta);
		    *cosp++ = windamp * cos(theta);
		}
		curfrq *= frqmlt;                           /*   step by log freq  */
	    }
	    if (*p->idsines != 0.0) {    /* if reqd, display windowed sines immediately */
	        dispset(&p->dwindow,p->sinp,(long)sumk,"octdft windowed sines:",
			0,"octdft");
		display(&p->dwindow);
	    }
	    SPECset(specp, (long)ncoefs);                /* prep the spec dspace */
	    specp->downsrcp = downp;                     /*  & record its source */
	}
	specp->dbout = p->dbout;                   /* enter specdata dbout type */
	specp->ktimstamp = 0;                      /* init specdata to not new  */
	specp->ktimprd = p->timcount;
	p->countdown = p->timcount;                /*     & prime the countdown */
}

noctdft(p)
 NOCTDFT *p;
{
	DOWNDAT *downp;
	SPECDAT *specp;
        OCTDAT  *octp;
	float   *dftp;
        int     nocts, wrap;
	float   a, b;
	double  c;

	if ((--p->countdown))  return;         /* if not yet time for new spec, return */
	p->countdown = p->timcount;            /* else reset counter & proceed:        */
	downp = p->dsig;
	specp = p->wsig;
	nocts = downp->nocts;
	octp = downp->octdata + nocts;
	dftp = (float *) specp->auxch.auxp;
	while (nocts--) {
	    register float  *bufp, *sinp, *cosp;
	    register int    len, *lenp, nfreqs;
	    register float   *begp, *curp, *endp;
	    octp--;                              /* for each octave (low to high)   */
	    begp = octp->begp;
	    curp = octp->curp;
	    endp = octp->endp;
	    wrap = curp - begp;
	    bufp = p->linbufp;
	    while (curp < endp)                    /*   copy circbuf to linbuf   */
	        *bufp++ = *curp++;
	    for (curp=begp,len=wrap; len--; )
	        *bufp++ = *curp++;
	    cosp = p->cosp;                        /*   get start windowed sines */
	    sinp = p->sinp;
	    lenp = p->winlen;
	    for (nfreqs=p->nfreqs; nfreqs--; ) {   /*   now for each freq this oct: */
	        a = 0.0;
		b = 0.0;
		bufp = p->linbufp;
		for (len = *lenp++; len--; bufp++) {    /*  apply windowed sine seg */
		    a += *bufp * *cosp++;
		    b += *bufp * *sinp++;
		}
		c = a*a + b*b;                          /*  get magnitude squared   */
		if (!(p->dbout))                        /*    & optionally convert  */
		    c = sqrt(c);                        /*    to  mag or db         */
		else if (p->dbout == 1) {
		    if (c < .001) c = .001;
		    c = 10. * log10(c);
		}
		*dftp++ = c;                            /*  store in out spectrum   */
	    }
	}
	specp->ktimstamp = kcounter;                    /* time-stamp the output    */
}

spsclset(p)
 SPECSCAL *p;
{
        SPECDAT *inspecp = p->wsig;
        SPECDAT *outspecp = p->wscaled;
	FUNC    *ftp;
	long    npts;

	if ((npts = inspecp->npts) != outspecp->npts) {  /* if size has changed,   */
	    SPECset(outspecp, (long)npts);               /*    realloc             */
	    outspecp->downsrcp = inspecp->downsrcp;
	    auxalloc((long)npts * 2 * sizeof(float), &p->auxch);
	}
	outspecp->ktimprd = inspecp->ktimprd;      /* pass the source spec info     */
	outspecp->dbout = inspecp->dbout;
	p->fscale = (float *) p->auxch.auxp;       /* setup scale & thresh fn areas */
	p->fthresh = p->fscale + npts;
	if ((ftp=ftfind(p->ifscale)) == NULL)            /* if fscale given,        */
	    initerror("missing fscale table");
	else {
	    register long nn = npts;
	    register long phs = 0;         
	    register long inc = (long)PMASK / npts;
	    register long lobits = ftp->lobits;
	    register float *ftable = ftp->ftable;
	    register float *flp = p->fscale;
	    do {
		*flp++ = *(ftable + (phs >> lobits));    /*  sample into scale area */
		phs += inc;
	    } while (--nn);
	}
	if ((p->thresh = *p->ifthresh)
	 && (ftp=ftfind(p->ifthresh)) != NULL) {         /* if fthresh given,       */
	    register long nn = npts;
	    register long phs = 0;         
	    register long inc = (long)PMASK / npts;
	    register long lobits = ftp->lobits;
	    register float *ftable = ftp->ftable;
	    register float *flp = p->fthresh;
	    do {
		*flp++ = *(ftable + (phs >> lobits));    /*  sample into thresh area */
		phs += inc;
	    } while (--nn);
	}
	else p->thresh = 0;
	outspecp->ktimstamp = 0;                        /* mark the out spec not new */
}

specscal(p)
 SPECSCAL *p;
{
        SPECDAT *inspecp = p->wsig;

	if (inspecp->ktimstamp == kcounter) {          /* if inspectrum is new:      */
	    SPECDAT *outspecp = p->wscaled;
	    register float *inp = (float *) inspecp->auxch.auxp;
	    register float *outp = (float *) outspecp->auxch.auxp;
	    register float *sclp = p->fscale;
	    register long npts = inspecp->npts;

	    if (p->thresh) {                              /* if thresh requested,    */
	        register float *threshp = p->fthresh;
		register float val;
		do {
		    if ((val = *inp++ - *threshp++) > 0.) /*   for vals above thresh */
		        *outp++ = val * *sclp;            /*     scale & write out   */
		    else *outp++ = 0.;                    /*   else output is 0.     */
		    sclp++;
		} while (--npts);
	    }
	    else {
	        do *outp++ = *inp++ * *sclp++;            /* no thresh: rescale only */
		while (--npts);
	    }
	    outspecp->ktimstamp = kcounter;               /* mark the outspec as new */
	}
}

spsumset(p)
 SPECSUM *p;
{
        p->kinterp = (*p->interp == 0.) ? 0 : 1;
        p->kval = 0.;
	p->kinc = 0.;
}

specsum(p)         /* sum all vals of a spectrum and put as ksig */
 SPECSUM *p;       /*         optionally interpolate the output  */
{
        SPECDAT *specp = p->wsig;

        if (specp->ktimstamp == kcounter) {                  /* if spectrum is new   */
	    register float *valp = (float *) specp->auxch.auxp;
	    register float sum = 0.;
	    register long npts = specp->npts;                /*   sum all the values */
	    do 	sum += *valp++;
	    while (--npts);
	    if (p->kinterp)                                  /*   new kinc if interp */
	        p->kinc = (sum - p->kval) / specp->ktimprd;
	    else p->kval = sum;
	}
	*p->ksum = p->kval;       /* output current kval */
	if (p->kinterp)           /*   & interp if reqd  */
	    p->kval += p->kinc;
}

spadmset(p)
 SPECADDM *p;
{
        SPECDAT *inspec1p = p->wsig1;
        SPECDAT *inspec2p = p->wsig2;
register int   npts;

	if ((npts = inspec1p->npts) != inspec2p->npts)
	    initerror("inputs have different sizes");     /* inspecs must agree in size */
	if (inspec1p->ktimprd != inspec2p->ktimprd)
	    initerror("inputs have diff. time periods");  /*                time period */
	if (inspec1p->dbout != inspec2p->dbout)
	    initerror("inputs have different amptypes");  /*                and db type */
	if (inerrcnt)
	    return;
	if (npts != p->waddm->npts) {                     /* if out doesn't match ins */
	    SPECset(p->waddm, (long)npts);                /*      reinit the out spec */
	    p->waddm->downsrcp = inspec1p->downsrcp;
	}
	p->waddm->ktimprd = inspec1p->ktimprd;             /* pass the other specinfo */
	p->waddm->dbout = inspec1p->dbout;
	p->waddm->ktimstamp = 0;                           /* mark the out spec not new */
}

specaddm(p)
 SPECADDM *p;
{
	if (p->wsig1->ktimstamp == kcounter) {               /* if inspec1 is new:     */
	    register float *in1p = (float *) p->wsig1->auxch.auxp;
	    register float *in2p = (float *) p->wsig2->auxch.auxp;
	    register float *outp = (float *) p->waddm->auxch.auxp;
	    register float mul2 = p->mul2;
	    register int   npts = p->wsig1->npts;

	    do *outp++ = *in1p++ + *in2p++ * mul2;           /* out = in1 + in2 * mul2 */
	    while (--npts);
	    p->waddm->ktimstamp = kcounter;             /* mark the output spec as new */
	}
}

spdifset(p)
 SPECDIFF *p;
{
        SPECDAT *inspecp = p->wsig;
register float *lclp;
register float *outp;
register int   npts;

	p->mode = *p->imode;          /* NOT YET USED */
	if ((npts = inspecp->npts) != p->specsave.npts) {  /* if inspec not matched  */
	    SPECset(&p->specsave, (long)npts);             /*   reinit the save spec */
	    SPECset(p->wdiff, (long)npts);                 /*   & the out diff spec  */
	    p->wdiff->downsrcp = inspecp->downsrcp;
	}
	p->wdiff->ktimprd = inspecp->ktimprd;             /* pass the other specinfo */
	p->wdiff->dbout = inspecp->dbout;
	lclp = (float *) p->specsave.auxch.auxp;
	outp = (float *) p->wdiff->auxch.auxp;
	do {
	    *lclp++ = 0.;                    /* clr local & out spec bufs */
	    *outp++ = 0.;
	} while (--npts);
	p->wdiff->ktimstamp = 0;             /* mark the out spec not new */
}

specdiff(p)
 SPECDIFF *p;
{
        SPECDAT *inspecp = p->wsig;

	if (inspecp->ktimstamp == kcounter) {     /* if inspectrum is new:     */
	    register float *newp = (float *) inspecp->auxch.auxp;
	    register float *prvp = (float *) p->specsave.auxch.auxp;
	    register float *difp = (float *) p->wdiff->auxch.auxp;
	    register float newval, prvval, diff, possum = 0.;
	    register int   npts = inspecp->npts;

	    do {
	        newval = *newp++;                   /* compare new & old coefs */
		prvval = *prvp;
		if ((diff = newval-prvval) > 0.) {  /* if new coef > prv coef  */
		    *difp++ = diff;
		    possum += diff;                 /*   enter & accum diff    */
		}
		else *difp++ = 0.;                  /* else enter zero         */
		*prvp++ = newval;                   /* sav newval for nxt time */
	    } while (--npts);
	    p->wdiff->ktimstamp = kcounter;     /* mark the output spec as new */
	}
}

spfilset(p)
 SPECFILT *p;
{
        SPECDAT *inspecp = p->wsig;
        SPECDAT *outspecp = p->wfil;
	FUNC    *ftp;
	long    npts;

	if ((npts = inspecp->npts) != outspecp->npts) {      /* if inspec not matched */
	    SPECset(outspecp, (long)npts);                   /*   reinit the out spec */
	    auxalloc((long)npts*2* sizeof(float), &p->auxch);/*   & local auxspace    */
	    p->coefs = (float *) p->auxch.auxp;              /*   reassign filt tbls  */
	    p->states = p->coefs + npts;
	}
	outspecp->ktimprd = inspecp->ktimprd;                /* pass other spect info */
	outspecp->dbout = inspecp->dbout;
	outspecp->downsrcp = inspecp->downsrcp;
	if ((ftp=ftfind(p->ifhtim)) == NULL) {          /* if fhtim table given,      */
	    initerror("missing htim ftable");
	    return;
	}
	{
	    register long nn = npts;
	    register long phs = 0;         
	    register long inc = (long)PMASK / npts;
	    register long lobits = ftp->lobits;
	    register float *ftable = ftp->ftable;
	    register float *flp = p->coefs;
	    do {
		*flp++ = *(ftable + (phs >> lobits));    /*  sample into coefs area */
		phs += inc;
	    } while (--nn);
	}
	{
	    register long  nn = npts;
	    register float *flp = p->coefs;
	    double halftim, reittim = inspecp->ktimprd/ekr;
	    do {
	        if ((halftim = *flp) > 0.)
		    *flp++ = pow((double).5, reittim/halftim);
		else initerror("htim ftable must be all-positive");
	    } while (--nn);
	}
  printf("coef range: %6.3f - %6.3f\n", *p->coefs, *(p->coefs+npts-1));
	{
	    register float *flp = (float *) p->states;
	    do  *flp++ = 0.;                          /* clr the persist buf state mem */
	    while (--npts);
	}
	outspecp->ktimstamp = 0;                     /* mark the output spec as not new */
}

specfilt(p)
 SPECFILT *p;
{
	if (p->wsig->ktimstamp == kcounter) {          /* if input spec is new,  */
	    SPECDAT *inspecp = p->wsig;
	    SPECDAT *outspecp = p->wfil;
	    register float *newp = (float *) inspecp->auxch.auxp;
	    register float *outp = (float *) outspecp->auxch.auxp;
	    register float curval, *coefp = p->coefs;
	    register float *persp = p->states;
	    register int   npts = inspecp->npts;

	    do {                                         /* for npts of inspec:     */
	        *outp++ = curval = *persp;               /*   output current point  */
		*persp++ = *coefp++ * curval + *newp++;  /*   decay & addin newval  */
	    } while (--npts);
	    outspecp->ktimstamp = kcounter;              /* mark output spec as new */
	}
}

spdspset(p)
 SPECDISP *p;
{
	if ((p->timcount = ekr * *p->iprd) <= 0) initerror("illegal iperiod");
	if (!(p->dwindow.windid)) {
	    SPECDAT *specp = p->wsig;
	    DOWNDAT *downp = specp->downsrcp;
	    sprintf(strmsg, "instr %d, dft (%s), %ld octaves (%ld - %ld Hz):",
		        p->h.insdshead->insno, getstrdbout(specp->dbout),
		        downp->nocts, downp->lofrq, downp->hifrq);
	    dispset(&p->dwindow, (float *)specp->auxch.auxp,
		    (long)specp->npts, strmsg, (int)*p->iwtflg, "specdisp");
	}
	p->countdown = p->timcount;          /* prime the countdown */
}

specdisp(p)
 SPECDISP *p;
{
	if (!(--p->countdown)) {               /* on countdown     */
	    display(&p->dwindow);	       /*    display spect */
	    p->countdown = p->timcount;        /*    & reset count */
	}
}

#define NTERMS  4
#define NCROSS  (NTERMS * (NTERMS-1))

tempeset(p)
 TEMPEST *p;
{
        int  npts, nptsm1, minlam, maxlam, lamspan, auxsiz;
	register float *fltp;
	FUNC *ftp;
	float b, iperiod = *p->iprd;

	if ((p->timcount = ekr * iperiod) <= 0)    initerror("illegal iperiod");
	if ((p->dtimcnt = ekr * *p->idisprd) < 0)  initerror("illegal idisprd");
	if ((p->tweek = *p->itweek) <= 0)          initerror("illegal itweek");
	if (iperiod != 0.) {
	    if ((minlam = *p->imindur/iperiod) <= 0)  initerror("illegal imindur");
	    if ((npts = *p->imemdur / iperiod) <= 0)  initerror("illegal imemdur");
	}
	if (*p->ihtim <= 0.)       initerror("illegal ihtim");
	if (*p->istartempo <= 0.)  initerror("illegal startempo");
	ftp = ftfind(p->ifn);
	if (ftp != NULL && *ftp->ftable == 0.) initerror("ifn table begins with zero");
	if (inerrcnt)
	   return;                                  /* if errors so far, return */
	nptsm1 = npts - 1;
	if (npts != p->npts || minlam != p->minlam) {
	    p->npts = npts;
	    p->minlam = minlam;
	    p->maxlam = maxlam = nptsm1/(NTERMS-1);
	    lamspan = maxlam - minlam + 1;          /* alloc 8 bufs: 2 circ, 6 lin */
	    auxsiz = (npts * 5 + lamspan * 3) * sizeof(float);
	    auxalloc((long)auxsiz, &p->auxch);
	    fltp = (float *) p->auxch.auxp;
	    p->hbeg = fltp;     fltp += npts;
	    p->hend = fltp;
	    p->xbeg = fltp;     fltp += npts;
	    p->xend = fltp;
	    p->stmemp = fltp;   fltp += npts;
	    p->linexp = fltp;   fltp += npts;
	    p->ftable = fltp;   fltp += npts;
	    p->xscale = fltp;   fltp += lamspan;
	    p->lmults = fltp;   fltp += lamspan;
	    p->lambdas = (short *) fltp;
	    p->stmemnow = p->stmemp + nptsm1;
	}
	if (p->dtimcnt && !(p->dwindow.windid)) {      /* init to display stmem & exp */
	    sprintf(strmsg, "instr %d, project:", p->h.insdshead->insno);
	    dispset(&p->dwindow,p->stmemp,(long)npts*2,strmsg,0,"tempest");
	    p->dwindow.danflag = 1;                    /* for mid-scale axis */
	}
	{
	    register float *funp = ftp->ftable;
	    register long phs = 0;
	    register long inc = (long)PMASK / npts;
	    register long nn, lobits = ftp->lobits;
	    for (fltp=p->hbeg, nn=npts*4; nn--; )      /* clr 2 circ & 1st 2 lin bufs */
	        *fltp++ = 0.;                   
	    for (fltp=p->ftable+npts, nn=npts; nn--; ) {    /* now sample the ftable  */
	        *--fltp = *(funp + (phs >> lobits));        /* backwards into tbl buf */
		phs += inc;
	    }
	}
	{
	    register float *tblp, sumraw, sumsqr;      /* calc the CROSS prod scalers */
	    register long terms;
	    long lambda, maxlam;
	    float crossprods, RMS, *endtable = p->ftable + nptsm1;
	    float coef, log001 = -6.9078;
	    float *xscale = p->xscale;

	    p->ncross = (float) NCROSS;
	    for (lambda=p->minlam,maxlam=p->maxlam; lambda <= maxlam; lambda++) {
	        tblp = endtable;
		sumraw = *tblp;
		sumsqr = *tblp * *tblp;
		terms = NTERMS - 1;
		do {
		    tblp -= lambda;
		    sumraw += *tblp;
		    sumsqr += *tblp * *tblp;
		} while (--terms);
		crossprods = sumraw * sumraw - sumsqr;
		RMS = (float) sqrt(crossprods / p->ncross);
/*		coef = exp(log001 * lambda / npts);
		*xscale++ = coef / RMS / (NTERMS - 1);  */
		*xscale++ = 0.05 / RMS / lambda;
	    }
	}
	b = 2. - cos((*p->ihp * 6.28318 / ekr));  /* calc input lo-pass filter coefs */
	p->coef1 = b - sqrt(b * b - 1.);
	p->coef0 = 1. - p->coef1;
	p->yt1 = 0.;
	p->fwdcoef = pow((double).5, p->timcount/ekr/(*p->ihtim));
	p->fwdmask = 0.;
  printf("kin lopass coef1 %6.4f, fwd mask coef1 %6.4f\n", p->coef1,p->fwdcoef);
	p->thresh = *p->ithresh;            /* record incoming loudness threshold */
	p->xfdbak = *p->ixfdbak;            /*    & expectation feedback fraction */
	p->tempscal = 60. * ekr / p->timcount;
	p->avglam = p->tempscal / *p->istartempo;       /* init the tempo factors */
	p->tempo = 0.;
	p->hcur = p->hbeg;                              /* init the circular ptrs */
	p->xcur = p->xbeg;
	p->countdown = p->timcount;                     /* & prime the countdowns */
	p->dcntdown = p->dtimcnt;
}

#define NMULTS 5
static float lenmults[NMULTS] = { 3., 2., 1., .5, .333 };
static float lenfracs[NMULTS*2] = { .30,.3667, .45,.55, .92,1.08, 1.88,2.12, 2.85,3.15 };

tempest(p)
 TEMPEST *p;
{
        p->yt1 = p->coef0 * *p->kin + p->coef1 * p->yt1;  /* get lo-pass of kinput */
	if (!(--p->countdown)) {                          /* then on countdown:    */
	    register float *memp;
	    float kin, expect, *xcur = p->xcur;           /* xcur from prv pass    */
	    float lamtot = 0., weightot = 0.;

	    p->countdown = p->timcount;           /* reset the countdown            */
	    expect = *xcur;                       /* get expected val from prv calc */
	    *xcur++ = 0.;                         /*    & clear the loc it occupied */
	    if (xcur >= p->xend) xcur = p->xbeg;  /* xcur now points to cur xarray  */
	    p->xcur = xcur;
	    if ((kin = *p->kin - p->yt1) < 0.)  kin = 0;  /* ignore input below lopass */
	    {
	        register float *hcur = p->hcur;
		register float *hend = p->hend;
		register float *tblp = p->ftable;
		register long  wrap;
		*hcur++ = kin + expect * p->xfdbak;   /* join insample & expect val */
		if (hcur < hend)  p->hcur = hcur;     /* stor pntr for next insamp  */
		else p->hcur = p->hbeg;
		wrap = hcur - p->hbeg;
		memp = p->stmemp;
		while (hcur < hend)                   /* now lineariz & envlp hbuf */
	            *memp++ = *hcur++ * *tblp++;      /*  into st_mem buf          */
		for (hcur=p->hbeg; wrap--; )
	            *memp++ = *hcur++ * *tblp++;
	    }
	    if (p->yt1 > p->thresh            /* if lo-pass of kinput now significant */
	      && kin > p->fwdmask) {          /*    & kin > masking due to prev kin   */
	        register float sumraw, sumsqr;
		register long lambda, minlam, maxlam;
		register int  terms, nn, npts = p->npts;
		float mult, crossprods, RMScross, RMStot, unilam, rd;
		float *xend = p->xend;
		float *xscale = p->xscale;
		float *mults, *fracs, *mulp;
		short minlen, maxlen, *lenp, *endlens;

		for (memp=p->stmemp,nn=npts,sumsqr=0.; nn--; memp++)
		    sumsqr += *memp * *memp;
		RMStot = sqrt(sumsqr/npts);
/*	  printf("RMStot = %6.1f\n",RMStot);    */
		mults = lenmults;                       /* use the static lentables  */
		fracs = lenfracs;
		mulp = p->lmults;
		lenp = p->lambdas;
		minlam = p->minlam;
		maxlam = p->maxlam;
	        nn = NMULTS;
		do {
		    mult = *mults++;
		    minlen = p->avglam * *fracs++;      /*      & the current avglam  */
		    maxlen = p->avglam * *fracs++;
		    if (minlen >= minlam && maxlen <= maxlam)
		        do {
			    *lenp++ = minlen++;         /*   creat lst of lambda lens */
			    *mulp++ = mult;             /*   & their unit multipliers */
			} while (minlen <= maxlen);
		} while (--nn);
		endlens = lenp;                         /* now for these lambda lens: */
		for (lenp=p->lambdas,mulp=p->lmults; lenp < endlens; ) {
		    lambda = *lenp++;
		    mult = *mulp++;
	            memp = p->stmemnow;
		    sumraw = *memp;
		    sumsqr = *memp * *memp;             /* autocorrelate the st_mem buf */
		    terms = NTERMS - 1;
		    do {
		        memp -= lambda;
			sumraw += *memp;
			sumsqr += *memp * *memp;
		    } while (--terms);
		    crossprods = sumraw * sumraw - sumsqr;
		    RMScross = sqrt(crossprods / p->ncross);
		    if (RMScross < 1.4 *  RMStot)         /* if RMScross significant:   */
		        continue;
/*   printf("RMScross = %6.1f, lambda = %ld\n", RMScross, lambda  );  */
/*		    RMS *= *xscale++;     */
		    unilam = lambda * mult;               /*    get unit lambda implied */
		    lamtot += unilam * RMScross;          /*    & add weighted to total */
		    weightot += RMScross;
/*    printf("lambda %d, unilam %6.2f, RMScross %6.2f\n", lambda, unilam, RMScross);  */
		    RMScross /= 5.;            
		    memp = xcur - 1;                /* multiply project into expect buf */
		    for (terms=1; terms < NTERMS; ++terms) {
			if ((memp += (lambda-terms+1)) >= xend)
			    memp -= npts;
			for (nn=terms,rd=RMScross/terms; nn--; ) {
			    *memp++ += rd;
			    if (memp >= xend)
			        memp -= npts;
			}
		    }
	        }   
	    }
	    if (weightot) {                                     /* if accumed weights, */
		p->avglam = (p->avglam + lamtot/weightot)/2.0;  /*   update the avglam */
		p->avglam /= p->tweek;
		p->tempo = p->tempscal / p->avglam;             /*   & cvt to tempo    */
/*       printf("lamtot %6.2f, weightot %6.2f, newavglam %6.2f, tempo %6.2f\n",
	      lamtot, weightot, p->avglam, p->tempo);    */
/*       printf("%6.1f\n",p->tempo);  */
	 fputc('.', stderr);  
	    }
	    else p->tempo = 0.;                                 /* else tempo is 0     */
	    p->fwdmask = p->fwdmask * p->fwdcoef + kin;
        }
	if (!(--p->dcntdown)) {                 /* on display countdown    */
	    register float *linp = p->linexp;
	    register float *xcur = p->xcur;
	    register float *xend = p->xend;
	    register long wrap = xcur - p->xbeg;
	    while (xcur < xend)                   /* lineariz the circ xbuf */
	        *linp++ = *xcur++;                /*  into linexp buf       */
	    for (xcur=p->xbeg; wrap--; )
	        *linp++ = *xcur++;
	    display(&p->dwindow);	          /* display double window  */
	    p->dcntdown = p->dtimcnt;             /*   & reset the counter  */
	}
	*p->kout = p->tempo;                      /* put current tempo */
}
