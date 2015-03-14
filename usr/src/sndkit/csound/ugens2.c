#include "cs.h"			/*					UGENS2.C	*/
#include "ugens2.h"
#include <math.h>

static float	fzero = 0, fone = 1;

phsset(p)
 register PHSOR *p;
{
register float	phs;
register long  longphs;
	if ((phs = *p->iphs) >= fzero) {
		if ((longphs = phs))
			warning("init phase truncation");
		p->curphs = phs - longphs;
	}
}

kphsor(p)
 register PHSOR *p;
{
register float	phs;
	*p->sr = phs = p->curphs;
	if ((phs += *p->xcps/ekr) >= fone)
		phs -= fone;
	else if (phs < fzero)
		phs += fone;
	p->curphs = phs;
}

phsor(p)
 register PHSOR *p;
{
register int	nsmps = ksmps;
register float	*rs, phase, incr;

	rs = p->sr;
	phase = p->curphs;
	if (p->XINCODE) {
		register float *cps = p->xcps;
		do {
			incr = *cps++ / esr;
			*rs++ = phase;
			phase += incr;
			if (phase >= fone)
				phase -= fone;
			else if (phase < fzero)
				phase += fone;
		} while (--nsmps);
	}
	else {
		incr = *p->xcps / esr;
		do {
			*rs++ = phase;
			phase += incr;
			if (phase >= fone)
				phase -= fone;
			else if (phase < fzero)
				phase += fone;
		} while (--nsmps);
	}
	p->curphs = phase;
}

tblchk(p)
 register TABLE *p;
{
register FUNC	*ftp;
register long	xbmul;
register float	xoff;

	if ((ftp = ftfind(p->ifn)) == NULL)
		return(0);
	if (*p->ixmode)
		xbmul = ftp->flen;
	else	xbmul = 1L;
	if ((xoff = xbmul * *p->ixoff) < 0. || xoff > ftp->flen) {
		initerror("invalid ixoff");
		return(0);
	}
	p->ftp = ftp;
	p->xbmul = xbmul;
	p->offset = xoff;
	p->wrap = *p->iwrap;
	return(1);
}

tblset(p)
 register TABLE *p;
{
	tblchk(p);
}

itable(p)
 register TABLE *p;
{
	if (tblchk(p))
		ktable(p);
}
 
ktable(p)
 register TABLE  *p;
{
register FUNC 	*ftp;
register long	indx, length;
register float 	ndx;

	ftp = p->ftp;
	ndx = *p->indx;
	length = ftp->flen;
	ndx *= p->xbmul;		/* flen if normalized, 1 if not */
	indx = (long) (ndx + p->offset);	/* offset  non-normalized */
	if (!p->wrap) {
		if (indx > length)
			indx = length;
		else if (indx < 0L)
			indx = 0L;
	}
	indx &= ftp->lenmask;
	*p->rslt = *(ftp->ftable + indx);
}

table(p)
 register TABLE  *p;
{
register FUNC 	*ftp;
register float 	*rslt, *pindx, *tab;
register long	indx, mask, length;
register int	nsmps = ksmps;
	float	ndx, xbmul, ixoff;

	ftp = p->ftp;
	rslt = p->rslt;
	length = ftp->flen;
	pindx = p->indx;
	xbmul = p->xbmul;
	ixoff = p->offset;
	mask = ftp->lenmask;
	tab = ftp->ftable;
	do {
		ndx = *pindx++;
		ndx *= xbmul;		/* flen if normalized, 1 if not */
		indx = (long) (ndx + ixoff);	/* offset  non-normalized */
		if (!p->wrap) {
			if (indx > length)  indx = length;
			else if (indx < 0L)  indx = 0L;
		}
		indx &= mask;
		*rslt++ = *(tab + indx);
	} 
	while(--nsmps);
}

ktabli(p)
 register TABLE  *p;
{
register FUNC 	*ftp;
register long	indx, length;
register float 	v1, v2, fract, ndx;

	ftp = p->ftp;
	ndx = *p->indx;
	length = ftp->flen;
	ndx *= p->xbmul;			/* flen if normalized, 1 not */
	if (ndx < 0L) {
		indx = (long) (ndx + p->offset - fone); 
		fract = fone +  ndx - (long)ndx ;
	}
	else {
		indx = (long) (ndx + p->offset);    /* offset non-normalized */
		fract = ndx - (long)ndx;
	}
	if (!p->wrap) {
		if (indx > length)  indx = length;
		else if (indx < 0L)  indx = 0L;
	}
	indx &= ftp->lenmask;
	v1 = *(ftp->ftable + indx);
	v2 = *(ftp->ftable + indx +1); 
	*p->rslt = v1 + (v2 - v1) * fract;
}

tabli(p)
 register TABLE  *p;
{
register FUNC 	*ftp;
register long	indx, mask, length;
register int	nsmps = ksmps;
register float 	*rslt, *pindx, *tab;
register float 	fract, v1, v2, ndx, xbmul, ixoff;

	ftp = p->ftp;
	rslt = p->rslt;
	length = ftp->flen;
	pindx = p->indx;
	xbmul = p->xbmul;
	ixoff = p->offset;
	mask = ftp->lenmask;
	tab = ftp->ftable;
	do {
		ndx = *pindx++;
		ndx *= xbmul;	/* flen if normalized, 1 if not */
		if (ndx < 0) {
			indx = (long) (ndx + ixoff - fone);
			fract = fone + ndx - (long) ndx;
		}
		else {
			indx = (long) (ndx + ixoff); 
			fract = ndx - (long) ndx;
		}
		if (!p->wrap) {
			if (indx > length)  indx = length;
			else if (indx < 0L)  indx = 0L;
		}
		indx &= mask;
		v1 = *(tab + indx);
		v2 = *(tab + indx +1);
		*rslt++ = v1 + (v2 - v1)*fract;
	} 
	while(--nsmps);
}

ko1set(p)
 register OSCIL1 *p;
{
register FUNC	*ftp;

	if ((ftp = ftfind(p->ifn)) == NULL)
		return;
	if (*p->idur <= fzero)
		warning("duration < zero");
	p->ftp = ftp;
	p->phs = 0;
	p->dcnt = *p->idel * ekr;
	p->kinc = (long) (kicvt / *p->idur);
}

kosc1(p) 
 register OSCIL1 *p;
{
register FUNC *ftp;
register long  phs, dcnt;

	ftp = p->ftp;
	phs = p->phs;
	*p->rslt = *(ftp->ftable + (phs >> ftp->lobits)) * *p->kamp;
	if ((dcnt = p->dcnt) > 0L)
		dcnt--;
	else if (dcnt == 0L) {
		phs += p->kinc;
		if (phs >= MAXLEN) {
			phs = MAXLEN;
			dcnt--;
		}
		p->phs = phs;
	}
	p->dcnt = dcnt;
}

kosc1i(p)
 register OSCIL1  *p;
{
register FUNC	*ftp;
register float	fract, v1, *ftab;
register long	phs, dcnt;

	ftp = p->ftp;
	phs = p->phs;
	fract = PFRAC(phs); 	
	ftab = ftp->ftable + (phs >> ftp->lobits);
	v1 = *ftab++;
	*p->rslt = (v1 + (*ftab - v1) * fract) * *p->kamp;
	if ((dcnt = p->dcnt) > 0L) {
		dcnt--;
		p->dcnt = dcnt;
	}
	else if (dcnt == 0L) {
		phs += p->kinc;
		if (phs >= MAXLEN) {
			phs = MAXLEN;
			dcnt--;
			p->dcnt = dcnt;
		}
		p->phs = phs;
	}
}

oscset(p)
 register OSC *p;
{
register FUNC	*ftp;

	if ((ftp = ftfind(p->ifn)) != NULL) {
		p->ftp = ftp;
		if (*p->iphs >= 0)
			p->lphs = ((long)(*p->iphs * fmaxlen)) & PMASK;
	}
}

koscil(p)
 register OSC *p;
{
register FUNC	*ftp;
register long	phs, inc;

	ftp = p->ftp;
	phs = p->lphs;
	inc = *p->xcps * kicvt;
	*p->sr = *(ftp->ftable + (phs >> ftp->lobits)) * *p->xamp;
	phs += inc;
	phs &= PMASK;
	p->lphs = phs;
}

osckk(p)
 register OSC *p;
{
register FUNC	*ftp;
register float	amp, *ar, *ftbl;
register long	phs, inc, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	ftbl = ftp->ftable;
	phs = p->lphs;
	inc = *p->xcps * sicvt;
	lobits = ftp->lobits;
	amp = *p->xamp;
	ar = p->sr;
	do {
		*ar++ = *(ftbl + (phs >> lobits)) * amp;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

oscka(p)
 register OSC *p;
{
register FUNC	*ftp;
register float	*ar, amp, *cpsp, *ftbl;
register long	phs, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	ftbl = ftp->ftable;
	lobits = ftp->lobits;
	amp = *p->xamp;
	cpsp = p->xcps;
	phs = p->lphs;
	ar = p->sr;
	do {
		register long inc;
		inc = *cpsp++ * sicvt;
		*ar++ = *(ftbl + (phs >> lobits)) * amp;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

oscak(p)
 register OSC *p;
{
register FUNC	*ftp;
register float	*ar, *ampp, *ftbl;
register long	phs, inc, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	ftbl = ftp->ftable;
	lobits = ftp->lobits;
	phs = p->lphs;
	inc = *p->xcps * sicvt;
	ampp = p->xamp;
	ar = p->sr;
	do {
		*ar++ = *(ftbl + (phs >>lobits)) * *ampp++;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

oscaa(p)
 register OSC *p;
{
register FUNC	*ftp;
register float	*ar, *ampp, *cpsp, *ftbl;
register long	phs, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	ftbl = ftp->ftable;
	lobits = ftp->lobits;
	phs = p->lphs;
	ampp = p->xamp;
	cpsp = p->xcps;
	ar = p->sr;
	do {
		register long inc;
		inc = *cpsp++ * sicvt;
		*ar++ = *(ftbl + (phs >>lobits)) * *ampp++;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

koscli(p)
 register OSC  *p;
{
register FUNC	*ftp;
register long	phs, inc;
register float  *ftab, fract, v1;

	phs = p->lphs;
	ftp = p->ftp;
	fract = PFRAC(phs);
	ftab = ftp->ftable + (phs >> ftp->lobits);
	v1 = *ftab++;
	*p->sr = (v1 + (*ftab - v1) * fract) * *p->xamp;
	inc = *p->xcps * kicvt;
	phs += inc;
	phs &= PMASK;
	p->lphs = phs;
}

osckki(p)
  register OSC  *p;
{
register FUNC	*ftp;
register float	fract, v1, amp, *ar, *ftab;
register long	phs, inc, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	lobits = ftp->lobits;
	phs = p->lphs;
	inc = *p->xcps * sicvt;
	amp = *p->xamp;
	ar = p->sr;
	do {
		fract = PFRAC(phs);
		ftab = ftp->ftable + (phs >> lobits);
		v1 = *ftab++;
		*ar++ = (v1 + (*ftab - v1) * fract) * amp;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

osckai(p)
 register OSC  *p;
{
register FUNC	*ftp;
register float	*ar, amp, *cpsp, fract, v1, *ftab;
register long	phs, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	lobits = ftp->lobits;
	amp = *p->xamp;
	cpsp = p->xcps;
	phs = p->lphs;
	ar = p->sr;
	do {
		register long inc;
		inc = *cpsp++ * sicvt;
		fract = PFRAC(phs);
		ftab = ftp->ftable + (phs >> lobits);
		v1 = *ftab++;
		*ar++ = (v1 + (*ftab - v1) * fract) * amp;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

oscaki(p)
 register OSC  *p;
{
register FUNC	*ftp;
register float	v1, fract, *ar, *ampp, *ftab;
register long	phs, inc, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	ftab = ftp->ftable;
	lobits = ftp->lobits;
	phs = p->lphs;
	inc = *p->xcps * sicvt;
	ampp = p->xamp;
	ar = p->sr;
	do {
		fract = PFRAC(phs);
		ftab = ftp->ftable + (phs >> lobits);
		v1 = *ftab++;
		*ar++ = (v1 + (*ftab - v1) * fract) * *ampp++;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

oscaai(p)
 register OSC  *p;
{
register FUNC	*ftp;
register float	v1, fract, *ar, *ampp, *cpsp, *ftab;
register long	phs, lobits;
register int	nsmps = ksmps;

	ftp = p->ftp;
	ftab = ftp->ftable;
	lobits = ftp->lobits;
	phs = p->lphs;
	ampp = p->xamp;
	cpsp = p->xcps;
	ar = p->sr;
	do {
		register long inc;
		inc = *cpsp++ * sicvt;
		fract = PFRAC(phs);
		ftab = ftp->ftable + (phs >> lobits);
		v1 = *ftab++;
		*ar++ = (v1 + (*ftab - v1) * fract) * *ampp++;
		phs += inc;
		phs &= PMASK;
	}
	while (--nsmps);
	p->lphs = phs;
}

foscset(p)
 register FOSC *p;
{
register FUNC	*ftp;

	if ((ftp = ftfind(p->ifn)) != NULL) {
		p->ftp = ftp;
		if (*p->iphs >= 0)
			p->cphs = p->mphs = *p->iphs * fmaxlen;
	}
}

foscil(p)
 register FOSC	*p;
{
register FUNC	*ftp;
register float	*ar, *ampp, car, fmod, cfreq, mod, ndx, *ftab;
register long	mphs, cphs, minc, cinc, lobits;
register int	nsmps = ksmps;

	ar = p->rslt;
	ftp = p->ftp;
	ftab = ftp->ftable;
	lobits = ftp->lobits;
	mphs = p->mphs;
	cphs = p->cphs;
	car = *p->kcps * *p->kcar;
	mod = *p->kcps * *p->kmod;
	ndx = *p->kndx * mod;
	ampp = p->xamp;
	minc = mod * sicvt; 
	if (p->XINCODE) {
		do {
			mphs &= PMASK;
			fmod = *(ftab + (mphs >>lobits)) * ndx;
			mphs += minc;
			cfreq = car + fmod;
			cinc = cfreq * sicvt;
			cphs &= PMASK;
			*ar++ = *(ftab + (cphs >>lobits)) * *ampp++;
			cphs += cinc;
		}
		while (--nsmps);
	}
	else {
		register float amp;
		amp = *ampp;
		do {
			mphs &= PMASK;
			fmod = *(ftab + (mphs >>lobits)) * ndx;
			mphs += minc;
			cfreq = car + fmod;
			cinc = cfreq * sicvt;
			cphs &= PMASK;
			*ar++ = *(ftab + (cphs >>lobits)) * amp;
			cphs += cinc;
		}
		while (--nsmps);
	}
	p->mphs = mphs;
	p->cphs = cphs;
}

foscili(p)
 register FOSC	*p;
{
register FUNC	*ftp;
register float	*ar, *ampp, fract, v1, car, fmod, cfreq, mod;
register float	ndx, *ftab;
register long	mphs, cphs, minc, cinc, lobits;
register int	nsmps = ksmps;

	ar = p->rslt;
	ftp = p->ftp;
	lobits = ftp->lobits;
	mphs = p->mphs;
	cphs = p->cphs;
	car = *p->kcps * *p->kcar;
	mod = *p->kcps * *p->kmod;
	ndx = *p->kndx * mod;
	ampp = p->xamp;
	minc = mod * sicvt;
	if (p->XINCODE) {
		do {
			mphs &= PMASK;
			fract = PFRAC(mphs);
			ftab = ftp->ftable + (mphs >>lobits);
			v1 = *ftab++;
			fmod = (v1 + (*ftab - v1) * fract) * ndx;
			mphs += minc;
			cfreq = car + fmod;
			cinc = cfreq * sicvt;
			cphs &= PMASK;
			fract = PFRAC(cphs);
			ftab = ftp->ftable + (cphs >>lobits);
			v1 = *ftab++;
			*ar++ = (v1 + (*ftab - v1) * fract) * *ampp++;
			cphs += cinc;
		}
		while (--nsmps);
	}
	else {
		register float amp;
		amp = *ampp;
		do {
			mphs &= PMASK;
			fract = PFRAC(mphs);
			ftab = ftp->ftable + (mphs >>lobits);
			v1 = *ftab++;
			fmod = (v1 + (*ftab - v1) * fract) * ndx;
			mphs += minc;
			cfreq = car + fmod;
			cinc = cfreq * sicvt;
			cphs &= PMASK;
			fract = PFRAC(cphs);
			ftab = ftp->ftable + (cphs >>lobits);
			v1 = *ftab++;
			*ar++ = (v1 + (*ftab - v1) * fract) * amp;
			cphs += cinc;
		}
		while (--nsmps);
	}
	p->mphs = mphs;
	p->cphs = cphs;
}

#define ISINSIZ 16384
#define ADMASK  16383L
	
static short *isintab;

adset(p)
 register ADSYN *p;
{
	int	n;
	char	filnam[16];
	MEMFIL	*mfp;
register short	*adp, *endata, val;
register PTLPTR	*ptlap, *ptlfp, *ptlim;

	if (isintab == NULL) {		/* if no sin table yet, make one */
		register short *ip;
		isintab = ip = (short *) mmalloc((long)ISINSIZ * sizeof(short));
		for (n = 0; n < ISINSIZ; n++)
			*ip++ = (short) (sin(6.28318*n/ISINSIZ) * 32767.);
	}
	sprintf(filnam,"adsyn.%d",(int)*p->ifilno);     /* construct filename */
	if ((mfp = p->mfp) == NULL || strcmp(mfp->filename,filnam) != 0) {
	    if ((mfp = ldmemfile(filnam)) == NULL) {    /*   readfile if reqd */
		sprintf(errmsg,"read error on %s",filnam);
		goto adserr;
	    }
	    p->mfp = mfp;                               /*   & record         */
	}
/*	adh = (ADHEADER *) mfp;  			/* chk header values */
/*	if (adh->admagic != ADSYN_MAGIC) {
/*		sprintf(errmsg,"%s not an ADSYN file",filnam);
/*		goto adserr;
/*	}
 */
 	adp = (short *) mfp->beginp;		/* align on file data */
	endata = (short *) mfp->endp;
	ptlap = ptlfp = p->ptlptrs;		/*  and pointer array */
	ptlim = ptlap + MAXPTLS;
	do if ((val = *adp++) < 0) {		/* then for each brkpt set, */
		switch (val) {
		case -1: ptlap->ap = (DUPLE *) adp;	/* record the start */
		         ptlap->amp = ptlap->ap->val;
			 if (++ptlap >= ptlim) goto adsful;
			 break;				/* of both amp & frq */
		case -2: ptlfp->fp = (DUPLE *) adp;
		         ptlfp->frq = ptlfp->fp->val;
			 ptlfp->phs = 0;		/* and clr the phase */
			 if (++ptlfp >= ptlim) goto adsful;
			 break;
		default: sprintf(errmsg,"illegal code %d encountered",val);
			 goto adserr;
		}
	} while (adp < endata);
	if (ptlap != ptlfp) {
		sprintf(errmsg,"%d amp tracks, %d freq tracks",
			ptlap - p->ptlptrs, ptlfp - p->ptlptrs);
		goto adserr;
	}
	ptlap->ap = NULL;
	p->kcnt = 0;
 	return;

adsful:	sprintf(errmsg,"partial count exceeds MAXPTLS");
adserr:	initerror(errmsg);
}

adsyn(p)
 register ADSYN *p;
{

	PTLPTR	*ptlp;
	DUPLE	*ap, *fp;
	short	curtim, atimtogo, ftimtogo, diff;
register long	phs, sinc, *sp, amp;
register int    nsmps;
register float	*ar;
	float	ampscale, frqscale;

	ampscale = *p->kamod * dv32768;	/* (since 15-bit sine table) */
	frqscale = *p->kfmod * ISINSIZ / esr;
	sp = (long *) p->rslt;		/* use out array for sums */
	nsmps = ksmps;
        do  *sp++ = 0;			/* cleared first to zero */
	while (--nsmps);
 	curtim = p->kcnt++ * 1000. / ekr;    /* ktime into millisecs */
        ptlp = p->ptlptrs;
        while( (ap = ptlp->ap) != NULL ) {   /* now for each partial:    */
	    fp = ptlp->fp;
	    while ((atimtogo = (ap+1)->tim - curtim) <= 0)    /* timealign ap */
		ptlp->ap = ap += 1;
	    while ((ftimtogo = (fp+1)->tim - curtim) <= 0)
		ptlp->fp = fp += 1;
	    if ((amp = ptlp->amp)) {		/* for non-zero amp   */
		sinc = ptlp->frq * frqscale;
		phs = ptlp->phs;
		sp = (long *) p->rslt;   
		nsmps = ksmps;			/*   addin a sinusoid */
		do {
		    *sp++ += *(isintab + phs) * amp;
		    phs += sinc;
		    phs &= ADMASK;
		}
		while (--nsmps);	
		ptlp->phs = phs;
	    }
/*	    if ((ap+1)->tim == 32767)      /* if last amp segment for this partial */
/*	        ptlp->amp = 0;              /*   terminate it                       */
/*	    else */ {
	        if ((diff = (ap+1)->val - amp))	    /* else calc its next amp,frq */
		    ptlp->amp += diff / atimtogo;
		if ((diff = (fp+1)->val - ptlp->frq))
		    ptlp->frq += diff / ftimtogo;
	    }
	    ptlp++;
	}						/* loop for all ptls */
	ar = p->rslt;
	sp = (long *) ar;
	nsmps = ksmps;
        do  *ar++ = *sp++ * ampscale;		/* float & scale the results */
	while (--nsmps);
}
