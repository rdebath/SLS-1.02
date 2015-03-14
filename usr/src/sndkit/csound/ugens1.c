#include "cs.h"			/*					UGENS1.C	*/
#include "ugens1.h"
#include <math.h>

static float	fzero = 0., fone = 1.;

linset(p)
 register LINE *p;
{
register float	dur;

	if ((dur = *p->idur) > fzero) {
		p->incr = (*p->ib - *p->ia) / dur / ekr;
		p->val = *p->ia;
	}
}

kline(p)
 register LINE *p;
{
	*p->xr = p->val;		/* rslt = val	*/
	p->val += p->incr;		/* val += incr	*/
}

aline(p)
 register LINE *p;
{
register float	val, inc, *ar;
register int	nsmps = ksmps;

	val = p->val;
	inc = p->incr;
	p->val += inc;		/* nxtval = val + inc */
	inc /= ensmps;
	ar = p->xr;
	do {	*ar++ = val;
		val += inc;	/* interp val for ksmps */
	}
	while (--nsmps);
}

expset(p)
 register EXPON *p;
{
register float	dur, a, b;

	if ((dur = *p->idur) > fzero ) {
		a = *p->ia;
		b = *p->ib;
		if ((a * b) > fzero) {
			p->mlt = pow((b / a),(1./dur/ekr));
			p->val = a;
		}
		else if (a == fzero)
			initerror("arg1 is zero");
		else if (b == fzero)
			initerror("arg2 is zero");
		else initerror("unlike signs");
	}
}

kexpon(p)
 register EXPON *p;
{
	*p->xr = p->val;		/* rslt = val	*/
	p->val *= p->mlt;		/* val *= mlt  */
}

expon(p)
 register EXPON *p;
{
register float	val, mlt, inc, *ar,nxtval;
register int	nsmps = ksmps;

	val = p->val;
	mlt = p->mlt;
	nxtval = val * mlt;	
	inc = nxtval - val;
	inc /= ensmps;		/* increment per sample */
	ar = p->xr;
	do {
		*ar++ = val;
		val += inc;	/* interp val for ksmps */
	}
	while (--nsmps);
	p->val = nxtval;	/*store next value */
}

lsgset(p)
 register LINSEG *p;
{
register LSEG	*segp;
register int	nsegs;
register float	d, **argp, val, dur, nxtval;

	nsegs = p->INCOUNT >> 1;		/* count segs & alloc if nec */
	if ((segp = (LSEG *) p->auxch.auxp) == NULL) {
	        auxalloc((long)nsegs*sizeof(LSEG), &p->auxch);
		p->cursegp = segp = (LSEG *) p->auxch.auxp;
		(segp+nsegs-1)->cnt = MAXPOS;   /* set endcount for safety */
	}
	argp = p->argums;
	nxtval = **argp++;
	if (**argp <= fzero)  return;		/* if idur1 <= 0, skip init  */
	p->cursegp = segp;                      /* else proceed from 1st seg */
	segp--;
	do {
		segp++; 	 	/* init each seg ..  */
		val = nxtval;
		dur = **argp++;
		nxtval = **argp++;
		if (dur > fzero) {
			d = dur * ekr;
			segp->val = val;
			segp->inc= (nxtval - val) / d;
			segp->cnt = (long) (d + .5); 
		}
		else break;		/*  .. til 0 dur or done */
	} while (--nsegs);
	segp->cnt = MAXPOS;     	/* set last cntr to infin */
}

klnseg(p)
 register LINSEG *p;
{
register LSEG	*segp;

	segp = p->cursegp;
	while (--segp->cnt < 0)
		p->cursegp = ++segp;
	*p->rslt = segp->val;
	segp->val += segp->inc;
}

linseg(p)
 register LINSEG *p;
{
register LSEG	*segp;
register int	nsmps = ksmps;
register float	li, val, *rs;

	segp = p->cursegp;
	while (--segp->cnt < 0)
		p->cursegp = ++segp;
	val = segp->val;
	li = segp->inc / ensmps;
	rs = p->rslt;
	do {
		*rs++ = val;
		val += li;
	} while (--nsmps);
	segp->val += segp->inc;
}

xsgset(p)
 register EXPSEG *p;
{
register XSEG	*segp;
register int	nsegs;
register float	d, **argp, val, dur, nxtval;
         int    n;

	nsegs = p->INCOUNT >> 1;		/* count segs & alloc if nec */
	if ((segp = (XSEG *) p->auxch.auxp) == NULL) {
	        auxalloc((long)nsegs*sizeof(XSEG), &p->auxch);
		p->cursegp = segp = (XSEG *) p->auxch.auxp;
		(segp+nsegs-1)->cnt = MAXPOS;   /* set endcount for safety */
	}
	argp = p->argums;
	nxtval = **argp++;
	if (**argp <= fzero)  return;		/* if idur1 <= 0, skip init  */
	p->cursegp = segp;                      /* else proceed from 1st seg */
	segp--;
	do {
		segp++; 	 	/* init each seg ..  */
		val = nxtval;
		dur = **argp++;
		nxtval = **argp++;
		if (dur > fzero) {
			if (val * nxtval <= fzero)
			        goto experr;
			d = dur * ekr;
			segp->val = val;
			segp->mlt = pow((nxtval / val), (1./d));
			segp->cnt = (long) (d + .5); 
		}
		else break;		/*  .. til 0 dur or done */
	} while (--nsegs);
	segp->cnt = MAXPOS;     	/* set last cntr to infin */
	return;

experr: n = segp - p->cursegp + 1;
        if (val == fzero)
		sprintf(errmsg,"ival%d is zero", n);
	else if (nxtval == fzero)
		sprintf(errmsg,"ival%d is zero", n+1);
	else sprintf(errmsg,"ival%d sign conflict", n+1);
	initerror(errmsg);
}

kxpseg(p)
  register EXPSEG *p;
{
register XSEG	*segp;

	segp = p->cursegp;
	while (--segp->cnt < 0)
		p->cursegp = ++segp;
	*p->rslt = segp->val;
	segp->val *= segp->mlt;
}

expseg(p)
  register EXPSEG *p;
{
register XSEG	*segp;
register int	nsmps = ksmps;
register float	li, val, *rs;
	float	nxtval;

	segp = p->cursegp;
	while (--segp->cnt < 0)
		p->cursegp = ++segp;
	val = segp->val;
	nxtval = val * segp->mlt;
	li = (nxtval - val) / ensmps;
	rs = p->rslt;
	do {
		*rs++ = val;
		val += li;
	} while (--nsmps);
	segp->val = nxtval ;
}

lnnset(p)
  register LINEN *p;
{
register float a,b,dur;

	if ((dur = *p->idur) > fzero) {
		p->cnt1 = (long)(*p->iris * ekr + .5);
		if (p->cnt1 > 0L) {
			p->inc1 = fone / (float) p->cnt1;
			p->val = fzero;
		}
		else p->inc1 = p->val = fone;
		a = dur * ekr + .5;
		b = *p->idec * ekr + .5;
		if ((long) b > 0L) {
			p->cnt2 = (long) (a - b);
			p->inc2 = fone /  b;
		}
		else {
			p->inc2 = fone;
			p->cnt2 = (long) a;
		}
		p->lin1 = fzero;
		p->lin2 = fone;
	}
}

klinen(p)
 register LINEN *p;
{
register float fact = fone;

	if (p->cnt1 > 0L) {
		fact = p->lin1;
		p->lin1 += p->inc1;
		p->cnt1--;
	}
	if (p->cnt2)
		p->cnt2--;
	else {
		fact *= p->lin2;
		p->lin2 -= p->inc2;
	}
	*p->rslt = *p->sig * fact;
}

linen(p)
 register LINEN *p;
{
register int flag=0, nsmps=ksmps;
register float *rs,*sg,li,val,nxtval=1;
	
	val = p->val;
	rs = p->rslt;
	sg = p->sig;
	if (p->cnt1 > 0L) {
		flag = 1;
		p->lin1 += p->inc1;
		p->cnt1--;
		nxtval = p->lin1;
	}
	if (p->cnt2 <= 0L) {
		flag = 1;
		p->lin2 -= p->inc2;
		nxtval *= p->lin2;
	}
	else p->cnt2--;
	p->val = nxtval;
	if (flag) {
		li = (nxtval - val)/ensmps;
		if (p->XINCODE) {
			do {
				*rs++ = *sg++ * val;
				val += li;
			}
			while(--nsmps);
		}	
		else {
			do {
				*rs++ = *sg * val;
				val += li;
			}
			while(--nsmps);
		}
	}
	else {
	        if (p->XINCODE) {
		  do *rs++ = *sg++;
		  while(--nsmps);
		}
		else {
		  do *rs++ = *sg;
		  while(--nsmps);
		}
	}
}

evxset(p)
  register ENVLPX *p;
{
register FUNC	*ftp;
register float	ixmod, iatss, idur, prod, diff, asym, nk, denom, irise;
static	float	f100 = 100.;
	long	cnt1;

	if ((ftp = ftfind(p->ifn)) == NULL)
		return;
	p->ftp = ftp;
	if ((idur = *p->idur) > fzero) {
		if ((iatss = fabs(*p->iatss)) == fzero) {
			initerror("iatss = 0");
			return;
		}
		if (iatss != fone && (ixmod = *p->ixmod) != fzero) {
			if (fabs(ixmod) > .95) {
				initerror("ixmod out of range.");
				return;
			}
			ixmod = -sin(sin(ixmod));
			prod = ixmod * iatss;
			diff = ixmod - iatss;
			denom = diff + prod + 1.;
			if (denom == fzero)
				asym = f100;
			else {
				asym = 2 * prod / denom;
				if(fabs(asym) > f100)
					asym = f100;
			}
			iatss = (iatss - asym) / (1. - asym);
			asym = asym* *(ftp->ftable + ftp->flen); /* +1 */
		}
		else asym = fzero;
		if ((irise = *p->irise) > fzero) {
			p->phs = fzero;
			p->ki = (long) (kicvt / irise);
			p->val = *ftp->ftable;
		}
		else {
			p->phs = -1.;
			p->val = *(ftp->ftable + ftp->flen)-asym;
			irise = fzero;	/* in case irise < 0 */
		}
		if (!(*(ftp->ftable + ftp->flen)))
			initerror("rise func ends with zero");
		cnt1 = (long) ((idur - irise - *p->idec) * ekr + .5);
		if (cnt1 < 0L) {
			cnt1 = 0L;
			nk = ekr;
		}
		else {
			if (*p->iatss < fzero || cnt1 <= 4L)
				nk = ekr;
			else nk = (float) cnt1;
		}
		p->mlt1 = pow(iatss, (fone/nk));
		if (*p->idec > fzero) {
			if (*p->iatdec <= fzero)
				initerror("non-positive iatdec.");
			else p->mlt2 = pow((double)*p->iatdec,
					   ((double)1./ *p->idec/ekr));
		}
		p->cnt1 = cnt1;
		p->asym = asym;
	}
}

knvlpx(p)
  register ENVLPX *p;
{
register FUNC 	*ftp;
register long	phs;
register float 	fact, v1, fract, *ftab;

	ftp = p->ftp;
	if ((phs = p->phs) >= 0) {
		fract = PFRAC(phs);
		ftab = ftp->ftable + (phs >> ftp->lobits);
		v1 = *ftab++;
		fact = (v1 + (*ftab - v1) * fract);
		phs += p->ki;
		if (phs >= MAXLEN) {  /* check that 2**N+1th pnt is good */
			p->val = *(ftp->ftable + ftp->flen );
			if (!p->val)
				perferror("envlpx rise func ends with zero");
			p->val -= p->asym;
			phs = -1L;
		}
		p->phs = phs;
	}
	else {
		fact = p->val;
		if (p->cnt1 > 0L) {
			p->val *= p->mlt1;
			fact += p->asym;
			p->cnt1--;
			if (p->cnt1 == 0L)
				p->val += p->asym;
		}
		else p->val *= p->mlt2;
	}
	*p->rslt = *p->xamp * fact;
}

envlpx(p)
  register ENVLPX *p;
{
register FUNC 	*ftp;
register long	phs;
register int	nsmps = ksmps;
register float 	*xamp, *rslt, val, nxtval, li, v1, fract, *ftab;

	xamp = p->xamp;
	rslt = p->rslt;
	val = p->val;
	if ((phs = p->phs) >= 0L) {
		ftp = p->ftp;
		fract = PFRAC(phs);
		ftab = ftp->ftable + (phs >> ftp->lobits);
		v1 = *ftab++;
		nxtval = (v1 + (*ftab - v1) * fract);
		phs += p->ki;
		if (phs >= MAXLEN) {  /* check that 2**N+1th pnt is good */
			nxtval = *(ftp->ftable + ftp->flen );
			if (!nxtval)
				perferror("envlpx rise func ends with zero");
			nxtval -= p->asym;
			phs = -1;
		}
		p->phs = phs;
	}
	else {
		nxtval = val;
		if (p->cnt1 > 0L) {
			nxtval *= p->mlt1;
			nxtval += p->asym;
			p->cnt1--;
		}
		else nxtval *= p->mlt2;
	}
	p->val = nxtval;
	li = (nxtval - val)/ensmps;	/* linear interpolation factor */
	if (p->XINCODE) {		/* for audio rate amplitude: */
		do {
			*rslt++ = *xamp++ * val;
			val += li;
		}
		while(--nsmps);
	}	
	else {
		do {
			*rslt++ = *xamp * val;
			val += li;
		}
		while(--nsmps);
	}
}
