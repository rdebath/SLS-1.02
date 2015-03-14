#include "cs.h"	/*							UGENS5.C	*/
#include "ugens5.h"
#include <math.h>

static float	log001 = -6.9078;	/* log(.001) */

downset(p)
 register DOWNSAMP *p;
{
	if ((p->len = *p->ilen) > ksmps)
		initerror("ilen > ksmps");
}

downsamp(p)
 register DOWNSAMP *p;
{
register float	*asig, sum;
register int	len;

	if (p->len <= 1)
		*p->kr = *p->asig;
	else {
		asig = p->asig;
		sum = 0.;
		len = p->len;
		do sum += *asig++;
		while (--len);
		*p->kr = sum / p->len;
	}
}

upsamp(p)
 register UPSAMP *p;
{
register float	*ar, kval;
register int	nsmps = ksmps;

	ar = p->ar;
	kval = *p->ksig;
	do *ar++ = kval;
	while (--nsmps);
}

indfset(p)
 register INDIFF *p;
{
	if (*p->istor == 0.0)
		p->prev = 0.0;
}

interp(p)
 register INDIFF *p;
{
register float	*ar, val, incr;
register int	nsmps = ksmps;

	ar = p->rslt;
	val = p->prev;
	incr = (*p->xsig - val) / ensmps;
	do *ar++ = val += incr;
	while (--nsmps);
	p->prev = val;
}

kntegrate(p)
 register INDIFF *p;
{
	*p->rslt = p->prev += *p->xsig;
}

integrate(p)
 register INDIFF *p;
{
register float	*rslt, *asig, sum;
register int	nsmps = ksmps;

	rslt = p->rslt;
	asig = p->xsig;
	sum = p->prev;
	do *rslt++ = sum += *asig++;
	while (--nsmps);
	p->prev = sum;
}

kdiff(p)
 register INDIFF *p;
{
	*p->rslt = *p->xsig - p->prev;
	p->prev = *p->xsig;
}

diff(p)
 register INDIFF *p;
{
register float	*ar, *asig, prev;
register int	nsmps = ksmps;

	ar = p->rslt;
	asig = p->xsig;
	prev = p->prev;
	do {
		*ar++ = *asig - prev;
		prev = *asig++;
	}
	while (--nsmps);
	p->prev = prev;
}

samphset(p)
 register SAMPHOLD *p;
{
	if (!(*p->istor))
		p->state = *p->ival;
	p->audiogate = (p->XINCODE & 01) ? 1 : 0;
}

ksmphold(p)
 register SAMPHOLD *p;
{
	if (*p->xgate > 0.)
		p->state = *p->xsig;
	*p->xr = p->state;
}

samphold(p)
 register SAMPHOLD *p;
{
register float	*ar, *asig, *agate, state;
register int	nsmps = ksmps;

	ar = p->xr;
	asig = p->xsig;
	state = p->state;
	if (p->audiogate) {
		agate = p->xgate;
		do {
			if (*agate++ > 0.)
				state = *asig;
			*ar++ = state;
			asig++;
		}
		while (--nsmps);
	}
	else {
		if (*p->xgate > 0.) {
			do *ar++ = state = *asig++;
			while (--nsmps);
		}
		else {
			do *ar++ = state;
			while (--nsmps);
		}
	}
	p->state = state;
}

delset(p)
 register DELAY *p;
{
register long	npts;
register char	*auxp;

	if (*p->istor && p->auxch.auxp != NULL)
		return;
	if ((npts = *p->idlt * esr) <= 0) {
		initerror("illegal delay time");
		return;
	}
	if ((auxp = p->auxch.auxp) == NULL || npts != p->npts) { /* new space if reqd */
		auxalloc((long)npts*sizeof(float), &p->auxch);
		auxp = p->auxch.auxp;
		p->npts = npts;
	}
	else if (!(*p->istor)) {  			/* else if requested */
		register long *lp = (long *)auxp;
		do  *lp++ = 0; 				/*   clr old to zero */
		while (--npts);
	}
	p->curp = (float *) auxp;
}

static	DELAYR	*dlrdadr;

delrset(p)
 register DELAYR *p;
{
register long	npts;
register char	*auxp;

	dlrdadr = p;				/* stor structadr for delayw */
	if (*p->istor && p->auxch.auxp != NULL)
		return;
	if ((npts = *p->idlt * esr) < ksmps) {		/* ksmps is min dely */
		initerror("illegal delay time");
		return;
	}
	if ((auxp = p->auxch.auxp) == NULL || npts != p->npts) { /* new space if reqd */
		auxalloc((long)npts*sizeof(float), &p->auxch);
		auxp = p->auxch.auxp;
		p->npts = npts;
	}
	else if (!(*p->istor)) {			/* else if requested */
		register long *lp = (long *)auxp;
		do  *lp++ = 0;				/*   clr old to zero */
		while (--npts);
	}
	p->curp = (float *) auxp;
}

delwset(p)
 register DELAYW *p;
{
	p->delayr = dlrdadr;				/* adr delayr struct */
}

tapset(p)
 register DELTAP *p;
{
	p->delayr = dlrdadr;				/* adr delayr struct */
}

delay(p)
 register DELAY *p;
{
register float	*ar, *asig, *curp, *endp;
register int	nsmps = ksmps;

	ar = p->ar;
	asig = p->asig;
	curp = p->curp;
	endp = (float *) p->auxch.endp;
	do {
		*ar++ = *curp;
		*curp = *asig++;
		if (++curp >= endp)
			curp = (float *) p->auxch.auxp;
	}
	while (--nsmps);
	p->curp = curp;					/* sav the new curp */
}

delayr(p)
 register DELAYR *p;
{
register float	*ar, *curp, *endp;
register int	nsmps = ksmps;

	ar = p->ar;
	curp = p->curp;
	endp = (float *) p->auxch.endp;
	do {
		*ar++ = *curp++;
		if (curp >= endp)
			curp = (float *) p->auxch.auxp;
	}
	while (--nsmps);
}							/* leave old curp */

delayw(p)
 register DELAYW *p;
{
register DELAYR	*q = p->delayr;
register float	*asig, *curp, *endp;
register int	nsmps = ksmps;
	
	asig = p->asig;
	curp = q->curp;
	endp = (float *) q->auxch.endp;
	do {
		*curp = *asig++;
		if (++curp >= endp)
			curp = (float *) q->auxch.auxp;
	}
	while (--nsmps);
	q->curp = curp;					/* now sav new curp */
}

deltap(p)
 register DELTAP *p;
{
register DELAYR	*q = p->delayr;
register float	*ar, *tap, *endp;
register int	nsmps = ksmps;
	
	ar = p->ar;
	if ((tap = q->curp - (long)(*p->xdlt * esr)) < (float *) q->auxch.auxp)
		tap += q->npts;
	endp = (float *) q->auxch.endp;
	do {
		if (tap >= endp)
			tap -= q->npts;
		*ar++ = *tap++;
	}
	while (--nsmps);
}

deltapi(p)
 register DELTAP *p;
{
register DELAYR	*q = p->delayr;
register float	*ar, *tap, *prv, *begp, *endp;
register int	nsmps = ksmps;
register long	idelsmps;
	float	delsmps, delfrac;

	ar = p->ar;
	begp = (float *) q->auxch.auxp;
	endp = (float *) q->auxch.endp;
	if (!p->XINCODE) {
		delsmps = *p->xdlt * esr;
		idelsmps = delsmps;
		delfrac = delsmps - idelsmps;
		if ((tap = q->curp - idelsmps) < begp)
			tap += q->npts;
		do {
			if (tap >= endp)
				tap -= q->npts;
			if ((prv = tap - 1) < begp)
				prv += q->npts;
			*ar++ = *tap + (*prv - *tap) * delfrac;
			tap++;
		}
		while (--nsmps);
	}
	else {
		register float *timp = p->xdlt, *curq = q->curp;
		do {
			delsmps = *timp++ * esr;
			idelsmps = delsmps;
			delfrac = delsmps - idelsmps;
			if ((tap = curq++ - idelsmps) < begp)
				tap += q->npts;
			else if (tap >= endp)
				tap -= q->npts;
			if ((prv = tap - 1) < begp)
				prv += q->npts;
			*ar++ = *tap + (*prv - *tap) * delfrac;
		}
		while (--nsmps);
	}
}

del1set(p)
 register DELAY1 *p;
{
	if (!(*p->istor))
		p->sav1 = 0.;
}

delay1(p)
 register DELAY1 *p;
{
register float	*ar, *asig;
register int	nsmps = ksmps - 1;

	ar = p->ar;
	asig = p->asig;
	*ar++ = p->sav1;
	if (nsmps) {
		do *ar++ = *asig++;
		while (--nsmps);
	}
	p->sav1 = *asig;
}

cmbset(p)
 register COMB *p;
{
register long	lpsiz, nbytes;

	if ((lpsiz = *p->ilpt * esr) <= 0) {
		initerror("illegal loop time");
		return;
	}
	nbytes = lpsiz * sizeof(float);
	if (p->auxch.auxp == NULL || nbytes != p->auxch.size) {
		auxalloc((long)nbytes, &p->auxch);
		p->pntr = (float *) p->auxch.auxp;
		p->prvt = 0.;
	}
	else if (!(*p->istor)) {
		register long *fp = (long *) p->auxch.auxp;
		p->pntr = (float *) fp;
		do  *fp++ = 0.;
		while (--lpsiz);
		p->prvt = 0.;
	}
}

comb(p)
 register COMB *p;
{
register int	nsmps = ksmps;
register float	*ar, *asig, *xp, *endp;

	if (p->prvt != *p->krvt) {
		p->coef = exp((double)(log001 * *p->ilpt / *p->krvt));
		p->prvt = *p->krvt;
	}
	xp = p->pntr;
	endp = (float *) p->auxch.endp;
	ar = p->ar;
	asig = p->asig;
	do {
		*ar++ = *xp;
		*xp *= p->coef;
		*xp += *asig++;
		if (++xp >= endp)
			xp = (float *) p->auxch.auxp;
	}
	while (--nsmps);
	p->pntr = xp;
}

alpass(p)
 register COMB *p;
{
register int	nsmps = ksmps;
register float	*ar, *asig, *xp, *endp;
register float	y, z;

	if (p->prvt != *p->krvt) {
		p->coef = exp((double)(log001 * *p->ilpt / *p->krvt));
		p->prvt = *p->krvt;
	}
	xp = p->pntr;
	endp = (float *) p->auxch.endp;
	ar = p->ar;
	asig = p->asig;
	do {
		y = *xp;
		*xp++ = z = p->coef * y + *asig++;
		*ar++ = y - p->coef * z;
		if (xp >= endp)
			xp = (float *) p->auxch.auxp;
	}
	while (--nsmps);
	p->pntr = xp;
}

static	float	revlptimes[6] = {.0297, .0371, .0411, .0437, .005, .0017};
static	long	revlpsiz[6];
static	long	revlpsum;

reverbinit()				/* called once by oload */
{					/*  to init reverb data */
register float	*lptimp = revlptimes;
register long	*lpsizp = revlpsiz;
register int	n = 6;

	revlpsum = 0;
	do {	*lpsizp = (long)(*lptimp++ * esr);
		revlpsum += *lpsizp++;
	}
	while (--n);
}

rvbset(p)
 register REVERB *p;
{
	if (p->auxch.auxp == NULL) {				/* if no space yet, */
		register long	*sizp = revlpsiz;
		auxalloc(revlpsum*sizeof(float),&p->auxch);	/*    allocate it   */
		p->adr1 = p->p1 = (float *) p->auxch.auxp;
		p->adr2 = p->p2 = p->adr1 + *sizp++;
		p->adr3 = p->p3 = p->adr2 + *sizp++;    	/*    & init ptrs   */
		p->adr4 = p->p4 = p->adr3 + *sizp++;
		p->adr5 = p->p5 = p->adr4 + *sizp++;
		p->adr6 = p->p6 = p->adr5 + *sizp++;
		if (p->adr6 + *sizp != (float *) p->auxch.endp) {
			printf("revlpsiz inconsistent\n");
			exit(1);
		}
		p->prvt = 0.;
	}
	else if (!(*p->istor)) {			/* else if istor = 0 */
		register float	*fp = p->adr1;
		register long	nn = revlpsum;
		do  *fp++ = 0.;				/*  clr existing spc */
		while (--nn);
		p->p1 = p->adr1;			/*  and reset	*/
		p->p2 = p->adr2;
		p->p3 = p->adr3;
		p->p4 = p->adr4;
		p->p5 = p->adr5;
		p->p6 = p->adr6;
		p->prvt = 0.;
	}
}

reverb(p)
 register REVERB *p;
{
register float	*asig, *p1, *p2, *p3, *p4, *p5, *p6, *ar, *endp;
	int	nsmps = ksmps;

	if (p->prvt != *p->krvt) {
		float	*lptimp = revlptimes;
		float	logdrvt = log001 / *p->krvt;
		p->c1 = exp(logdrvt * *lptimp++);
		p->c2 = exp(logdrvt * *lptimp++);
		p->c3 = exp(logdrvt * *lptimp++);
		p->c4 = exp(logdrvt * *lptimp++);
		p->c5 = exp(logdrvt * *lptimp++);
		p->c6 = exp(logdrvt * *lptimp++);
	}
	p1 = p->p1;
	p2 = p->p2;
	p3 = p->p3;
	p4 = p->p4;
	p5 = p->p5;
	p6 = p->p6;
	endp = (float *) p->auxch.endp;

	ar = p->ar;
	asig = p->asig;
	do {
		float	cmbsum, y1, y2, z;
		cmbsum = *p1 + *p2 + *p3 + *p4;
		*p1 = p->c1 * *p1 + *asig;
		*p2 = p->c2 * *p2 + *asig;
		*p3 = p->c3 * *p3 + *asig;
		*p4 = p->c4 * *p4 + *asig++;
		p1++; p2++; p3++; p4++;
		y1 = *p5;
		*p5++ = z = p->c5 * y1 + cmbsum;
		y1 -= p->c5 * z;
		y2 = *p6;
		*p6++ = z = p->c6 * y2 + y1;
		*ar++ = y2 - p->c6 * z;
		if (p1 >= p->adr2)	p1 = p->adr1;
		if (p2 >= p->adr3)	p2 = p->adr2;
		if (p3 >= p->adr4)	p3 = p->adr3;
		if (p4 >= p->adr5)	p4 = p->adr4;
		if (p5 >= p->adr6)	p5 = p->adr5;
		if (p6 >= endp) 	p6 = p->adr6;
	}
	while (--nsmps);
	p->p1 = p1;
	p->p2 = p2;
	p->p3 = p3;
	p->p4 = p4;
	p->p5 = p5;
	p->p6 = p6;
}

panset(p)
 register PAN *p;
{
register FUNC *ftp;

	if ((ftp = ftfind(p->ifn)) == NULL)
		return;
	p->ftp = ftp;
	if (*p->imode)
		p->xmul = ftp->flen;
	else	p->xmul = 1.;
	if (*p->ioffset)
		p->xoff = ftp->flen >> 1;
	else	p->xoff = 0;
}

pan(p)
 PAN *p;
{
register float	*r1, *r2, *r3, *r4, *sigp, ch1, ch2, ch3, ch4;
register long	xndx, yndx, flen;
register int	nsmps = ksmps;
register FUNC	*ftp;

	ftp = p->ftp;
	flen = ftp->flen;
	xndx = (long)(*p->kx * p->xmul) - p->xoff;
	yndx = (long)(*p->ky * p->xmul) - p->xoff;
	if (xndx < 0L || xndx > flen
	 || yndx < 0L || yndx > flen) {
		register long xt, yt, off = flen >>1;
		xt = xndx - off;
		yt = yndx - off;
		if (xt*xt > yt*yt) {
			if (xt < 0) xt = -xt;
			yndx = yt * off / xt + off;
		}
		else {
			if (yt < 0) yt = -yt;
			xndx = xt * off / yt + off;
		}
		if (xndx < 0)		xndx = 0;
		else if (xndx > flen)	xndx = flen;
		if (yndx < 0)		yndx = 0;
		else if (yndx > flen)	yndx = flen;
	}
	ch2 = *(ftp->ftable + xndx) * *(ftp->ftable + yndx);
	ch4 = *(ftp->ftable + xndx) * *(ftp->ftable + flen - yndx);
	ch1 = *(ftp->ftable + flen - xndx) * *(ftp->ftable + yndx);
	ch3 = *(ftp->ftable + flen - xndx) * *(ftp->ftable + flen - yndx);
	r1 = p->r1;
	r2 = p->r2;
	r3 = p->r3;
	r4 = p->r4;
	sigp = p->asig;
	do {
		*r1++ = *sigp * ch1;
		*r2++ = *sigp * ch2;
		*r3++ = *sigp * ch3;
		*r4++ = *sigp * ch4;
		sigp++;
	}
	while (--nsmps);
}
