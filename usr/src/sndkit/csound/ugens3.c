#include "cs.h"			/*					UGENS3.C	*/
#include "ugens3.h"
#include <math.h>

bzzset(p)
 register BUZZ *p;
{
register FUNC	*ftp;

	if ((ftp = ftfind(p->ifn)) != NULL) {
		p->ftp = ftp;
		if (*p->iphs >= 0)
			p->lphs = *p->iphs / 2. * fmaxlen;
		p->ampcod = (p->XINCODE & 02) ? 1 : 0;
		p->cpscod = (p->XINCODE & 01) ? 1 : 0;
	}
}

buzz(p)
 register BUZZ *p;
{
	FUNC	*ftp;
register float	*ar, *ampp, *cpsp, *ftbl, *loc;
register long	phs, inc, lobits, tnp1, tnphs, nn;
	float	sicvt2, over2n, scal, num, denom;
	int	n;

	ftp = p->ftp;
	ftbl = ftp->ftable;
	sicvt2 = sicvt / 2;		/* for theta/2	*/
	lobits = ftp->lobits;
	ampp = p->xamp;
	cpsp = p->xcps;
	if ((n = *p->knh) <= 0) {		/* fix n = knh */
		perferror("buzz knh <= 0");
		return;
	}
	tnp1 = (n <<1) + 1;			/* calc 2n + 1 */
	over2n = .5 / n;
	scal = *ampp * over2n;
	inc = *cpsp * sicvt2;
	ar = p->ar;
	phs = p->lphs;
	nn = ksmps;
	do {
		tnphs = phs * tnp1 & PMASK;
		loc = ftbl + (tnphs >>lobits);
		num = *loc + (*(loc+1)-*loc) * PFRAC(tnphs);
		loc = ftbl + (phs >>lobits);
		denom = *loc + (*(loc+1)-*loc) * PFRAC(phs);
		if (!denom)
			*ar++ = *ampp;
		else *ar++ = (num / denom - 1) * scal;
		phs += inc;
		phs &= PMASK;
		if (p->ampcod)
			scal = *(++ampp) * over2n;
		if (p->cpscod)
			inc = *(++cpsp)* sicvt2;
	}
	while (--nn);
	p->lphs = phs;
}

gbzset(p)
 register GBUZZ *p;
{
register FUNC	*ftp;

	if ((ftp = ftfind(p->ifn)) != NULL) {
		p->ftp = ftp;
		if (*p->iphs >= 0) {
			p->lphs = *p->iphs * fmaxlen;
			p->prvr = 0;
		}
		p->ampcod = (p->XINCODE & 02) ? 1 : 0;
		p->cpscod = (p->XINCODE & 01) ? 1 : 0;
	}
}

gbuzz(p)
 register GBUZZ *p;
{
	FUNC	*ftp;
register float	*ar, *ampp, *cpsp, *ftbl;
register long	phs, inc, lobits, lenmask, k, km1, kpn, kpnm1, nn;
	long	n;
	float	r, absr, num, denom, scal;

	ftp = p->ftp;
	ftbl = ftp->ftable;
	lobits = ftp->lobits;
	lenmask = ftp->lenmask;
	ampp = p->xamp;
	cpsp = p->xcps;
	k = *p->kk;				/* fix k and n	*/
	if ((n = *p->kn) <= 0) {		/* n must be > 0 */
		perferror("gbuzz knh <= 0");
		return;
	}
	km1 = k - 1;
	kpn = k + n;
	kpnm1 = kpn - 1;
	if ((r = *p->kr) != p->prvr || n != p->prvn) {
		p->twor = r * 2;
		p->rsqp1 = r * r + 1;
		p->rtn = pow((double) r, (double) n);
		p->rtnp1 = p->rtn * r;
		if ((absr = fabs(r)) > .999 && absr < 1.001)
			p->rsumr = 1. / n;
		else p->rsumr = (1 - absr) / (1 - fabs(p->rtn));
		p->prvr = r;
		p->prvn = n;
	}
	scal =  *ampp * p->rsumr;
	inc = *cpsp * sicvt;
	ar = p->ar;
	nn = ksmps;
	do {
		phs = p->lphs >>lobits;
		num = *(ftbl + (phs * k & lenmask))
		    - r * *(ftbl + (phs * km1 & lenmask))
		    - p->rtn * *(ftbl + (phs * kpn & lenmask))
		    + p->rtnp1 * *(ftbl + (phs * kpnm1 & lenmask));
		denom = p->rsqp1 - p->twor * *(ftbl + phs);
		if (denom > .0001 || denom < -.0001)
			*ar++ = num / denom * scal;
		else *ar++ = *ampp;
		if (p->ampcod)
			scal =  p->rsumr * *(++ampp);
		p->lphs += inc;
		p->lphs &= PMASK;
		if (p->cpscod)
			inc = *(++cpsp) * sicvt;
	}
	while (--nn);
}

#define PLUKMIN 64

plukset(p)
 register PLUCK *p;
{
register int	n;
register long	npts, iphs;
register char	*auxp;
register FUNC	*ftp;
register float	*ap, *fp;
	 float	phs, phsinc;
	 static short	rand16();

	if ((npts = esr / *p->icps) < PLUKMIN)  	/* npts is wavelen in sampls */
		npts = PLUKMIN; 			/*  (but at least min size)  */
	if ((auxp = p->auxch.auxp) == NULL || npts > p->maxpts) { /* get newspace    */
		auxalloc((npts+1)*sizeof(float),&p->auxch);
		auxp = p->auxch.auxp;
		p->maxpts = npts;       			/*	if reqd    */
	}
	ap = (float *)auxp;     				/* as float array   */
	if (*p->ifn == 0.0)
		for (n=npts; n--; )     			/* f0: fill w. rands */
			*ap++ = (float)rand16() * dv32768;
	else if ((ftp = ftfind(p->ifn)) != NULL) {
		fp = ftp->ftable;       			/* else from ftable  */
		phs = 0;
		phsinc = ftp->flen/npts;
		for (n=npts; n--; phs += phsinc) {
			iphs = phs;
			*ap++ = *(fp + iphs);
		}
	}
	*ap = *(float *)auxp;				/* last= copy of 1st */
	p->npts = npts;
	p->sicps = (npts * 256. + 128.) / esr;		/* tuned pitch convt */
	p->phs256 = 0;
	p->method = *p->imeth;
	p->param1 = *p->ipar1;
	p->param2 = *p->ipar2;
	switch(p->method) {
	case 1:	/* ignore any given parameters */ 
		break;
	case 2:	/* stretch factor: param1 >= 1 */
		if (p->param1 < 1.)
			initerror("illegal stretch factor(param1) value");
		else p->thresh1 = 32768. / p->param1;
		break;
	case 3: /* roughness factor: 0 <= param1 <= 1 */
		if (p->param1 < 0 || p->param1 > 1)
			initerror("illegal roughness factor(param1) value");
		p->thresh1 = 32768. * p->param1;
		break;
	case 4: /* rough and stretch factor: 0 <= param1 <= 1, param2 >= 1 */
		if (p->param1 < 0 || p->param1 > 1)
			initerror("illegal roughness factor(param1) value");
		else p->thresh1 = 32768. * p->param1;
		if (p->param2 < 1.)
			initerror("illegal stretch factor(param2) value");
		else p->thresh2 = 32768. / p->param2;
		break;
	case 5: /* weighting coeff's: param1 + param2 <= 1 */
		if (p->param1 + p->param2 > 1)
			initerror("coefficients too large(param1 + param2)");
		break;
	case 6: /* ignore any given parameters */
		break;

	default:initerror("unknown method code");
	}
}

pluck(p)
 register PLUCK *p;
{
register float	*ar, *fp;
register long	phs256, phsinc, ltwopi, offset;
register int	nsmps;
	float	frac, diff;
	static  short	rand15();

	ar = p->ar;
	phsinc = *p->kcps * p->sicps;
	phs256 = p->phs256;
	ltwopi = p->npts << 8;
	nsmps = ksmps;
	do {
		offset = phs256 >> 8;	
		fp = (float *)p->auxch.auxp + offset;	/* lookup position  */
		diff = *(fp+1) - *fp;
		frac = (float)(phs256 & 255) / 256.;	/*  w. interpolation */
		*ar++ =	(*fp + diff*frac) * *p->kamp;	/*  gives output val */
		if ((phs256 += phsinc) >= ltwopi) {
			register int nn;
			float	newval, preval;
			phs256 -= ltwopi;		/* at phase wrap,    */
			fp=(float *)p->auxch.auxp;
			preval = *fp;			/*   copy last pnt   */
			*fp = *(fp + p->npts);		/*     to first,     */
			fp++;				/*   apply smoothing */
			nn = p->npts;			/*     up to npts+1  */
			switch(p->method) {
			case 1:	do {			/* simple averaging */
				    newval = (*fp + preval) / 2.; 
				    preval = *fp;
				    *fp++ = newval;
				} while (--nn);
				break;
			case 2: do {			/* stretched avrging */
				    if (rand15() < p->thresh1) {
					newval = (*fp + preval) / 2.;
					preval = *fp;
					*fp++ = newval;
				    }
				    else preval = *fp++;
				} while (--nn);
				break;
			case 3: do {			/* simple drum */
				    if (rand15() < p->thresh1)
  					newval = -(*fp + preval) / 2.;
				    else newval = (*fp + preval) / 2.;
				    preval = *fp;
				    *fp++ = newval;
				} while (--nn);
				break;
			case 4: do {			/* stretched drum */
				    if (rand15() < p->thresh2) {	
					if (rand15() < p->thresh1)
			     		    newval = -(*fp + preval) / 2.;
					else newval = (*fp + preval) / 2.;
					preval = *fp;
					*fp++ = newval;
				    }	   
				    else preval = *fp++;
				} while (--nn);
				break;
			case 5:	do {			/* weighted avraging */
				    newval = p->param1 * *fp
				    	+ p->param2 * preval;
				    preval = *fp;
				    *fp++ = newval;
				} while (--nn);
				break;
			case 6:	do {		/* 1st order recursive filter*/
				    preval = (*fp + preval)/2.;
				    *fp++ = preval;
				} while (--nn);
				break;
			}
		}	
	}
	while (--nsmps);
	p->phs256 = phs256;
}

#define	RNDMUL	15625L
#define MASK16   0xFFFFL
#define MASK15   0x7FFFL

static short
rand16()	/* quick generate a random short between -32768 and 32767 */
{
static long rand = 1000;
	rand *= RNDMUL;
	rand += 1L;
	rand &= MASK16;
	return((short)rand);
}

static short
rand15()	/* quick generate a random short between 0 and 32767 */
{
static long rand = 1000;
	rand *= RNDMUL;
	rand += 1L;
	rand &= MASK15;
	return((short)rand);
}


rndset(p)
 register RAND *p;
{
	if (*p->iseed >= 0)
		p->rand = *p->iseed * 32768;
	p->ampcod = (p->XINCODE & 02) ? 1 : 0;	/* (not used by krand) */
}

krand(p)
 register RAND *p;
{
	p->rand *= RNDMUL;
	p->rand += 1;
	*p->ar = (float)p->rand * *p->xamp * dv32768;
}

arand(p)
 register RAND *p;
{
register float	*ar;
register short	rand, rndmul = RNDMUL, n = ksmps;
register float	ampscl;

	rand = p->rand;
	ar = p->ar;
	if (!(p->ampcod)) {
		ampscl = *p->xamp * dv32768;
		do {	rand *= rndmul;
			rand += 1;
			*ar++ = (float)rand * ampscl;
		}
		while (--n);
	}
	else {
		register float *xamp = p->xamp;
		do {	rand *= rndmul;
			rand += 1;
			*ar++ = (float)rand * *xamp++ * dv32768;
		}
		while (--n);
	}
	p->rand = rand;			/* save current rand */
}

rhset(p)
 register RANDH *p;
{
	if (*p->iseed >= 0) {			/* new seed:		*/
		p->rand = *p->iseed * 32768;	/*	init rand integ */
		p->phs = 0;			/*	& phs		*/
		p->num1 = *p->iseed;		/*	store fnum	*/
	}
	p->ampcod = (p->XINCODE & 02) ? 1 : 0;	/* (not used by krandh) */
	p->cpscod = (p->XINCODE & 01) ? 1 : 0;
}

krandh(p)
 register RANDH *p;
{
	*p->ar = p->num1 * *p->xamp;		/* rslt = num * amp	*/
	p->phs += (long)(*p->xcps * kicvt);	/* phs += inc		*/
	if (p->phs >= MAXLEN) {			/* when phs overflows,	*/
		p->phs &= PMASK;		/*	mod the phs	*/
		p->rand *= RNDMUL;		/*	& recalc number	*/
		p->rand += 1;
		p->num1 = (float)p->rand * dv32768;
	}
}

randh(p)
 register RANDH *p;
{
register long	phs = p->phs, inc;
register int	n = ksmps;
register float	*ar, *ampp, *cpsp;

	cpsp = p->xcps;
	ampp = p->xamp;
	ar = p->ar;
	inc = *cpsp++ * sicvt;
	do {
		*ar++ = p->num1 * *ampp;	/* rslt = num * amp */
		if (p->ampcod)
			ampp++;
		phs += inc;				/* phs += inc	    */
		if (p->cpscod)
			inc = *cpsp++ * sicvt;
		if (phs >= MAXLEN) {			/* when phs o'flows, */
			phs &= PMASK;
			p->rand *= RNDMUL;		/*   calc new number */
			p->rand += 1;
			p->num1 = (float)p->rand * dv32768;
		}
	}
	while (--n);
	p->phs = phs;
}

riset(p)
 register RANDI *p;
{
	if (*p->iseed >= 0) {			/* new seed:		*/
		p->rand = *p->iseed * 32768;	/*	init rand integ */
		p->rand *= RNDMUL;		/*	to 2nd value	*/
		p->rand += 1;
		p->phs = 0;			/*	& clear phs	*/
		p->num1 = *p->iseed;		/*	store num1,2	*/
		p->num2 = (float)p->rand * dv32768;
		p->dfdmax = (p->num2 - p->num1) / fmaxlen;  /* & diff	*/
	}
	p->ampcod = (p->XINCODE & 02) ? 1 : 0;	/* (not used by krandi) */
	p->cpscod = (p->XINCODE & 01) ? 1 : 0;
}

krandi(p)
 register RANDI *p;
{					/* rslt = (num1 + diff*phs) * amp */
	*p->ar = (p->num1 + (float)p->phs * p->dfdmax) * *p->xamp;
	p->phs += (long)(*p->xcps * kicvt);	/* phs += inc		*/
	if (p->phs >= MAXLEN) {			/* when phs overflows,	*/
		p->phs &= PMASK;		/*	mod the phs	*/
		p->rand *= RNDMUL;		/*	recalc random	*/
		p->rand += 1;
		p->num1 = p->num2;		/*	& new num vals	*/
		p->num2 = (float)p->rand * dv32768;
		p->dfdmax = (p->num2 - p->num1) / fmaxlen;
	}
}

randi(p)
 register RANDI *p;
{
register long	phs = p->phs, inc;
register int	n = ksmps;
register float	*ar, *ampp, *cpsp;

	cpsp = p->xcps;
	ampp = p->xamp;
	ar = p->ar;
	inc = *cpsp++ * sicvt;
	do {
		*ar++ = (p->num1 + (float)phs * p->dfdmax) * *ampp;
		if (p->ampcod)
			ampp++;
		phs += inc;				/* phs += inc	    */
		if (p->cpscod)
			inc = *cpsp++ * sicvt;		/*   (nxt inc)	    */
		if (phs >= MAXLEN) {			/* when phs o'flows, */
			phs &= PMASK;
			p->rand *= RNDMUL;		/*   calc new numbers*/
			p->rand += 1;
			p->num1 = p->num2;
			p->num2 = (float)p->rand * dv32768;
			p->dfdmax = (p->num2 - p->num1) / fmaxlen;
		}
	}
	while (--n);
	p->phs = phs;
}
