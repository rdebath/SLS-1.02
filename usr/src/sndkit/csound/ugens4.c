#include "cs.h"		/*							UGENS4.C	*/
#include "ugens4.h"
#include <math.h>

porset(p)
 register PORT *p;
{
	p->c2 = pow((double).5, (double)1./ *p->ihtim / ekr);
	p->c1 = 1. - p->c2;
	if (*p->isig >= 0)
		p->yt1 = *p->isig;
}

port(p)
 register PORT *p;
{
	*p->kr = p->yt1 = p->c1 * *p->ksig + p->c2 * p->yt1;
}

tonset(p)
 register TONE *p;
{
	p->c1 = p->prvhp = 0;
	p->c2 = 1;
	if (!(*p->istor))
		p->yt1 = 0;
}

tone(p)
 register TONE *p;
{
register float	*ar, *asig;
register int	nsmps = ksmps;

	if (*p->khp != p->prvhp) {
		float b;
		p->prvhp = *p->khp;
		b = 2. - cos((double)(*p->khp * tpidsr));
		p->c2 = b - sqrt((double)(b * b - 1.));
		p->c1 = 1. - p->c2;
	}
	ar = p->ar;
	asig = p->asig;
	do  *ar++ = p->yt1 = p->c1 * *asig++ + p->c2 * p->yt1;
	while (--nsmps);
}

atone(p)
 register TONE *p;
{
register float	*ar, *asig;
register int	nsmps = ksmps;

	if (*p->khp != p->prvhp) {
		float b;
		p->prvhp = *p->khp;
		b = 2. - cos((double)(*p->khp * tpidsr));
		p->c2 = b - sqrt((double)(b * b - 1.));
		p->c1 = 1. - p->c2;
	}
	ar = p->ar;
	asig = p->asig;
	do {
		*ar++ = p->yt1 = p->c2 * (p->yt1 + *asig);
		p->yt1 -= *asig++;		/* yt1 contains yt1-xt1 */
	}
	while (--nsmps);
}

rsnset(p)
 register RESON *p;
{
 register int scale;
        p->scale = scale = *p->iscl;
	if (scale && scale != 1 && scale != 2) {
	        sprintf(errmsg,"illegal reson iscl value, %f",*p->iscl);
		initerror(errmsg);
	}
	p->prvcf = p->prvbw = -100.;
	if (!(*p->istor))
		p->yt1 = p->yt2 = 0.;
}

reson(p)
 register RESON *p;
{
register int	flag = 0, nsmps = ksmps;
register float	*ar, *asig;
register float	c3p1, c3t4, omc3, c2sqr;

	if (*p->kcf != p->prvcf) {
		p->prvcf = *p->kcf;
		p->cosf = cos((double)(*p->kcf * tpidsr));
		flag = 1;
	}
	if (*p->kbw != p->prvbw) {
		p->prvbw = *p->kbw;
		p->c3 = exp((double)(*p->kbw * mtpdsr));
		flag = 1;
	}
	if (flag) {
		c3p1 = p->c3 + 1.;
		c3t4 = p->c3 * 4.;
		omc3 = 1 - p->c3;
		p->c2 = c3t4 * p->cosf / c3p1;		/* -B, so + below */
		c2sqr = p->c2 * p->c2;
		if (p->scale == 1)
			p->c1 = omc3 * sqrt((double)1. - c2sqr / c3t4);
		else if (p->scale == 2)
			p->c1 = sqrt((double)((c3p1*c3p1-c2sqr) * omc3/c3p1));
		else p->c1 = 1.;
	}
	asig = p->asig;
	ar = p->ar;
	do {
		*ar = p->c1 * *asig++ + p->c2 * p->yt1 - p->c3 * p->yt2;
		p->yt2 = p->yt1;
		p->yt1 = *ar++;
	}
	while (--nsmps);
}

areson(p)
 register RESON *p;
{
register int	flag = 0, nsmps = ksmps;
register float	*ar, *asig;
register float	c3p1, c3t4, omc3, c2sqr;

	if (*p->kcf != p->prvcf) {
		p->prvcf = *p->kcf;
		p->cosf = cos((double)(*p->kcf * tpidsr));
		flag = 1;
	}
	if (*p->kbw != p->prvbw) {
		p->prvbw = *p->kbw;
		p->c3 = exp((double)(*p->kbw * mtpdsr));
		flag = 1;
	}
	if (flag) {
		c3p1 = p->c3 + 1.;
		c3t4 = p->c3 * 4.;
		omc3 = 1 - p->c3;
		p->c2 = c3t4 * p->cosf / c3p1;
		c2sqr = p->c2 * p->c2;
		if (p->scale == 1)			/* i.e. 1 - A(reson) */
			p->c1 = 1. - omc3 * sqrt((double)1. - c2sqr / c3t4);
		else if (p->scale == 2)
			p->c1 = 1.-sqrt((double)((c3p1*c3p1-c2sqr)*omc3/c3p1));
		else p->c1 = 0.;
	}
	asig = p->asig;
	ar = p->ar;
	do {
		*ar = p->c1 * *asig + p->c2 * p->yt1 - p->c3 * p->yt2;
		p->yt2 = p->yt1;
		p->yt1 = *ar++ - *asig++;	/* yt1 contains yt1-xt1 */
	}
	while (--nsmps);
}

static	LPREAD	*lprdadr;
static	char	lpfilname[10];

lprdset(p)
 register LPREAD *p;
{
register LPHEADER *lph;
register MEMFIL	*mfp;
	long	totvals;  /* NB - presumes sizeof(float) == sizeof(long) !! */

	lprdadr = p;				     /* adr opds for lpreson */
	sprintf(lpfilname,"lp.%d",(int)*p->ifilno);	/* construct filename */
        if ((mfp = p->mfp) != NULL && strcmp(mfp->filename,lpfilname) == 0)
	        goto lpend;                             /* rtn if file prv known */
	if ((mfp = ldmemfile(lpfilname)) == NULL) {     /* else read file  */
		sprintf(errmsg,"lpread cannot read %s",lpfilname);
		goto lperr;
	}
	p->mfp = mfp;                                   /*  & record facts   */
	lph = (LPHEADER *) mfp->beginp;
	if (lph->lpmagic == LP_MAGIC) {			/* Header on file:   */
		p->headlongs = lph->headersize/sizeof(long);/* hdsiz in longs */
		if (*p->inpoles || *p->ifrmrate)
			warning("lpheader overriding inputs");
		if (lph->srate != esr)
			warning("lpfile srate != orch sr");
		p->npoles = lph->npoles;		/* note npoles, etc. */
		p->nvals = lph->nvals;
		p->framrat16 = lph->framrate * 65536.;	/* scaled framno cvt */
	}
	else if (BYTREVL(lph->lpmagic) == LP_MAGIC) {	/* Header reversed:  */
		sprintf(errmsg,"file %s bytes are in wrong order",lpfilname);
		goto lperr;
	}
	else {						/* No Header on file:*/
		p->headlongs = 0;
		p->npoles = *p->inpoles;		/*  data from inargs */
		p->nvals = p->npoles + 4;
		p->framrat16 = *p->ifrmrate * 65536.;
		if (!p->npoles || !p->framrat16) {
			sprintf(errmsg,"insufficient args and no file header");
			goto lperr;
		}
	}
	if (p->npoles > MAXPOLES) {
		sprintf(errmsg,"npoles > MAXPOLES");
		goto lperr;
	}
	totvals = (mfp->length/sizeof(long)) - p->headlongs;        /* see NB above!! */
	p->lastfram16 = (((totvals - p->nvals) / p->nvals) << 16) - 1;
        if (odebug) printf("npoles %ld, nvals %ld, totvals %ld, lastfram16 = %lx\n",
			   p->npoles, p->nvals, totvals, p->lastfram16);
lpend:	p->lastmsg = 0;
	return;

lperr:	initerror(errmsg);
}

lpread(p)
 register LPREAD *p;
{
register float	*bp, *np, *cp;
register long	nn, framphase;
	float	fract;

	if ((framphase = *p->ktimpt * p->framrat16) < 0) { /* for kfram reqd */
		perferror("lpread timpnt < 0");
		return;
	}
	if (framphase > p->lastfram16) {		/* not past last one */
		framphase = p->lastfram16;
		if (!p->lastmsg) {
			p->lastmsg = 1;
			warning("lpread ktimpnt truncated to last frame");
		}
	}
	nn = (framphase >> 16) * p->nvals + p->headlongs;        /* see NB above!! */
	bp = (float *)p->mfp->beginp + nn;		/* locate begin this frame */
	np = bp + p->nvals;		                /* & interp betw adj frams */
	fract = (framphase & 0x0FFFFL) / 65536.;
	*p->krmr = *bp + (*np - *bp) * fract;	bp++;	np++; /* for 4 rslts */
	*p->krmo = *bp + (*np - *bp) * fract;	bp++;	np++;
	*p->kerr = *bp + (*np - *bp) * fract;	bp++;	np++;
	*p->kcps = *bp + (*np - *bp) * fract;	bp++;	np++;
	cp = p->kcoefs;
	nn = p->npoles;					      /* & n coefs */
	do {
	    *cp = *bp + (*np - *bp) * fract;
	    cp++; bp++; np++;
	}
	while (--nn);
        if (odebug) {
	    printf("phase:%lx fract:%6.2f rmsr:%6.2f rmso:%6.2f kerr:%6.2f kcps:%6.2f\n",
		framphase,fract,*p->krmr,*p->krmo,*p->kerr,*p->kcps);
	    cp = p->kcoefs;
	    nn = p->npoles;
	    do    printf(" %6.2f",*cp++);
	    while (--nn);
	    putchar('\n');
	}
}

lprsnset(p)
 register LPRESON *p;
{
register LPREAD *q;

	p->lpread = q = lprdadr;		    /* get adr lpread struct */
	p->circjp = p->circbuf;
	p->jp2lim = p->circbuf + (q->npoles << 1);  /* npoles det circbuflim */
}

lpreson(p)
 register LPRESON *p;
{
register LPREAD *q = p->lpread;
register int	nn, nsmps = ksmps;
register float	*coefp, *pastp, *jp, *jp2, *rslt = p->ar, *asig = p->asig;
	float	x;

	jp = p->circjp;
	jp2 = jp + q->npoles;
	do {
		x = *asig++;
		coefp = q->kcoefs;		/* using lpread interp coefs */
		pastp = jp;
		nn = q->npoles;
		do  x += *coefp++ * *pastp++;
		while (--nn);
		*jp++ = *jp2++ = x;
		*rslt++ = x;
		if (jp2 >= p->jp2lim) {
			jp2 = jp;
			jp = p->circbuf;
		}
	} while (--nsmps);
	p->circjp = jp;
}

lpfrsnset(p)
 register LPFRESON *p;
{
 	p->lpread = lprdadr;
	p->prvratio = 1;
	p->d = 0;
	p->prvout = 0;
}

lpfreson(p)
 register LPFRESON *p;
{
register LPREAD	*q = p->lpread;
register int	nn, nsmps = ksmps;
register float	*coefp, *pastp, *pastp1, *rslt = p->ar, *asig = p->asig;
	float	x, temp1, temp2, ampscale, cq;

	if (*p->kfrqratio != p->prvratio) {		/* for new freqratio */
		if (*p->kfrqratio <= 0.) {
			sprintf(errmsg,"illegal frqratio, %5.2",*p->kfrqratio);
			perferror(errmsg);
			return;
		}					/*	calculate d  */
		p->d = (*p->kfrqratio - 1.) / (*p->kfrqratio + 1.);
		p->prvratio = *p->kfrqratio;
	}
	if (p->d != 0.) {				/* for non-zero d,   */
		coefp = q->kcoefs;
		nn = q->npoles - 1;
		do {
			temp1 = p->d * *coefp++;	/*    shift formants */
			*coefp += temp1;
		}
		while (--nn);
		ampscale = 1. / (1. - p->d * *coefp);	/*    & reset scales */
		cq = (1. - p->d * p->d) * ampscale;
	}
	else {
		cq = 1.;
		ampscale = 1.;
	}
	x = p->prvout;
	do {
		nn = q->npoles - 1;
		pastp  = pastp1 = p->past + nn;
		temp1 = *pastp;
		*pastp = cq * x - p->d * *pastp;
		pastp--;
		do {
			temp2 = *pastp;
			*pastp = (*pastp1 - *pastp) * p->d + temp1;
			pastp--;   pastp1--;
			temp1 = temp2;
		}
		while (--nn);
		x = *asig++;
		pastp = p->past;
		coefp = q->kcoefs;
		nn = q->npoles;
		do  x += *coefp++ * *pastp++;
		while (--nn);
		*rslt++ = x * ampscale;
	}
	while (--nsmps);
	p->prvout = x;
}

rmsset(p)
 register RMS *p;
{
register float	b;

	b = 2. - cos((double)(*p->ihp * tpidsr));
	p->c2 = b - sqrt((double)(b * b - 1.));
	p->c1 = 1. - p->c2;
	if (!*p->istor)
		p->prvq = 0;
}

gainset(p)
 register GAIN *p;
{
register float	b;

	b = 2. - cos((double)(*p->ihp * tpidsr));
	p->c2 = b - sqrt((double)(b * b - 1.));
	p->c1 = 1. - p->c2;
	if (!*p->istor)
		p->prvq = p->prva = 0;
}

balnset(p)
 register BALANCE *p;
{
register float	b;

	b = 2. - cos((double)(*p->ihp * tpidsr));
	p->c2 = b - sqrt((double)(b * b - 1.));
	p->c1 = 1. - p->c2;
	if (!*p->istor)
		p->prvq = p->prvr = p->prva = 0;
}

rms(p)
 register RMS *p;
{
register int	nsmps = ksmps;
register float	*asig;
register float	q;

	q = p->prvq;
	asig = p->asig;
	do {	q = p->c1 * *asig * *asig + p->c2 * q;
		asig++;
	}
	while (--nsmps);
	p->prvq = q;
	*p->kr = (float) sqrt((double)q);
}

gain(p)
 register GAIN *p;
{
register int	nsmps = ksmps;
register float	*ar, *asig;
register float	q, a, m, diff, inc;

	q = p->prvq;
	asig = p->asig;
	do {	q = p->c1 * *asig * *asig + p->c2 * q;
		asig++;
	}
	while (--nsmps);
	p->prvq = q;
	if (q = sqrt(q))
		a = *p->krms / q;
	else	a = *p->krms;
	asig = p->asig;
	ar = p->ar;
	nsmps = ksmps;
	if ((diff = a - p->prva) != 0) {
		m = p->prva;
		inc = diff/ksmps;
		do {	*ar++ = *asig++ * m;
			m += inc;
		}
		while (--nsmps);
		p->prva = a;
	}
	else {	do *ar++ = *asig++ * a;
		while (--nsmps);
	}
}

balance(p)
 register BALANCE *p;
{
register int	nsmps = ksmps;
register float	*ar, *asig, *csig;
register float	q, r, a, m, diff, inc;

	q = p->prvq;
	r = p->prvr;
	asig = p->asig;
	csig = p->csig;
	do {	q = p->c1 * *asig * *asig + p->c2 * q;
		r = p->c1 * *csig * *csig + p->c2 * r;
		asig++;	csig++;
	}
	while (--nsmps);
	p->prvq = q;
	p->prvr = r;
	if (q)
		a = sqrt(r/q);
	else	a = sqrt(r);
	asig = p->asig;
	ar = p->ar;
	nsmps = ksmps;
	if ((diff = a - p->prva) != 0) {
		m = p->prva;
		inc = diff/ksmps;
		do {	*ar++ = *asig++ * m;
			m += inc;
		}
		while (--nsmps);
		p->prva = a;
	}
	else {	do *ar++ = *asig++ * a;
		while (--nsmps);
	}
}
