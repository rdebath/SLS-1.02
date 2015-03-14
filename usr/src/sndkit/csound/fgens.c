#include "cs.h"			/*					FGENS.C		*/
#include "soundio.h"
#include "window.h"
#include <math.h>

#define	FMAX	100
#define	GENMAX	15

typedef	int	(*GEN)();

int	gen01(), gen02(), gen03(), gen04(), gen05();
int	gen06(), gen07(), gen08(), gen09(), gen10();
int	gen11(), gen12(), gen13(), gen14(), gen15();

static	FUNC	*flist[FMAX+1], *ftp;
static	GEN	gensub[GENMAX+1] = { NULL, gen01, gen02, gen03, gen04, gen05,
					   gen06, gen07, gen08, gen09, gen10,
					   gen11, gen12, gen13, gen14, gen15 };
static	EVTBLK	*e;

static	double	tpdlen, tpd360 = 0.017453293;
static	int	fno, guardreq, nargs, fterrcnt;
static	long	flen, flenp1, lenmask;

#define	FTERR(s)	{fterror(s);  return;}

fgens(evtblkp)				/* create ftable using evtblk data */
 EVTBLK	*evtblkp;
{
	long	ltest, lobits, lomod, genum;

	e = evtblkp;
	fterrcnt = 0;
	if ((fno = (int)e->p[1]) < 0) {			/* fno < 0: remove */
		if ((fno = -fno) > FMAX)
			FTERR("illegal ftable number")
		if ((ftp = flist[fno]) == NULL)
			FTERR("ftable does not exist")
		flist[fno] = NULL;
		free((char *)ftp);
		printf("ftable %d now deleted\n",fno);
		return;
	}
	if (!fno)				/* fno = 0, return	*/
		return;
	if (fno > FMAX)
		FTERR("illegal ftable number")
	flen = e->p[3];				/* get user flen	*/
	guardreq = flen & 01;			/* & set guard request flg  */
	flen &= -2;				/* flen now len w/o guardpt */
	flenp1 = flen + 1;			/* & flenp1 is with guardpt */
	if (flen <= 0 || flen > MAXLEN)
		FTERR("illegal table length")
	for (ltest=flen,lobits=0; (ltest & MAXLEN) == 0; lobits++,ltest<<=1);
	if (ltest != MAXLEN)			/* flen must be power-of-2  */
		FTERR("illegal table length")
	lenmask = flen-1;
	if ((genum = e->p[4]) < 0)
		genum = -genum;
	if (!genum || genum > GENMAX)		/* chk legal gen number, */
		FTERR("illegal gen number")
	if ((nargs = e->pcnt - 4) <= 0)		/*   & minimum args, */
		FTERR("insufficient gen arguments")
	ftalloc();				/* now alloc ftable space */
	ftp->flen = flen;			/*  & fill header consts */
	ftp->lenmask = lenmask;
	ftp->lobits = lobits;
	lomod = MAXLEN / flen;
	ftp->lomask = lomod - 1;
	ftp->lodiv = 1./((float)lomod);		/*  & other useful vals */
	tpdlen = twopi / flen;
	(*gensub[genum])();			/* call gen subroutine	*/
	if (fterrcnt)
		return;
	ftresdisp();				/* rescale and display */
}

gen01()				/* read ftable values from a sound file */
{				/* stops reading when table is full	*/
static	ARGLST	arglst = {1};		/* pretend mono	*/
static	OPTXT	optxt;			/* this dummy optext	*/
register SOUNDIN *p;			/* is for sndgetset	*/
register float	*fp = ftp->ftable;
register long	nlocs = flenp1, n, audformat;
extern  short   ulaw_decode[];
extern  int     sndgetset();
	char	tmpspace[sizeof(SOUNDIN)];
        int     fd;

	if (nargs < 3)
		FTERR("insufficient args")
	optxt.t.outlist = &arglst;
	p = (SOUNDIN *) tmpspace;		   /* create temporary opds */
	p->h.optext = &optxt;
	p->ifilno = &e->p[5];
	p->iskptim = &e->p[6];
	p->iformat = &e->p[7];
	if (!(fd = sndgetset(p))) 		/* sndinset to open the file */
		FTERR ("\ngen01 soundin error")
	if (p->endfile) {
	        printf("GEN01 early end-of-file\n");
		goto gn1rtn;
	}
	switch (p->format) {			/* now do simplified soundin */
	    case AE_CHAR: {
		register char *inbufp, *bufend;
		inbufp = p->inbufp;
		bufend = p->bufend;
		do {	*fp++ = (float) ( (short)*inbufp++ << 8 );
			if (inbufp >= bufend) {
				if ((n = readin(fd,p->inbuf,SNDINBUFSIZ)) == 0)
					break;
				inbufp = p->inbuf;
				bufend = p->inbuf + n;
			}
		} while (--nlocs);
	    } break;
	    case AE_ULAW: {
		register unsigned char *inbufp, *bufend;
		inbufp = (unsigned char *) p->inbufp;
		bufend = (unsigned char *) p->bufend;
		do {	*fp++ = (float) ulaw_decode[*inbufp++];
			if (inbufp >= bufend) {
				if ((n = readin(fd,p->inbuf,SNDINBUFSIZ)) == 0)
					break;
				inbufp = (unsigned char *) p->inbuf;
				bufend = (unsigned char *) (p->inbuf + n);
			}
		} while (--nlocs);
	    } break;
	    case AE_SHORT: {
		register short	*inbufp, *bufend;
		inbufp = (short *) p->inbufp;
		bufend = (short *) p->bufend;
		do {	*fp++ = (float) *inbufp++;
			if (inbufp >= bufend) {
				if ((n = readin(fd,p->inbuf,SNDINBUFSIZ)) == 0)
					break;
				inbufp = (short *) p->inbuf;
				bufend = (short *) (p->inbuf + n);
			}
		} while (--nlocs);
	    } break;
	    case AE_LONG: {
		register long  *inbufp, *bufend;
		inbufp = (long *) p->inbufp;
		bufend = (long *) p->bufend;
		do {	*fp++ = (float) *inbufp++;
			if (inbufp >= bufend) {
				if ((n = readin(fd,p->inbuf,SNDINBUFSIZ)) == 0)
					break;
				inbufp = (long *) p->inbuf;
				bufend = (long *) (p->inbuf + n);
			}
		} while (--nlocs);
	    } break;
	    case AE_FLOAT: {
		register float	*inbufp, *bufend;
		inbufp = (float *) p->inbufp;
		bufend = (float *) p->bufend;
		do {	*fp++ = *inbufp++;
			if (inbufp >= bufend) {
				if ((n = readin(fd,p->inbuf,SNDINBUFSIZ)) == 0)
					break;
				inbufp = (float *) p->inbuf;
				bufend = (float *) (p->inbuf + n);
			}
		} while (--nlocs);
	    } break;
	    default: {
	        extern char *getstrformat();
	        sprintf(errmsg,"GEN01 cannot read sformat %s\n",
			     getstrformat((int)p->format));
		fterror(errmsg);
		goto gn1rtn;
	    }
	}
gn1rtn: while (nlocs--)                  /* if file was shorter than gen blk */
	        *fp++ = 0.;              /*     pad out with zeros           */
        close(fd);
}

gen02()				/* read ftable values directly from p-args */
{
register float	*fp = ftp->ftable, *pp = &e->p[5];
register int	nvals = nargs;

	if (nvals > flenp1)
		nvals = flenp1;			/* for all vals up to flen+1 */
	do  *fp++ = *pp++;			/*   copy into ftable	*/
	while (--nvals);
}

gen03()
{
	int	ncoefs;
	float	xintvl, xscale;
register int	xloc, nlocs;
register float	*fp = ftp->ftable, x, sum, *coefp, *coef0, *coeflim;

	if ((ncoefs = nargs - 2) <= 0)
		FTERR("no coefs present")
	coef0 = &e->p[7];
	coeflim = coef0 + ncoefs;
	if ((xintvl = e->p[6] - e->p[5]) <= 0)
		FTERR("illegal x interval")
	xscale = xintvl / (float)flen;
	xloc = e->p[5] / xscale;		/* initial xloc	*/
	nlocs = flenp1;
	do {					/* for each loc:	*/
		x = xloc++ * xscale;
		coefp = coeflim;
		sum = *--coefp;			/* init sum to coef(n)	*/
		while (coefp > coef0) {
			sum *= x;		/*  & accum by Horner's rule */
			sum += *--coefp;
		}
		*fp++ = sum;
	} while (--nlocs);
}

gen04()
{
register float	*valp, *rvalp, *fp = ftp->ftable;
register int	n, r;
register FUNC	*srcftp;
	float	val, max, maxinv;
	int	srcno, srcpts, ptratio;

	if (nargs < 2)
		FTERR("insufficient args")
	if ((srcno = (int)e->p[5]) <= 0 || srcno > FMAX
	  || (srcftp = flist[srcno]) == NULL)
	  	FTERR("unknown srctable number")
	if (!e->p[6]) {
		srcpts = srcftp->flen;
		valp = &srcftp->ftable[0];
		rvalp = NULL;
	}
	else {
		srcpts = srcftp->flen >>1;
		valp = &srcftp->ftable[srcpts];
		rvalp = valp - 1;
	}
	if ((ptratio = srcpts / flen) < 1)
		FTERR("table size too large")
	if (val = *valp++) {
		if (val < 0.)	val = -val;
		max = val;
		maxinv = 1. / max;
	}
	else {
		max = 0.;
		maxinv = 1.;
	}
	*fp++ = maxinv;
	for (n = flen; n--; ) {
		for (r = ptratio; r--; ) {
			if (val = *valp++) {
				if (val < 0.)	val = -val;
				if (val > max) {
					max = val;
					maxinv = 1. / max;
				}
			}
			if (rvalp != NULL && (val = *rvalp--)) {
				if (val < 0.)	val = -val;
				if (val > max) {
					max = val;
					maxinv = 1. / max;
				}
			}
			*fp++ = maxinv;
		}
	}
	guardreq = 1;			/* disable new guard point */
	e->p[4] = -4.;			/*   and rescaling	   */
}

gen05()
{
register int	nsegs, seglen;
register float	*valp, *fp, *finp;
register float	amp1, mult;

	if ((nsegs = (nargs - 1) >> 1) <= 0)	     /* nsegs = nargs-1 /2 */
		return;
	valp = &e->p[5];
	fp = ftp->ftable;
	finp = fp + flen;
	if (*valp == 0) goto gn5er2;
	do {	amp1 = *valp++;
		if (!(seglen = *valp++)) continue;
		if (seglen < 0) goto gn5er1;
		if ((mult = *valp/amp1) <= 0) goto gn5er2;
		mult = pow( (double)mult, (double)1/seglen );
		while (seglen--) {
			*fp++ = amp1;
			amp1 *= mult;
			if (fp > finp) return;
		}
	} while (--nsegs);
	if (fp == finp)			/* if 2**n pnts, add guardpt */
		*fp = amp1;
	return;

gn5er1: fterror("gen call has negative segment size:");
        return;
gn5er2:	fterror("illegal input vals for gen call, beginning:");
}

gen07()
{
register int	nsegs, seglen;
register float	*valp, *fp, *finp;
register float	amp1, incr;

	if ((nsegs = (nargs - 1) >> 1) <= 0)	     /* nsegs = nargs-1 /2 */
		return;
	valp = &e->p[5];
	fp = ftp->ftable;
	finp = fp + flen;
	do {	amp1 = *valp++;
		if (!(seglen = *valp++)) continue;
		if (seglen < 0) goto gn7err;
		incr = (*valp - amp1) / seglen;
		while (seglen--) {
			*fp++ = amp1;
			amp1 += incr;
			if (fp > finp) return;
		}
	} while (--nsegs);
	if (fp == finp)			/* if 2**n pnts, add guardpt */
		*fp = amp1;
        return;

gn7err: fterror("gen call has negative segment size:");
}

gen06()
{
register float	*segp, *extremp, *inflexp, *segptsp, *fp, *finp;
	float	y, diff2;
register int	pntno, pntinc, nsegs, npts;

	if ((nsegs = (nargs - 1) >>1) < 1)
		FTERR("insufficient args")
	fp = ftp->ftable;
	finp = fp + flen;
	pntinc = 1;
	for (segp = &e->p[3]; nsegs > 0; nsegs--) {
		segp += 2;
		segptsp = segp + 1;
		if ((npts = *segptsp) < 0)
			FTERR("negative segsiz")
		if (pntinc > 0) {
			pntno = 0;
			inflexp = segp + 2;
			extremp = segp;
		}
		else {
			pntno = npts;
			inflexp = segp;
			extremp = segp + 2;
		}
		diff2 = (*inflexp - *extremp) / 2.;
		for ( ; npts > 0 && fp < finp; pntno += pntinc, npts--) {
			y = (float)pntno / *segptsp;
			*fp++ = (3.-y) * y * y * diff2 + *extremp;
		}
		pntinc = -pntinc;
	}
	*fp = *(segp + 2);			/* write last target point */
}

gen08()
{
register float	R, x, c3, c2, c1, c0, *fp, *fplim, *valp;
	float	f2, f1, f0, df1, df0, dx01, dx02, dx12, curx;
	float	slope, resd1, resd0;
	int	nsegs, npts;

	if ((nsegs = (nargs - 1) >>1) <= 0)
		FTERR("insufficient args");
	valp = &e->p[5];
	fp = ftp->ftable;
	fplim = fp + flen;
	f0 = *valp++;			/* 1st 3 params give vals at x0, x1  */
	if ((dx01 = *valp++) <= 0.)	/*	and dist between	     */
		FTERR("illegal x interval");
	f1 = *valp++;
	curx = df0 = 0.;		/* init x to origin; slope at x0 = 0 */
	do {				/* for each spline segmnt (x0 to x1) */
	    if (nsegs > 1) {			/* if another seg to follow  */
		if ((dx12 = *valp++) <= 0.)	/*    read its distance	     */
			FTERR("illegal x interval");
		f2 = *valp++;			/*    and the value at x2    */
		dx02 = dx01 + dx12;
		df1 = ( f2*dx01*dx01 + f1*(dx12-dx01)*dx02 - f0*dx12*dx12 )
			/ (dx01*dx02*dx12);
	    }				   /* df1 is slope of parabola at x1 */
	    else df1 = 0.;
	    if ((npts = dx01 - curx) > fplim - fp)
		npts = fplim - fp;
	    if (npts > 0) {			/* for non-trivial segment: */
		slope = (f1 - f0) / dx01;	/*   get slope x0 to x1	    */
		resd0 = df0 - slope;		/*   then residual slope    */
		resd1 = df1 - slope;		/*     at x0 and x1	    */
		c3 = (resd0 + resd1) / (dx01*dx01);
		c2 = - (resd1 + 2.*resd0) / dx01;
		c1 = df0;			/*   and calc cubic coefs   */
		c0 = f0;
		for (x = curx; npts>0; --npts, x += 1.) {
		    R = c3;
		    R *= x;
		    R += c2;	     /* f(x) = ((c3 x + c2) x + c1) x + c0  */
		    R *= x;
		    R += c1;
		    R *= x;
		    R += c0;
		    *fp++ = R;			/* store n pts for this seg */
		}
		curx = x;
	    }
	    curx -= dx01;		/* back up x by length last segment */
	    dx01 = dx12;		/* relocate to the next segment	*/
	    f0 = f1;			/*   by assuming its parameters	*/
	    f1 = f2;
	    df0 = df1;
	}
	while (--nsegs && fp<fplim);	/* loop for remaining segments	*/
	while (fp <= fplim)
	    *fp++ = f0;			/* & repeat the last value	*/
}

gen09()
{
register int	hcnt;
register float	*valp, *fp, *finp;
	double	phs, inc, amp;

	if ((hcnt = nargs / 3) <= 0)		/* hcnt = nargs / 3 */
		return;
	valp = &e->p[5];
	finp = &ftp->ftable[flen];
	do	for (inc=(*valp++)*tpdlen, amp=(*valp++),
		     phs=(*valp++)*tpd360, fp=ftp->ftable; fp<=finp; fp++) {
			*fp += sin(phs) * amp;
			if ((phs += inc) >= twopi)
				phs -= twopi;
		}
	while (--hcnt);
}

gen10()
{
register long	phs, hcnt;
register float	amp, *fp, *finp;

	if ((hcnt = nargs) <= 0)			/* hcnt is nargs   */
		return;
	finp = &ftp->ftable[flen];
	do if ((amp = e->p[hcnt+4]) != 0)		/* for non-0 amps,  */
		for (phs=0, fp=ftp->ftable; fp<=finp; fp++) {
			*fp += sin(phs*tpdlen) * amp;	/* accum sin pts  */
			phs += hcnt;			/* phsinc is hno   */
			phs &= lenmask;
		}
	while (--hcnt);
}

gen11()
{
register float  *fp, *finp;
register long   phs;
	double	x;
	float	denom, r, scale;
	int	n, k;
  
	if (nargs < 1)
		FTERR ("insufficient arguments");
	if ((n = e->p[5]) < 1)
		FTERR ("nh partials < 1");
	k = 1;
	r = 1.;
	if (nargs > 1)
		k = e->p[6];
	if (nargs > 2)
		r = e->p[7];
	fp = ftp->ftable;
	finp = fp + flen;
	if (nargs == 1 || k == 1 && r == 1.) {     /* simple "buzz" case */
		int tnp1;
		float pdlen;

		tnp1 = (n << 1) + 1;
		scale = .5 / n;
		pdlen = tpdlen / 2.;
		for (phs = 0; fp <= finp; phs++) {
			x = phs * pdlen;
			if (!(denom = sin(x)))
				*fp++ = 1.;
			else *fp++ = (sin(tnp1 * x) / denom - 1.) * scale;
		}
	}
	else {                                   /* complex "gbuzz" case */
		float numer, twor, rsqp1, rtn, rtnp1, absr;
		int   km1, kpn, kpnm1;

		km1   = k - 1;
		kpn   = k + n;
		kpnm1 = kpn - 1;
		twor  = r * 2.;
		rsqp1 = r * r + 1.;
		rtn   = pow((double) r, (double) n);
		rtnp1 = rtn * r;
		if ((absr = fabs(r)) > .999 && absr < 1.001)
			scale = 1. / n;
		else scale = (1. - absr) / (1. - fabs(rtn));
		for (phs=0; fp <= finp; phs++) {
			x = phs * tpdlen;
			numer = cos(x*k) - r * cos(x*km1) - rtn * cos(x*kpn)
				+ rtnp1 * cos(x*kpnm1);
			if ((denom = rsqp1 - twor*cos(x)) > .0001
			  || denom < -.0001)
			  	*fp++ = numer / denom * scale;
			else *fp++ = 1.;
		}
	}
}

gen12()
{
static double coefs[] = { 3.5156229, 3.0899424, 1.2067492,
			  0.2659732, 0.0360768, 0.0045813 };
register double *coefp, sum, tsquare, evenpowr, *cplim = coefs + 6;
register int    n;
register float	*fp;
register double xscale;

	if (nargs < 1)
		FTERR ("insufficient arguments");
	xscale = (double) e->p[5] / flen / 3.75;
	for (n=0,fp=ftp->ftable; n<=flen; n++) {
	        tsquare = (double) n * xscale;
		tsquare *= tsquare;
		for (sum=evenpowr=1.0, coefp=coefs; coefp<cplim; coefp++) {
			evenpowr *= tsquare;
			sum += *coefp * evenpowr;
	        }
		*fp++ = (float) log(sum);
	}
}

static	float	mxval, mxscal;

gen13()
{
	mxval = 2.;
	mxscal = .5;
	gn1314();
}

gen14()
{
	mxval = 1.;
	mxscal = 1.;
	gn1314();
}

gn1314()
{
register long	nh, nn;
register float	*mp, *mspace, *hp, *oddhp;
	float	xamp, xintvl, scalfac, sum, prvm;

	if ((nh = nargs - 2) <= 0)
		FTERR("insufficient args")
	if ((xintvl = e->p[5]) <= 0)
		FTERR("illegal xint value")
	if ((xamp = e->p[6]) <= 0)
		FTERR("illegal xamp value")
	e->p[5] = -xintvl;
	e->p[6] = xintvl;
        nn = nh * sizeof(float) / 2;	    /* alloc spc for terms 3,5,7,... */
	mp = mspace = (float *)mcalloc(nn);     /* of 1st row of matrix, and */
	for (nn = (nh + 1) >>1; --nn; )		/* form array of non-0 terms */
		*mp++ = mxval = -mxval;		/*  -val, val, -val, val ... */
	scalfac = 2 / xamp;
	hp = &e->p[7];				/* beginning with given h0,  */
	do {
		mp = mspace;
		oddhp = hp;
		sum = *oddhp++;			/* sum = diag(=1) * this h   */
		for (nn = (nh+1) >>1; --nn; ) {
			oddhp++;		/*  + odd terms * h+2,h+4,.. */
			sum += *mp++ * *oddhp++;
		}
		*hp++ = sum * mxscal;		/* repl this h w. coef (sum) */
		mp = mspace;
		prvm = 1;
		for (nn = nh>>1; --nn > 0; mp++)/* calc nxt row matrix terms */
			*mp = prvm = *mp - prvm;
		mxscal *= scalfac;
	} while (--nh);				/* loop til all h's replaced */
	free((char *)mspace);
printf("calling gen03 with");
hp = &e->p[5];
for (nn = nargs; nn--; )
  printf(" %6.2f",*hp++);
putchar('\n');
	gen03();				/* then call gen03 to write */
}

gen15()
{
	float	xint, xamp, hsin[PMAX/2], h, angle;
register float	*fp, *cosp, *sinp;
register int	n, nh;
register long	*lp, *lp13;

	if (nargs & 01)
		FTERR("uneven number of args");
	nh = (nargs - 2) >>1;
	fp = &e->p[5];					/* save p5, p6	*/
	xint = *fp++;
	xamp = *fp++;
	for (n = nh, cosp = fp, sinp = hsin; n > 0; n--) {
		h = *fp++;				/* rpl h,angle pairs */
		angle = *fp++ * tpd360;
		*cosp++ = h * cos((double)angle);	/*  with h cos angle */
		*sinp++ = h * sin((double)angle);	/* and save the sine */
	}
	nargs -= nh;
	gen13();					/* call gen13	*/
	if (fterrcnt) return;
	ftresdisp();					/* and display fno   */
	lp13 = (long *)ftp;
	fno++;					/* alloc eq. space for fno+1 */
	ftalloc();
	for (lp = (long *)ftp; lp < (long *)ftp->ftable; )  /* & copy header */
		*lp++ = *lp13++;
	fp = &e->p[5];
	*fp++ = xint;					/* restore p5, p6,   */
	*fp++ = xamp;
	for (n = nh-1, sinp = hsin+1; n > 0; n--)	/* then skip h0*sin  */
		*fp++ = *sinp++;			/* & copy rem hn*sin */
	nargs--;		
	gen14();					/* now draw ftable   */
}

fterror(s)
 char *s;
{
	printf("FTERROR, ftable %d: %s\n",fno,s);
	printf("f%3.0f%8.2f%8.2f%8.2f%8.2f ...\n",
		e->p[1],e->p2orig,e->p3orig,e->p[4],e->p[5]);
	fterrcnt++;
}

ftresdisp()		/* set guardpt, rescale the function, and display it */
{
register float	*fp, *finp = &ftp->ftable[flen];
register float	abs, maxval;
static	WINDAT	dwindow;

	if (!guardreq)				/* if no guardpt yet, do it */
	  ftp->ftable[flen] = ftp->ftable[0];
	if (e->p[4] > 0.) {			/* if genum positve, rescale */
	  for (fp=ftp->ftable, maxval = 0.0; fp<=finp; ) {
	    if ((abs = *fp++) < 0.)
	      abs = -abs;
	    if (abs > maxval)
	      maxval = abs;
	  }
	  if (maxval != 0. && maxval != 1.)
	    for (fp=ftp->ftable; fp<=finp; fp++)
	      *fp /= maxval;
	}
        sprintf(strmsg,"ftable %d:",fno);
	dispset(&dwindow,ftp->ftable,(long)(flen+guardreq),strmsg,0,"ftable");
	display(&dwindow);
}

ftalloc()		/* alloc ftable space for fno (or replace one)  */
{			/*	set ftp to point to that structure	*/
	if ((ftp = flist[fno]) != NULL) {
	    printf("replacing previous ftable %d\n",fno);
	    if (flen != ftp->flen) {    	/* if redraw & diff len, */
		extern INSDS actanchor;
		free((char *)ftp);      	/*   release old space   */
		flist[fno] = NULL;
		if (actanchor.nxtact != NULL) { /*   & chk for danger    */
		    sprintf(errmsg,"ftable %d relocating due to size change\n\
  currently active instruments may find this disturbing", fno);
		    warning(errmsg);
		}
	    }
	    else {				/* else clear it to zero */
	        register float	*fp = ftp->ftable;
		register float	*finp = &ftp->ftable[flen];
		while (fp <= finp)
		    *fp++ = 0;
	    }
	}
	if ((ftp = flist[fno]) == NULL) {	/*   alloc space as reqd */
	    ftp = (FUNC *) mcalloc((long)sizeof(FUNC) + flen*sizeof(float));
	    flist[fno] = ftp;
	}
}

 FUNC *
ftfind(argp)		/* find the ptr to an existing ftable structure */
 float *argp;		/*   called by oscils, etc at init time		*/
{
register int	fno;
register FUNC	*ftp;

	if ((fno = *argp) <= 0 || fno > FMAX || (ftp = flist[fno]) == NULL) {
		sprintf(errmsg, "invalid ftable no. %f", *argp);
		initerror(errmsg);
		return(NULL);
	}
	else return(ftp);
}
