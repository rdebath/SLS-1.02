#include "cs.h"		/*							AOPS.C		*/
#include "aops.h"
#include <math.h>

static	double	eipt3=8.3333333, onept=1.021975, oct;
static	double	log10d20=0.11512925;
static	float	logtwo=0.693147, fzero=0.;
extern	float 	*spin, *spout;
extern  int     spoutactive;

rassign(p)
 register ASSIGN *p;
{
register char *s;

	*p->r = *p->a;
	s = p->ORTXT.outlist->arg[0];
	if (strcmp(s,"sr") == 0)
		esr = *p->a;
	else if (strcmp(s,"kr") == 0)
		ekr = *p->a;
	else if (strcmp(s,"ksmps") == 0) {
		ensmps = *p->a;
		ksmps = *p->a;
	}
	else if (strcmp(s,"nchnls") == 0)
		nchnls = *p->a;
}

assign(p)
 register ASSIGN *p;
{
	*p->r = *p->a;
}

aassign(p)
 register ASSIGN *p;
{
 register float *r, *a;
 register int	nsmps = ksmps;

 	r = p->r;
	a = p->a;
	if (p->XINCODE) {
		do *r++ = *a++;
		while (--nsmps);
	}
	else {
		do *r++ = *a;
		while (--nsmps);
	}
}

init(p)
 register ASSIGN *p;
{
	*p->r = *p->a;
}

ainit(p)
 register ASSIGN *p;
{
 register float	*r, *a;
 register int	nsmps = ksmps;

 	r = p->r;
	a = p->a;
	do  *r++ = *a;
	while (--nsmps);
}

#define	RELATN(OPNAME,OP) OPNAME(p) register RELAT *p; { *p->rbool = (*p->a OP *p->b) ? 1 : 0; /* printf("bool = %d\n",*p->rbool);*/ }

RELATN(gt,>)
RELATN(ge,>=)
RELATN(lt,<)
RELATN(le,<=)
RELATN(eq,==)
RELATN(ne,!=)

#define	LOGCLX(OPNAME,OP) OPNAME(p) register LOGCL *p; { *p->rbool = (*p->ibool OP *p->jbool) ? 1 : 0; }

LOGCLX(and,&&)
LOGCLX(or,||)

#define	KK(OPNAME,OP) OPNAME(p) register AOP *p; { *p->r = *p->a OP *p->b; }

KK(addkk,+)
KK(subkk,-)
KK(mulkk,*)
KK(divkk,/)

#define KA(OPNAME,OP) OPNAME(p) register AOP *p; {	\
	register int	nsmps = ksmps;			\
	register float	*r, a, *b;			\
	r = p->r;					\
	a = *p->a;					\
	b = p->b;					\
	do *r++ = a OP *b++;				\
	while (--nsmps);				\
}

KA(addka,+)
KA(subka,-)
KA(mulka,*)
KA(divka,/)

#define AK(OPNAME,OP) OPNAME(p) register AOP *p; {	\
	register int	nsmps = ksmps;			\
	register float	*r, *a, b;			\
	r = p->r;					\
	a = p->a;					\
	b = *p->b;					\
	do *r++ = *a++ OP b;				\
	while (--nsmps);				\
}

AK(addak,+)
AK(subak,-)
AK(mulak,*)
AK(divak,/)

#define AA(OPNAME,OP) OPNAME(p) register AOP *p; {	\
	register int	nsmps = ksmps;			\
	register float	*r, *a, *b;			\
	r = p->r;					\
	a = p->a;					\
	b = p->b;					\
	do *r++ = *a++ OP *b++;				\
	while (--nsmps);				\
}

AA(addaa,+)
AA(subaa,-)
AA(mulaa,*)
AA(divaa,/)

conval(p)
 register CONVAL *p;
{
	if (*p->cond)
		*p->r = *p->a;
	else *p->r = *p->b;
}

aconval(p)
 register CONVAL *p;
{
register float	*r, *s;
register int	nsmps = ksmps;

	r = p->r;
	if (*p->cond)
		s = p->a;
	else s = p->b;
	do *r++ = *s++;
	while (--nsmps);
}
	
int1(p)					/* returns signed whole no. */
 register EVAL *p;
{
	double intpart;
	modf((double)*p->a, &intpart);
	*p->r = intpart;
}

frac1(p)				/* returns positive frac part */
 register EVAL *p;
{
	double intpart, fracpart;
	fracpart = modf((double)*p->a, &intpart);
	*p->r = fracpart;
}

#define LIB1(OPNAME,LIBNAME)	OPNAME(p) register EVAL *p;	 \
				{ *p->r = LIBNAME((double)*p->a); }
LIB1(abs1,fabs)
LIB1(exp01,exp)
LIB1(log01,log)
LIB1(sqrt1,sqrt)
LIB1(sin1,sin)
LIB1(cos1,cos)

#define LIBA(OPNAME,LIBNAME)	OPNAME(p) register EVAL *p; {	\
				register int	nsmps = ksmps;	\
				register float	*r, *a;		\
				r = p->r;			\
				a = p->a;			\
				do *r++ = LIBNAME((double)*a++);\
				while (--nsmps);		\
				}
LIBA(absa,fabs)
LIBA(expa,exp)
LIBA(loga,log)
LIBA(sqrta,sqrt)
LIBA(sina,sin)
LIBA(cosa,cos)

dbamp(p)
 register EVAL *p;
{
	*p->r = log(fabs((double)*p->a)) / log10d20;
}

ampdb(p)
 register EVAL *p;
{
	*p->r = exp((double)*p->a * log10d20);
}

aampdb(p)
 register EVAL *p;
{
register int	nsmps = ksmps;
register float	*r, *a;
	r = p->r;
	a = p->a;
	do *r++ = exp((double)*a++ * log10d20);
	while (--nsmps);
}

ftlen(p)
 register EVAL *p;
{
register FUNC	*ftp;

	if ((ftp = ftfind(p->a)) != NULL)
		*p->r = ftp->flen;
}

octpch(p)
 register EVAL *p;
{
register double	fract;
	fract = modf((double)*p->a, &oct);
	fract *= eipt3;
	*p->r = oct + fract;
}

pchoct(p)
 register EVAL *p;
{
register double fract;
	fract = modf((double)*p->a, &oct);
	fract *= 0.12;
	*p->r = oct + fract;
}

cpsoct(p)
 register EVAL *p;
{
	*p->r = pow((double)2.,(double)*p->a) * onept;
}

acpsoct(p)
 register EVAL *p;
{
register float	*r, *a;
register double	d1, d2;
register int	nsmps = ksmps;
	a = p->a;
	r = p->r;
	d1 = onept;
	d2 = (double)2.;
	do *r++ = pow(d2,(double)*a++) * d1;
	while (--nsmps);
}

octcps(p)
 register EVAL *p;
{
	*p->r = log((double)*p->a / onept) / logtwo;
}

cpspch(p)
 register EVAL *p;
{
register double	fract;
	fract = modf((double) *p->a, &oct);
	fract *= eipt3;
	*p->r = pow((double)2.,oct+fract) * onept;
}

in(p)
 register IN *p;
{
register float	*sp, *ar;
register int	nsmps = ksmps;

	sp = spin;
	ar = p->ar;
	do  *ar++ = *sp++;
	while (--nsmps);
}

ins(p)
 register INS *p;
{
register float	*sp, *ar1, *ar2;
register int	nsmps = ksmps;

	sp = spin;
	ar1 = p->ar1;
	ar2 = p->ar2;
	do {
	    *ar1++ = *sp++;
	    *ar2++ = *sp++;
	}
	while (--nsmps);
}

inq(p)
 register INQ *p;
{
register float	*sp, *ar1, *ar2, *ar3, *ar4;
register int	nsmps = ksmps;

	sp = spin;
	ar1 = p->ar1;
	ar2 = p->ar2;
	ar3 = p->ar3;
	ar4 = p->ar4;
	do {
	    *ar1++ = *sp++;
	    *ar2++ = *sp++;
	    *ar3++ = *sp++;
	    *ar4++ = *sp++;
	}
	while (--nsmps);
}

out(p)
 register OUT *p;
{
register float	*sp, *ap;
register int	nsmps = ksmps;

	ap = p->asig;
	sp = spout;
        if (!spoutactive) {
	    do 	*sp++ = *ap++;
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    do {
	        *sp += *ap++;   sp++;
	    }
	    while (--nsmps);
	}
}

outs(p)
 register OUTS *p;
{
register float	*sp, *ap1, *ap2;
register int	nsmps = ksmps;

	ap1 = p->asig1;
	ap2 = p->asig2;
	sp = spout;
        if (!spoutactive) {
 	    do {
		*sp++ = *ap1++;
		*sp++ = *ap2++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    do {
		*sp += *ap1++;	sp++;
		*sp += *ap2++;	sp++;
	    }
	    while (--nsmps);
	}
}

outq(p)
 register OUTQ *p;
{
register float	*sp, *ap1, *ap2, *ap3, *ap4;
register int	nsmps = ksmps;

	ap1 = p->asig1;
	ap2 = p->asig2;
	ap3 = p->asig3;
	ap4 = p->asig4;
	sp = spout;
        if (!spoutactive) {
	    do {
		*sp = *ap1++;	sp++;
		*sp = *ap2++;	sp++;
		*sp = *ap3++;	sp++;
		*sp = *ap4++;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    do {
		*sp += *ap1++;	sp++;
		*sp += *ap2++;	sp++;
		*sp += *ap3++;	sp++;
		*sp += *ap4++;	sp++;
	    }
	    while (--nsmps);
	}
}

outs1(p)
 register OUT *p;
{
register float	*sp, *ap1;
register int	nsmps = ksmps;

	ap1 = p->asig;
	sp = spout;
	if (!spoutactive) {
 	    do {
		*sp = *ap1++;	sp++;
		*sp = fzero;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    do {
		*sp += *ap1++;	sp += 2;
	    }
	    while (--nsmps);
	}
}

outs2(p)
 register OUT *p;
{
register float	*sp, *ap2;
register int	nsmps = ksmps;

	ap2 = p->asig;
	if (!spoutactive) {
	    sp = spout;
 	    do {
		*sp = fzero;	sp++;
		*sp = *ap2++;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    sp = spout + 1;
	    do {
		*sp += *ap2++;	sp += 2;
	    }
	    while (--nsmps);
	}
}

outq1(p)
 register OUT *p;
{
register float	*sp, *ap1;
register int	nsmps = ksmps;

	ap1 = p->asig;
	sp = spout;
	if (!spoutactive) {
 	    do {
		*sp = *ap1++;	sp++;
		*sp = fzero;	sp++;
		*sp = fzero;	sp++;
		*sp = fzero;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    do {
		*sp += *ap1++;	sp += 4;
	    }
	    while (--nsmps);
	}
}

outq2(p)
 register OUT *p;
{
register float	*sp, *ap2;
register int	nsmps = ksmps;

	ap2 = p->asig;
	if (!spoutactive) {
	    sp = spout;
 	    do {
		*sp = fzero;	sp++;
		*sp = *ap2++;	sp++;
		*sp = fzero;	sp++;
		*sp = fzero;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    sp = spout + 1;
	    do {
		*sp += *ap2++;	sp += 4;
	    }
	    while (--nsmps);
	}
}

outq3(p)
 register OUT *p;
{
register float	*sp, *ap3;
register int	nsmps = ksmps;

	ap3 = p->asig;
	if (!spoutactive) {
	    sp = spout;
 	    do {
		*sp = fzero;	sp++;
		*sp = fzero;	sp++;
		*sp = *ap3++;	sp++;
		*sp = fzero;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    sp = spout + 2;
	    do {
		*sp += *ap3++;	sp += 4;
	    }
	    while (--nsmps);
	}
}

outq4(p)
 register OUT *p;
{
register float	*sp, *ap4;
register int	nsmps = ksmps;

	ap4 = p->asig;
	if (!spoutactive) {
	    sp = spout;
 	    do {
		*sp = fzero;	sp++;
		*sp = fzero;	sp++;
		*sp = fzero;	sp++;
		*sp = *ap4++;	sp++;
	    }
	    while (--nsmps);
	    spoutactive = 1;
	}
        else {
	    sp = spout + 3;
	    do {
		*sp += *ap4++;	sp += 4;
	    }
	    while (--nsmps);
	}
}
