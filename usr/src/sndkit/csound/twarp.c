#include "sort.h"                                       /*    TWARP.C  */
#include "cs.h"   /* for PMAX */

#define TSEGMAX (PMAX/2)

typedef struct { 
	float betbas;
	float durslp;
	float durbas;
	float timbas;
} TSEG;

static TSEG  *tseg, *tpsave, *tplim;

twarp()                 /* time-warp a score section acc to T-statement */
{
	register SRTBLK *bp;
	float absp3, endtime, realt();
	int negp3;

	if ((bp = frstbp) == NULL)              /* if null file,        */
		return;
	while (bp->text[0] != 't')              /*  or can't find a t,  */
		if ((bp = bp->nxtblk) == NULL)
			return;                 /*      we're done      */
	bp->text[0] = 'w';                      /* else mark the t used  */
	if (!realtset(bp))                      /*  and init the t-array */
		return;                         /* (done if t0 60 or err) */
	bp  = frstbp;
	negp3 = 0;
	do      switch(bp->text[0]) {           /* else warp all timvals */
		case 'i':
			if ((absp3 = bp->newp3) < 0) {
				absp3 *= -1;
				negp3++;
			}
			endtime = bp->newp2 + absp3;
			bp->newp2 = realt(bp->newp2);
			bp->newp3 = realt(endtime) - bp->newp2;
			if (negp3) {
				bp->newp3 *= -1.;
				negp3--;
			}
			break;
		case 'a':
			endtime = bp->newp2 + bp->newp3;
			bp->newp2 = realt(bp->newp2);
			bp->newp3 = realt(endtime) - bp->newp2;
			break;
                case 'f':
			bp->newp2 = realt(bp->newp2);
			break;
		case 't':
		case 'w':
		case 's':
		case 'e':
			break;
		default:
			fprintf(stderr,"twarp: illegal opcode\n");
			break;
		}
	while ((bp = bp->nxtblk) != NULL);
}

realtset(bp)
 SRTBLK *bp;
{
	register char *p;
	char c;
	float tempo, betspan, durbas, avgdur, stof();
	register TSEG *tp, *prvtp;

	if (tseg == NULL) {                       /* if no space yet, alloc */
	        tseg = (TSEG *) mmalloc((long)TSEGMAX * sizeof(TSEG));
		tplim = tseg + TSEGMAX-1;
	}
	tp = tpsave = tseg;
	if (bp->pcnt < 2)
		goto error1;
	p = bp->text;                             /* first go to p1       */
	p += 2;
	if ((tp->betbas = stof(p)) != 0)          /* betbas1 must be zero */
		goto error1;
	while ((c = *p++) != SP)
		;
	if ((tempo = stof(p)) <= 0)               /* durbas = 60/tempo    */
		goto error2;
	if (bp->pcnt == 2 && tempo == (float)60)  /* just t0 60 means done */
		return(0);
	tp->durbas = 60/tempo;
	tp->timbas = 0;                           /* timbas1 = 0          */
	while ((c = *p++) != SP && c != LF)
		;
	while (c != LF) {                        /* for each time-tempo pair: */
		prvtp = tp;
		if (++tp > tplim)
			goto error3;
		tp->betbas = stof(p);                   /* betbas = time    */
		while ((c = *p++) != SP && c != LF)
			;
		if (c == LF)
			goto error1;
		if ((tempo = stof(p)) <= 0)             /* get tempo         */
			goto error2;
		if ((betspan = tp->betbas - prvtp->betbas) <= 0) {
			if (betspan < 0)                /* if time = lastime */
				goto error1;
			tp--;                           /* overwrit prvdurbas*/
			tp->durbas = 60./tempo;         /*   with 60/tempo   */
			goto align;                     /* and reloop        */
		}
		tp->durbas = durbas = 60/tempo;         /* else this durbas  */
		avgdur = (durbas + prvtp->durbas)/2.;
		tp->timbas = avgdur*betspan + prvtp->timbas;    /* set timbas*/
		prvtp->durslp = (avgdur - prvtp->durbas)/betspan;/* prvdurslp*/
	  align:while ((c = *p++) != SP && c != LF)
			;
	}
	tp->durslp = 0;                         /* clear last durslp    */
	if (++tp > tplim)
		goto error3;
	tp->betbas = 999999.;                    /* and cap with large betval */
	return(1);

error1: fprintf(stderr,"twarp: t has extra or disordered beat value\n");
	return(0);
error2: fprintf(stderr,"twarp: t has non-positive tempo\n");
	return(0);
error3: fprintf(stderr,"twarp: t segments exceed twarp array\n");
	return(0);
}                 


 float
realt(srctim)
 float srctim;
{
	register TSEG *tp;
	float diff;

	tp = tpsave;
	while (srctim >= (tp+1)->betbas)
		tp++;
        while ((diff = srctim - tp->betbas) < 0)
		tp--;
	tpsave = tp;
	return((tp->durslp * diff + tp->durbas) * diff + tp->timbas);
}
