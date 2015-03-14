#include "sort.h"                                           /*    EXTRACT.C   */

#define INSMAX  256

static char    inslst[INSMAX];                 /*   values set by readxfil     */
static int     onsect, offsect;                /*      "       "       "       */
static float   onbeat, offbeat;                /*      "       "       "       */
static float   ontime, offtime;                /* set by readxfil, mod by w-stmnt */

static SRTBLK *frstout, *prvout;        /* links for building new outlist */

static SRTBLK a0 = { NULL,NULL,0,3,0.,0.,0.,0.,0.,SP,"a 0 0 0\n" };
static SRTBLK f0 = { NULL,NULL,0,2,0.,0.,0.,0.,0.,SP,"f 0 0\n"   };
static SRTBLK e  = { NULL,NULL,0,0,0.,0.,0.,0.,0.,SP,"e\n"       };

readxfil(xfp)                         /* read the extract control file */
  FILE *xfp;
{
	int  flag, all;
	char s[82];

	all = 1;
	flag = 'i';                     /* default -i flag supplied */
	onsect = 1;     onbeat = 0.;    /* other default vals   */
	offsect = 999;  offbeat = 0.;
	while (fscanf(xfp, "%s", s) != EOF) {
		char *c = s;
		int i;
		switch(*c) {
		case 'i':
			all = 0;
		case 'f':
		case 't':
			flag = *c++;
			break;
		default:
			switch(flag) {
			case 'i':
				sscanf(s, "%d", &i);
				inslst[i] = 1;
				all = 0;
				break;
			case 'f':
				sscanf(s, "%d:%f", &onsect, &onbeat);
				break;
			case 't':
				offsect = onsect;       /* default offsect */
				sscanf(s, "%d:%f", &offsect, &offbeat);
			}
		}
	}
	if (all) {
		register char *ip;
		for(ip = &inslst[0]; ip < &inslst[INSMAX]; *ip++ = 1);
	}
	ontime = a0.newp3 = a0.p3val = onbeat;
	offtime = f0.newp2 = f0.p2val = offbeat;
}

extract()                    /* extract instr events within the time period */
{
	register SRTBLK *bp;
	float turnoff, anticip;
	int   warped;
static  int   sectno, a0done;
extern  int   realtset();
extern  float realt();

	if ((bp = frstbp) == NULL)              /* if null file         */
		return;
	if (++sectno > offsect) {               /* or later section,    */
		frstbp = NULL;
		return;                         /*      return          */
	}

        frstout = prvout = NULL;
	if (sectno < onsect) {                  /* for sects preceding, */
	    do  switch(bp->text[0]) {
		case 'f':			/*   include f's at time 0 */
			bp->p2val = bp->newp2 = 1.0;	/* time 1 for now!!*/
			include(bp);
			break;
		case 'w':
		case 's':
		case 'e':
			include(bp);            /*   incl w,s,e verbatim  */
			break;
		case 't':
		case 'i':
		case 'a':
			break;                  /*   but skip all t,i,a   */
		}
	    while ((bp = bp->nxtblk) != NULL);
	}
	else {                                  /* for sections in timespan: */
	    do  switch(bp->text[0]) {
		case 'w':
			warped = realtset(bp);
			if (sectno == onsect && warped)
				ontime = a0.newp3 = realt(onbeat);
			if (sectno == offsect && warped)
				offtime = f0.newp2 = realt(offbeat);
			include(bp);
			break;
		case 't':
			include(bp);
			break;
                case 'f':   
		 casef: if (sectno == onsect && bp->newp2 < ontime)
				bp->newp2 = ontime;
			else if (sectno == offsect && bp->newp2 > offtime)
				break;
			if (sectno == onsect && !a0done) {
				if (onbeat > 0)
					include(&a0);
				a0done++;
			}
			include(bp);
			break;
		case 'i':
			if (!inslst[bp->insno]) /* skip insnos not required */
				break;
			if (bp->newp3 < 0)      /* treat indef dur like f */
				goto casef;
		case 'a':turnoff = bp->newp2 + bp->newp3;   /* i and a: */
			if (sectno == onsect) {
				if (turnoff < ontime)
					break;
				if ((anticip = ontime - bp->newp2) > 0) {
					if ((bp->newp3 -= anticip) < .001)
						break;
                                        bp->p3val -= onbeat - bp->p2val;  
					bp->newp2 = ontime;
					bp->p2val = onbeat;
				}
			}
			if (sectno == offsect) {
				if (bp->newp2 >= offtime)
					break;
				if (turnoff > offtime) {
					bp->newp3 = offtime - bp->newp2;
					bp->p3val = offbeat - bp->p2val;
				}
			}
			if (sectno == onsect && !a0done) {
				if (onbeat > 0)
					include(&a0);
				a0done++;
			}
                        include(bp);
			break;
		case 's':
		case 'e':
			if (sectno == offsect) {
				include(&f0);
				include(&e);
			}
			else include(bp);
			break;
		}
	    while ((bp = bp->nxtblk) != NULL);
	}
	frstbp = frstout;
	if (prvout != NULL)
		prvout->nxtblk = NULL;
}

include(bp)                             /* wire a srtblk into the outlist */
 register SRTBLK *bp;
{
	if (frstout == NULL)                    /* first one is special */
		frstout = bp;
	else prvout->nxtblk = bp;               /* others just add onto list */
	bp->prvblk = prvout;                    /* maintain the backptr      */
	prvout = bp;                            /* and get ready for next    */
}
