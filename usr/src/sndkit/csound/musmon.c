
#include "cs.h"                 /*                                 MUSMON.C        */
#include "soundio.h"

#define	SEGAMPS	01
#define	SORMSG	02

extern	INSTRTXT *instrtxtp[];
extern	INSDS	actanchor, *frstoff;

int	insno;
char	strmsg[100];

float	maxamp[4], *maxampend, smaxamp[4], omaxamp[4];
long	rngcnt[4], srngcnt[4], orngcnt[4];
short	rngflg = 0, srngflg = 0, multichan = 0;

extern  int	Beatmode;
extern  EVTBLK	*RTevtblk;
static  EVTBLK	*scorevtblk;
static	short	offonly = 0;
static	short	sectno = 1;
static	long	kcnt = 0;
static	float	timtot = 0.;
static  float   betsiz, ekrbetsiz;

static void settempo(tempo)
 float tempo;
{
        if (tempo > 0.) {
	    betsiz = 60. / tempo;
	    ekrbetsiz = ekr * betsiz;
	}
}

tempset(p)
 TEMPO *p;
{
        float tempo;

        if ((tempo = *p->istartempo) <= 0.)
	    initerror("illegal istartempo value");
        else {
	    settempo(tempo);
	    p->prvtempo = tempo;
	}
}

tempo(p)
 TEMPO *p;
{
       if (*p->ktempo != p->prvtempo) {
	   settempo(*p->ktempo);
	   p->prvtempo = *p->ktempo;
       }
}

musmon()
{
	EVTBLK	*e;
	int	n;
	float	prvbt, curbt, curp2, nxtim, nxtbt, *maxp, *smaxp;
	long	*rngp, *srngp;
	int     RTsense, segamps, sormsg;

	oload();			/* load orch desblks, etc	*/
	if (synterrcnt) {
	    printf("%d syntax errors in orchestra.   performance cancelled\n",
			synterrcnt);
		exit(1);
	}
	printf("orch now loaded\n");
        multichan = (nchnls > 1) ? 1:0;
	maxampend = &maxamp[nchnls];
	segamps = msglevel & SEGAMPS;
	sormsg = msglevel & SORMSG;

	if (sfread)     			/* if audio-in requested,	*/
		sfopenin();     		/*   open/read?? infile or device	*/
	if (sfwrite)    			/* if audio-out requested,	*/
		sfopenout();    		/*   open the outfile or device	*/
	else printf("not writing to sound disk\n");
	iotranset();    		/* point recv & tran to audio formatter	*/

	RTsense = 0;
	scorevtblk = (EVTBLK *) mcalloc((long)sizeof(EVTBLK));
	e = scorevtblk;
	curp2 = curbt = 0.0;
	if (Beatmode)                   /* if performing from beats	*/
	        settempo(60.);          /*   set the default tempo  	*/
	printf("SECTION 1:\n");
	while (1) {			/* read next score event	*/
		if (!(rdscor(e)))
		         e->opcod = 'e';
	retest:	offonly = 0;
		switch(e->opcod) {
		case 'w':
		        if (Beatmode)                 /* Beatmode: read 'w'  */
			    settempo(e->p2orig);      /*   to init the tempo */
			continue;                     /*   for this section  */
		case 'i':
		case 'f':
		case 'a':
			if (frstoff != NULL) {
			    if (Beatmode) {
				if (frstoff->offbet < e->p2orig)
				    goto setoff;
			    }
			    else {
				if (frstoff->offtim < e->p[2])
				    goto setoff;
			    }
			}
			nxtim = e->p[2];
			nxtbt = e->p2orig;
			break;
		case 's':
		case 'e':
			if (frstoff == NULL)
				goto scode;
	    setoff:	nxtim = frstoff->offtim;
	      		nxtbt = frstoff->offbet;
			offonly = 1;
			break;
		default:
			printf("error in score.  illegal opcode %c (ASCII %d)\n",
			       e->opcod, e->opcod);
			perferrcnt++;
			continue;
		}
		if (Beatmode)
		        kcnt = (nxtbt - curbt) * ekrbetsiz + 0.5;
		else kcnt = (nxtim - curp2) * ekr + 0.5;
		if (kcnt > 0) { 				/* perf for kcnt kprds	*/
		    if (!initonly) {
		        long kdone, kperf();
			if ((kdone = kperf(kcnt)) < kcnt) {     /* early rtn:  RTevent  */
			    RTsense = 1;
			    e = RTevtblk;
			    curp2 += kdone / ekr;               /*    update only curp2 */
			    e->p[2] = curp2;                    /*    & put into evtblk */
			    if (segamps || sormsg && rngflg)
			        printf("\t\t   T%7.3f TT%7.3f M:",curp2,timtot+curp2);
			}
			else {                                  /* else a score event:  */
			    RTsense = 0;
			    e = scorevtblk;
			    prvbt = curbt;                      /*    update beats too  */
			    curbt = nxtbt;
			    curp2 = nxtim;
			    if (segamps || sormsg && rngflg)
				printf("B%7.3f ..%7.3f T%7.3f TT%7.3f M:",
					prvbt,	curbt,	curp2,	timtot+curp2);
			}
			if (segamps || sormsg && rngflg) {
			    for (n=nchnls, maxp=maxamp; n--;)
					printf("%9.1f", *maxp++);
			    putchar('\n');
			    if (rngflg) {
			        printf("\t number of samples out of range:");
				for (n=nchnls, rngp=rngcnt; n--;)
				    printf("%9ld", *rngp++);
				putchar('\n');
				rngflg = 0;
				srngflg++;
			    }
			}
			for (n=nchnls,maxp=maxamp-1,smaxp=smaxamp-1,
			     rngp=rngcnt,srngp=srngcnt; n--; ) {
				if (*++maxp > *++smaxp)
					*smaxp = *maxp;
				*maxp = 0;
				*srngp++ += *rngp;
				*rngp++ = 0;
			}
		    }
		}
		if (!RTsense) {                /* if this was a score or turnoff time:  */
		    if (frstoff != NULL) {		/*   if turnoffs pending,	*/
		        if (Beatmode)
			    beatexpire(curbt + hfkprd);	/*     rm any expired instrs    */
			else timexpire(curp2 + hfkprd);
		    }
		    if (offonly)
			goto retest;    		/*    if offonly, loop back     */
		}
		switch(e->opcod) {             /* now we can deal with the event proper */
		case 'i':
			insno = abs((int)e->p[1]);
			if (insno > MAXINSNO || instrtxtp[insno] == NULL) {
			    if (RTsense) printf("\t\t   T%7.3f",curp2);
			    else  printf("\t  B%7.3f",curbt);
			    printf(" - note deleted. instr %d undefined\n",insno);
			    perferrcnt++;
			}
			else if (e->p[1] < 0.)  	   /* if p1 neg,        	*/
			    infoff(-e->p[1]);              /*   turnoff any infin copy  */
			else {
			    if (e->p3orig >= 0.) {
				if (Beatmode)
				    e->p[3] = e->p3orig * betsiz;
				e->p3orig += e->p2orig;
			    }
			    if (n = insert(insno,e)) {    /* else alloc,init & activat */
			        if (RTsense) printf("\t\t   T%7.3f",curp2);
				else  printf("\t  B%7.3f",curbt);
				printf(" - note deleted.  i%d had %d init errors\n",
				   insno, n);
				perferrcnt++;
			    }
			}
			break;
		case 'f':
			fgens(e);
			break;
		case 'a':
			curp2 = e->p[2] + e->p[3];
			curbt = e->p2orig + e->p3orig;
			printf("time advanced %5.3f beats by score request\n",e->p3orig);
			break;
		}
		if (RTsense) {                     /* RT event now done with, so  */
		    e = scorevtblk;                /*    return to score context  */
		    goto retest;                   /*    and resume the kperf     */
		}
		continue;                          /* else get next score event   */

	scode:	if (e->opcod == 's' || sectno > 1) {  /* for s, or e after s */
			timtot += curp2;
			prvbt = curbt = curp2 = 0;
			printf("end of section %d\t sect peak amps:",sectno);
			for (n=nchnls, maxp=smaxamp; n--; )
				printf("%9.1f", *maxp++);
			putchar('\n');
			if (srngflg) {
				printf("\t number of samples out of range:");
				for (n=nchnls, srngp=srngcnt; n--; )
					printf("%9ld", *srngp++);
				putchar('\n');
				srngflg = 0;
			}
		}
		for (n=nchnls, smaxp=smaxamp-1, maxp=omaxamp-1, 
				srngp=srngcnt,   rngp=orngcnt; n--; ) {
			if (*++smaxp > *++maxp)
				*maxp = *smaxp;		/* keep ovrl maxamps */
			*smaxp = 0;
			*rngp++ += *srngp;		/*   and orng counts */
			*srngp++ = 0;
		}
		if (e->opcod == 's') {			/* if s code,	     */
			orcompact();			/*   rtn inactiv spc */
			if (actanchor.nxtact == NULL)   /*   if no indef ins */
			        rlsmemfiles();          /*    purge memfiles */
			curp2 = curbt = 0.0;		/*   reset sec times */
			printf("SECTION %d:\n", ++sectno);
		}					/*   & back for more */
		else break;
	}

	printf("end of score.\t\t   overall amps:");	/* else we're done */
	for (maxp=omaxamp, n=nchnls; n--; )
		printf("%9.1f", *maxp++);
	if (outformat != AE_FLOAT) {
		printf("\n\t   overall samples out of range:");
		for (rngp=orngcnt, n=nchnls; n--; )
			printf("%9ld", *rngp++);
	}
	printf("\n%d errors in performance\n",perferrcnt);
	if (sfread)
	        sfclosein();
	if (sfwrite) {
	        extern char *getoutname(), *getoutformat();
		extern long nrecs;
		extern int  iobufsamps,outsampsiz;
		sfcloseout();
		printf("%ld %d-byte soundblks written to %s (%s)\n",
		       nrecs, iobufsamps*outsampsiz, getoutname(), getoutformat());
	}
	else printf("no sound written to disk\n");
}
