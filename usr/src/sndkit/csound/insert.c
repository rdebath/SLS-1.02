#include "cs.h"			/*				INSERT.C	*/
#include "insert.h"	/* for goto's */
#include "aops.h"	/* for cond's */

#define	ERRSIZ	200
#define	WARNMSG	04

extern	INSTRTXT *instrtxtp[];
extern	OENTRY	opcodlst[];

INSDS	actanchor, *curip, *frstoff = NULL, *instance();
long    kcounter = 0;         /* count of k-periods throughout performance  */
int	inerrcnt = 0, perferrcnt = 0;
char	errmsg[ERRSIZ];	      /* sprintf space available for compiling msgs */

static	int	tieflag = 0;		/* toggled by insert for tigoto */
static	int	reinitflag = 0;		/* toggled by reinit for others */
static	OPDS	*ids, *pds;		/* used by init and perf loops	*/
					/*  & modified by igoto, kgoto	*/
init0()
{
register INSDS	*ip;

	curip = ip = instance(0);		/* allocate instr 0	*/
	ids = pds = (OPDS *)ip;
	if ((pds = pds->nxtp) != NULL)		/* p-code not allowed	*/
		synterr("p-pass statements illegal in header blk");
	while ((ids = ids->nxti) != NULL)
		(*ids->iopadr)(ids);		/*	but run all i-code */
	return(inerrcnt);			/* return errcnt	*/
}

insert(insno, newevtp)		/* insert an instr copy into active list */
 int    insno;			/*	then run an init pass		*/
 EVTBLK	*newevtp;
{
register INSTRTXT *tp;
register INSDS	*ip, *prvp, *nxtp;

	if (odebug) printf("activating instr %d\n",insno);
	inerrcnt = 0;
	tp = instrtxtp[insno];
	if ((ip = tp->instance) != NULL) {	/* if allocs of text exist: */
		do	if (ip->insno == insno	/*   if find this insno,  */
			  && ip->actflg		/*	active		  */
			  && ip->offtim < 0	/*	with indef (tie)  */
			  && ip->p1 == newevtp->p[1]){ /*  & matching p1  */
				tieflag++;	
				goto init;	/*     continue that event */
			}
		while ((ip = ip->nxtinstance) != NULL);
		ip = tp->instance;		/*   else get alloc of text */
		do 	if (!ip->actflg)	/*	that is free	    */
				goto actlnk;	/*	and use its space   */
		while ((ip = ip->nxtinstance) != NULL);
	}
	printf("new alloc for instr %d:\n",insno);
	ip = instance(insno);			/* else alloc new dspace  */
	
actlnk:	ip->insno = insno;
	nxtp = &actanchor;			/* now splice into activ lst */
	while ((prvp = nxtp) && (nxtp = prvp->nxtact) != NULL)
		if (nxtp->insno > insno
		 || nxtp->insno == insno
		   && nxtp->p1 > newevtp->p[1]) {
			nxtp->prvact = ip;
			break;
		}
	ip->nxtact = nxtp;
	ip->prvact = prvp;
	prvp->nxtact = ip;
	ip->actflg++;			/*    and mark the instr active */
        {
	        register int	n;
		register float	*flp, *fep;

	init:   if ((n = tp->pmax) != newevtp->pcnt) {
                    sprintf(errmsg,"instr %d pmax = %d, note pcnt = %d",
					insno, n, newevtp->pcnt);
		    warning(errmsg);
		}
		flp = &ip->offbet;
		fep = &newevtp->p3orig;
		n += 3;			/* allow for p3orig, offtim, p[0] */
		if (odebug) printf("psave beg at %lx\n",flp);
		do  *flp++ = *fep++;		/* psave	*/
		while (--n);
		if (odebug) printf("   ending at %lx\n",flp);
	}
	ip->offtim = ip->p3;			/* & duplicate p3 for now */
	curip = ip;
	ids = (OPDS *)ip;
	while ((ids = ids->nxti) != NULL) {   /* do init pass for this instr */
		if (odebug) printf("init %s:\n",
		 opcodlst[ids->optext->t.opnum].opname);
		(*ids->iopadr)(ids);
	}
	tieflag = 0;
	if (inerrcnt || !ip->p3) {
		deact(ip);
		return(inerrcnt);
	}
	if (ip->p3 > 0. && ip->offtim > 0.) {	    /* if still finite time, */
		ip->offtim = ip->p2 + ip->p3;
		if ((nxtp = frstoff) == NULL
		 || nxtp->offtim > ip->offtim)		/*   set into	   */
			frstoff = ip;			/*   firstoff chain */
		else {
			while ((prvp = nxtp)
			  && (nxtp = nxtp->nxtoff) != NULL
		  	  && ip->offtim >= nxtp->offtim);
			prvp->nxtoff = ip;
		}
		ip->nxtoff = nxtp;
	}
	else ip->offtim = -1;				/* else mark indef */
	if (odebug) {
		printf("instr %d now active:\n",insno);
		showallocs();
	}
	return(0);
}

showallocs()				/* debugging aid	*/
{
extern	 INSTRTXT instxtanchor;
register INSTRTXT *txtp;
register INSDS	 *p;
	printf("insno\tinstanc\tnxtinst\tprvinst\tnxtact\tprvact\tnxtoff\tactflg\tofftim\n");
	for (txtp = &instxtanchor;  txtp != NULL;  txtp = txtp->nxtinstxt)
	    if ((p = txtp->instance) != NULL) {
		do  printf("%d\t%lx\t%lx\t%lx\t%lx\t%lx\t%lx\t%d\t%3.1f\n",
		    (int)p->insno,p,p->nxtinstance,p->prvinstance,p->nxtact,
		    p->prvact,p->nxtoff,(int)p->actflg,p->offtim);
		while ((p = p->nxtinstance) != NULL);
	    }
}

deact(ip)			/* unlink single instr from activ chain */
 register INSDS *ip;		/*	and mark it inactive		*/
{				/*   close any files in fd chain	*/
register INSDS	*nxtp;

	if ((nxtp = ip->prvact->nxtact = ip->nxtact) != NULL)
			nxtp->prvact = ip->prvact;
	ip->actflg = 0;
	if (ip->fdch.nxtchp != NULL)
		fdchclose(ip);
}

beatexpire(beat)		/* unlink expired notes from activ chain */
 register float beat;		/*	and mark them inactive		*/
{				/*    close any files in each fdchain	*/
register INSDS	*ip, *nxtp;

	if ((ip = frstoff) != NULL && ip->offbet <= beat) {
		do {
			if ((nxtp = ip->prvact->nxtact = ip->nxtact) != NULL)
				nxtp->prvact = ip->prvact;
			ip->actflg = 0;
			if (ip->fdch.nxtchp != NULL)
				fdchclose(ip);
		}
		while ((ip = ip->nxtoff) != NULL && ip->offbet <= beat);
		frstoff = ip;
		if (odebug) {
			printf("deactivated all notes to beat %7.3f\n",beat);
			printf("frstoff = %lx\n",frstoff);
		}
	}
}
		
timexpire(time)			/* unlink expired notes from activ chain */
 register float time;		/*	and mark them inactive		*/
{				/*    close any files in each fdchain	*/
register INSDS	*ip, *nxtp;

	if ((ip = frstoff) != NULL && ip->offtim <= time) {
		do {
			if ((nxtp = ip->prvact->nxtact = ip->nxtact) != NULL)
				nxtp->prvact = ip->prvact;
			ip->actflg = 0;
			if (ip->fdch.nxtchp != NULL)
				fdchclose(ip);
		}
		while ((ip = ip->nxtoff) != NULL && ip->offtim <= time);
		frstoff = ip;
		if (odebug) {
			printf("deactivated all notes to time %7.3f\n",time);
			printf("frstoff = %lx\n",frstoff);
		}
	}
}
		
ihold()					/* make this note indefinit duration */
{					/* called by ihold statmnt at Itime */
	if (!reinitflag) {		/* no-op at reinit		    */
		curip->offbet = -1.;
		curip->offtim = -1.;
	}
}

turnoff()				/* terminate the current instrument  */
{					/* called by turnoff statmt at Ptime */
register INSDS	*ip, *curip, *prvip;

	curip = pds->insdshead;			/* remove from activ chain   */
	deact(curip);
	if (curip->offtim < 0.			/* skip indefinite durs,     */
	 || (ip = frstoff) == NULL)
		return;
	if (ip == curip)			/* else rm from nxtoff chain */
		frstoff = ip->nxtoff;
	else while ((prvip = ip) && (ip = ip->nxtoff) != NULL)
		if (ip == curip) {
			prvip->nxtoff = ip->nxtoff;
			return;
		}
}

orcompact()			/* free all inactive instr spaces */
{
extern	 INSTRTXT instxtanchor;
	 INSTRTXT *txtp;
register INSDS	 *ip, *nxtip, *prvip, **prvnxtloc;
register OPDS	 *op, *nxtop;

	for (txtp = &instxtanchor;  txtp != NULL;  txtp = txtp->nxtinstxt)
		if ((ip = txtp->instance) != NULL) {	/* if instanc exists */
			prvip = NULL;
			prvnxtloc = &txtp->instance;
			do {	if (ip->actflg == 0) {
					if (ip->fdch.nxtchp != NULL)
						fdchclose(ip);
					if (ip->auxch.nxtchp != NULL)
						auxchfree(ip);
					if ((nxtip = ip->nxtinstance) != NULL)
						nxtip->prvinstance = prvip;
					*prvnxtloc = nxtip;
					op = (OPDS *)ip;
					do {	nxtop = op->nxtopds;
						free((char *)op);
					} while ((op = nxtop) != NULL);
				}
				else {
					prvip = ip;
					prvnxtloc = &ip->nxtinstance;
				}
			}
			while ((ip = *prvnxtloc) != NULL);
		}
	printf("inactive allocs returned to freespace\n");
}

infoff(p1)			/*  turn off an indef copy of instr p1	*/
 float p1;			/*	called by musmon		*/
{
 register INSDS *ip;
 register int	insno;
 
	insno = p1;
	if ((ip = (instrtxtp[insno])->instance) != NULL) {
		do	if (ip->insno == insno		/* if find the insno */
			  && ip->actflg			/*	active	     */
			  && ip->offtim < 0		/*	but indef,   */
			  && ip->p1 == p1) {
				if (odebug) printf(
				   "turning off inf copy of instr %d\n",insno);
				deact(ip);
				return;			/*  turn it off	*/
			}
		while ((ip = ip->nxtinstance) != NULL);
	}
	printf("could not find indefinitely playing instr %d\n",insno);
}

long kperf(kcnt)	/* perform currently active instrs for kcnt kperiods */
 long kcnt;     	/*	& send audio result to output buffer	*/
{
extern	int	RTevents, Linein, sensLine(), Midiin, sensMidi();
extern	int	ksensing, sfread, spoutactive;
extern  void    (*spinrecv)(), (*spoutran)(), (*nzerotran)();
register INSDS	*ip;
        long    kreq = kcnt;

	if (odebug) printf("perfing %ld kprds\n",kcnt);
	if (!ksensing && actanchor.nxtact == NULL) {	/* if !kreads & !instrs_activ, */
	        kcounter += kcnt;
		(*nzerotran)(kcnt);             	/*   send kcnt zerospouts  */
	}
	else do {               			/* else for each kcnt:	   */
	        if (RTevents)
		    if ( Linein && sensLine()   	/*   if RT activity found  */
		      || Midiin && sensMidi() )
			return(kreq - kcnt);    	/*      do early return    */
		kcounter += 1;
	        if (sfread)     			/*   if audio_infile open  */
		        (*spinrecv)();			/*      fill the spin buf  */
		spoutactive = 0;			/*   make spout inactive   */
		ip = &actanchor;
		while ((ip = ip->nxtact) != NULL) {	/*   for each instr active */
			pds = (OPDS *)ip;
			while ((pds = pds->nxtp) != NULL)
				(*pds->opadr)(pds);	/*      run each opcod     */
		}
		if (spoutactive)        		/*   results now in spout? */
		        (*spoutran)();     		/*      send to audio_out  */
		else (*nzerotran)(1L);             	/*   else send zerospout   */
#ifdef THINK_C
		STasks();                           /* on Mac, allow system events */
#endif
	} while (--kcnt);
	return(kreq);
}

initerror(s)
 register char *s;
{
	printf("INIT ERROR in instr %d: %s\n", ids->insdshead->insno, s);
	putop(&ids->optext->t);
	inerrcnt++;
}

perferror(s)
 register char *s;
{
	printf("PERF ERROR in instr %d: %s\n", pds->insdshead->insno, s);
	putop(&pds->optext->t);
	printf("   note aborted\n");
	perferrcnt++;
	deact(pds->insdshead);				/* rm ins fr actlist */
	while (pds->nxtp != NULL)
		pds = pds->nxtp;			/* loop to last opds */
}							/* contin from there */

warning(s)
 register char *s;
{
	if (msglevel & WARNMSG)
		printf("WARNING: %s\n", s);
}

igoto(p)
 register GOTO *p;
{
	ids = p->lblblk->prvi;
}

kgoto(p)
 register GOTO *p;
{
	pds = p->lblblk->prvp;
}

icgoto(p)
 register CGOTO *p;
{
	if (*p->cond)
		ids = p->lblblk->prvi;
}

kcgoto(p)
 register CGOTO *p;
{
	if (*p->cond)
		pds = p->lblblk->prvp;
}

timset(p)
 register TIMOUT *p;
{
	if ((p->cnt1 = (long)(*p->idel * ekr + .5)) < 0L
	 || (p->cnt2 = (long)(*p->idur * ekr + .5)) < 0L)
	 	initerror("negative time period");
}

timout(p)
 register TIMOUT *p;
{
	if (p->cnt1)				/* once delay has expired, */
		p->cnt1--;
	else if (--p->cnt2 >= 0L)		/*  br during idur countdown */
		pds = p->lblblk->prvp;
}

rireturn()	{}

reinit(p)
 register GOTO *p;
{
	reinitflag = 1;
	curip = p->h.insdshead;
	ids = p->lblblk->prvi;
	while ((ids = ids->nxti) != NULL && ids->iopadr != rireturn)
		(*ids->iopadr)(ids);
	reinitflag = 0;
}

rigoto(p)
 register GOTO *p;
{
	if (reinitflag)
		ids = p->lblblk->prvi;
}

tigoto(p)				/* I-time only, NOP at reinit */
 register GOTO *p;
{
	if (tieflag && !reinitflag)
		ids = p->lblblk->prvi;
}

tival(p)				/* I-time only, NOP at reinit */
 register EVAL *p;
{
	if (!reinitflag)
		*p->r = (tieflag) ? 1. : 0.;
}
