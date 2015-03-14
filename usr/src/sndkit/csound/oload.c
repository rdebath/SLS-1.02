#include "cs.h"				/*					OLOAD.C		*/
#include "oload.h"

INSTRTXT *instrtxtp[MAXINSNO+1], instxtanchor;
static	NAME	*gblnames, *gblnxtslot, *gblnamlim;
static	NAME	*lclnames, *lclnxtslot, *lclnamlim;
static	int	lclnxtoffset = 0, gblnxtoffset = 0;
static	float	*lclbas, *lclpbas, *gblspace;

float	esr = 10000, ekr = 1000, ensmps = 10;		/* default values */
int	nchnls = 1, ksmps = 10;

float	pi = 3.1415927, twopi = 6.2831853, tpidsr, mtpdsr, pid100;
float   hfkprd, *spin, *spout, *plgadr();
float	sicvt, kicvt, fmaxlen = MAXLEN, dv32768, onedsr;
int	nspin, nspout, spoutactive;

oload()
{
	TEXT	*tp, *getoptxt();
register INSTRTXT *ip;
	 INSTRTXT *prvinstxt = &instxtanchor;
register OPTXT	*bp, *prvbp;
	ARGLST	*alp;
	char	*s;
register int	pmax, nn;
	int	n;
	
	gblnames = (NAME *)mmalloc((long)(NNAMES*sizeof(NAME)));
	lclnames = (NAME *)mmalloc((long)(NNAMES*sizeof(NAME)));
	gblnamlim = gblnames + NNAMES;
	lclnamlim = lclnames + NNAMES;
	gblnxtslot = gblnames;

	gbloffset("sr");		/* enter global reserved words */
	gbloffset("kr");
	gbloffset("ksmps");
	gbloffset("nchnls");

	rdorchfile();   			/* go read orch file	*/
	while ((tp = getoptxt()) != NULL) {	/*   then for each opcode: */
		switch(tp->opnum) {
		case INSTR:
			ip = (INSTRTXT *) mcalloc((long)sizeof(INSTRTXT));
			prvinstxt = prvinstxt->nxtinstxt = ip;
			txtcpy((char *)&ip->t,(char *)tp);
			prvbp = (OPTXT *) ip;	     /* begin an optxt chain */
			alp = ip->t.inlist;
			if (sscanf(alp->arg[0], "%d", &n) && n) 
				putop(&ip->t);		/* print, except i0 */ 
			for (nn = alp->count; nn>0; ) {
				s = alp->arg[--nn];	/* log all insnos */
				if (!(sscanf(s, "%d", &n))
				  || n < 0 || n > MAXINSNO) {
					synterr("illegal instr number");
					continue;
				}
				if (instrtxtp[n] != NULL) {
					sprintf(errmsg,"instr %d redefined",n);
					synterr(errmsg);
				}
				instrtxtp[n] = ip;
			}
			lclnxtslot = lclnames;		/* clear lcl namlist */
			lclnxtoffset = 0;		/*   for rebuilding  */
			pmax = 3;			/* set minimum pflds */
			break;
		case ENDIN:
			bp = (OPTXT *) mcalloc((long)sizeof(OPTXT));
			txtcpy((char *)&bp->t, (char *)tp);
			prvbp->nxtop = bp;
			bp->nxtop = NULL;	/* terminate the optxt chain */
			if (odebug) {
				putop(&bp->t);
				printf("pmax = %d, localen = %d\n",
					pmax,lclnxtoffset);
			}
			ip->pmax = pmax;
			ip->pextrab = ((n = pmax-3) > 0) ? n*sizeof(float) : 0;
			ip->localen = lclnxtoffset;
			break;
		default:
			bp = (OPTXT *) mcalloc((long)sizeof(OPTXT));
			txtcpy((char *)&bp->t,(char *)tp);
			prvbp = prvbp->nxtop = bp;  /* link into optxt chain */
			if (odebug) putop(&bp->t);
			for (alp=bp->t.inlist, nn=alp->count; nn>0; ) {
				s = alp->arg[--nn];
				if ((n = pnum(s)) >= 0)
					{ if (n > pmax)  pmax = n; }
				else lgbuild(s);
			}
			for (alp=bp->t.outlist, nn=alp->count; nn>0; ) {
				s = alp->arg[--nn];
				if ((n = pnum(s)) >= 0)
					{ if (n > pmax)  pmax = n; }
				else lgbuild(s);
				if (!nn && *bp->t.opcod == 'r'	 /* rsvd gbl?*/
				  && strcmp(bp->t.opcod,"r=") == 0) {
					n = *(plgadr(bp->t.inlist->arg[0]));
					if (strcmp(s,"ksmps") == 0)
						ksmps = ensmps = n;   /* hit */
					else if (strcmp(s,"nchnls") == 0)
						nchnls = n;	  /* defaults*/
				}
			}
			break;
		}
	}
	if (odebug) printf("globalen = %d\n", gblnxtoffset);
	gblspace = (float *)mcalloc((long)gblnxtoffset);        /* alloc gblspace    */
	*(gblspace + gbloffset("sr")/sizeof(float)) = esr;	/*   & enter	     */
	*(gblspace + gbloffset("kr")/sizeof(float)) = ekr;	/*   reserved word   */
	*(gblspace + gbloffset("ksmps")/sizeof(float)) = ensmps;/*   default values  */
	*(gblspace + gbloffset("nchnls")/sizeof(float)) = nchnls;
	if ((nn = init0()) > 0)				/* run instr 0 inits */
		synterr("header init errors");
	if (ksmps != esr / ekr)				/* & chk consistency */
		synterr("inconsistent sr, kr, ksmps");
	if (odebug)
		printf("esr = %f, ekr = %f, ksmps = %d, nchnls = %d\n",
		   esr,ekr,ksmps,nchnls);
	tpidsr = twopi / esr;				/* now set internal  */
	mtpdsr = -tpidsr;				/*    consts	     */
	pid100 = pi / 100.;
	sicvt = fmaxlen / esr;
	kicvt = fmaxlen / ekr;
	hfkprd = .5 / ekr;
	onedsr = 1. / esr;
	dv32768 = 1./32768.;
	reverbinit();
	nspin = nspout = ksmps * nchnls;		/* alloc spin & spout  */
	spin =  (float *) mcalloc((long)nspout*sizeof(float));
	spout = (float *) mcalloc((long)nspout*sizeof(float));
}

 INSDS *
instance(insno)			/* create instance of an instr template	*/
 int insno;			/*   allocates and sets up all pntrs	*/
{
	INSTRTXT *tp;
extern	INSDS	anchor;
	INSDS	*ip, *prvip;
	OPTXT	*optxt;
	OPDS	*opds, *prvopds, *prvids, *prvpds, *prvlds;
extern	OENTRY	opcodlst[];
	OENTRY	*ep;
	LBLARG	larg[NGOTOS], *largp;
	int	n, pextent, opnum, inreqd;
	float	**argpp, *plgadr();

	tp = instrtxtp[insno];
	pextent = sizeof(INSDS) + tp->pextrab;		/* alloc new space,  */
	ip = (INSDS *) mcalloc((long)pextent + tp->localen);
	if ((prvip = tp->instance) == NULL)
		tp->instance = ip;			/*    and add to     */
	else {
		while (prvip->nxtinstance != NULL)	/*    txt instance   */
			prvip = prvip->nxtinstance;	/*    chain	     */
		prvip->nxtinstance = ip;
		ip->prvinstance = prvip;
	}
	largp = larg;					/* clear lcl lblargs */
	lclnxtslot = lclnames;				/* clear lcl namlist */
	lclnxtoffset = 0;				/*   for rebuilding  */
	lclpbas = &ip->p0;				/*   w. real offsets */
	lclbas = (float *)((char *)ip + pextent);
	if (odebug) printf("instr %d allocated at %lx\n    lclbas begins at %lx\n",
				insno,ip,lclbas);
	optxt = (OPTXT *)tp;
	prvopds = prvids = prvpds = prvlds = (OPDS *)ip;
	while ((optxt = optxt->nxtop) != NULL) {     /* for each op in instr */
		register TEXT *ttp = &optxt->t;
		if ((opnum = ttp->opnum) == ENDIN)	/*  (until ENDIN)  */
			break;
		ep = &opcodlst[opnum];			/* for all ops:     */
		opds = (OPDS *) mcalloc((long)ep->dsblksiz);/* alloc dspace */
		if (odebug) printf("op %d (%s) allocated at %lx\n",
				opnum,ep->opname,opds);
		prvopds = prvopds->nxtopds = opds;	/* set opds thread   */
		opds->optext = optxt;			/*  & common headata */
		opds->insdshead = ip;
		if (opnum == LABEL) {			/* LABEL:	*/
			register LBLBLK	*lblbp;
			prvlds = prvlds->nxtlbl = opds;	/*    set lbl thread */
			lblbp = (LBLBLK *) opds;
			lblbp->prvi = prvids;		/*    save i/p links */
			lblbp->prvp = prvpds;
			lblbp->lbltxt = ttp->opcod;	/*    save lbl textp */
			continue;			/*    for later refs */
		}
		if (!ep->thread) {			/* thread 1 OR 2:  */
			if (ttp->pftype == 'b') {
				prvids = prvids->nxti = opds;
				opds->iopadr = ep->iopadr;
			}
			else {	prvpds = prvpds->nxtp = opds;
				opds->opadr = ep->kopadr;
			}
			goto args;
		}
		if (ep->thread & 01) {			/* thread 1:	    */
			prvids = prvids->nxti = opds;	/* link into ichain */
			opds->iopadr = ep->iopadr;	/*   & set exec adr */
			if (opds->iopadr == NULL)
				die("null iopadr");
		}
		if (n = ep->thread & 06) {		/* thread 2 OR 4:    */
			prvpds = prvpds->nxtp = opds;	/* link into pchain  */
			if (!(n & 04)
			  || ttp->pftype == 'k' && ep->kopadr != NULL)
				opds->opadr = ep->kopadr; /*	krate or     */
			else opds->opadr = ep->aopadr;	  /*	arate         */
			if (odebug) printf("opadr = %lx\n",opds->opadr);
			if (opds->opadr == NULL)
				die("null opadr");
		}
	args:	argpp = (float **)((char *)opds + sizeof(OPDS));
		if (odebug) printf("argptrs:");
		for (n=0; n < ttp->outlist->count; ) {	/* & set argpts */
			if (odebug)printf("\t%lx",plgadr(ttp->outlist->arg[n]));
			*argpp++ = plgadr(ttp->outlist->arg[n++]); 
		}
		while (n++ < strlen(ep->outypes)) {
			if (odebug) printf("\tPADOUT");
			*argpp++ = NULL;		/* if 4 outypes, pad */
		}
		inreqd = strlen(ep->intypes);
		for (n=0; n < ttp->inlist->count; ) {
			if (n < inreqd && ep->intypes[n] == 'l') {
				if (odebug) printf("\t***lbl");
				largp->lbltxt = ttp->inlist->arg[n++];
				largp->argpp = argpp++;
				largp++;
			}
			else {
				if (odebug)
				    printf("\t%lx",plgadr(ttp->inlist->arg[n]));
				*argpp++ = plgadr(ttp->inlist->arg[n++]);
			}
		}
		if (odebug) putchar('\n');
	}
	{
	register LBLARG	*lap = larg;
	register LBLBLK *lbp;
	if (odebug) printf("lap = %lx, largp = %lx\n",lap,largp);
	lblarg:	while (lap < largp) {
			for (lbp = (LBLBLK *) ip->nxtlbl; lbp != NULL;
						lbp = (LBLBLK *) lbp->h.nxtlbl)
				if (strcmp(lap->lbltxt,lbp->lbltxt) == 0) {
					*lap->argpp = (float *) lbp;
					lap++;
					goto lblarg;
				}
			die("inconsistent label resolution");
		}
	}
	return(ip);
}

txtcpy(s,t)
 char *s, *t;
{
	int n = sizeof(TEXT);
	while (n--)
		*s++ = *t++;
}

pnum(s)				/* check a char string for pnum format	*/
 register char *s;		/*   and return the pnum ( >= 0 )	*/
{				/* else return -1			*/
	int	n;

	if (*s == 'p' || *s == 'P')
		if (sscanf(++s, "%d", &n))
			return(n);
	return(-1);
}

lgbuild(s)			/* build pool of floating const values	*/
 char *s;			/* build lcl/gbl list of ds names, offsets */
{				/*   (no need to save the returned values) */
	register char c;
	float	*constadr();

	if (((c = *s) >= '0' && c <= '9')
	  || c == '.' || c == '-' || c == '+')
		constadr(s);
	else if (!(lgexist(s))) {
		if (c == 'g' || c == '#' && *(s+1) == 'g')
			gbloffset(s);
		else lcloffset(s);
	}
}

 float *
plgadr(s)		/* get address of const, pnum, lcl or gbl argument */
 char *s;
{
	register char	c;
	register int	n;
	float	*constadr();

	if (((c = *s) >= '0' && c <= '9')
	  || c == '.' || c == '-' || c == '+')
		return(constadr(s));
	if ((n = pnum(s)) >= 0)
		return(lclpbas + n);
	if (c == 'g' || c == '#' && *(s+1) == 'g' || gexist(s))
		return((float *)((char *)gblspace + gbloffset(s)));
	return((float *)((char *)lclbas + lcloffset(s)));
}

 float *
constadr(s)			/* get adr of floating const value	*/
 register char *s;		/* builds value pool on 1st occurrence	*/
{
static	float	*pool = NULL, *nxtslot, *flim;
	float	newval;
register float	*fp;
register int	c;
register long   ival = 0L, iscale = 1L;
register char	*str = s;

	if (pool == NULL) {
	        pool = (float *)mmalloc((long)NCONSTS * sizeof(float));
		nxtslot = pool;
		flim = pool + NCONSTS;
	}
	while (*s == '-') { iscale = -iscale; s++; }	/* simulate scanf:  */
	while (*s == '+') s++;
	while (*s == '0') s++;
	while ((c = *s++) != '\0') {		/* collect the digits,	*/
		if (c == '0')
			ival *= 10;
		else if (c > '0' && c <= '9') {
			ival *= 10;
			ival += c;
			ival -= '0';
		}
		else if (c == '.') break;
		else goto flerror;
	}
	if (c != '\0')
		while ((c = *s++) != '\0') {
			if (c < '0' || c > '9')		
				goto flerror;
			ival *= 10;
			ival += c;
			ival -= '0';
			iscale *= 10;
		}
	newval = (float)ival / iscale;		/* & scale to float val	*/
	for (fp = pool; fp < nxtslot; fp++)	/* now search constpool */
		if (newval == *fp)		/* if val is there	*/
			return(fp);		/*    return w. address	*/
	*fp = newval;				/* else enter newval	*/
	if (++nxtslot >= flim)
		die("flconst pool is full");
	return(fp);				/*   and return new adr */

flerror:sprintf(errmsg,"numeric syntax '%s'",str);
	synterr(errmsg);
	return(pool);
}

gbloffset(s)			/* get named offset into gbl dspace	*/
 char *s;			/* builds namelist & offsets on 1st occur */
{
register NAME	*np;

	for (np = gblnames; np < gblnxtslot; np++) /* search gbl namelist: */
		if (strcmp(s,np->namep) == 0)	/* if name is there	*/
			return(np->offset);	/*    return w. offset	*/
	np->namep = s;				/* else record newname	*/
	np->offset = gblnxtoffset;		/*   & its offset	*/
	if (*s == '#')	s++;
	if (*s == 'g')	s++;
	if (*s == 'a')				/* then calculate next	*/
		gblnxtoffset += sizeof(float) * ksmps;
	else gblnxtoffset += sizeof(float);
	if (++gblnxtslot >= gblnamlim)		/* chk for full table	*/
		die("gbl namelist is full");
	return(np->offset);			/*   and rtn this offset */
}

lcloffset(s)			/* get named offset into instr lcl dspace */
 char *s;			/* builds namelist & offsets on 1st occur */
{				/*	(this list redone for each instr) */
register NAME	*np;

	for (np = lclnames; np < lclnxtslot; np++) /* search lcl namelist: */
		if (strcmp(s,np->namep) == 0)	/* if name is there	*/
			return(np->offset);	/*    return w. offset	*/
	np->namep = s;				/* else record newname	*/
	np->offset = lclnxtoffset;		/*   & its offset	*/
	if (*s == '#')	s++;
	switch(*s) {    			/* then calculate next	*/
	  case 'd':  lclnxtoffset += sizeof(DOWNDAT);  break;
	  case 'w':  lclnxtoffset += sizeof(SPECDAT);  break;
	  case 'a':  lclnxtoffset += sizeof(float) * ksmps;  break;
	  default:   lclnxtoffset += sizeof(float);  break;
	}
	if (++lclnxtslot >= lclnamlim)		/* chk for full table	*/
		die("lcl namelist is full");
	return(np->offset);			/*   and rtn this offset */
}

gexist(s)			/* tests whether variable name exists	*/
 char *s;			/*	in gbl namelist			*/
{
register NAME	*np;

	for (np = gblnames; np < gblnxtslot; np++) /* search gbl namelist: */
		if (strcmp(s,np->namep) == 0)	/* if name is there	*/
			return(1);		/*	return 1	*/
	return(0);				/* else return 0	*/
}

lgexist(s)			/* tests whether variable name exists	*/
 char *s;			/*	in gbl or lcl namelist		*/
{
register NAME	*np;

	for (np = gblnames; np < gblnxtslot; np++) /* search gbl namelist: */
		if (strcmp(s,np->namep) == 0)	/* if name is there	*/
			return(1);		/*	return 1	*/
	for (np = lclnames; np < lclnxtslot; np++) /* search lcl namelist: */
		if (strcmp(s,np->namep) == 0)	/* if name is there	*/
			return(1);		/*	return 1	*/
	return(0);				/* cannot find, return 0 */
}

putop(tp)
 TEXT *tp;
{
	register int n, nn;

	if (n = tp->outlist->count) {
		nn = 0;
		while (n--)
			putstr(tp->outlist->arg[nn++]);
	}
	else putchar('\t');
	putstr(tp->opcod);
	if (n = tp->inlist->count) {
		nn = 0;
		while (n--)
			putstr(tp->inlist->arg[nn++]);
	}
	putchar('\n');
}

putstr(cp)
 char *cp;
{
	while (*cp)
		putchar(*cp++);
	putchar('\t');
}
