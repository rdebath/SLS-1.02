#include "cs.h"			/*						RDORCH.C	*/

#define	ORTXTSIZ  40000L
#define	LINMAX	  1000
#define	LENMAX	  200
#define	GRPMAX	  VARGMAX
#define	LBLMAX	  100
#define	STRSPACE  4000
#define	ARGSPACE  20000

typedef	struct	{
	int	reqline;
	char	*label;
} LBLREQ;

static	char	**linadr;		/* adr of each line in text	*/
static	int	curline;		/* current line being examined	*/
static	char	*group[GRPMAX];		/* splitline local storage	*/
static	char	*grpsav[GRPMAX];	/* copy of above		*/
static	int	opgrpno;		/* grpno identified as opcode	*/
static	int	linopnum;		/* data for opcode in this line */
static	char	*linopcod;
static	int	linlabels;		/* count of labels this line	*/
static	LBLREQ	lblreq[LBLMAX];
static	int	lblcnt;
static	int	lgprevdef = 0;
static	int	opnum;			/* opcod data carriers		*/
static	char	*opcod;			/*  (line or subline)		*/
static	char	*argspace;
static	ARGLST	*nxtarglist, *argspacend;
static	char	*sspace, *sspnxt, *ssplim;

int	synterrcnt = 0;

rdorchfile()		/* read entire orch file into txt space */
{
	FILE	*fopen(), *fp;
	register int c, lincnt;
	register char *cp, *endspace, *ortext;

	printf("orch compiler:\n");
	if ((fp = fopen(orchname,"r")) == NULL)
		dies("cannot open orch file %s",orchname);
	ortext = mmalloc((long)ORTXTSIZ);              /* alloc mem spaces */
	linadr = (char **) mmalloc((long)(LINMAX+1)*sizeof(char **));
	argspace = mmalloc((long)ARGSPACE);
	sspace = mmalloc((long)STRSPACE);
	lincnt = 1;
	cp = linadr[1] = ortext;
	endspace = ortext + ORTXTSIZ;
	nxtarglist = (ARGLST *) argspace;
	argspacend = (ARGLST *) (argspace + ARGSPACE - 80);
	sspnxt = sspace;
	ssplim = sspace + STRSPACE;
	strcpy(sspace,"sr");

	while ((c = getc(fp)) != EOF) {		/* read entire orch file  */
		*cp++ = c;
		if (c == '\n') {			/* at each new line */
			if (++lincnt >= LINMAX)
				die("too many lines");
			linadr[lincnt] = cp;		/* record the adrs */
		}
		if (cp >= endspace)
			die("file too large for ortext space");
	}
	if (*(cp-1) != '\n')			/* if no final NL,	*/
		*cp++ = '\n';			/*    add one		*/
	else --lincnt;
	linadr[lincnt+1] = NULL;		/* terminate the adrs list */
	printf("%d lines read\n",lincnt);
	fclose(fp);				/* close the file	*/
	curline = 0;				/*   & reset to line 1	*/
}

splitline()		/* split next orch line into atomic groups */
{			/* cnt labels this line, and set opgrpno where found */
static	char	collectbuf[LENMAX];
	int	grpcnt, prvif, logical, condassgn, parens;
register int	c, collecting;
register char	*cp, *lp, *grpp=NULL;

nxtlin:	if ((lp = linadr[++curline]) == NULL)	/* point at next line	*/
		return(0);
	if (odebug) printf("LINE %d:\n",curline);
	linlabels = opgrpno = 0;
	grpcnt = prvif = logical = condassgn = parens = collecting = 0;
	cp = collectbuf;
	while ((c = *lp++) != '\n') {		/* for all chars this line:  */
		if (cp - collectbuf >= LENMAX)
			die("line LENMAX exceeded");
		if (c == ';') {
			while ((c = *lp++) != '\n');	/* comments:  gobble */
			break;				/*    & exit linloop */
		}
		if (c == ' ' || c == '\t') {		/* spaces, tabs:     */
			if (!opgrpno && collecting) {	/*  those befor args */
				*cp++ = '\0';		/*  can be delimitrs */
				collecting = 0;
				if (strcmp(grpp,"if") == 0) { /* of if opcod */
					strcpy(grpp,"gcgoto");  /* (replace) */
					cp = grpp + 7;
					prvif++;
				}
				if (isopcod(grpp))	/*   or maybe others */
					opgrpno = grpcnt;
			}
			continue;			/* now discard blanks*/
		}
		if (c == ':' && collecting && grpcnt == linlabels+1) {
			linlabels++;			/* colon in 1st grps */
			*cp++ = '\0';			/*  is also delimitr */
			collecting = 0;			/*  (do not copy it) */
			continue;
		}
		if (c == '=' && !opgrpno) {		/* assign befor args */
			if (collecting)			/* can be a delimitr */
				*cp++ = '\0';
			grpp = group[grpcnt++] = cp;	/* is itslf an opcod */
			*cp++ = c;
			*cp++ = '\0';
			isopcod(grpp);
			opgrpno = grpcnt;
			collecting = 0;			/* & self-delimiting */
			continue;
		}
		if (c == ',') {				/* comma:	 */
			if (!collecting)
				synterrp(lp-1,"misplaced comma");
			if (parens) {
				synterrp(lp-2,"unbalanced parens");
				parens = 0;
			}
			*cp++ = '\0';			/*  terminate strng */
			collecting = logical = condassgn = 0;
			continue;
		}
		if (prvif && collecting) {		/* for prev "if":    */
			if (strncmp(lp-1,"goto",4) == 0) {/* if found "goto" */
				*cp++ = '\0';		/*	delimit cond */
				lp += 3;		/*	& step over  */
				prvif = collecting = 0;
				continue;
			}
			else if ((c == 'i' || c == 'k')	/*  if preced i or k */
			  && strncmp(lp,"goto",4) == 0) { /*  before "goto"  */
			 	*group[opgrpno-1] = c;  /*     modify gcgoto */
				isopcod(group[opgrpno-1]);
				*cp++ = '\0';		/*     then delimit  */
				lp += 4;		/*	etc	     */
				prvif = collecting = 0;
				continue;
			}
		}
		if (!collecting++) {			/* remainder are     */
			if (grpcnt >= GRPMAX)		/* collectable chars */
				die("GRPMAX overflow");
			grpp = group[grpcnt++] = cp;
		}
		if ( c >= 'a' && c <= 'z'	    /* establish validity */
		   || c >= '0' && c <= '9'
		   || c == '+' || c == '-'
		   || c == '*' || c == '/'
		   || c == '.' )
		   	;
		else if (c == '(')
			parens++;		    /* and monitor function */
		else if (c == ')')
			--parens;
		else if ((c == '>' || c == '<' || c == '='
		       || c == '!' || c == '&' || c == '|')
		    && (prvif || parens) )
			logical++;
		else if (c == '?' && logical )
			condassgn++;
		else if (c == ':' && condassgn)
			;
		else {
			sprintf(errmsg,"illegal character %c",c);
			synterrp(lp-1,errmsg);
		}
		*cp++ = c;			    /* then collect the char */
	}					    /*  and loop for next    */
	
	if (!grpcnt)				/* if line was trivial,	*/
		goto nxtlin;			/*	try another	*/
	if (collecting) {			/* if still collecting, */
		*cp = '\0';			/* 	terminate	*/
		if (!opgrpno)			/*	& chk for opcod	*/
			if (isopcod(grpp))
				opgrpno = grpcnt;
	}
	if (parens)				/* check balanced parens */
		synterrp(lp-1,"unbalanced parens");
	if (grpcnt > linlabels && !opgrpno) {	/* if no full line opcod, */
		synterr("no legal opcode");	/*	complain &	*/
		goto nxtlin;			/*	try another	*/
	}
	linopnum = opnum;			/* else save full line ops */
	linopcod = opcod;
	if (odebug) printgroups(grpcnt);
	return(grpcnt);
}

 TEXT *
getoptxt()			/* get opcod and args from current line */
{				/*	returns pntr to a TEXT struct	*/
static	short	grpcnt = 0, nxtest = 1;
static	short	xprtstno = 0, polcnt = 0;
static	short	instrblk = 0, instrcnt = 0;
static	ARGLST	nullist = {0};
static	TEXT	optext;		      /* struct to be passed back to caller */

extern	char	tokenstring[];
extern	POLISH	polish[];
register ARGLST	*alp;
register TEXT	*tp;
	char	c, d, str[20], *s, *strsav(), argtyp();
	float	*constadr();
	int	nn, incnt, outcnt;

tstnxt:	tp = &optext;
	if (nxtest >= grpcnt) {			/* if done with prevline, */
		if (!(grpcnt = splitline()))	/*    attack next line	*/
			return((TEXT *)0);	/*    (else we're done)	*/
		for (nn=0; nn<grpcnt; nn++)	/*    save the group pntrs */
			grpsav[nn] = group[nn];
		xprtstno = grpcnt - 1;		/*    and reinit indices */
		nxtest = 0;
		tp->linenum = curline;
	}
	if (linlabels) {
		s = strsav(group[nxtest]);
		lblfound(s);
		tp->opnum = LABEL;
		tp->opcod = s;
		tp->inlist = tp->outlist = &nullist;
		linlabels--;
		nxtest++;
		return(tp);
	}
	if (!instrcnt) {			/* send initial "instr 0"    */
		tp->opnum = INSTR;
		tp->opcod = "instr";		/*    to hold global assigns */
		tp->outlist = &nullist;
		tp->inlist = alp = nxtarglist;
		alp->count = 1;
		alp->arg[0] = strsav("0");
		nxtarglist = (ARGLST *) &alp->arg[1];
		instrcnt = instrblk = 1;
		return(tp);
	}					/*  then at 1st real INSTR,  */
	if (instrcnt == 1 && instrblk && opnum == INSTR) {
		tp->opnum = ENDIN;			/*  send an endin to */
		tp->opcod = "endin";			/*  term instr 0 blk */
		tp->outlist = tp->inlist = &nullist;
		instrblk = 0;
		instrcnt = 2;
		return(tp);
	}
	while (xprtstno >= 0) {			/* for each arg (last 1st):  */
		if (!polcnt)			/* if not midst of expressn  */
			polcnt = express(group[xprtstno--]);  /* tst nxtarg  */
		if (polcnt < 0) {		     /* polish but arg only, */
			group[xprtstno+1] = strsav(tokenstring); /* redo ptr */
			polcnt = 0;				 /* & contin */
		}
		else if (polcnt) {
			register POLISH	*pol;	     /* for real polish ops, */
			register int n;
			pol = &polish[--polcnt];     /*    grab top one      */
			if (isopcod(pol->opcod) == 0) {	/* and check it out  */
				synterr("illegal opcod from expr anal");
				goto tstnxt;
			}
			tp->opnum = opnum;		/* ok to send subop */
			tp->opcod = opcod;
			tp->outlist = alp = nxtarglist;
			alp->count = outcnt = 1;
			alp->arg[0] = strsav(pol->arg[0]);
			tp->inlist = alp = (ARGLST *) &alp->arg[1];
			n = alp->count = incnt = pol->incount;
			do  alp->arg[n-1] = strsav(pol->arg[n]);
			while (--n);
			nxtarglist = (ARGLST *) &alp->arg[incnt];
			if (!polcnt)		/* last op? hit the grp ptr */
				group[xprtstno+1] = tp->outlist->arg[0];
			goto spctst;
		}
	}
	if (nxtest < opgrpno-1) {
		c = argtyp(group[nxtest]);		/* use outype	    */
		if (strcmp(linopcod,"=") == 0		/*    to modify     */
		 || strcmp(linopcod,"init") == 0	/*    some opcodes  */
		 || (strcmp(linopcod,"table") == 0	/*    with prefix   */
		     && (c == 'i' || c == 'p'))) {
			if (c == 'p')	c = 'i';
			if (c == '?')	c = 'a';  /* tmp */
			*str = c;
			*(str+1) = '\0';
			isopcod(strcat(str,linopcod));
			linopnum = opnum;
			linopcod = opcod;
			if (odebug) printf("modified opcod: %s\n",opcod);
		}
		else if (strcmp(linopcod,"oscil") == 0		/* for OSCIL's     */
		      || strcmp(linopcod,"oscili") == 0) {	/*  inarg types -> */
		        if ((c = argtyp(group[opgrpno ] )) != 'a') c = 'k';
		        if ((d = argtyp(group[opgrpno+1])) != 'a') d = 'k';
			sprintf(str,"%s%c%c",linopcod,c,d);
			isopcod(str);			    /*  opcode with suffix */
			linopnum = opnum;
			linopcod = opcod;
			if (odebug)  printf("modified opcod: %s\n",opcod);
			c = argtyp(group[nxtest]);  /* reset outype params */
		}			/* need we reset outype again here ?? */
	}
	tp->opnum = linopnum;			/* now use identified	*/
	tp->opcod = linopcod;			/*   full line opcode	*/
	if (strncmp(linopcod,"out",3) == 0)
		if (nchnls == 1 && strcmp(linopcod,"out" ) != 0
		 || nchnls == 2 && strncmp(linopcod,"outs",4) != 0
		 || nchnls == 4 && strncmp(linopcod,"outq",4) != 0)
		 	synterr("out inconsistent with global nchnls");
	incnt = outcnt = 0;
	tp->outlist = alp = nxtarglist;
	while (nxtest < opgrpno-1)		/* create the out arglist  */
		alp->arg[outcnt++] = strsav(group[nxtest++]);
	if ((alp->count = outcnt) == 0)
		tp->outlist = &nullist;
	else alp = nxtarglist = (ARGLST *) &alp->arg[outcnt]; /* & prep ins */
	tp->inlist = alp;
	nxtest++;
	while (nxtest < grpcnt)			/*	& ensuing inargs  */
		alp->arg[incnt++] = strsav(group[nxtest++]);
	if ((alp->count = incnt) == 0)
		tp->inlist = &nullist;
	else nxtarglist = (ARGLST *) &alp->arg[incnt];
	grpcnt = 0;				/* all done w. these groups */
	
spctst:	if (nxtarglist >= argspacend)
		die("low on ARGSPACE");
	tp->xincod = 0;
	if (tp->opnum == INSTR) {			/* for opcod INSTR  */
		if (instrblk)
			synterr("instr blks cannot be nested");
		else instrblk = 1;
		resetouts();				/* reset #out counts */
		lblclear();				/* restart labelist  */
	}
	else if (tp->opnum == ENDIN) {			/* ENDIN:	*/
		lblchk();				/* chk missed labels */
		if (!instrblk)
			synterr("unmatched endin");
		else instrblk = 0;
	}
	else {					/* for all other opcodes:  */
	extern	 OENTRY	opcodlst[];
	register OENTRY	*ep = opcodlst + tp->opnum;
	register int	n, nreqd;
	register char	tfound, treqd, *types;

		if (!instrblk)
			synterr("misplaced opcode");
		if ((n = incnt) > (nreqd = strlen(types = ep->intypes))) {
			if ((treqd = types[nreqd-1]) == 'n') {/* indef args: */
				if (!(incnt & 01))	      /* require odd */
					synterr("missing or extra arg");
			}
			else if (treqd != 'm')		      /* else any no */
				synterr("too many input args");
		}
		else if (incnt < nreqd) {		/*  or set defaults: */
			do switch(types[incnt]) {
				case 'o': alp->arg[incnt++] = strsav("0");
					  break;
				case 'p': alp->arg[incnt++] = strsav("1");
					  break;
				case 'q': alp->arg[incnt++] = strsav("10");
					  break;
				case 'v': alp->arg[incnt++] = strsav(".5");
					  break;
				default:  synterr(
					    "insufficient required arguments");
					  goto chkin;
			   }
			while (incnt < nreqd);
			alp->count = n = incnt;		/*    in extra space */
			nxtarglist = (ARGLST *) &alp->arg[incnt];
		}
	chkin:	while (n--) {					/* inargs:   */
			s = tp->inlist->arg[n];
			if (n >= nreqd)			/* det type required */
				treqd = 'i';		/*   (indef in-type */
			else treqd = types[n];		/*	 or given)   */
			if (treqd == 'l') {		/* if arg takes lbl  */
				if (odebug) printf("treqd = l\n");
				lblrequest(s);		/*	req a search */
				continue;		/*	chk it later */
			}
			tfound = argtyp(s);		/* else get arg type */
			if (tfound != 'c' && tfound != 'p' && !lgprevdef) {
				sprintf(errmsg,
				 "input arg '%s' used before defined",s);
				synterr(errmsg);
			}
			if (odebug)
				printf("treqd %c, tfound %c\n",treqd,tfound);
			if (tfound == 'a' && n < 4) {
			        static short xincod[4] = {2,1,8,4};
				tp->xincod += xincod[n];
			}
			switch(treqd) {
			case 'd': if (tfound != 'd')
					intyperr(n,tfound);
				  break;
			case 'w': if (tfound != 'w')
					intyperr(n,tfound);
				  break;
			case 'a': if (tfound != 'a')
					intyperr(n,tfound);
				  break;
			case 's':
			case 'x': if (tfound == 'a') {
					if (tp->outlist != &nullist
					 && argtyp(tp->outlist->arg[0]) != 'a')
						intyperr(n,tfound);
					break;
				  }
			case 'k': if (tfound == 'k')
					break;
			case 'i':
			case 'm':
			case 'n':
			case 'o':
			case 'p':
			case 'q':
			case 'v': if (treqd != 's'
				   && (tfound == 'i' || tfound == 'p'
				    || tfound == 'c' || tfound == 'r'))
				  	break;
				  intyperr(n,tfound);
				  break;
			case 'B': if (tfound == 'B')
					break;
			case 'b': if (tfound == 'b')
					break;
			default:  intyperr(n,tfound);
				  break;
			}
		}
		if (odebug) printf("xincod = %d\n",tp->xincod);
		if ((n = outcnt) != strlen(types = ep->outypes)
		  && (*types != 'm' || !n || n > 4))
			synterr("illegal no of output args");
		while (n--) {					/* outargs:  */
			s = tp->outlist->arg[n];
			treqd = types[n];
			tfound = argtyp(s);			/*  found    */
			if (odebug)
				printf("treqd %c, tfound %c\n",treqd,tfound);
			if (tfound == 'd' || tfound == 'w')
			    if (lgprevdef) {
			        sprintf(errmsg,"output name previously used, \
 type '%c' must be uniquely defined", tfound);
				synterr(errmsg);
			    }
			if (tfound == treqd)			/*  as reqd, */
				continue;
			switch(treqd) {				/*  or else  */
			case 's': if (tfound == 'a' || tfound == 'k')
					continue;
				  break;
			case 'i': if (tfound == 'p')
					continue;
				  break;
			case 'B': if (tfound == 'b')
					continue;
				  break;
			case 'm': if (tfound == 'a')
					continue;
				  break;
			}
			sprintf(errmsg,"output arg '%s' illegal type",s);
			synterr(errmsg);
		}
		if (outcnt)			/* pftype defined by outarg */
			tp->pftype = tfound;
	        else if (incnt) {
		  if (ep->intypes[0] != 'l')		/* else by 1st inarg */
			tp->pftype = argtyp(tp->inlist->arg[0]);
		  else tp->pftype = 'l';		/*   (unless label)  */
		}
	}
	return(tp);				/* return the text blk */
}

intyperr(n,tfound)
 int n;
 char tfound;
{
register char *s = grpsav[opgrpno + n];
	char t[10];

	switch(tfound) {
	case 'd':
	case 'w':
	case 'a':
	case 'k':
	case 'i':
	case 'p': t[0] = tfound;
		  t[1] = '\0';
		  break;
	case 'r':
	case 'c': strcpy(t,"const");
		  break;
	case 'b':
	case 'B': strcpy(t,"boolean");
		  break;
	case '?': strcpy(t,"??");
		  break;
	}
	sprintf(errmsg,"input arg '%s' of type %s not allowed",s,t);
	synterr(errmsg);
}

 char *
strsav(s)
 register char *s;
{
register char	*t = sspace, *u;

	do {
		if (*s == *t && strcmp(s,t) == 0) /* srch storage for match */
			return(t);		   /*  & return where found  */
		while (*t++);
	}
	while (t < sspnxt);
	u = t;
	while (*t++ = *s++);			/* else enter as new string */
	if ((sspnxt = t) >= ssplim)
		die("STRSPACE overflow");
	return(u);				/* & return with its address */
}

isopcod(s)				/* tst a string against opcodlst  */
 char *s;				/*   & set op carriers if matched */
{
extern	 OENTRY	opcodlst[], *oplstend;
register OENTRY	*ep;
register char	*ename;

	ep = opcodlst;
	while (++ep < oplstend && (ename = ep->opname) != NULL)
		if (strcmp(s,ename) == 0) {		/* on corr match,   */
			opnum = ep - opcodlst;		/*  set op carriers */
			opcod = ename;
			return(1);			/*  & report success */
		}
	return(0);
}

 char
argtyp(s)		/* find arg type:  d, w, a, k, i, c, p, r, B, b */
 register char *s;	/*   also set lgprevdef if !c && !p */
{
	register char c;

	if (((c = *s) >= '0' && c <= '9')
	  || c == '.' || c == '-' || c == '+')
	  	return('c');					/* const */
	if (pnum(s) >= 0)
		return('p');					/* pnum	*/
	lgprevdef = lgexist(s);					/* (lgprev) */
	if (strcmp(s,"sr") == 0 || strcmp(s,"kr") == 0
	 || strcmp(s,"ksmps") == 0 || strcmp(s,"nchnls") == 0)
	 	return('r');					/* rsvd	*/
	if (c == 'd' || c == 'w') /* N.B. d,w NOT YET #TYPE OR GLOBAL */
		return(c);
	if (c == '#')
		c = *(++s);
	if (c == 'g')
		c = *(++s);
	if (c == 'a' || c == 'k' || c == 'i' || c == 'B' || c == 'b')
		return(c);
	else return('?');
}

lblclear()
{
	lblcnt = 0;
}

lblrequest(s)
 register char *s;
{
register LBLREQ *reqp, *reqlim;

	for (reqp=lblreq, reqlim=lblreq+lblcnt; reqp<reqlim; reqp++)
		if (strcmp(reqp->label,s) == 0)
			return;
	reqp->reqline = curline;
	reqp->label =s;
	lblcnt++;
}

lblfound(s)
 register char *s;
{
register LBLREQ *reqp, *reqlim;

	for (reqp=lblreq, reqlim=lblreq+lblcnt; reqp<reqlim; reqp++ )
		if (strcmp(reqp->label,s) == 0) {
			if (reqp->reqline == 0)
				synterr("duplicate label");
			goto noprob;
		}
	if (++lblcnt >= LBLMAX)
		die("label list is full");
	reqp->label = s;
noprob:	reqp->reqline = 0;
}

lblchk()
{
register LBLREQ *reqp, *reqlim;
register int	n;

	for (reqp=lblreq, reqlim=lblreq+lblcnt; reqp<reqlim; reqp++ )
		if (n = reqp->reqline) {
			register char	*s;
			printf("error line %d.  unknown label:\n",n);
			s = linadr[n];
			do putchar(*s);
			while (*s++ != '\n');
			synterrcnt++;
		}
}

synterr(s)
 char *s;
{
	int	c;
	char	*cp;

	printf("error:  %s",s);
	if ((cp = linadr[curline]) != NULL) {
		printf(", line %d:\n",curline);
		do putchar((c = *cp++));
		while (c != '\n');
	}
	else putchar('\n');
	synterrcnt++;
}

synterrp(errp,s)
 char *errp, *s;
{
	char	*cp;

	synterr(s);
	cp = linadr[curline];
	while (cp < errp) {
		if (*cp++ == '\t')
		putchar('\t');
		else putchar(' ');
	}
	printf("^\n");
}

dies(s,t)
 char *s, *t;
{
	sprintf(errmsg,s,t);
	die(errmsg);
}

die(s)
 char *s;
{
	printf("%s\n",s);
	exit(1);
}

printgroups(grpcnt)			/*   debugging aid (onto stdout) */
 register int grpcnt;
{
register char	c, *cp = group[0];
	printf("groups:\t");
	while (grpcnt--) {
		while (c = *cp++)
			putchar(c);
		putchar(' ');
	}
	putchar('\n');
}
