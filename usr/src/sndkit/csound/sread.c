#include "sort.h"                                     /*   SREAD.C     */
#include "cs.h"  /* for PMAX */

FILE    *SCOREIN, *SCOREOUT;
int     sectcnt;                /* count of sections in scorefile       */
SRTBLK  *frstbp;                /* logical frstblk in srtblk chain      */
static SRTBLK  *bp, *prvibp;    /* current srtblk,  prev w/same int(p1) */
static char    *sp, *nxp;       /* string pntrs into srtblk text        */
static char    op;              /* opcode of current event              */
static int     warpin;          /* input format sensor                  */
static int     linpos;          /* line position sensor                 */
static int     lincnt;          /* count of lines/section in scorefile  */

sread()                     /*  called from main,  reads from SCOREIN   */
{                           /*  each score statement gets a sortblock   */
	char getop();
	int  rtncod;            /* return code to calling program:      */
				/*   2 = section read, more remaining   */
				/*   1 = last section,   0 = null file  */
	bp = prvibp = frstbp = NULL;
	nxp = NULL;
	warpin = 0;
        lincnt = 1; 
        rtncod = 1; 
	sectcnt++;

	salcinit();              /* init the mem space for this section */
	while ((op = getop()) != EOF) {  /* read next op from scorefile */
		rtncod = 2;
		salcblk();       /* build a line structure; init bp,nxp */
		switch(op) {            /*  and dispatch on opcodes     */
		case 'i':
		case 'f':
		case 'a':
			ifa();
			break;
		case 'w':
			warpin++;
			copypflds();
			break;
                case 't':
			copypflds();
			break;
		case 's':
			copylin();
			return(rtncod);
                case 'e':       
			copylin();
			return(--rtncod);
		default:
			fprintf(stderr,"sread is confused on legal opcodes\n");
			break;
		}
	}
	return(--rtncod);
}

copylin()                       /* copy source line to srtblk   */
{
	char c;
	nxp--;
	while ((c = *nxp++ = getc(SCOREIN)) != LF && c != EOF);
		;
	lincnt++;
	linpos = 0;
}                

copypflds()
{
        bp->pcnt = 0;
	while (getpfld())       /* copy each pfield,    */
		bp->pcnt++;     /* count them,          */
	*(nxp-1) = LF;          /* terminate with newline */
 }

ifa()
{
	SRTBLK *prvbp;
	float stof();
	int n;

	bp->pcnt = 0;
	while (getpfld()) {             /* while there's another pfield,  */
		if (++bp->pcnt == PMAX) {
			fprintf(stderr,"sread: instr pcount exceeds PMAX\n");
			fprintf(stderr,"\t sect %d line %d\n", sectcnt, lincnt);
			fprintf(stderr,"      remainder of line flushed\n");
			flushlin();
			continue;
		}
		if (nxp-sp == 2 && (*sp == '.' || *sp == '+')) {
			if (op == 'i'
			  && (*sp == '.' || bp->pcnt == 2)
			  && ((bp->pcnt >= 2 && (prvbp = prvibp) != NULL
					&& bp->pcnt <= prvbp->pcnt)
			   || (bp->pcnt == 1 && (prvbp = bp->prvblk) != NULL
					&& prvbp->text[0] == 'i'))) {
				if (*sp == '.') {
					nxp = sp;
					pcopy((int)bp->pcnt, 1, prvbp);
				}
				else bp->p2val = prvbp->p2val + prvbp->p3val;
			}
			else carryerror();
		}
		else switch(bp->pcnt) {         /*  watch for p1,p2,p3, */
		case 1: bp->p1val = stof(sp);   /*   & float, setinsno..*/
			if (op == 'i')
				setprv();
		        else prvibp = NULL;
			break;
		case 2: bp->p2val = stof(sp);
			break;
		case 3: bp->p3val = stof(sp);
			break;
		default:break;
		}
		switch(bp->pcnt) {              /* newp2, newp3:        */
		case 2: if (warpin) {                   /* for warpin,  */
				getpfld();              /*   newp2 follows */
				bp->newp2 = stof(sp);
				nxp = sp;               /*    (skip text)  */
			}
			else bp->newp2 = bp->p2val;     /* else use p2val  */
			break;
		case 3: if (warpin) {
				getpfld();
				bp->newp3 = stof(sp);   /* same for newp3  */
				nxp = sp;
			}
			else bp->newp3 = bp->p3val;
			break;
		}
	}
	if (op == 'i'                   /* then carry any rem pflds */
	  && ((prvbp = prvibp) != NULL
	    || !bp->pcnt && (prvbp = bp->prvblk) != NULL
			  && prvbp->text[0] == 'i')
          && (n = prvbp->pcnt - bp->pcnt) > 0) {
		pcopy((int)bp->pcnt + 1, n, prvbp);
		bp->pcnt += n;
	}
	*(nxp-1) = LF;                  /* terminate this stmnt with newline */
}

setprv()                                /*  set insno = (int) p1val     */
{                                       /*  prvibp = prv note, same insno */
	register SRTBLK *p;
	register short n;

	n = bp->insno = (short) bp->p1val;              /* set current insno */
	p = bp;
	while ((p = p->prvblk) != NULL)
	        if (p->insno == n) {
		        prvibp = p;                     /* find prev same */
			return;
		}
	prvibp = NULL;                                  /*  if there is one */
}

carryerror()                            /* print offending text line */
{                                       /*      (partial)            */
	register char *p;

	fprintf(stderr,
	 "sread: illegal use of carry, sect %d line %d,   0 substituted\n",
		sectcnt,lincnt);
	*(nxp-3) = SP;
	p = bp->text;
	while (p <= nxp-2)
		putc(*p++,stderr);
	fprintf(stderr,"<=\n");
	*(nxp-2) = '0';
}

pcopy(pfno,ncopy,prvbp)         /* copy pfields from prev note of this instr */
 int pfno,ncopy;                /*      begin at pfno, copy 'ncopy' fields   */
 SRTBLK *prvbp;                 /*      uses *nxp++;    sp untouched         */
{
	register char *p, *pp, c;
	int  n;

	pp = prvbp->text;                       /* in text of prev note,    */
	n = pfno;
	while (n--)
		while (*pp++ != SP)             /*    locate starting pfld  */
			;
	n = ncopy;
	p = nxp;
	while (n--) {                           /*      and copy n pflds    */
		while ((*p++ = c = *pp++) != SP && c != LF)
			;
		switch(pfno) {
		case 1: bp->p1val = prvbp->p1val;       /*  with p1-p3 vals */
			setprv();
			break;
		case 2: if (*(p-2) == '+')              /* (interpr . of +) */
				bp->p2val = prvbp->p2val + prvbp->p3val;
			else bp->p2val = prvbp->p2val;
			bp->newp2 = bp->p2val;
			break;
		case 3: bp->newp3 = bp->p3val = prvbp->p3val;
			break;
		default:break;
		}
		pfno++;
	}
	nxp = p;                                /* adjust globl nxp pntr */
}

#define MEMSIZ  40000L          /* size of memory requests from system  */
#define MARGIN  400             /* minimum remaining before new request */

static MEMHDR *basmem = NULL;
static MEMHDR *curmem;
static char   *memend;          /* end of cur memblk     */

salcinit()                      /* init the sorter mem space for a new section */
{                               /*  alloc 1st memblk if nec; init *nxp to this */
	if ((curmem = basmem) == NULL) {
		curmem = basmem = (MEMHDR *) mmalloc((long)MEMSIZ);
		curmem->nxtmem = NULL;
		curmem->memend = (char *)curmem + MEMSIZ;
        }                                 
	memend = curmem->memend;
	nxp = (char *)curmem + sizeof(MEMHDR);
}

salcblk()                   /* alloc a srtblk from current mem space:   */
{                           /*   align following *nxp, set new bp, nxp  */
			    /*   set srtblk lnks, put op+blank in text  */

	register SRTBLK *prvbp;

	if (memend - nxp < MARGIN) {            /* if this memblk exhausted */
                if (nxp > memend)  goto margerr;
		if (curmem->nxtmem != NULL)          /*      chain to next  */
			curmem = curmem->nxtmem;
		else {                               /*      or alloc a new */
		        MEMHDR *prvmem = curmem;
			fprintf(stderr,"sread: requesting more memory\n");
			curmem = (MEMHDR *) mmalloc((long)MEMSIZ);
			prvmem->nxtmem = curmem;
			curmem->nxtmem = NULL;
			curmem->memend = (char *)curmem + MEMSIZ;
		}
		memend = curmem->memend;
		nxp = (char *)curmem + sizeof(MEMHDR);
	}
				/* now allocate a srtblk from this space:   */
	prvbp = bp;
	bp = (SRTBLK *) ((((long) nxp) + 3) & -4);
	if (frstbp == NULL)
		frstbp = bp;
        if (prvbp != NULL)
		prvbp->nxtblk = bp;     /* link with prev srtblk        */
	bp->prvblk = prvbp;
	bp->nxtblk = NULL;
	nxp = bp->text;
	*nxp++ = op;                    /* place op, blank into text    */
	*nxp++ = SP;
	return;

margerr:  fprintf(stderr,"sread:  text space overrun, increase MARGIN\n");
	exit(1);
}

sfree()                           /* free all sorter allocated space */
{                                 /*    called at completion of sort */
  register MEMHDR *curmem, *nxtmem;

        for (curmem = basmem; curmem != NULL; curmem = nxtmem) {
		nxtmem = curmem->nxtmem;
		free((char *)curmem);
	}
	basmem = NULL;
}

flushlin()                      /* flush input to end-of-line;  inc lincnt */
{
	register char c;

	while ((c = getc(SCOREIN)) != LF && c != EOF)
		;
	lincnt++;
	linpos = 0;
}

 char
sget1()                         /* get first non-white, non-comment char */
{
	register char c;

srch:   while ((c = getc(SCOREIN)) == SP || c == '\t' || c == LF)
		if (c == LF) {
			lincnt++;
			linpos = 0;
		}
        if (c == ';' || c == 'c') {
		flushlin();
		goto srch;
	}
	return(c);
}

 char
getop()                         /* get next legal opcode */
{
	register char c;

nextc:  c = sget1();                    /* get first active char */
	switch(c) {
	case 'i':                       /*   and check legality  */
	case 'f':
	case 'a':
	case 't':
	case 'w':
	case 's':
	case 'e':
	case EOF:
		break;                  /* if ok, go with it    */
	default:                        /*   else complain      */
		fprintf(stderr,
		 "sread: illegal opcode %c, sect %d line %d\n",c,sectcnt,lincnt);
		fprintf(stderr,"      remainder of line flushed\n");
		flushlin();
		goto nextc;
	}
	linpos++;
        return(c);
}

getpfld()                               /* get pfield val from SCOREIN file */
{                                       /*      set sp, nxp                 */
	register char c, *p;

	c = sget1();                    /* get 1st non-white,non-comment c  */
	if ((c < '0' || c > '9')                /* if non-numeric          */
	  && c != '.' && c != '+' && c != '-'   /*    and non-carry        */
	  && c != 'n' && c != 'p'               /*    and non-special-char */
	  && c != '<' && c != '>') {
		ungetc(c,SCOREIN);                /* then no more pfields    */
                if (linpos)
			fprintf(stderr,
			 "sread: unexpected char %c, sect %d line %d\n",
				c,sectcnt,lincnt);
		return(0);                      /*    so return            */
	}
	p = sp = nxp;                       /* else start copying to text  */
	*p++ = c;
	linpos++;
	while (((c = getc(SCOREIN)) >= '0' && c <= '9')
	  || c == '.' || c == '+' || c == '-'
	  || c == 'n' || c == 'p'               /* while still legal chars, */
	  || c == '<' || c == '>')              /*   continue to bld string */
		*p++ = c;
	ungetc(c,SCOREIN);                      /* any illegal is delimiter */
	*p++ = SP;
	nxp = p;                                /*  add blank      */
	return(1);                              /*  and report ok  */
}

 float
stof(s)                 /* convert string to float  */
 char s[];              /* (assumes no white space at beginning */
{                       /*      but a blank or nl at end)       */
	double val, power;
	int sign;
	char *p;

	sign = 1;
	p = s;
	if (*p == '+')
		p++;
	if (*p == '-') {
		p++;
		sign = -1;
	}
	for (val = 0; *p >= '0' && *p <= '9'; p++)
		val = 10 * val + (*p - '0');
	if (*p == '.')
		p++;
	for (power = 1; *p >= '0' && *p <= '9'; p++) {
		val = 10 * val + (*p - '0');
		power *= 10;
	}
	if (*p != SP && *p != LF) {
		fprintf(stderr,
		 "sread: illegal number format, sect %d line %d:  ",
			sectcnt,lincnt);
		p = s;
		while (*p != SP && *p != LF) {
			putc(*p,stderr);
			*p++ = '0';
		}
		fprintf(stderr,"   zero substituted.\n");
		val = 0;
        }
        return((float)(sign * val / power));
}
