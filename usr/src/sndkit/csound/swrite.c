#include "sort.h"                                          /*    SWRITE.C  */

static int    lincnt, pcnt;

swrite()
{
	SRTBLK *bp;
	register char *p, c;
	char *pfout();

	if ((bp = frstbp) == NULL)
		return;
        lincnt = 0;
	if ((c = bp->text[0]) != 'w'
	  && c != 's' && c != 'e') {     /* if no warp stmnt but real data,  */
		fprintf(SCOREOUT,"w 0 60\n");/* create warp-format indicator */
		lincnt++;
	}
nxtlin: lincnt++;                       /* now for each line:           */
	p = bp->text;
	c = *p++;
	putc(c,SCOREOUT);
	switch(c) {
	case 'i':
	case 'f':
	case 'a':
		putc(*p++,SCOREOUT);
		while ((c = *p++) != SP && c != LF)
			putc(c,SCOREOUT);               /* put p1       */
		putc(c,SCOREOUT);
		if (c == LF)
			break;
		fltout(bp->p2val);                      /* put p2val,   */
		putc(SP,SCOREOUT);
		fltout(bp->newp2);                      /*   newp2,     */
		while ((c = *p++) != SP && c != LF)
			;
		putc(c,SCOREOUT);                       /*   and delim  */
		if (c == LF)
			break;
                fltout(bp->p3val);                      /* put p3val,   */
		putc(SP,SCOREOUT);
		fltout(bp->newp3);                      /*   newp3,     */
		while ((c = *p++) != SP && c != LF)
			;
		putc(c,SCOREOUT);                       /*   and delim  */
		pcnt = 3;
		while (c != LF) {
			pcnt++;
			p = pfout(bp,p);        /* now put each pfield  */
			c = *p++;
			putc(c,SCOREOUT);       /*  and its delimiter   */
		}
		break;
	case 'w':
	case 't':
	case 's':
	case 'e':
		while ((c = *p++) != LF)        /* put entire line      */
			putc(c,SCOREOUT);
		putc(LF,SCOREOUT);
		break;
	default:
		fprintf(stderr,"swrite: unexpected opcode, section %d line %d\n",
		  sectcnt,lincnt);
                break;
	}
	if ((bp = bp->nxtblk) != NULL)
		goto nxtlin;
}

 char *
pfout(bp,p)
 SRTBLK *bp;
 register char *p;
{
	char *nextp(), *prevp(), *ramp(), *fpnum();

	switch(*p) {
	case 'n':
		p = nextp(bp,p);
		break;
	case 'p':
		p = prevp(bp,p);
		break;
	case '<':
	case '>':
		p = ramp(bp,p);
		break;
	default:
		p = fpnum(p);
		break;
	}
	return(p);
}

 SRTBLK *
nxtins(bp)                              /* find nxt note with same p1 */
 SRTBLK *bp;
{
	float p1;

	p1 = bp->p1val;
	while ((bp = bp->nxtblk) != NULL
	    && (bp->p1val != p1 || bp->text[0] != 'i'))
		;
	return(bp);
}

 SRTBLK *
prvins(bp)                              /* find prv note with same p1 */
 SRTBLK *bp;
{
	float p1;

	p1 = bp->p1val;
	while ((bp = bp->prvblk) != NULL
	    && (bp->p1val != p1 || bp->text[0] != 'i'))
                ;              
	return(bp);
}

 char *
nextp(bp,p)
 SRTBLK *bp;
 register char *p;
{
	char *q;
	int n;

	q = p;
	p++;                                    /* 1st char     */
	if (*p++ != 'p')                        /* 2nd char     */
		goto error;
	n = 999;
	if (*p >= '0' && *p <= '9')
		n = *p++ - '0';
	if (*p >= '0' && *p <= '9')             /* n is np subscript no */
		n = 10*n + (*p++ - '0');
	if (*p != SP && *p != LF)
		goto error;
	if ((bp = nxtins(bp)) != NULL           /* for nxtins, same p1  */
	  && n <= bp->pcnt) {
		q = bp->text;
		while (n--)
			while (*q++ != SP)      /*   go find the pfield */
				;
		pfout(bp,q);                    /*   and put it out     */
	}
	else {
	 error: fprintf(stderr,
		  "swrite: output, sect%d line%d p%d makes illegal reference to ",
		  sectcnt,lincnt,pcnt);
		while (q < p)
			putc(*q++,stderr);
		while (*p != SP && *p != LF)
			putc(*p++,stderr);
		fprintf(stderr,"   Zero substituted\n");
		putc('0',SCOREOUT);
	}
	return(p);
}

 char *
prevp(bp,p)
 SRTBLK *bp;
 register char *p;
{
	char *q;
	int n;

	q = p;
	p++;                                    /* 1st char     */
	if (*p++ != 'p')                        /* 2nd char     */
		goto error;
	n = 999;
	if (*p >= '0' && *p <= '9')
		n = *p++ - '0';
	if (*p >= '0' && *p <= '9')             /* n is np subscript no */
		n = 10*n + (*p++ - '0');
	if (*p != SP && *p != LF)
		goto error;
	if ((bp = prvins(bp)) != NULL           /* for prvins, same p1, */
	  && n <= bp->pcnt) {
		q = bp->text;
		while (n--)
			while (*q++ != SP)      /*   go find the pfield */
				;
		pfout(bp,q);                    /*   and put it out     */
	}
	else {
	 error: fprintf(stderr,
		  "swrite: output, sect%d line%d p%d makes illegal reference to ",
		  sectcnt,lincnt,pcnt);
		while (q < p)
			putc(*q++,stderr);
		while (*p != SP && *p != LF)
			putc(*p++,stderr);
		fprintf(stderr,"   Zero substituted\n");
		putc('0',SCOREOUT);
	}
	return(p);
}

 char *
ramp(bp,p)                      /*     NB np's may reference a ramp  */
 SRTBLK *bp;                    /*  but ramps must terminate in valid nums */
 register char *p;
{
	register char *q;
	char   *psav;
	SRTBLK *prvbp, *nxtbp;
	float pval, qval, rval, p2span;
extern  float stof();
        int pnum, n;

	psav = ++p;
	if (*psav != SP && *psav != LF)
		goto error1;
	pnum = 0;
	q = bp->text;
	while (q < p)
		if (*q++ == SP)
			pnum++;
	prvbp = bp;
backup: if ((prvbp = prvins(prvbp)) != NULL) {
		p = prvbp->text;
		n = pnum;
		while (n--)
			while (*p++ != SP)
				;
		if (*p == '>' || *p == '<')
			goto backup;
	}
	else goto error2;
	nxtbp = bp;
forwrd: if ((nxtbp = nxtins(nxtbp)) != NULL) {
		q = nxtbp->text;
		n = pnum;
		while (n--)
			while (*q++ != SP)
				;
		if (*q == '>' || *q == '<')
			goto forwrd;
	}
	else goto error2;
	pval = stof(p);         /* the error msgs generated by stof     */
	qval = stof(q);                                 /* are misleading */
	if ((p2span = nxtbp->newp2 - prvbp->newp2) <= 0)
		goto error2;
	rval = (qval - pval) * (bp->newp2 - prvbp->newp2) / p2span + pval;
	fltout(rval);
	return(psav);

error1: fprintf(stderr,"swrite: output, sect%d line%d p%d has illegal ramp symbol\n",
	 sectcnt,lincnt,pcnt);
	goto put0;
error2: fprintf(stderr,
	"swrite: output, sect%d line%d p%d ramp has illegal forward or backward ref\n",
	 sectcnt,lincnt,pcnt);
put0:   putc('0',SCOREOUT);
        return(psav);
}

 char *
fpnum(p)                        /*   moves ascii string to SCOREOUT file */
 register char *p;              /*      with fpnum format chk            */
{
	char *q;
	int dcnt;
                
	q = p;
	if (*p == '+')
		p++;
	if (*p == '-')
		putc(*p++,SCOREOUT);
	dcnt = 0;
	while (*p >= '0' && *p <= '9') {
		putc(*p++,SCOREOUT);
		dcnt++;
	}
	if (*p == '.')
		putc(*p++,SCOREOUT);
	while (*p >= '0' && *p <= '9') {
		putc(*p++,SCOREOUT);
		dcnt++;
	}
	if (*p != SP && *p != LF || !dcnt) {
		fprintf(stderr,
		"swrite: output, sect%d line%d p%d has illegal number  ",
			sectcnt,lincnt,pcnt);
		while (q < p)
			putc(*q++,stderr);
		while (*p != SP && *p != LF)
			putc(*p++,stderr);
		fprintf(stderr,"    String truncated\n");
		if (!dcnt)
			putc('0',SCOREOUT);
        }                                
	return(p);
}

fltout(num)                     /* float to ascii on SCOREOUT file  */
 float num;
{
	float incnum, precision, precmult;
	int tenpower, printcnt, digit;

	precision = .000001;
	precmult = 1.0000005;   /* is this ok for float without double? */

	tenpower = printcnt = 0;

	if (num == 0)
		goto done;
	if (num < 0) {
		putc('-',SCOREOUT);
		num *= -1;
	}
	num *= precmult;
	while (num >= 1) {
		num /= 10;
		tenpower++;
	}
	incnum = num;
	while (num/incnum > precision) {
		if (!tenpower--)
			putc('.',SCOREOUT);
		incnum *= 10;
		num *= 10;
		digit = (int) num;
		num -= (float) digit;
		putc((char)(digit + 060),SCOREOUT);
		printcnt++;
	}
	while (tenpower-- > 0) {
		putc('0',SCOREOUT);
		printcnt++;
	}
done:   if (!printcnt)
		putc('0',SCOREOUT);
}
