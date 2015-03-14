#include "cs.h"			/*					RDSCOR.C	*/

extern FILE *scfp;

static scanflt(pfld)     /* read a float from scorefile; return 1 if OK, else 0 */
 float *pfld;
{
    register int  c;
    register long val=0, scale=1;

    while ((c = getc(scfp)) == ' ' || c == '\t')     /* skip leading white space */
        ;
    if (!(c>='0' && c<='9' || c=='+' || c=='-' || c=='.'))
        return(0);                                  /* fail if not valid float  */
    if (c == '-') {scale = -1; c = getc(scfp);}
    if (c == '+' || c == '0')  c = getc(scfp);
    while (c >= '0' && c <= '9') {
        val *= 10;
	val += c - '0';
	c = getc(scfp);
    }
    if (c == '.')
        c = getc(scfp);
    while (c >= '0' && c <= '9') {
        val *= 10;
	val += c - '0';
	scale *= 10;
	c = getc(scfp);
    }
    *pfld = (float) val/scale;          /* write pfield */
    ungetc(c,scfp);                    /* push next whitespace back to scorefile */
    return(1);
}

static void flushline()			/* flush scorefile to next newline */
{
    int c;
                                    
    while ((c = getc(scfp)) != EOF && c != '\n')
	;
}

rdscor(e)			/* read next score-line from scorefile  */
 register EVTBLK *e;            /* presumes good format from scsort     */
{
    register float *pp, *plim;
    int  c;

    while ((c = getc(scfp)) != EOF)
	switch (c) {
	case ';':
	case 'w':
	    flushline();
	    break;
	default:
	    e->opcod = c;				           /* opcod */
	    pp = &e->p[0];
	    plim = &e->p[PMAX];
	    if (getc(scfp) != '\n' && scanflt(++pp))	            /* p1      */
	      if (getc(scfp) != '\n' && scanflt(&e->p2orig))        /* p2 orig */
		if (getc(scfp) != '\n' && scanflt(++pp))            /* p2 warp */
		  if (getc(scfp) != '\n' && scanflt(&e->p3orig))    /* p3 orig */
		    if (getc(scfp) != '\n' && scanflt(++pp))        /* p3 warp */
		      while (getc(scfp) != '\n' && scanflt(++pp))   /* p4....  */
			if (pp >= plim) {
			  flushline();
			  ++pp;
			  break;
			}
	    e->pcnt = pp - &e->p[0];
	    return(1);
	}
    return(0);
}
