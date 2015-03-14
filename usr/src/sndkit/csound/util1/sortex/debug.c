#include "../../sort.h"                                 /*    DEBUG.C  */

allout()                        /* print out all srtblks */
{
	register SRTBLK *p;

	if ((p = frstbp) != NULL) {
		do blkout(p);
		while ((p = p->nxtblk) != NULL);
	}
}

blkout(bp)                      /* print out one sortblk */
 register SRTBLK *bp;
{
	char c,*p;

	fprintf(stderr,"%d:\t%d\t%d\t%d\t%4.2f\t%4.2f\t%4.2f\n",
		bp,bp->prvblk,bp->insno,bp->pcnt,
		bp->p1val,bp->p2val,bp->p3val);
	fprintf(stderr,"\t%d\t",
		bp->nxtblk);
	p = bp->text;
	while ((c = *p++) != '\n')
		putc(c,stderr);
	putc('\n',stderr);
}

textout()
{
	register SRTBLK *bp;
	register char c, *p;

	if ((bp = frstbp) != NULL) {
		do {
			p = bp->text;
			while ((c = *p++) != '\n')
				putc(c,stderr);
			putc(c,stderr);
		}
		while ((bp = bp->nxtblk) != NULL);
	}
}
