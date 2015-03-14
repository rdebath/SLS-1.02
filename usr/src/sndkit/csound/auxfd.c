#include "cs.h"							/*	AUXFD.C		*/

extern	INSDS	*curip;		/* current insds, maintained by insert.c */

static  void auxrecord(),auxchprint(),fdchprint();

void auxalloc(nbytes,auxchp)	/* allocate an auxds, or expand an old one */
 register long  nbytes;		/*    call only from init (xxxset) modules */
 register AUXCH *auxchp;
{
register char *auxp;

	if ((auxp = auxchp->auxp) != NULL)      /* if size change only,	     */
		free(auxp);     		/*	free the old space   */
	else auxrecord(auxchp); 		/* else linkin new auxch blk */
	auxp = mcalloc(nbytes);                 /* now alloc the space       */
	auxchp->size = nbytes;                  /* update the internal data  */
	auxchp->auxp = auxp;
	auxchp->endp = auxp + nbytes;
	if (odebug) auxchprint(curip);
}

static void auxrecord(auxchp)	/* put auxch into chain of xp's for this instr */
 register AUXCH  *auxchp;        /*	called only from auxalloc	*/
{
register AUXCH	*prvchp, *nxtchp;

	prvchp = &curip->auxch;         		/* from current insds,	*/
	while ((nxtchp = prvchp->nxtchp) != NULL)       /* chain through xplocs */
		prvchp = nxtchp;
	prvchp->nxtchp = auxchp;			/* then add this xploc	*/
        auxchp->nxtchp = NULL;  		        /* & terminate the chain */
}

void fdrecord(fdchp)		/* put fdchp into chain of fd's for this instr */
 register FDCH  *fdchp; 	/*	call only from init (xxxset) modules   */
{			
register FDCH	*prvchp, *nxtchp;

	prvchp = &curip->fdch;          		/* from current insds,	*/
	while ((nxtchp = prvchp->nxtchp) != NULL)	/* chain through fdlocs */
		prvchp = nxtchp;
	prvchp->nxtchp = fdchp; 			/* then add this fdloc	*/
	fdchp->nxtchp = NULL;           		/* & terminate the chain */
	if (odebug) fdchprint(curip);
}

void fdclose(fdchp)		/* close a file and remove from fd chain */
 register FDCH  *fdchp;		/*  call only from inits, after fdrecord */
{
register FDCH	*prvchp, *nxtchp;

	prvchp = &curip->fdch;          		/* from current insds,	*/
	while ((nxtchp = prvchp->nxtchp) != NULL) {     /* chain through fdlocs */
		if (nxtchp == fdchp) {  		/*   till find this one	*/
			close(fdchp->fd);		/* then close the file	*/
			fdchp->fd = 0;  		/*   delete the fd &	*/
			prvchp->nxtchp = fdchp->nxtchp; /* unlnk from fdchain */
			if (odebug) fdchprint(curip);
			return;
		}
		else prvchp = nxtchp;
	}
	fdchprint(curip);
	sprintf(errmsg,"fdclose: no record of fd %d", fdchp->fd);
	die(errmsg);
}

void auxchfree(ip)		/* release all xds in instr auxp chain  */
 register INSDS *ip;		/*   called by insert at orcompact	*/
{
register AUXCH	*curchp = &ip->auxch;
register char	*auxp;

	if (odebug) auxchprint(ip);
	while ((curchp = curchp->nxtchp) != NULL) {  	/* for all xp's in chain: */
		if ((auxp = curchp->auxp) == NULL) {
			auxchprint(ip);
			dies("auxchfree: illegal auxp %lx in chain",auxp);
		}
		free(auxp);			/*	free the space	*/
		curchp->auxp = NULL;		/*	& delete the pntr */
	}
	ip->auxch.nxtchp = NULL;   		/* finally, delete the chain */
	if (odebug) auxchprint(ip);
}

void fdchclose(ip)		/* close all files in instr fd chain     */
 register INSDS *ip;		/*   called by insert on deact & expire  */
{			/*   (also musmon on s-code, & fgens for gen01)	 */
register FDCH	*curchp = &ip->fdch;
register int	fd;

	if (odebug) fdchprint(ip);
	while ((curchp = curchp->nxtchp) != NULL) {	/* for all fd's in chain: */
		if ((fd = curchp->fd) <= 2) {
			fdchprint(ip);
			sprintf(errmsg,"fdclose: illegal fd %d in chain",fd);
			die(errmsg);
		}
		close(fd);			/*	close the file	*/
		curchp->fd = 0;			/*	& delete the fd	*/
	}
	ip->fdch.nxtchp = NULL; 		/* finally, delete the chain */
	if (odebug) fdchprint(ip);
}

static void auxchprint(ip)		/* print the xp chain for this insds blk */
 register INSDS *ip;
{
register AUXCH	*curchp = &ip->auxch;

	printf("auxlist for instr %d (%lx):\n", ip->insno, ip);
	while ((curchp = curchp->nxtchp) != NULL)         /* chain through auxlocs */
		printf("\tauxch at %lx: size %lx, auxp %lx, endp %lx\n",
		       curchp, curchp->size, curchp->auxp, curchp->endp);
}

static void fdchprint(ip)		/* print the fd chain for this insds blk */
 register INSDS *ip;
{
register FDCH	*curchp = &ip->fdch;

	printf("fdlist for instr %d (%lx):", ip->insno, ip);
	while ((curchp = curchp->nxtchp) != NULL)    /* chain through fdlocs */
		printf("  fd %d in %lx", curchp->fd, curchp);
	putchar('\n');
}
