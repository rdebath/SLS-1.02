#include "cs.h"			/*	       		SOUNDIN.C	*/
#include "soundio.h"

static	float	fzero = 0.;

extern	int	ksmps;
extern  HEADATA *readheader();
extern  short   ulaw_decode[];
extern  char    *buildsfname();	/*  build a soundfile name	*/
extern  char    *getstrformat();
extern  int     getsizformat();
extern  int     readin();      	/* special handling of sound input	  */

 int
sndgetset(p)			/* core of soundinset                */
 register SOUNDIN *p;		/* called from sndinset & from gen01 */
{
  register int	n;
  register HEADATA *hdr;
	long	hdrsize = 0, readlong = 0, sampsinbuf, skipsamps;
	char	*sfname, numstr[10];
	int	sinfd;

	if ((n = p->OUTCOUNT) != 1 && n != 2 && n != 4) {
		sprintf(errmsg,"illegal no of receiving channels");
		goto errtn;
	}
	sfname = buildsfname(SOUNDIN_NAME);
	sprintf(numstr, "%d", (int) *p->ifilno);
	strcat(sfname,numstr);
	if ((sinfd = open(sfname,O_RDONLY)) < 0) {
		sprintf(errmsg,"soundin cannot open %s",sfname);
		goto errtn;
	}
        if ((p->format = *p->iformat) > 0)         /* convert spec'd format code */
	    p->format |= 0x100;
	p->endfile = 0;
	if ((hdr=readheader(sinfd,sfname)) != NULL 	/* if headerblk returned */
	      && !(readlong = hdr->readlong)) {         /* & hadn't readin audio */
	    if (hdr->sr != (long)esr) {   	        /*    chk the hdr codes  */
		sprintf(errmsg,"%s sr = %ld, orch sr = %ld",
			sfname, hdr->sr, (long)esr);
		warning(errmsg);
	    }
	    if (hdr->nchnls != p->OUTCOUNT) {
	        sprintf(errmsg,"%s nchnls = %ld, orch nchnls = %d",
			sfname, hdr->nchnls, nchnls);
		warning(errmsg);
	    }
	    if (p->format && hdr->format != p->format) {
	        sprintf(errmsg,"soundin %s superceded by %s header format %s",
			getstrformat(p->format), sfname, getstrformat((int)hdr->format));
	        warning(errmsg);
	    }
	    switch ((p->format = hdr->format)) {        /*    & copy header data */
	    case AE_CHAR:   break;
	    case AE_ULAW:   break;
	    case AE_SHORT:  break;
	    case AE_LONG:   break;
	    case AE_FLOAT:  break;
	    default: sprintf(errmsg,"%s format %s not yet supported",
			     sfname, getstrformat((int)p->format));
		     goto errcls;
	    }
	    p->sampframsiz = hdr->sampsize * hdr->nchnls;
	    hdrsize = hdr->hdrsize;
	}
	else {                                  /* no hdr:   if no format spec'd, */
	    if (!p->format) {
	        sprintf(errmsg,"%s has no soundfile header, assuming %s",
			sfname, getstrformat(outformat) );
		warning(errmsg);
		p->format = outformat;		/*       use outformat as default */
	    }
	    p->sampframsiz = getsizformat(p->format) * p->OUTCOUNT;
	}
        sampsinbuf = SNDINBUFSIZ / p->sampframsiz;
        if ((skipsamps = *p->iskptim*esr) < sampsinbuf) { /* if sound within 1st buffer */
	    int nreq;
	    if (readlong) {                             /*    fill by direct read     */
	        nreq = SNDINBUFSIZ - sizeof(long);
	        *(long *)p->inbuf = hdr->firstlong;
		n = readin(sinfd, p->inbuf+sizeof(long), nreq);
		p->bufend = p->inbuf + sizeof(long) + n;
	    }
	    else {
	        nreq = SNDINBUFSIZ;
		n = readin(sinfd, p->inbuf, nreq);
		p->bufend = p->inbuf + n;
	    }
	    p->inbufp = p->inbuf + skipsamps * p->sampframsiz;
	}
	else {                 				/* for greater skiptime: */
	    long nbytes = skipsamps * p->sampframsiz;
	    if (hdrsize < 0) {
		int nbufs = nbytes/SNDINBUFSIZ;		/* if headersize unknown,   */
		int nrem = nbytes - (long)nbufs * SNDINBUFSIZ;
		while (--nbufs)        	                /*   spinread to reqd boundary */
		    readin(sinfd,p->inbuf,SNDINBUFSIZ);
		if (nrem)
		    readin(sinfd,p->inbuf,nrem);
	    }
	    else {
	        if (readlong)  nbytes -= sizeof(long);  /* else can seek to boundary */
		if (hdrsize > 0) nbytes += hdrsize;
	        if (lseek(sinfd, nbytes, 0) < 0)
		    die("soundin seek error");
	    }           				/* now read full buf for real */
	    if ((n = readin(sinfd,p->inbuf,SNDINBUFSIZ)) == 0)
		p->endfile = 1;
	    p->inbufp = p->inbuf;
	    p->bufend = p->inbuf + n;
	}
	if (p->inbufp >= p->bufend)   /* needed? */
		p->endfile = 1;
	return(sinfd);                             /* return the active fd  */

 errcls: close(sinfd);               /* init error:  close any open file */
 errtn:  return(0);                  /*              return empty handed */
}

sndinset(p)            /* init routine for instr soundin   */
 SOUNDIN *p;           /* shares above sndgetset with gen01*/
{
        int    sinfd;

	if (p->fdch.fd != 0)   return;           /* if file already open, rtn  */
	if ((sinfd = sndgetset(p)) > 0) {        /* if soundinset successful   */
	        p->fdch.fd = sinfd;		 /*     store & log the fd     */
		fdrecord(&p->fdch);              /*     instr will close later */
	}
	else initerror(errmsg);			 /* else just print the errmsg */
}

soundin(p)
 register SOUNDIN *p;
{
register short	nsmps;
register float	*r1, *r2, *r3, *r4;
	int	chnsout, n, ntogo;

	r1 = p->r1;
	r2 = p->r2;
	r3 = p->r3;
	r4 = p->r4;
	chnsout = p->OUTCOUNT;
	ntogo = ksmps;
	if (p->endfile)
		goto filend;
	nsmps = (p->bufend - p->inbufp) / p->sampframsiz;
	if (nsmps > ksmps)
		nsmps = ksmps;
	ntogo -= nsmps;
	switch (p->format) {
	    case AE_CHAR: {
		register char *inbufp = p->inbufp;
chars:		switch(chnsout) {
		case 1:	do {	*r1++ = (float) ( (short)*inbufp++ << 8 );
			} while (--nsmps);
			break;
		case 2:	do {	*r1++ = (float) ( (short)*inbufp++ << 8 );
				*r2++ = (float) ( (short)*inbufp++ << 8 );
			} while (--nsmps);
			break;
		case 4:	do {	*r1++ = (float) ( (short)*inbufp++ << 8 );
				*r2++ = (float) ( (short)*inbufp++ << 8 );
				*r3++ = (float) ( (short)*inbufp++ << 8 );
				*r4++ = (float) ( (short)*inbufp++ << 8 );
			} while (--nsmps);
		}
		if (inbufp >= p->bufend) {
			if ((n = readin(p->fdch.fd,p->inbuf,SNDINBUFSIZ)) == 0) {
				p->endfile = 1;
				if (ntogo) goto filend;
				else return;
			}
			inbufp = p->inbuf;
			p->bufend = p->inbuf + n;
			if (ntogo > 0) {
				if ((nsmps = n / p->sampframsiz) > ntogo)
					nsmps = ntogo;
				ntogo -= nsmps;
				goto chars;
			}
		}
		p->inbufp = inbufp;
		break;
	    }
	    case AE_ULAW: {
		register unsigned char *inbufp = (unsigned char *)p->inbufp;
ulaw:		switch(chnsout) {
		case 1:	do {	*r1++ = (float) ulaw_decode[*inbufp++];
			} while (--nsmps);
			break;
		case 2:	do {	*r1++ = (float) ulaw_decode[*inbufp++];
				*r2++ = (float) ulaw_decode[*inbufp++];
			} while (--nsmps);
			break;
		case 4:	do {	*r1++ = (float) ulaw_decode[*inbufp++];
				*r2++ = (float) ulaw_decode[*inbufp++];
				*r3++ = (float) ulaw_decode[*inbufp++];
				*r4++ = (float) ulaw_decode[*inbufp++];
			} while (--nsmps);
		}
		if (inbufp >= (unsigned char *)p->bufend) {
			if ((n = readin(p->fdch.fd,p->inbuf,SNDINBUFSIZ)) == 0) {
				p->endfile = 1;
				if (ntogo) goto filend;
				else return;
			}
			inbufp = (unsigned char *)p->inbuf;
			p->bufend = p->inbuf + n;
			if (ntogo > 0) {
				if ((nsmps = n / p->sampframsiz) > ntogo)
					nsmps = ntogo;
				ntogo -= nsmps;
				goto ulaw;
			}
		}
		p->inbufp = (char *)inbufp;
		break;
	    }
	    case AE_SHORT: {
		register short *inbufp = (short *)p->inbufp;
shorts:		switch(chnsout) {
		case 1:	do {	*r1++ = (float) *inbufp++;
			} while (--nsmps);
			break;
		case 2:	do {	*r1++ = (float) *inbufp++;
				*r2++ = (float) *inbufp++;
			} while (--nsmps);
			break;
		case 4:	do {	*r1++ = (float) *inbufp++;
				*r2++ = (float) *inbufp++;
				*r3++ = (float) *inbufp++;
				*r4++ = (float) *inbufp++;
			} while (--nsmps);
		}
		if (inbufp >= (short *)p->bufend) {
			if ((n = readin(p->fdch.fd,p->inbuf,SNDINBUFSIZ)) == 0) {
				p->endfile = 1;
				if (ntogo) goto filend;
				else return;
			}
			inbufp = (short *) p->inbuf;
			p->bufend = p->inbuf + n;
			if (ntogo > 0) {
				if ((nsmps = n / p->sampframsiz) > ntogo)
					nsmps = ntogo;
				ntogo -= nsmps;
				goto shorts;
			}
		}
		p->inbufp = (char *) inbufp;
		break;
	    }
	    case AE_LONG: {
		register long *inbufp = (long *)p->inbufp;
longs:		switch(chnsout) {
		case 1:	do {	*r1++ = (float) *inbufp++;
			} while (--nsmps);
			break;
		case 2:	do {	*r1++ = (float) *inbufp++;
				*r2++ = (float) *inbufp++;
			} while (--nsmps);
			break;
		case 4:	do {	*r1++ = (float) *inbufp++;
				*r2++ = (float) *inbufp++;
				*r3++ = (float) *inbufp++;
				*r4++ = (float) *inbufp++;
			} while (--nsmps);
		}
		if (inbufp >= (long *)p->bufend) {
			if ((n = readin(p->fdch.fd,p->inbuf,SNDINBUFSIZ)) == 0) {
				p->endfile = 1;
				if (ntogo) goto filend;
				else return;
			}
			inbufp = (long *)p->inbuf;
			p->bufend = p->inbuf + n;
			if (ntogo > 0) {
				if ((nsmps = n / p->sampframsiz) > ntogo)
					nsmps = ntogo;
				ntogo -= nsmps;
				goto longs;
			}
		}
		p->inbufp = (char *)inbufp;
		break;
	    }
	    case AE_FLOAT: {
		register float *inbufp = (float *)p->inbufp;
floats:		switch(chnsout) {
		case 1:	do {	*r1++ = *inbufp++;
			} while (--nsmps);
			break;
		case 2:	do {	*r1++ = *inbufp++;
				*r2++ = *inbufp++;
			} while (--nsmps);
			break;
		case 4:	do {	*r1++ = *inbufp++;
				*r2++ = *inbufp++;
				*r3++ = *inbufp++;
				*r4++ = *inbufp++;
			} while (--nsmps);
		}
		if ((char *)inbufp >= p->bufend) {
			if ((n = readin(p->fdch.fd,p->inbuf,SNDINBUFSIZ)) == 0) {
				p->endfile = 1;
				if (ntogo) goto filend;
				else return;
			}
			inbufp = (float *) p->inbuf;
			p->bufend = p->inbuf + n;
			if (ntogo > 0) {
				if ((nsmps = n / p->sampframsiz) > ntogo)
					nsmps = ntogo;
				ntogo -= nsmps;
				goto floats;
			}
		}
		p->inbufp = (char *) inbufp;
		break;
	    }
	    default: dies("soundin of %s not implemented",
			  getstrformat((int)p->format));
	}
	return;

filend:	nsmps = ntogo;
	switch(chnsout) {			/* if past end of file, */
	case 1:	do *r1++ = fzero;		/*    move in zeros	*/
		while (--nsmps);
		break;
	case 2:	do {	*r1++ = fzero;
			*r2++ = fzero;
		} while (--nsmps);
		break;
	case 4:	do {	*r1++ = fzero;
			*r2++ = fzero;
			*r3++ = fzero;
			*r4++ = fzero;
		} while (--nsmps);
	}
}
