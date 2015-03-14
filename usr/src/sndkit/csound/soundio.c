#include "cs.h"			/*	       		SOUNDIO.C	*/
#include "soundio.h"

static  char	*sfoutname;     		    /* soundout filename    */
static	char	*inbuf,    *outbuf;   		    /* contin sndio buffers */
static	char	*chinbufp, *choutbufp;		    /* char  pntr to above  */
static	short	*shinbufp, *shoutbufp;		    /* short pntr	    */
static	long	*llinbufp, *lloutbufp;		    /* long  pntr	    */
static	float	*flinbufp, *floutbufp;		    /* float pntr	    */
static	unsigned inbufrem,  outbufrem;       	    /* in monosamps (see openin,iotranset) */
static	unsigned inbufsiz,  outbufsiz;   	    /* alloc in sfopenin/out     */
static	int	isfd, isfopen = 0, infilend = 0;    /* (real set in sfopenin)    */
static	int	osfd, osfopen = 0;      	    /* (real set in sfopenout)   */
static	int	pipdevin = 0, pipdevout = 0;	    /* mod by sfopenin,sfopenout */
static	float	fzero = 0.;
#ifdef LOFI
        int     dansLoFi  = 0;			    /* flag for dacs via lofi */
        int     dansLoFiI = 0;			    /* flag for adcs via lofi */
#endif
	long	nrecs = 0;
extern	float	*spin, *spout, maxamp[], *maxampend;
extern  long	rngcnt[];
extern	short	rngflg, multichan;
extern	int	nspin, nspout, nchnls, ksmps;
extern  HEADATA *readheader();
extern  short   ulaw_decode[];

void (*spinrecv)(), (*spoutran)(), (*nzerotran)();
static void charrecv(),  alawrecv(), ulawrecv(), shortrecv(),longrecv(), floatrecv();
static void chartran(),  alawtran(), ulawtran(), shortran(), longtran(), floatran();
static void czerotran(), azerotran(),uzerotran(),szerotran(),lzerotran(),fzerotran();

char *getstrformat(format)  /* used here, and in sfheader.c (IRCAM) */
 register int format;
{
        switch(format) {
	case AE_CHAR:  return("signed chars");
	case AE_ALAW:  return("alaw bytes");
	case AE_ULAW:  return("ulaw bytes");
	case AE_SHORT: return("shorts");
	case AE_LONG:  return("longs");
	case AE_FLOAT: return("floats");
	default: die("unknown sound format");
	}
}

int getsizformat(format)
 register int format;
{
 static int formatsiz[] = {0, sizeof(char), sizeof(char), sizeof(char),
			    sizeof(short), sizeof(long), sizeof(float)};
        if (format > AE_FLOAT)
	        die("illegal input to getsizformat");
        return(formatsiz[format & 0xF]);
}

char *getoutname()   { return(sfoutname); }
char *getoutformat() { return(getstrformat(outformat)); }

void iotranset()   /* direct recv & tran calls to the right audio formatter  */
{                  /*                            & init its audio_io bufptr  */
        switch(informat) {
	case AE_CHAR:  spinrecv = charrecv;
	               chinbufp = inbuf;
	               break;
	case AE_ALAW:  spinrecv = alawrecv;
	               chinbufp = inbuf;
	               break;
	case AE_ULAW:  spinrecv = ulawrecv;
	               chinbufp = inbuf;
	               break;
	case AE_SHORT: spinrecv = shortrecv;
	               shinbufp = (short *)inbuf;
	               break;
	case AE_LONG:  spinrecv = longrecv;
	               llinbufp = (long  *)inbuf;
	               break;
	case AE_FLOAT: spinrecv = floatrecv;
	               flinbufp = (float *)inbuf;
	               break;
	default: die("unknown audio_in format");
	}

        switch(outformat) {
	case AE_CHAR:  spoutran = chartran;
	               nzerotran = czerotran;
	               choutbufp = outbuf;
	               break;
	case AE_ALAW:  spoutran = alawtran;
		       nzerotran = azerotran;
	               choutbufp = outbuf;
	               break;
	case AE_ULAW:  spoutran = ulawtran;
		       nzerotran = uzerotran;
	               choutbufp = outbuf;
	               break;
	case AE_SHORT: spoutran = shortran;
		       nzerotran = szerotran;
	               shoutbufp = (short *)outbuf;
	               break;
	case AE_LONG:  spoutran = longtran;
		       nzerotran = lzerotran;
	               lloutbufp = (long  *)outbuf;
	               break;
	case AE_FLOAT: spoutran = floatran;
		       nzerotran = fzerotran;
	               floutbufp = (float *)outbuf;
	               break;
	default: die("unknown audio_out format");
	}
	outbufrem = iobufsamps;  /* in mono */
}

#define MAXFULNAME 128

 char *
buildsfname(name)			/*  build a soundfile name	*/
 register char *name;			/*  <mod of carl getsfname()>	*/
{
static	char	fullname[MAXFULNAME];
#ifdef THINK_C
	extern char sfdir_path[];
	/*
	 * If name is not a pathname, then construct using sfdir_path[].
	 */
	fullname[0] = 0;
	if (strchr(name,':') == NULL) {
		strcpy(fullname,sfdir_path);
		strcat(fullname,":");
	}
	strcat(fullname,name);
	return fullname;
#else
	char	*sfdir, *getenv();

	if (*name == '/' || *name == '.')	/* if path already given */
		return(strcpy(fullname,name));	/*	copy as is	*/
	else {					/* else build right here  */
		if ((sfdir = getenv("SFDIR")) == NULL)
			die("buildsfname: environment variable SFDIR undefined");
		sprintf(fullname,"%s/%s",sfdir,name);
		return(fullname);
	}
#endif
}

readin(infd,inbuf,nbytes)       	/* special handling of sound input	  */
 register int	infd, nbytes;   	/* to accomodate reads thru pipes & net	  */
 register char	*inbuf; 		/* where nbytes rcvd can be < n requested */
{
    register int	n, ntot=0;

#ifdef LOFI
    if (dansLoFiI) {		/* ADC input via LoFi dpwe 08sep90 */
	rec_on(inbuf,nbytes>>1); /* instead */
	return(nbytes);
	}
    else
#endif
    do {
	if ((n = read(infd, inbuf+ntot, nbytes-ntot)) < 0)
	    die("soundfile read error");
	/*	    if (n != nbytes-ntot)
		    printf("readin: got %d of %d\n",n,nbytes-ntot);    */
	}
    while (n > 0 && (ntot += n) < nbytes);
    return(ntot);
}

void sndwrterr(nret, nput)      	/* report soundfile write(osfd) error	*/
 unsigned nret, nput;                   /* called after chk of write() bytecnt  */
{
	sprintf(errmsg,"soundfile write returned bytecount of %d, not %d",nret,nput);
	die(errmsg);
}

sfopenin()					/* init for continuous soundin */
{						/* (called only if -i flag) */
        register char	 *sfname;
        register HEADATA *hdr;
        long     n, readlong = 0;

        if (infilename != NULL && strcmp(infilename,"stdin") == 0) {
	    sfname = infilename;
	    isfd = infd;                    /* get sound from stdin if requested */
	    pipdevin = 1;
	}
#ifdef LOFI
        else if (strcmp(infilename,"lofi") == 0) {	/* Glas DAC dpwe 08sep90 */
	    sfname = "lofi-real-time-handler";	/* write to DAC via LoFi */
	    isfd = NULL;		/* no actual file .. */
	    rec_set(nchnls, 2 /* dsize */, esr, 2 /* scale */);  /* instead */
	    pipdevin   = 1;		/* no backward seeks ! */
	    dansLoFiI  = 1;		/* set a global flag for ease */
        }
#endif
	else {                              /* else build filename and open that */
	    sfname = buildsfname(infilename);
	    if ((isfd = open(sfname,O_RDONLY)) < 0)
		dies("isfinit: cannot open %s", sfname);
	}
	if (
#ifdef LOFI
	    !dansLoFiI && 
#endif
	    (hdr = readheader(isfd,sfname)) != NULL     /* if headerblk returned */
	      && !(readlong = hdr->readlong)) {         /* & hadn't readin audio */
	    if (hdr->sr != (long)esr) {   	        /*    chk the hdr codes  */
		sprintf(errmsg,"audio_in %s has sr = %ld, orch sr = %ld",
			sfname, hdr->sr, (long)esr);
		warning(errmsg);
	    }
	    if (hdr->nchnls != nchnls) {
	        sprintf(errmsg,"audio_in %s has %ld chnls, orch %d chnls",
			sfname, hdr->nchnls, nchnls);
		die(errmsg);
	    }
	    insampsiz = hdr->sampsize;                  /*    & cpy header vals  */
	    informat = hdr->format;
	}
	else {                                          /* no header:  defaults  */
	    sprintf(errmsg,"%s has no soundfile header, assuming %s",
		    sfname, getstrformat(outformat) );
            warning(errmsg);
	}
	inbufsiz = (unsigned)iobufsamps * insampsiz;    /* calc inbufsize reqd   */
	inbuf = mcalloc((long)inbufsiz);                /* alloc inbuf space     */
	printf("reading %d-byte blks from %s (%s)\n",
	       inbufsiz, sfname, getstrformat(informat) );
	isfopen = 1;
	if (readlong) {                                 /*     & fill it         */
	    *(long *)inbuf = hdr->firstlong;
	    n = readin(isfd, inbuf+sizeof(long), inbufsiz-sizeof(long));
	    n += sizeof(long);
	}
	else n = readin(isfd, inbuf, inbufsiz);
	inbufrem = n / insampsiz;                       /* datasiz in monosamps  */
}                                                       /* set here, not iotranset */

sfclosein()
{
#ifdef LOFI
	if(dansLoFiI && !dansLoFi)	/* only if not open for output too */
	    LoFiClose();
        else
#endif
	  if (isfopen)
	    close(isfd);
	isfopen = 0;
}

static HEADATA HeadOut;  /* NOT USED YET */

sfopenout()					/* init for sound out */
{						/* (not called if nosound)  */
extern  void    writeheader();
        char    *builtname;

        if (outfilename == NULL)  outfilename = "test";
        if (strcmp(outfilename,"stdout") == 0) {
	    sfoutname = outfilename;
	    osfd = outfd;                /* send sound to stdout if requested */
	    pipdevout = 1;
	}    
#ifdef LOFI
        else if (strcmp(outfilename,"lofi") == 0) {	/* Glas DAC dpwe 08sep90 */
	    sfoutname = "lofi-real-time-handler";	/* write to DAC via LoFi */
	    osfd = NULL;		/* no actual file .. */
/*	    MyBootLoFi(esr);		/* initialise LoFi for given sampling rate */
	    play_set(nchnls, 2 /* dsize */, esr, 2 /* scale */);  /* instead */
	    pipdevout = 1;		/* no backward seeks ! */
	    dansLoFi  = 1;		/* set a global flag for ease */
        }
#endif
	else {
	    builtname = buildsfname(outfilename);           /* else build filename */
	    sfoutname = mmalloc((long)strlen(builtname)+1);
	    strcpy(sfoutname,builtname);                    /* preserve the name   */
#ifdef THINK_C                                              /*     & open the file */
	    if ((osfd = open(sfoutname,O_TRUNC|O_CREAT|O_WRONLY|O_BINARY)) < 0)
#else
	    if ((osfd = open(sfoutname,O_TRUNC|O_CREAT|O_WRONLY, 0644)) < 0)
#endif
		dies("sfinit: cannot open %s", sfoutname);
	    if (strcmp(sfoutname,"/dev/audio") == 0) {
	/*	ioctl(   );   */
	        pipdevout = 1;
	    }
	}
        outbufsiz = (unsigned)iobufsamps * outsampsiz;
        outbuf = mmalloc((long)outbufsiz);             /*  & alloc bufspace */
        if (sfheader)   	
	    writeheader(osfd, sfoutname);	/* write header as required      */
        printf("writing %d-byte blks to %s (%s)\n",
	       outbufsiz, sfoutname, getstrformat(outformat) );
	osfopen = 1;
}

sfcloseout()
{
register int	nb, n;
extern  void    rewriteheader();

#ifdef LOFI
        if(dansLoFi)	{	/* dpwe 08sep90 : special case for Lofi */
	    LoFiClose();
	    return;
	}
#endif
	if (!osfopen) return;
	if ((nb = (iobufsamps - outbufrem) * outsampsiz) > 0) {  /* flush the outbuffer */
		if ((n = write(osfd,outbuf,nb)) < nb)
			sndwrterr(n, nb);
		nrecs++;
	}
        if (sfheader && !pipdevout) {               /* if header, & backward seeks ok */
	        unsigned datasize = (nrecs-1) * outbufsiz + nb;
		rewriteheader(osfd, datasize);      /* (sfname reqd only on open) */
	}
#ifndef SFSUN41
        if (!pipdevout)
#endif
	        close(osfd);
        osfopen = 0;
}

static void shortran()			/* fix spout vals and put in outbuf */
{					/*	write buffer when full	    */
register float	*sp, *maxampp;
register long   longsmp, *rngp;
register int	n, spoutrem;
	float	absamp;

	sp = spout;			/* adr spout	*/	
	spoutrem = nspout;		/* smps to go	*/
	maxampp = maxamp;

nchk:	if ((n = spoutrem) > outbufrem)	/* if nspout remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	spoutrem -= n;
	outbufrem -= n;
	do {
		if ((longsmp = *sp) >= 0) {		/* +ive samp:	*/
			if (*sp > *maxampp)		/*  maxamp this seg  */
				*maxampp = *sp;
			if (longsmp > 32767) {		/* out of range?     */
				longsmp = 32767;	/*   clip and report */
				rngp = rngcnt + (maxampp - maxamp);
				(*rngp)++;
				rngflg = 1;
			}
		}
		else {					/* ditto -ive samp */
			if ((absamp = -*sp) > *maxampp)
				*maxampp = absamp;
			if (longsmp < -32768) {
				longsmp = -32768;
				rngp = rngcnt + (maxampp - maxamp);
				(*rngp)++;
				rngflg = 1;
			}
		}
		if (osfopen)
			*shoutbufp++ = (short) longsmp;
		if (multichan && ++maxampp >= maxampend)
			maxampp = maxamp;
		sp++;
	} while (--n);
	if (!outbufrem) {
#ifdef LOFI
	        if (dansLoFi) {		/* DAC output via LoFi dpwe 08sep90 */
		/*        play(outbuf, ((char *)outbuf)+outbufsiz,
			     nchnls,2,esr,2); */
			play_on(outbuf,outbufsiz>>1);	/* instead */
			n = outbufsiz;
			nrecs++;
			shoutbufp = (short *) outbuf;
		}
		else 
#endif		  
		  if (osfopen) {
			if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
				sndwrterr(n, outbufsiz);
			nrecs++;
			shoutbufp = (short *) outbuf;
		}
		outbufrem = iobufsamps;
		if (spoutrem) goto nchk;
	}
}

static void chartran()			/* same as above, but 8-bit char output */
{					/*   sends HI-ORDER 8 bits of shortsamp */
register float	*sp, *maxampp;
register long   longsmp, *rngp;
register int	n, spoutrem;
	float	absamp;

	sp = spout;			/* adr spout	*/	
	spoutrem = nspout;		/* smps to go	*/
	maxampp = maxamp;

nchk:	if ((n = spoutrem) > outbufrem)	/* if nspout remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	spoutrem -= n;
	outbufrem -= n;
	do {
		if ((longsmp = *sp) >= 0)		/* +ive samp:	*/
			if (*sp > *maxampp) {		/*  maxamp this seg  */
				*maxampp = *sp;
			if (longsmp > 32767) {		/* out of range?     */
				longsmp = 32767;	/*   clip and report */
				rngp = rngcnt + (maxampp - maxamp);
				(*rngp)++;
				rngflg = 1;
			}
		}
		else {					/* ditto -ive samp */
			if ((absamp = -*sp) > *maxampp)
				*maxampp = absamp;
			if (longsmp < -32768) {
				longsmp = -32768;
				rngp = rngcnt + (maxampp - maxamp);
				(*rngp)++;
				rngflg = 1;
			}
		}
		if (osfopen)
			*choutbufp++ = longsmp >> 8;
		if (multichan && ++maxampp >= maxampend)
			maxampp = maxamp;
		sp++;
	} while (--n);
	if (!outbufrem) {
		if (osfopen) {
			if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
				sndwrterr(n, outbufsiz);
			nrecs++;
			choutbufp = outbuf;
		}
		outbufrem = iobufsamps;
		if (spoutrem) goto nchk;
	}
}

static void alawtran()
{ die("alaw not yet implemented"); }

#define MUCLIP  32635
#define BIAS    0x84
#define MUZERO  0x02
#define ZEROTRAP 

static void ulawtran()			/* ulaw-encode spout vals & put in outbuf */
{					/*	write buffer when full	    */
register float	*sp, *maxampp;
register long   longsmp, *rngp;
register int	n, spoutrem, sign;
extern  char    exp_lut[];               /* mulaw encoding table */

	sp = spout;			/* adr spout	*/	
	spoutrem = nspout;		/* smps to go	*/
	maxampp = maxamp;

nchk:	if ((n = spoutrem) > outbufrem)	/* if nspout remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	spoutrem -= n;
	outbufrem -= n;
	do {
		if ((longsmp = *sp) < 0) {		/* if sample negative	*/
		        sign = 0x80;
			longsmp = - longsmp;		/*  make abs, save sign	*/
		}
		else sign = 0;
		if (longsmp > *maxampp)         	/* save maxamp this seg  */
			*maxampp = longsmp;
		if (longsmp > MUCLIP) { 		/* out of range?     */
			longsmp = MUCLIP;       	/*   clip and report */
			rngp = rngcnt + (maxampp - maxamp);
			(*rngp)++;
			rngflg = 1;
		}
		if (osfopen) {
			int sample, exponent, mantissa, ulawbyte;
			sample = longsmp + BIAS;
			exponent = exp_lut[( sample >> 8 ) & 0x7F];
			mantissa = ( sample >> (exponent+3) ) & 0x0F;
			ulawbyte = ~ (sign | (exponent << 4) | mantissa );
#ifdef ZEROTRAP
			if (ulawbyte == 0) ulawbyte = MUZERO;    /* optional CCITT trap */
#endif
			*choutbufp++ = ulawbyte;
		}
		if (multichan && ++maxampp >= maxampend)
			maxampp = maxamp;
		sp++;
	} while (--n);
	if (!outbufrem) {
		if (osfopen) {
			if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
				sndwrterr(n, outbufsiz);
			nrecs++;
			choutbufp = outbuf;
		}
		outbufrem = iobufsamps;
		if (spoutrem) goto nchk;
	}
}

static void longtran()			/* send long_int spout vals to outbuf */
{					/*	write buffer when full	    */
register float	*sp, *maxampp;
register int	n, spoutrem;
register float	absamp;

	sp = spout;			/* adr spout	*/	
	spoutrem = nspout;		/* smps to go	*/
	maxampp = maxamp;

nchk:	if ((n = spoutrem) > outbufrem)	/* if nspout remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	spoutrem -= n;
	outbufrem -= n;
	do {
		if ((absamp = *sp) < 0.)
			absamp = -absamp;
		if (absamp > *maxampp)  	/*  maxamp this seg  */
			*maxampp = absamp;
		if (osfopen)
			*lloutbufp++ = (long) *sp;
		if (multichan && ++maxampp >= maxampend)
			maxampp = maxamp;
		sp++;
	} while (--n);
	if (!outbufrem) {
		if (osfopen) {
			if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
				sndwrterr(n, outbufsiz);
			nrecs++;
			lloutbufp = (long *) outbuf;
		}
		outbufrem = iobufsamps;
		if (spoutrem) goto nchk;
	}
}

static void floatran()			/* send float spout vals to outbuf */
{					/*	write buffer when full	    */
register float	*sp, *maxampp;
register int	n, spoutrem;
register float	absamp;

	sp = spout;			/* adr spout	*/	
	spoutrem = nspout;		/* smps to go	*/
	maxampp = maxamp;

nchk:	if ((n = spoutrem) > outbufrem)	/* if nspout remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	spoutrem -= n;
	outbufrem -= n;
	do {
		if ((absamp = *sp) < 0.)
			absamp = -absamp;
		if (absamp > *maxampp)  	/*  maxamp this seg  */
			*maxampp = absamp;
		if (osfopen)
			*floutbufp++ = *sp;
		if (multichan && ++maxampp >= maxampend)
			maxampp = maxamp;
		sp++;
	} while (--n);
	if (!outbufrem) {
		if (osfopen) {
			if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
				sndwrterr(n, outbufsiz);
			nrecs++;
			floutbufp = (float *) outbuf;
		}
		outbufrem = iobufsamps;
		if (spoutrem) goto nchk;
	}
}

static void szerotran(kcnt)	/* copy kcnt zerospouts to short soundbuf, */
 long kcnt;			/*	sending buffer whenever full	 */
{
register long	n, smpsrem, clearcnt = 0;

	if (!osfopen)  return;
	smpsrem = nspout * kcnt;	/* calculate total smps to go	*/
nchk:	if ((n = smpsrem) > outbufrem)	/* if smps remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	smpsrem -= n;
	outbufrem -= n;
	if (clearcnt < iobufsamps) {
		clearcnt += n;		/* clear buf only till clean */
		do *shoutbufp++ = (short) 0;
		while (--n);
	}
	else shoutbufp += n;
	if (!outbufrem) {
#ifdef LOFI
	    if(dansLoFi) {                /* DAC output via LoFi */
	        play_on(outbuf, outbufsiz>>1);
		n = outbufsiz;
		nrecs++;
		shoutbufp = (short *)outbuf;
	    }
	    else
#endif
	    if(osfopen) {
		if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
		    sndwrterr((unsigned)n, outbufsiz);
		nrecs++;
		shoutbufp = (short *) outbuf;
	    }
	    outbufrem = iobufsamps;
	    if (smpsrem) goto nchk;
	}
}

static void czerotran(kcnt)	/* copy kcnt zerospouts to (signed) char soundbuf, */
 long kcnt;			/*	sending buffer whenever full	 */
{
register long	n, smpsrem, clearcnt = 0;

	if (!osfopen)  return;
	smpsrem = nspout * kcnt;	/* calculate total smps to go	*/
nchk:	if ((n = smpsrem) > outbufrem)	/* if smps remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	smpsrem -= n;
	outbufrem -= n;
	if (clearcnt < iobufsamps) {
		clearcnt += n;		/* clear buf only till clean */
		do *choutbufp++ = 0x00;
		while (--n);
	}
	else choutbufp += n;
	if (!outbufrem) {
		if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
			sndwrterr((unsigned)n, outbufsiz);
		nrecs++;
		choutbufp = outbuf;
		outbufrem = iobufsamps;
		if (smpsrem) goto nchk;
	}
}

static void azerotran(kcnt) { die("alaw not yet implemented"); }

static void uzerotran(kcnt)	/* copy kcnt zerospouts to ulaw soundbuf, */
 long kcnt;			/*	sending buffer whenever full	 */
{
register long	n, smpsrem, clearcnt = 0;

	if (!osfopen)  return;
	smpsrem = nspout * kcnt;	/* calculate total smps to go	*/
nchk:	if ((n = smpsrem) > outbufrem)	/* if smps remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	smpsrem -= n;
	outbufrem -= n;
	if (clearcnt < iobufsamps) {
		clearcnt += n;		/* clear buf only till clean */
		do *choutbufp++ = 0xFF; /* no signal is 0xFF in mulaw */
		while (--n);
	}
	else choutbufp += n;
	if (!outbufrem) {
		if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
			sndwrterr((unsigned)n, outbufsiz);
		nrecs++;
		choutbufp = outbuf;
		outbufrem = iobufsamps;
		if (smpsrem) goto nchk;
	}
}

static void lzerotran(kcnt)	/* copy kcnt zerospouts to long_int soundbuf, */
 long kcnt;			/*	sending buffer whenever full	 */
{
register long	n, smpsrem, clearcnt = 0;

	if (!osfopen)  return;
	smpsrem = nspout * kcnt;	/* calculate total smps to go	*/
nchk:	if ((n = smpsrem) > outbufrem)	/* if smps remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	smpsrem -= n;
	outbufrem -= n;
	if (clearcnt < iobufsamps) {
		clearcnt += n;		/* clear buf only till clean */
		do *lloutbufp++ = 0L;
		while (--n);
	}
	else lloutbufp += n;
	if (!outbufrem) {
		if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
			sndwrterr((unsigned)n, outbufsiz);
		nrecs++;
		lloutbufp = (long *) outbuf;
		outbufrem = iobufsamps;
		if (smpsrem) goto nchk;
	}
}

static void fzerotran(kcnt)	/* copy kcnt zerospouts to float soundbuf, */
 long kcnt;			/*	sending buffer whenever full	 */
{
register long	n, smpsrem, clearcnt = 0;

	if (!osfopen)  return;
	smpsrem = nspout * kcnt;	/* calculate total smps to go	*/
nchk:	if ((n = smpsrem) > outbufrem)	/* if smps remaining > buf rem, */
		n = outbufrem;		/*	prepare to send in parts  */
	smpsrem -= n;
	outbufrem -= n;
	if (clearcnt < iobufsamps) {
		clearcnt += n;		/* clear buf only till clean */
		do *floutbufp++ = fzero;
		while (--n);
	}
	else floutbufp += n;
	if (!outbufrem) {
		if ((n = write(osfd,outbuf,outbufsiz)) < outbufsiz)
			sndwrterr((unsigned)n, outbufsiz);
		nrecs++;
		floutbufp = (float *) outbuf;
		outbufrem = iobufsamps;
		if (smpsrem) goto nchk;
	}
}

static void clrspin1(r,spinrem)      /* clear remainder of spinbuf to zeros */
 register float *r;                  /* called only once, at EOF   */
 register int spinrem;
{
	infilend = 1;                        /* 1st filend pass:   */
	while (spinrem--)                    /*   clear spin rem   */
		*r++ = fzero;
}

static void clrspin2()             /* clear spinbuf to zeros   */
{                                  /* called only once, at EOF */
        register float *r = spin;
        register int n = nspin;
        infilend = 2;                        /* at 2nd filend pass  */
	do *r++ = fzero;                     /*   clr whole spinbuf */
	while (--n);
}

static void charrecv()              /* get spin values from char inbuf */
{
        register float *r = spin;
	register int   n, spinrem = nspin;

	if (infilend == 2) return;
nchk:   if ((n = spinrem) > inbufrem)   /* if nspin remaining > buf rem,  */
            n = inbufrem;               /*       prepare to get in parts  */
	spinrem -= n;
	inbufrem -= n;
	do *r++ = (float) ( (short)*chinbufp++ << 8 );
	while (--n);
	if (!inbufrem) {
	    if (!infilend) {
		if ((n = readin(isfd, inbuf, inbufsiz)) != 0) {
		    chinbufp = inbuf;
		    inbufrem = n / sizeof(char);
		    if (spinrem) goto nchk;
		} else clrspin1(r,spinrem);  /* 1st filend pass: partial clr  */
	    } else clrspin2();           /* 2nd filend pass: zero the spinbuf */
	}
}

static void alawrecv() { die("alaw audio_in not yet implemented"); }

static void ulawrecv()              /* get spin values from ulaw inbuf */
{
        register float *r = spin;
	register int   n, spinrem = nspin;

	if (infilend == 2) return;
nchk:   if ((n = spinrem) > inbufrem)   /* if nspin remaining > buf rem,  */
            n = inbufrem;               /*       prepare to get in parts  */
	spinrem -= n;
	inbufrem -= n;
	do *r++ = (float) ulaw_decode[*(unsigned char *)chinbufp++];
	while (--n);
	if (!inbufrem) {
	    if (!infilend) {
		if ((n = readin(isfd, inbuf, inbufsiz)) != 0) {
		    chinbufp = inbuf;
		    inbufrem = n / sizeof(char);
		    if (spinrem) goto nchk;
		} else clrspin1(r,spinrem);  /* 1st filend pass: partial clr  */
	    } else clrspin2();           /* 2nd filend pass: zero the spinbuf */
	}
}

static void shortrecv()              /* get spin values from short_int inbuf */
{
        register float *r = spin;
	register int   n, spinrem = nspin;

	if (infilend == 2) return;
nchk:   if ((n = spinrem) > inbufrem)   /* if nspin remaining > buf rem,  */
            n = inbufrem;               /*       prepare to get in parts  */
	spinrem -= n;
	inbufrem -= n;
	do *r++ = (float) *shinbufp++;
	while (--n);
	if (!inbufrem) {
	    if (!infilend) {
		if ((n = readin(isfd, inbuf, inbufsiz)) != 0) {
		    shinbufp = (short *) inbuf;
		    inbufrem = n / sizeof(short);
		    if (spinrem) goto nchk;
		} else clrspin1(r,spinrem);  /* 1st filend pass: partial clr  */
	    } else clrspin2();           /* 2nd filend pass: zero the spinbuf */
	}
}

static void longrecv()              /* get spin values from long_int inbuf */
{
        register float *r = spin;
	register int   n, spinrem = nspin;

	if (infilend == 2) return;
nchk:   if ((n = spinrem) > inbufrem)   /* if nspin remaining > buf rem,  */
            n = inbufrem;               /*       prepare to get in parts  */
	spinrem -= n;
	inbufrem -= n;
	do *r++ = (float) *llinbufp++;
	while (--n);
	if (!inbufrem) {
	    if (!infilend) {
		if ((n = readin(isfd, inbuf, inbufsiz)) != 0) {
		    llinbufp = (long *) inbuf;
		    inbufrem = n / sizeof(long);
		    if (spinrem) goto nchk;
		} else clrspin1(r,spinrem);  /* 1st filend pass: partial clr  */
	    } else clrspin2();           /* 2nd filend pass: zero the spinbuf */
	}
}

static void floatrecv()              /* get spin values from float inbuf */
{
        register float *r = spin;
	register int   n, spinrem = nspin;

	if (infilend == 2) return;
nchk:   if ((n = spinrem) > inbufrem)   /* if nspin remaining > buf rem,  */
            n = inbufrem;               /*       prepare to get in parts  */
	spinrem -= n;
	inbufrem -= n;
	do *r++ = *flinbufp++;
	while (--n);
	if (!inbufrem) {
	    if (!infilend) {
		if ((n = readin(isfd, inbuf, inbufsiz)) != 0) {
		    flinbufp = (float *) inbuf;
		    inbufrem = n / sizeof(float);
		    if (spinrem) goto nchk;
		} else clrspin1(r,spinrem);  /* 1st filend pass: partial clr  */
	    } else clrspin2();           /* 2nd filend pass: zero the spinbuf */
	}
}
