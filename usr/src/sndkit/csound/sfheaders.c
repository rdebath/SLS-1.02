#include "cs.h"				/*			       SFHEADERS.C	*/
#include <sfheader.h>
#include "soundio.h"
#include <stdio.h>

static char *incodbeg,  *incodend;   /* re-defined by each readheader */
static char *outcodbeg, *outcodend;  /* defined during writeheader */

typedef struct {                  /* header for each sfcode block */
	short	type;
	short	blksiz;
} CODE_HDR;

static short codblksiz[] = {   sizeof(CODE_HDR),
			       sizeof(CODE_HDR) + sizeof(SFMAXAMP),
			       sizeof(CODE_HDR) + sizeof(SFAUDIOENCOD),
			       sizeof(CODE_HDR) + sizeof(SFPVDATA),
			       sizeof(CODE_HDR) + sizeof(SFCOMMENT)  };

static void putendcode(cp)
 char *cp;
{
	register CODE_HDR *cdhp = (CODE_HDR *)cp;
	cdhp->type = SF_END;
	cdhp->blksiz = sizeof(CODE_HDR);  /* header, no body */
}

/* These next rtn ptr to beg of struct reqd (not the CODE_HDR preceding it).
   SR, NCHNLS, magic number, bytes/chan, are NOT coded via these routines. */

char *findsfcode(ctype)         /* locate start of sfcode in current in_header */
 int  ctype;                    /*    incodbeg,incodend prev set by readheader */
{                               /*      used here,  & by ugens7.c (PVOC)       */
	register char     *cp;
	register CODE_HDR *cdhp;

	if (ctype <= 0 || ctype > SF_CODMAX)
	    die("illegal sfcode type");
	for (cp = incodbeg; cp < incodend; ) {   /* starting from beg codespace  */
	    cdhp = (CODE_HDR *)cp;
	    if (cdhp->type == ctype)                  /* if find required code */
	        return(cp + sizeof(CODE_HDR));        /*   return ptr to data  */
	    if (cdhp->type == SF_END)                 /* can't find -- exit    */
	        break;
	    if (cdhp->blksiz <= 0                     /* if false-sized struct or */
	     || (cp += cdhp->blksiz) < incodbeg) {    /* wrap-around from bad hdr */
	        fprintf(stderr,"sfheader codes corrupted\n");         /* complain */
	        break;
	    }
	}
	return(NULL);                             /* no-find: return NULL pointer */
}

char *creatsfcode(ctype)        /* add a new sfcode struct to current out_header */
 int  ctype;                    /*   outcodbeg,outcodend prev set by writeheader */
{
	register char     *cp;
	register CODE_HDR *cdhp;

	if (ctype <= 0 || ctype > SF_CODMAX)
	    die("illegal sfcode type");
	for (cp=outcodbeg; cp<outcodend; ) {    /* starting from beg codespace  */
	    cdhp = (CODE_HDR *)cp;
	    if (cdhp->type == SF_END) {                  /* if find end code      */
	        cdhp->type = ctype;                      /*   redo as newtyp hdr  */
		cdhp->blksiz = codblksiz[ctype];
		putendcode(cp+cdhp->blksiz);             /*   reconstruct endcode */
	        return(cp+sizeof(CODE_HDR));             /*   & rtn newtyp datptr */
	    }
	    if (cdhp->blksiz <= 0                     /* if false-sized struct or */
	     || (cp += cdhp->blksiz) < outcodbeg) {   /* wrap-around from bad hdr */
	        fprintf(stderr,"sfheader codes corrupted\n");         /* complain */
	        break;
	    }
	}
	return(NULL);                             /* bad ptrs: return NULL pointer */
}

#define INFOMAX   100    /* blksiz for SUN info data */

static	char	*inhdrblk, *outhdrblk; 		/* (whichever) soundfile header blks */
static  char    *ininfop, *outinfop;            /* blks of INFOMAX bytes for SUN info */
static  HEADATA HeadData;                       /* datablk for general return        */

HEADATA *readheader(ifd,sfname)      /* read soundfile header, & fill a HEADATA struct */
 int ifd;                            /*       called by sfopenin() and sndinset()      */
 char *sfname;                       /* NULL means no header & nothing to preserve     */
{
        register HEADATA *hdp = &HeadData;
#ifdef SFIRCAM
	register SFHEADER *sfh;
	SFAUDIOENCOD *aep;
	long firstlong;
	int  n;
extern  char *getstrformat();

        if (inhdrblk == NULL)
	    inhdrblk = mmalloc((long)sizeof(SFHEADER));
	readin(ifd, inhdrblk, sizeof(long));            /* read the 1st long */
	sfh = (SFHEADER *) inhdrblk;
	if ((firstlong=sfmagic(sfh)) == SF_MAGIC) {	/* if header, read fully */
	    readin(ifd, inhdrblk+sizeof(long), sizeof(SFHEADER)-sizeof(long));
	    hdp->sr = (long) sfsrate(sfh);
	    hdp->nchnls = sfchans(sfh);                 /*   and record the data */
	    hdp->sampsize = sfclass(sfh);
	    incodbeg = &sfcodes(sfh);
	    incodend = (char *)sfh + sizeof(SFHEADER);
	    if ((aep = (SFAUDIOENCOD *) findsfcode(SF_AUDIOENCOD)) != NULL)
	        hdp->format = aep->encoding;
	    else {                              /* if no audioencode info,          */
		switch (hdp->sampsize) {        /* FORMAT BASED ONLY ON BYTE-COUNT! */
		case SF_ULAW:	hdp->format = AE_ULAW;   break;
		case SF_SHORT:	hdp->format = AE_SHORT;  break;
		case SF_FLOAT:	hdp->format = AE_FLOAT;  break;
		default:        hdp->format = informat;
		}
		sprintf(errmsg,"audio_in %s format unclear, deducing %s",
			sfname, getstrformat((int)hdp->format));
		warning(errmsg);
	    }
	    hdp->hdrsize = sizeof(SFHEADER);        /* hdrsize (for later seeks)  */
	    hdp->readlong = 0;                      /* now aligned on audio_start */
	}
	else if (BYTREVL(firstlong) == SF_MAGIC)        /* else byte-revrsed: die */
	    dies("%s is soundfile with bytes in the wrong order",sfname);
	else {
	    hdp->hdrsize = 0;                       /* else no header,        */
	    hdp->readlong = 1;                      /*   but we read a long   */
	    hdp->firstlong = firstlong;             /*   which had this value */
	}
	return(hdp);                      /* SFIRCAM always returns hdp, and data */
#endif
#ifdef SFSUN41
	if (inhdrblk == NULL) {
	    inhdrblk = mmalloc((long)sizeof(Audio_hdr));
	    ininfop = mcalloc((long)INFOMAX);
	}
	if (audio_isaudiofile(sfname) == TRUE) {
	    register Audio_hdr *ahp = (Audio_hdr *) inhdrblk;
	    char *infop = NULL;
	    unsigned ilen;
	    if (audio_read_filehdr(ifd, ahp, ininfop, INFOMAX) != AUDIO_SUCCESS)
		die("error reading audio_filehdr");
	    hdp->sr = ahp->sample_rate;
	    hdp->sampsize = ahp->bytes_per_unit;     /* record the data */
	    hdp->nchnls = ahp->channels;
	    switch (ahp->encoding) {                /* Convert format code */
	    case AUDIO_ENCODING_ALAW:
		    hdp->format = AE_ALAW;   break;
	    case AUDIO_ENCODING_ULAW:
		    hdp->format = AE_ULAW;   break;
	    case AUDIO_ENCODING_LINEAR:
		    switch (ahp->bytes_per_unit) {
		    case 1:	hdp->format = AE_CHAR;  break;
		    case 2:	hdp->format = AE_SHORT;  break;
		    case 4:	hdp->format = AE_LONG;  break;
		    default:    sprintf(errmsg,"unexpected audio input length of %d (linear)",
					(int) ahp->bytes_per_unit);
		                die(errmsg);
		    }
		    break;
	    case AUDIO_ENCODING_FLOAT:
		    switch (ahp->bytes_per_unit) {
		    case 4:	hdp->format = AE_FLOAT;  break;
		    default:    sprintf(errmsg,"unexpected audio input length of %d (float)",
					(int) ahp->bytes_per_unit);
		                die(errmsg);
		    }
		    break;
	    default:
		    hdp->format = AE_ULAW;
		    sprintf(errmsg,"audio_in %s format unclear, deducing %s",
			    sfname, getstrformat((int)hdp->format));
		    warning(errmsg);
		    break;
	    }
	    hdp->hdrsize = -1;               /* don't know real header size */
	    hdp->readlong = 0;               /*  but aligned on audio_start */
	    return(hdp);
	}
	else return(NULL);
#endif
#ifdef SFDIGDES
        float   fsr;               /* file sample rate */
        int     fnch, fbpd;        /* file channels & bytes per datum */

	if (ReadMacHeader(sfname,&fnch,&fsr,&fbpd) == 0) {  /* get resource */
	    hdp->sr = fsr;
	    hdp->nchnls = fnch;                         /*  record data */
	    hdp->sampsize = fbpd;
	    hdp->format = (fbpd == 4)?AE_FLOAT: AE_SHORT; /* 2 poss formats  */
	    hdp->hdrsize = 0;                /* no header on file       */
	    hdp->readlong = 0;               /* aligned to audio_start  */
	    return(hdp);
	}
	else return(NULL);
#endif        
#ifdef AIFF
extern  HEADATA *aiffReadHeader();
	return(aiffReadHeader(ifd,sfname,&HeadData));
#endif
}

void writeheader(ofd,ofname)    /* write an sfheader struct into output stream */
 int  ofd;                      /*        called only by sfopenout()           */
 char *ofname;
{
#ifdef SFIRCAM
        register SFHEADER *sfh;
	SFAUDIOENCOD *aep;
	int n;
	void sndwrterr();
	outhdrblk = mcalloc((long)sizeof(SFHEADER));	/* allocate header blk */
	sfh = (SFHEADER *)outhdrblk;
	sfmagic(sfh) = SF_MAGIC;
	sfsrate(sfh) = esr;     			/*   & assgn headrvals */
	sfchans(sfh) = nchnls;
	sfclass(sfh) = outsampsiz;
	outcodbeg = &sfcodes(sfh);                      /* set sfcode limits   */
	outcodend = (char *)sfh + sizeof(SFHEADER);
	putendcode(outcodbeg);          	        /* put initial sfendcode */
	if ((aep = (SFAUDIOENCOD *) creatsfcode(SF_AUDIOENCOD)) != NULL) {
	    aep->encoding = outformat;                  /*   then add encode blk */
	    aep->grouping = 1;
	}
	if ((n = write(ofd,sfh,sizeof(SFHEADER))) < sizeof(SFHEADER))
	        sndwrterr(n, sizeof(SFHEADER));
#endif
#ifdef SFSUN41
	register Audio_hdr *hp;
	unsigned encode;               /* chk that requested coding is legal */
	if (!(encode = (outformat==AE_ALAW)? AUDIO_ENCODING_ALAW :
	               (outformat==AE_ULAW)? AUDIO_ENCODING_ULAW :
	               (outformat==AE_LONG)? AUDIO_ENCODING_LINEAR :
		       (outformat==AE_FLOAT)? AUDIO_ENCODING_FLOAT : 0))
	        die("illegal encode for SFSUN41");
	outhdrblk = mcalloc((long)sizeof(Audio_hdr));	/* allocate a header blk */
	hp = (Audio_hdr *)outhdrblk;
	hp->sample_rate = (unsigned) esr;           /*  & fill in its values */
	hp->samples_per_unit = 1;
	hp->bytes_per_unit = (unsigned) outsampsiz;
	hp->channels = (unsigned) nchnls;
	hp->encoding = encode;
	hp->data_size = AUDIO_UNKNOWN_SIZE;
	if (audio_write_filehdr(ofd, hp, NULL, 0) != AUDIO_SUCCESS)
	        die("couldn't write the outfile header");
#endif
#ifdef SFDIGDES
        AddMacHeader(ofname,nchnls,esr,outsampsiz);
        MacSetCreator(ofname);        /*   set creator & file type */
#endif
#ifdef AIFF
	aiffWriteHdr(ofd,outsampsiz,nchnls,esr);
	if (sfheader)
	          SetTypeCreator(ofname,(long)'AIFF',(long)'????');
#endif
}

void rewriteheader(ofd, datasize)   /* write MaxAmps (IRCAM) or datasize (SUN) into */
  int      ofd;                     /* existing sfheader;    called by sfcloseout() */
  unsigned datasize;
{
	int n;
#ifdef SFIRCAM
	register SFHEADER *sfh;
	SFMAXAMP *maxp;
extern  float    omaxamp[];

	sfh = (SFHEADER *)outhdrblk;	/* Update initial header blk with maxamps */
	if ((maxp = (SFMAXAMP *) creatsfcode(SF_MAXAMP)) != NULL)
	    for (n = 0; n < SF_MAXCHAN; n++)
		maxp->value[n] = omaxamp[n];
	lseek(ofd,0L,0);
	wheader(ofd,sfh);
#endif
#ifdef SFSUN41
	if ((n = audio_rewrite_filesize(ofd, datasize)) != AUDIO_SUCCESS
	    && n != AUDIO_ERR_NOEFFECT)
	        die("error writing size into sfheader");
#endif
#ifdef AIFF
	aiffReWriteHdr(ofd);
#endif
}

