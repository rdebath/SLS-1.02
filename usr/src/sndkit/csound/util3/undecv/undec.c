/* undec.c bv 5/90 - convert a binary data file from dec-risc (byte reversed) format
/*                                                  to standard (byte normal) format
/* usage:	undec  -{slfh} [-i] [infile outfile]
/* options:	-s	short ints      (adsyn file)
/*		-l	long ints
/*		-f	floats
/*		-h	header-defined  (soundfile, pvoc or lpc file)
/*		-i	the inverse conversion
/* 'infile outfile' default to stdin and stdout.
/* Example:	undec -hi < sunformatsoundfile  > decformatsoundfile	*/

#include <stdio.h>
#include <sfheader.h>
#include <lpc.h>
#include "../../cs.h"
#include "../../soundio.h"

#define  revfloat(A)  revlong(A)
#define  NULLC        '\0'

char	 cbuf[1024];
FILE	 *fopen();

isbytrev()                  /* are we doing this on a byte-reversed machine? */
{
  static short one = 1;
  return((int) *(char *)&one);
}

main(argc,argv)
 int argc;
 char **argv;
{
register FILE *ifp = stdin, *ofp = stdout;
register int  c, d, e;
register char *cp, filetype = NULLC;
	 long *lp, id, hdrsiz;
         int  bytrev, revfirst, inverse = 0;

	 argc--;  argv++;
	 while (argc && (cp = *argv) && *cp++ == '-') {
		while ((c = *cp++) != NULLC)
		    switch(c) {
		    case 's':			/* for short,long,float */
		    case 'l':			/*	or header	*/
		    case 'f':
		    case 'h': if (filetype != NULLC)
				dieu("option conflict");
			      filetype = c;
			      break;
		    case 'i': inverse = 1;	/* for inverse of undec */	
			      break;
		    default:  dieu("unknown option");
		    }
		argc--; argv++;
	 }
	 if (filetype == NULLC)
	        dieu("no filetype option flag");
	 if (argc--) {
		if ((ifp = fopen(*argv,"r")) == NULL)
		    dies("cannot open %s",*argv);
		if (argc--) {
		    if ((ofp = fopen(*++argv,"w")) == NULL)
		        dies("cannot open %s",*argv);
		    if (argc)
		        dieu("too many file arguments");
		}
	 }
         bytrev = isbytrev();
         revfirst = (bytrev == inverse) ? 1 : 0;
         fprintf(stderr,"bytrev = %d, revfirst = %d\n", bytrev, revfirst);

choose:	 switch(filetype) {
	 case 's': while ((c = getc(ifp)) != EOF) {	/* shorts:	*/
		 	putc(getc(ifp),ofp);		/*  rev 2 bytes	*/
			putc(c,ofp);
		   }
		   break;
	 case 'l':
	 case 'f': while ((c = getc(ifp)) != EOF) {	/* longs,floats: */
			d = getc(ifp);
			e = getc(ifp);
			putc(getc(ifp),ofp);		/*  rev 4 bytes */
			putc(e,ofp);
			putc(d,ofp);
			putc(c,ofp);
		   }
		   break;
	 case 'h': getbuf(sizeof(long),ifp);   		/* file with header,	*/
		   lp = (long *)cbuf;   		/*  chk ID:             */
		   if (revfirst)  revlong(lp);
		   id = *lp;
		   if (!revfirst) revlong(lp);
		   if (id == SF_MAGIC) {	        /* SF header:   	*/
		        SFHEADER *sfh;
			SFCODE *sfcp;
			SFPVDATA *sfpv;
		        long class, code, len;
			char *endp;
			fprintf(stderr,"undec: sf header\n");
			morebuf(sizeof(long),sizeof(SFHEADER)-sizeof(long),ifp);
			sfh = (SFHEADER *)cbuf;
			revfloat(&sfsrate(sfh));	      /* adjust contents  */
			revlong(&sfchans(sfh));
			if (revfirst)  revlong(&sfclass(sfh));
			class = sfclass(sfh);
			if (!revfirst) revlong(&sfclass(sfh));
			cp = (char *) &sfcodes(sfh);        /* for non-zero SFCODE: */
			while ((sfcp = (SFCODE *)cp) && sfcp->code != SF_END) {
			    if (revfirst)  revcodes(cp);
			    code = sfcp->code;                /*   decipher it      */
			    len = sfcp->bsize;                /*   & find its len   */
			    if (!revfirst) revcodes(cp);
			    endp = cp + len;
			    cp += sizeof(SFCODE);
			    switch(code) {
			    case SF_MAXAMP:                   /* NB: assume MAXAMPS */
				lp = (long *)cp;              /* are floats & longs */
				while ((char *)lp < endp)
				    revlong(lp++);
				break;
			    case SF_PVDATA:                   /* NB: presume we know */
				sfpv = (SFPVDATA *)cp;        /* the SFPVDATA struct */
				fprintf(stderr,"\tpvoc data\n");
				revshort((char *)&sfpv->frameSize);
				revshort((char *)&sfpv->frameIncr);
				break;
			    case SF_COMMENT:                  /* pass comments as is */
				break;
			    default:  die("unrecognizable sfcode");
			    }
			    cp = endp;
			}
			putbuf(sizeof(SFHEADER),ofp);	  /* now put modified sf_hdr */
			if (class == SF_FLOAT)
			    filetype = 'f';		  /* rest is either floats   */
			else
			    filetype = 's';		  /*             or shorts   */
			goto choose;
		   }
		   hdrsiz = id; 			/* must have been hdrsiz */
		   morebuf(sizeof(long),sizeof(long),ifp); /*   so try next long */
		   lp++;
		   if (revfirst)  revlong(lp);
		   id = *lp;
		   if (!revfirst) revlong(lp);
		   if (id == LP_MAGIC) {		/* LPC header:	*/
			LPHEADER *lph;
			fprintf(stderr,"undec: lp header\n");
			morebuf(2*sizeof(long),hdrsiz-2*sizeof(long),ifp);
			lph = (LPHEADER *)cbuf;
			revlong(&lph->npoles);  	/* adjust contents  */
			revlong(&lph->nvals);
			revfloat(&lph->framrate);
			revfloat(&lph->srate);
			revfloat(&lph->duration);
			putbuf(hdrsiz,ofp);		/* put modified hdr */
			filetype = 'f'; 		/* rest is floats   */
			goto choose;
		   }
		   die("infile has no recognized header code");
		   break;
	 default:  dieu("unknown option flag");
	 }
	 fclose(ofp);
}

revshort(byte)
 register char byte[];
{
register int c = byte[0];
	 byte[0] = byte[1];
	 byte[1] = c;
}

revlong(byte)
 register char byte[];
{
register int c = byte[0];
	 byte[0] = byte[3];
	 byte[3] = c;
	 c = byte[1];
	 byte[1] = byte[2];
	 byte[2] = c;
}

revcodes(cp)
 char *cp;
{
         SFCODE *sfc = (SFCODE *)cp;
	 revshort((char *)&sfc->code);
	 revshort((char *)&sfc->bsize);
}

getbuf(nbytes,ifp)
 register int nbytes;
 register FILE *ifp;
{
register char *cp = cbuf;
	 do *cp++ = getc(ifp);
	 while (--nbytes);
}
 
morebuf(start,nbytes,ifp)
 register int start, nbytes;
 register FILE *ifp;
{
register char *cp = &cbuf[start];
	 do *cp++ = getc(ifp);
	 while (--nbytes);
}

putbuf(nbytes,ofp)
 register int nbytes;
 register FILE *ofp;
{
register char *cp = cbuf;
	 do putc(*cp++,ofp);
	 while (--nbytes);
}

dies(s,t)
 char *s, *t;
{
	 char errmsg[100];
	 sprintf(errmsg,s,t);
	 die(errmsg);
}

dieu(s)
 char *s;
{
	 fprintf(stderr,"usage:  undec -{slfh} [-i] [infile outfile]\n");
	 die(s);
}

die(s)
 char *s;
{
	 fprintf(stderr,"undec:  %s\n",s);
	 exit(1);
}
