/* vaxify.c bv 5/90 - convert a binary file containing floats from dec-risc to dec-vax
/* usage:	vaxify  {-fh} [-i] [infile outfile]
/* options:	-f	floats
/*		-h	header-defined  (soundfile, pvoc or lpc file)
/*		-i	the inverse conversion
/* 'infile outfile' default to stdin and stdout.
/* Example:	vaxify -hi <vaxformatsoundfile >decriscformatsoundfile	*/

#include <stdio.h>
#include <sfheader.h>
#include <lpc.h>
#include "../../cs.h"
#include "../../soundio.h"

#define  FMOD   1		/* binary float exponent-byte modifier */
#define  NULLC '\0'

char	 cbuf[1024];
FILE	 *fopen();
int	 inverse = 0;

main(argc,argv)
 int argc;
 char **argv;
{
register FILE *ifp = stdin, *ofp = stdout;
register int  c, d, e;
register char *cp, filetype = NULLC;
	 long *lp, id, hdrsiz;

	 argc--;  argv++;
	 while (argc && (cp = *argv) && *cp++ == '-') {
		while ((c = *cp++) != NULLC)
		    switch(c) {
		    case 's':
		    case 'l':         /* for short, long, float */
		    case 'f':         /*     or header          */
		    case 'h': if (filetype != NULLC)
				dieu("option conflict");
		              filetype = c;
			      break;
		    case 'i': inverse = 1;	/* for inverse of vaxify */	
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
		        dieu("extra file argument");
		}
	 }
choose:	 switch(filetype) {
	 case 'f': if (!inverse)			/* floats:	*/
		      while ((c = getc(ifp)) != EOF) {	/* vaxify:	*/
			  d = getc(ifp);		/* save 1st pair */
			  putc(getc(ifp),ofp);          /* byte 3 now byte 1 */
			  if ((e = getc(ifp)))  	/* if byte 4 non-zero */
			    e += FMOD;  		/*	modify it  */
			  putc(e,ofp);  		/* byte 4 now byte 2 */
			  putc(c,ofp);                  /* 1st pair now 2nd */
			  putc(d,ofp);
		      }
	   	   else while ((c = getc(ifp)) != EOF) {/* vaxify -i:	*/
			  d = getc(ifp);
			  if (d)			/* if byte 2 non-zero */
			    d -= FMOD;  		/*	modify it  */
			  putc(getc(ifp),ofp);  	/* 2nd pair now 1st */
			  putc(getc(ifp),ofp);
			  putc(c,ofp);  		/* 1st pair now 2nd */
			  putc(d,ofp);
		      }
		   break;
	 case 'h': getbuf(sizeof(long),ifp);   		/* file with header,	*/
		   lp = (long *)cbuf;   		/*  chk ID:     */
		   id = *lp;
		   if (id == SF_MAGIC) {	           /* SF header:	*/
		        SFHEADER *sfh;
			SFCODE   *sfcp;
			SFMAXAMP *sfm;
			int i, class;
			char *endp;
			fprintf(stderr,"vaxify: sf header\n");
			morebuf(sizeof(long),sizeof(SFHEADER)-sizeof(long),ifp);
			sfh = (SFHEADER *)cbuf;
			tweekfloat(&sfsrate(sfh));	/* selectively adjust contents */
			class = sfclass(sfh);
			cp = (char *) &sfcodes(sfh);
			while ((sfcp = (SFCODE *)cp) && sfcp->code != SF_END) {
			    endp = cp + sfcp->bsize;        /* for each SFCODE:     */
			    cp += sizeof(SFCODE);
			    switch(sfcp->code) {
			    case SF_MAXAMP:                 /*   MAXAMPS - tweek    */
			        sfm = (SFMAXAMP *)cp;
				for (i = 0; i < SF_MAXCHAN; i++)
				    tweekfloat(&sfmaxamp(sfm,i));
				break;
			    case SF_PVDATA:                 /*   PVDATA shorts only */
			    case SF_COMMENT:                /*   COMMENTS ascii     */
				break;
			    default:  die("unrecognizable sfcode");
			    }
			    cp = endp;
			}
			putbuf(sizeof(SFHEADER),ofp);        /* put modified hdr */
			if (class == SF_FLOAT)
			    filetype = 'f';		     /* check for floats */
			else
			    filetype = 'r';		     /* else remainder as is */
			goto choose;
		   }
		   hdrsiz = id; 			   /* must have been hdrsiz */
		   morebuf(sizeof(long),sizeof(long),ifp); /*   so try next long */
		   id = *++lp;
		   if (id == LP_MAGIC) {		   /* LPC header:	*/
			LPHEADER *lph;
			fprintf(stderr,"vaxify: lp header\n");
			morebuf(2*sizeof(long),hdrsiz-2*sizeof(long),ifp);
			lph = (LPHEADER *)cbuf;
			tweekfloat(&lph->framrate);   /* selectively adjust contents  */
			tweekfloat(&lph->srate);
			tweekfloat(&lph->duration);
			putbuf(hdrsiz,ofp);		     /* put modified hdr */
			filetype = 'f';	        	     /* rest is floats   */
			goto choose;
		   }
		   die("infile has no recognized header code");
		   break;
	 case 's':
	 case 'l': printf("vaxify -%c: straight copy\n",filetype);
	 case 'r': while ((c = getc(ifp)) != EOF)      /* pass remainder as is */
	                putc(c,ofp);
	           break;
	 default:  dieu("unknown option flag");
	 }
	 fclose(ofp);
}

tweekfloat(byte)
 register char byte[];
{
register int b, d;
	 b = byte[1];
	 d = byte[3];
	 if (!inverse)
	   {if (d) d += FMOD;}
	 else if (b) b -= FMOD;
	 byte[3] = b;
	 byte[1] = d;
	 d = byte[0];
	 byte[0] = byte[2];
	 byte[2] = d;
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
	 fprintf(stderr,"usage:  vaxify {-fh} [-i] [infile outfile]\n");
	 die(s);
}

die(s)
 char *s;
{
	 fprintf(stderr,"vaxify:  %s\n",s);
	 exit(1);
}
