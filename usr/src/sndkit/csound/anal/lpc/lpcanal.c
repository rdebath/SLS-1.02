/* Standalone analysis program, reading fixed-point monaural sound files.
 * Currently set for maximum of 34 poles, & max anal segment of 500 samples,
 * meaning that the frame slices cannot exceed 250 samples.
 * Program size expands linearly with slice size, and as square of npoles.
 */

/* #include <sysdep.h> */
#include <stdio.h>
#include <math.h>    /* removed include <sys/file.h> */
#include <pwd.h>

#include "crack.h"
#include <lpc.h>
#include <sndf.h>

#ifndef SEEK_CUR		/* stdio.h not ansi! */
#define	SEEK_SET	0
#define SEEK_CUR	1
#define SEEK_END	2
#endif

#define INCR 4
#define NAMESIZE 1024		/* commandline string size limit */

static int NPOLE;
static int FRAME;
static int NPP1;

char	*buildsfname();
int debug=0, verbose=0;
char	*programName;

#define POLEMAX	34
#define DEFNPOLES POLEMAX
#define FRAMAX  500 
#define DEFSLICE 200	/* <= FRAMAX/2 */
#define NDATA 	4	/* number of data values stored with frame */
#define PITCHMIN	70.0
#define PITCHMAX	200.0	/* default limits in Hz for pitch search */

/* globals for pitch tracker */
int 	LSLICE;
float	SR;
float	NYQ;
int 	JMAX;
int 	MM;
float	gtphi[50][5][25], gtpsi[50][6][25];
float	gtgamph[50][5],   gtgamps[50][6];
float	gfreq[50];
extern float getpch();
int	doPitch = 1;		/* flag to do pitch tracking */

main(argc, argv)
    int argc;
    char **argv;
    {
    int	jj, slice, counter;
    float	coef[POLEMAX+NDATA], inskip, dur;
    char	*sfname;
    char    iname[NAMESIZE];
    char    oname[NAMESIZE];
    double	errn, rms1, rms2, cc[40];
    short	sigi[FRAMAX];	/* changed to short */
    long	i, nsamps, skipbytes, outskip;
    int	nbytes, nblpc, firstframe, nread;
    int	n, hsize, esize;	/* new declarations */
    SFSTRUCT *sfh;
    FILE	*sf,*analf;
    LPHEADER *lph;
    char	*lpbuf, *tp;
    char    c;
    int rflag = 0;
    float	pchlow, pchhigh;

    lpbuf = (char *) calloc(LPBUFSIZ, sizeof(char));
    lph = (LPHEADER *) lpbuf;
    tp = lph->text;

    /* DEFAULTS... */
    programName = argv[0];
    *iname = '\0';		/* '\0' ==> stdin */
    *oname = '\0';		/* '\0' ==> stdout */
    NPOLE = DEFNPOLES;	
    slice = DEFSLICE;
    inskip = 0.0;
    dur = 0.0;			/* default duration is to length of ip */
    *tp = '\0';
    pchlow = PITCHMIN;
    pchhigh = PITCHMAX;

    if (debug) verbose++;
    /* set options from commandline */
    arg_index = 0;		/* re-crack */
    while ((c = crack(argc, argv, 
		      "p|i|s|d|o|C|P|Q|", 
		      0)) != NULL) 
	{
	if (c == EOF) usage("error cracking commandline");
	switch (c) {		/* cases w/o options */
	default:		/* cases w/ options */
	    if (arg_option == NULL) usage("flag missing an option");
	    switch (c) {
		/* sound flags... */
	    case 'p':		/* npoles */
		sscanf(arg_option,"%d", &NPOLE); break;
	    case 'i':		/* frame size */
		sscanf(arg_option,"%d", &slice); break; 
	    case 's':		/* skiptime */
		sscanf(arg_option,"%f", &inskip); break;	
	    case 'd':		/* duration */
		sscanf(arg_option,"%f", &dur); break; 
	    case 'o':		/* output filename */
		if ( strlen(arg_option) >= NAMESIZE )
		    die("outputfile name too long!");
		strcpy(oname,arg_option); break; 
	    case 'C':		/* comment string */
		strncat(tp,arg_option,(LPBUFSIZ - sizeof(LPHEADER) + 4));
		tp += strlen(tp);
		break;
	    case 'P':		/* pitch search lower limit */
		sscanf(arg_option,"%f", &pchlow);
		if(pchlow == 0)
		    doPitch = 0;	/* -P0 inhibits ptrack */
		break;
	    case 'Q':		/* pitch upper search limit */
		sscanf(arg_option,"%f", &pchhigh); break;
	    default: usage("unrecognized flag");
		}
	    }
	}
    if ( arg_index != argc ) {	/* soundfile name here */
	if ( strlen(argv[arg_index]) >= NAMESIZE )
	    die("soundfile name too long!");
	strcpy(iname,argv[arg_index]);
	}
    if (verbose) {
	fprintf(stderr,"Writing to lpfile ");
	if (*oname == '\0') fprintf(stderr,"<stdout>\n");
	else fprintf(stderr,"%s\n",oname);
	fprintf(stderr,"Reading from soundfile ");
	if (*iname == '\0') fprintf(stderr,"<stdin>\n");
	else fprintf(stderr,"%s\n",iname);
	fprintf(stderr, "poles=%d interframeoffset=%d skip=%f duration=%f\n",
		NPOLE, slice, inskip, dur);
	fprintf(stderr, "lpheader comment=\n%s\n", lph->text);
	if(pchlow>0)
	    fprintf(stderr, 
		    "pitch track range from %f to %f Hz\n",pchlow,pchhigh);
	else
	    fprintf(stderr,"pitch tracking inhibited\n");
	}
    if ( *oname == '\0' ) analf = stdout; /* write stdout */
    else {
	if((analf = fopen(oname,"w")) == NULL)
	    die("cannot open output data file");
	}
    if ( *iname == '\0' ) sf = stdin; /* read stdin */
    else 
	sfname = buildsfname(iname);

    if(  (sf = SFOpenAllocRdHdr(sfname, &sfh))==NULL)
	usage("cannot open sound file"); /* %s", sfname); */

    if (debug) {
	fprintf(stderr, "sf_chans=%d sf_srate=%f sf_fmt=%d\n", 
		sfh->channels, sfh->samplingRate, sfh->format);
	}

    if(sfh->channels != 1)
	die("takes monaural files only");
    if(sfh->format != SFMT_SHORT)
	die("takes integer (shortsam) files only");

    if (NPOLE > POLEMAX)
	dies("poles exceeds maximum allowed");
    FRAME = slice * 2;
    if (FRAME > FRAMAX)
	die("framesize (inter-frame-offset*2) exceeds maximum allowed");

    if(dur == 0)
	dur = (((float)sfh->dataBsize / (sfh->channels*sizeof(short)) )
	       / sfh->samplingRate) - inskip;
    lph->lpmagic = LP_MAGIC;
    lph->srate = sfh->samplingRate;
    lph->npoles = NPOLE;
    lph->nvals = NPOLE + 4;
    lph->framrate = lph->srate / (float) slice;
    lph->duration = dur;

    /* compute header size */
    hsize = tp - (char *) lph;	/* sizeof text */
    esize = INCR - (hsize % INCR); /* round up to 4 byte boundary */
    if (esize == 4) esize = 0;	/* if we need it */
    lph->headersize = hsize + esize;
    if (lph->headersize >= (LPBUFSIZ - sizeof(LPHEADER) + 4))
	lph->headersize = LPBUFSIZ;

    if (fwrite((char *) lph, sizeof(char), lph->headersize, analf) 
	< lph->headersize)
	die("anallpc: can't write header");
	
    skipbytes = (long)(inskip * sfh->samplingRate) * sizeof(short);
    if (*iname == '\0') { /* not seekable */
	while (skipbytes > FRAMAX) { /* remote: spinread for lseek */
	    if ((nread = fread((char *)sigi,sizeof(char),FRAMAX,sf)) < FRAMAX)
		die("soundfile skip error");
	    skipbytes -= nread;
	    }
	if ((nread = fread((char *)sigi, skipbytes,sizeof(char),sf)) 
	    < skipbytes)
	    die("soundfile skip error");
	}
    else if (fseek(sf,skipbytes,SEEK_CUR) < 0)
	die("bad seek on skip");

    firstframe = 0;

    NPP1 = NPOLE+1;
    nblpc = (NPOLE + NDATA)*sizeof(float);
    outskip = firstframe * nblpc + lph->headersize;

    if (*oname != '\0')
	if ((fseek(analf,outskip,SEEK_SET)) < 0)
	    die("Bad lseek on analysis file");
	else if (firstframe != 0)
	    fprintf(stderr,"warning: can't lseek on stdout");

    nsamps = dur * sfh->samplingRate;
    nbytes = FRAME * sizeof(short); /* bytes for whole frame */
    if ((nread = fread((char *)sigi, sizeof(char), nbytes, sf)) != nbytes)
	die("soundfile read error: couldn't fill first frame");
    /*	for(i=0;i<FRAME;i++) printf("%d ",sigi[i]); 	*/

    if (debug) fprintf(stderr, "nbytes : %d\n", nbytes);
    nbytes = slice * sizeof(float); /* bytes for half frame */
    /* FRAME is slice * 2 */

    /* set up pitch tracker */
    SR     = sfh->samplingRate;
    NYQ    = SR/2;
    LSLICE = FRAME;	/* NOT slice, JSLIDE = slice; */
    JMAX   = LSLICE/10;
    MM     = (JMAX+1)/2;
    if(doPitch)
	ptable(pchlow,pchhigh,gtphi,gtpsi,gtgamph,gtgamps,gfreq,0);

    if (debug) fprintf(stderr, "nbytes shifted: %d\n", nbytes);
    i = 0;
    counter = 1;
    while (i < nsamps) {
	alpol(sigi,&errn,&rms1,&rms2,cc);
	coef[0] = (float)rms2; 
	coef[1] = (float)rms1; 
	coef[2] = (float)errn; 
	if(doPitch)
	    coef[3] = getpch(sigi,gtphi,gtpsi,gtgamph,gtgamps,gfreq,0);
	else
	    coef[3] = 0.0;

	/* 		fprintf(stderr,"%d %f %f %f %f\n",
			counter++,coef[0],coef[1],coef[2],coef[3]);	 */
			
	/* reverse the coefs & change sign */
	for(jj=NDATA; jj<NPOLE+NDATA; jj++)
	    coef[jj] = (float)-cc[NPOLE-jj+(NDATA - 1)];  
	/* then write the anal frame */
	if ((n = fwrite((char *)coef, sizeof(char), nblpc, analf)) 
	    != nblpc)
	    die("write error");

	for(jj=0; jj<slice; jj++,i++) /* now move slice forward */
	    sigi[jj] = sigi[jj+slice]; /*   & refill:	     */
	if ((n = fread((char *)(sigi+slice), sizeof(char), nbytes, 
			 sf)) != nbytes)
	    break;		/* ran up against eof */
	}
    }

alpol(sig, errn, rms1, rms2, c)
 double *errn, *rms1, *rms2, *c;
 short *sig;					/* sig now short */
{
	double a[POLEMAX][POLEMAX], v[POLEMAX], b[POLEMAX];
	double x[FRAMAX], y[FRAMAX];
	double *vp=v, *bp=b, *xp=x, *yp=y;
	double sum, sumx, sumy;
	int k1, i, l, k, limit, j;

	for (xp=x; xp-x < FRAME ;++xp,++sig) 
		*xp = (double) *sig;
	k1 = NPOLE + 1;
	for (i=0; i < NPOLE ;++i)  {
		sum = (double) 0.0;
		for (k=NPOLE; k < FRAME ;++k)
			sum += x[k-(i+1)] * x[k];
		v[i] = -sum;
		if (i != NPOLE - 1)  {
			limit = NPOLE - (i+1);
			for (l=0; l < limit ;++l)  {
				sum += x[NPOLE-(i+1)-(l+1)]* x[NPOLE-(l+1)] - x[FRAME-(i+1)-(l+1)]* x[FRAME-(l+1)];
				a[(i+1)+l][l] = a[l][(i+1)+l] = sum;
			}
		}
	}
	sum = (double) 0.0;
	for (k=NPOLE; k < FRAME ;++k)
		sum += pow(x[k], (double) 2.0);
	sumy = sumx = sum;
	for (l=0; l < NPOLE ;++l)  {
		sum += pow(x[NPOLE-(l+1)], (double) 2.0) - pow(x[FRAME-(l+1)], (double) 2.0);
		a[l][l] = sum;
	}
	gauss(a, v, b);
/*	filtn(x, y, b);   */
	for (i=0; i < NPOLE ;++i)
		sumy = sumy - b[i]*v[i];
	*rms1 = sqrt(sumx/(double) (FRAME - k1 + 1) );
	*rms2 = sqrt(sumy/(double) (FRAME - k1 + 1) );
	*errn = pow(((*rms2)/(*rms1)), (double) 2.0);
	for (bp=b; bp-b < NPOLE ;++bp,++c)
		*c = *bp;
	return(0);
}

gauss(aold, bold, b)
double aold[POLEMAX][POLEMAX], *bold, *b;
{
	double amax, dum, pivot;
	double c[POLEMAX], a[POLEMAX][POLEMAX];
	int i, j, k, l, istar, ii, lp;

	/* aold and bold untouched by this subroutine */
	for (i=0; i < NPOLE ;++i)  {
		c[i] = bold[i];
		for (j=0; j < NPOLE ;++j)
			a[i][j] = aold[i][j];
	}
	/* eliminate i-th unknown */
	for (i=0; i < NPOLE - 1 ;++i)  {        /* find largest pivot */
		amax = 0.0;
		for (ii=i; ii < NPOLE ;++ii)  {
			if (fabs(a[ii][i]) >= amax)  {
				istar = ii;
				amax = fabs(a[ii][i]);
			}
		}
		if (amax < 1e-20)
			die("gauss: ill-conditioned");

		for (j=0; j < NPOLE ;++j)  {    /* switch rows */
			dum = a[istar][j];
			a[istar][j] = a[i][j];
			a[i][j] = dum;
		}
		dum = c[istar];
		c[istar] = c[i];
		c[i] = dum;
		/* pivot */
		for (j=i+1; j < NPOLE ;++j)  {
			pivot = a[j][i] / a[i][i];
			c[j] = c[j] - pivot * c[i];
			for (k=0; k < NPOLE ;++k)
				a[j][k] = a[j][k] - pivot * a[i][k];
		}
	}       /* return if last pivot is too small */
	if (fabs(a[NPOLE-1][NPOLE-1]) < 1e-20)
		die("gauss: ill-conditioned");

	*(b+NPOLE-1) = c[NPOLE-1] / a[NPOLE-1][NPOLE-1];
	/* back substitute */
	for (k=0; k<NPOLE-1; ++k)  {
		l = NPOLE-1 -(k+1);
		*(b+l) = c[l];
		lp = l + 1;
		for (j = lp; j<NPOLE; ++j)
			*(b+l) += -a[l][j] * *(b+j);
		*(b+l) /= a[l][l];
	}
	return(1);
}


filtn(x, y, b)
double x[], b[], y[];
{
	double sum;
	int i, j;
	double *yp;

	for (yp=y; yp-y < NPOLE ;++yp)
		*yp = (double) 0.0;
	yp = y;
	for (i=NPOLE; i < FRAME; ++i)  {
		sum = x[i];
		for (j=0; j < NPOLE ;++j)  {
			sum = sum + b[j] * x[i-(j+1)];
		}
		*(yp+i) = sum;
	}
	return(0);
}

iarrayprint(array, size, name)
short array[];
int size;
char *name;
{
	 int i;
	 printf("\n%s : \n", name);
	 for (i = 0; i < size; i++)	{
		  if ((i % 10) == 0)
		    printf("\n");
		  printf("%d ", array[i]);
	 }
}

farrayprint(array, size, name)
 double array[];
 int size;
 char *name;
{
	 int i;
	 printf("\n%s : \n", name);
	 for (i = 0; i < size; i++)	{
		  if ((i % 10) == 0)
		    printf("\n");
		  printf("%.1f ", array[i]);
	 }
}
	
usage(msg)
     char *msg;
{
  fprintf (stderr,
   "   Usage:  %s [flag][option] ... [soundfile]\n", programName);
  fprintf (stderr,
   "[flag][option] from among:\n");
  fprintf (stderr,
   "-pNPOLES          number of poles for analysis (default %d)\n",
	   DEFNPOLES);
  fprintf (stderr,
   "-iINTERFRAMEOFSET offset between frames in samples (default %d)\n",
	   DEFSLICE);
  fprintf (stderr,
   "                     (framesize will be twice INTERFRAMEOFSET)\n");
  fprintf (stderr,
   "-sSKIPTIME        initial seconds of sound to skip over (default 0.0)\n");
  fprintf (stderr,
   "-dDURATION        duration in seconds to analyze (default: to EOF)\n");
  fprintf (stderr,
   "-oOUTPUTFILE      analysis output file (stdout if absent, default)\n");
  fprintf (stderr,
   "-CCOMMENT_STRING  comment field of lp header (default empty)\n");
  fprintf (stderr,
   "-PMINPITCH        lower limit for pitch search (default %f Hz)\n",
	   PITCHMIN);
  fprintf (stderr,
   "                     (-P0 inhibits pitch tracking)\n");
  fprintf (stderr,
   "-QMAXPITCH        upper limit for pitch search (default %f Hz)\n",
	   PITCHMAX);
  fprintf (stderr,
   "soundfile         input : SNDF soundfile\n");
  fprintf (stderr,
   "see also:  man anallpc\n\n");
	   die(msg);
}
