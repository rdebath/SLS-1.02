/*******************************************************\
*   scale.c						*
*   scale a sound file by a float factor		*
*   dpwe 19sep90					*
\*******************************************************/

#include <stdio.h>
#include <sndf.h>

/* Constants */

#define SHORTMAX 32767

/* Static function prototypes */
#ifdef __STDC__
static void ScaleSound(SFSTRUCT *sound, FLOATARG factor);
static int scanflt(char *s, float *pfl);
#else
static void ScaleSound();
static int scanflt();
#endif

/* Externs */

/* Static global variables */
static char *programName;
static int debug = 1;

main(argc, argv)
    int argc;
    char *argv[];
    {
    SFSTRUCT	*theSound;
    char	*inputfile, *outputfile;
    int		rate,err=0;
    float	factor = 0.0;

    /* Check arguments */
    programName = argv[0];
    if(argc < 2 )	err = 1;
    else
	{
	if(argv[1][0] == '-')	/* an option */
	    {
	    if(argv[1][1] == 'f') /* is a scaling */
		{
		if(argc != 5)	
		    err = 1;
		else
		    {
		    scanflt(argv[2], &factor);
		    if(factor == 0)
			printf("Don't get factor of %s\n",argv[2]);
		    inputfile  = argv[3];
		    outputfile = argv[4];
		    }
		}
	    else		/* assume just scanning */
		{
		if(argc < 3)
		    err = 1;
		else
		    inputfile = argv[2];
		}
	    }
	else			/* no options - just scan file */
	    inputfile = argv[1];
	}
    if (err) 
	{
        fprintf(stderr, 
		"Usage: %s [-scan | -f fctr] <inputSndfile> [<outputSndfile>]\n",
	        programName);
	exit(1);
	}
    /* Read sound file */
    if (err = SFRead(inputfile, &theSound))
	{
	fprintf(stderr, "%s: sound error %s on %s\n", 
		programName, SFErrMsg(err), inputfile);
	exit(1);
	}
    if(debug) printf("Read in %s; %d bytes\n", 
		     inputfile, theSound->dataBsize);
    if(factor != 0.0)
	{
	/* perform scaling */
	ScaleSound(theSound,factor);
	/* Write sound file */
	if (err = SFWrite(outputfile, theSound))
	    {
	    fprintf(stderr, "%s: sound error %s on %s\n", 
		    programName, SFErrMsg(err), inputfile);
	    exit(1);
	    }
	}
    else
	FindAndReportMax(theSound);
    }

static void ScaleSound(sound,fctr)
    SFSTRUCT *sound;
    FLOATARG fctr;
    {
    short	*sptr;
    long	d;
    int 	samples;
    int 	dsize;

    d = (1L<<16)*fctr;
    sptr = (short *)SFDataLoc(sound);
    samples = sound->dataBsize/sizeof(short);
    ++samples;
    while(--samples)
	{
	*sptr = ((d * (long)(*sptr))>>16L);
	++sptr;
	}
    return;
    }

FindAndReportMax(sound)
    SFSTRUCT	*sound;
    {
    short 	s,max,min;
    long	maxpos,minpos;
    long	i;
    short	*sptr;
    float	tpersample;
    int 	chans;

    chans = sound->channels;
    tpersample = 1.0/sound->samplingRate;
    sptr = (short *)SFDataLoc(sound);
    max = *sptr++;	maxpos = 0;
    min = max;		minpos = 0;
    for(i = 1L; i< sound->dataBsize/sizeof(short); ++i)
	{
	if( (s = *sptr++)>max )
	    {
	    max = s;
	    maxpos = i;
	    }
	if( s<min )
	    {
	    min = s;
	    minpos = i;
	    }
	}
    printf("Max val %d at index %ld (time %.2f, chan %d)\n",
	   max,maxpos,tpersample*(maxpos/chans),(int)maxpos%chans);
    printf("Min val %d at index %ld (time %.2f, chan %d)\n",
	   min,minpos,tpersample*(minpos/chans),(int)minpos%chans);
    printf("Max scale factor = %.2f\n", 
	   (float)SHORTMAX/(float)((max>-min)?max:-min) );
    }

static int scanflt(s,pfl) /* read a float from a str; return 1 if OK, else 0 */
    char  *s;
    float *pfl;
    {
    register int  c;
    register long val=0, scale=1;

    while ((c = *s++) == ' ' || c == '\t') /* skip leading white space */
        ;
    if (!(c>='0' && c<='9' || c=='+' || c=='-' || c=='.'))
        return(0);		/* fail if not valid float  */
    if (c == '-') {scale = -1; c = *s++;}
    if (c == '+' || c == '0')  c = *s++;
    while (c >= '0' && c <= '9') {
        val *= 10;
	val += c - '0';
	c = *s++;
	}
    if (c == '.')
        c = *s++;
    while (c >= '0' && c <= '9') {
        val *= 10;
	val += c - '0';
	scale *= 10;
	c = *s++;
	}
    *pfl = (float) val/scale;	/* write to float */
    return(1);
    }
