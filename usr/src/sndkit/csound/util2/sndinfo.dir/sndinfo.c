/*******************************************************\
*   sndinfo.c						*
*   read headers of soundfile(s)			*
*   dpwe 19sep90					*
\*******************************************************/

#include <stdio.h>
/* personal lib */
#include <sndf.h>	/* all the Soundfile stuff */
#include <pvoc.h>	/* stft type files */

/* Constants */

/* Static function prototypes */

/* Externs */

/* Static global variables */

static int 	debug = 1;
static char	*programName;

main(argc, argv)
    int argc;
    char *argv[];
    {
    SFSTRUCT	*theSound;
    PVSTRUCT	*thePvh;
    char	*inputfile;
    FILE	*file;

    /* Check arguments */
    programName = argv[0];
    if ((argc < 2)) {
        fprintf(stderr, "Usage: %s Soundfile [ Soundfile ...] \n",
	        programName);
	exit(1);
    }

    ++argv;
    while(--argc)
	{
	/* Read the file as maybe sndf, pvoc .. */
	if ( (file = SFOpenAllocRdHdr(argv[0], &theSound) ) != NULL)
	    PrintSNDFinfo(theSound, argv[0]);
	else if( (file = PVOpenAllocRdHdr(argv[0], &thePvh) ) != NULL)
	    PrintPVOCinfo(thePvh, argv[0]);
	else
	    fprintf(stderr,"**%s: unreadable file %s\n",
		    programName, argv[0]);
	if(file != NULL)
	    fclose(file);
	++argv;
	}
    }

PrintSNDFinfo(theSound, name)
    SFSTRUCT *theSound;
    char *name;
    {
    float	durn;
    int 	dsize,chans,infolen;
    long 	frames;
    char	type[32];

    switch(theSound->format)
	{
    case SFMT_CHAR:
	strcpy(type, "linear bytes");
	dsize = 1;
	break;
    case SFMT_ALAW:
	strcpy(type, "a-law bytes");
	dsize = 1;
	break;
    case SFMT_ULAW:
	strcpy(type, "mu-law bytes");
	dsize = 1;
	break;
    case SFMT_SHORT:
	strcpy(type, "16 bit linear");
	dsize = 2;
	break;
    case SFMT_LONG:
	strcpy(type, "32 bit linear");
	dsize = 4;
	break;
    case SFMT_FLOAT:
	strcpy(type, "4 byte floating point");
	dsize = 4;
	break;
    default:	sprintf(type, "unrecognised type (0x%lx) - assuming 16 bit", 
		theSound->format);
	dsize = 2;
	break;
	}
    infolen = SFInfoSize(theSound);
    chans = theSound->channels;
    frames = theSound->dataBsize/(long)(dsize*chans);
    durn = ((float)frames)/theSound->samplingRate;
    printf("\n%s : (SNDF)\n  Sample rate %5.0f Hz\t Duration  %5.2f s\n",
	   name, theSound->samplingRate, durn);
    if(chans==1)
	printf("  Mono   \t \t %s\n",type);
    else if(chans==2)
	printf("  Stereo \t \t %s\n",type);
    else printf("  %d channels, %s\n",chans,type);
    printf("  data bytes %ld \t info bytes  %5d\n", 
	   theSound->dataBsize, infolen);
    if(PrintableStr(theSound->info, infolen))
	printf("'%s'\n", theSound->info);
    }

PrintPVOCinfo(thePvh, name)
    PVSTRUCT *thePvh;
    char *name;
    {
    float	durn;
    int 	dsize,chans,infolen;
    long 	frames;
    char	type[32];
    long	fSiz, fInc;
    int 	wdsPerFrame;

    switch(thePvh->format)
	{
    case PVSHORT:
	strcpy(type, "pvoc, short words");
	dsize = 2;
	break;
    case PVFLOAT:
	strcpy(type, "pvoc, floats");
	dsize = 4;
	break;
    default:
	sprintf(type, "unrecognised type (0x%lx) - assuming 32 bit", 
		thePvh->format);
	dsize = 4;
	break;
	}
    fSiz = thePvh->frameSize;	fInc = thePvh->frameIncr;
    infolen = PVDFLTBYTS + thePvh->headBsize - sizeof(PVSTRUCT);
    chans = thePvh->channels;
    wdsPerFrame = fSiz + 2;		/* !! 2 * (n/2 + 1) */
    frames = thePvh->dataBsize/(long)(dsize*chans*wdsPerFrame);
    if(frames == 0)
	durn = 0.0;
    else
	durn = ((float)(frames-1)*fInc+fSiz)/(thePvh->samplingRate);
    printf("\n%s : (PVOC)\n  Sample rate %5.0f Hz\t Duration  %5.2f s\n",
	   name, thePvh->samplingRate, durn);
    if(chans==1)
	printf("  Mono   \t \t %s\n",type);
    else if(chans==2)
	printf("  Stereo \t \t %s\n",type);
    else printf("  %d channels, %s\n",chans,type);
    printf("  frame size  %5ld \t frame incr %5ld\n",
	   fSiz, fInc);
    printf("  data bytes %ld \t info bytes  %5d\n", 
	    thePvh->dataBsize, infolen);
    if(PrintableStr(thePvh->info, infolen))
	printf("'%s'\n", thePvh->info);
    }

int PrintableStr(s, max)
    char *s;
    int max;
    /* return whether to print string s, of len <= max, coz it looks good */
    {
    char c, *t;
    int  n,g=0;

    if(max > 0)
	{
	g = 1;
	n = max;
	t = s;
	while( g && (c = *t++) && --max)
	    {
	    if(c!='\n' && c!='\r' && c!='\t' && c<' ')
		g = 0;	/* nasty control code */
	    if(c>0x7F)
		g = 0;	/* beyond ascii */
	    }
	if(c)	g = 0;	/* must finish on nul */
	if(t = s+1)	g = 0;	/* null string - don't bother */
	}
    return(g);
    }







