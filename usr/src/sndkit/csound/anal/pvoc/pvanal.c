/************************************************************************/
/*  pvanal.c	    (really fdan.c)					*/
/*  Frequency Domain Analysis	(before fdsyn.c)			*/
/*  Takes a time domain soundfile and converts it into a file of	*/
/*  FFT frames.  These can then by used further..			*/
/*  01aug90 upgraded to use new pvoc.c filing functions for pvoc files	*/
/*  dpwe  25jan90, 23may90 Csound rls vsn, bv mods 5jun90, dpwe 25jul90 */
/************************************************************************/

#include <stdio.h>

#ifdef THINK_C
#include <console.h>
#include <tc_main.h>
#endif

#include <fft.h>
#include <dsputil.h>
#include <sndf.h>
#include <pvoc.h>

/*** Static function prototypes ***/
#ifdef __STDC__			/* prototype arguments */
static void CreatePVblock(SFSTRUCT *inputSound, PVSTRUCT **outputPVH );
static void takeFFTs(SFSTRUCT *inputSound, PVSTRUCT  *outputPVH );
static void PrintBuf(float *buf, int size, char *msg);
static void convertToMono(SFSTRUCT **theSound);
#else				/* no argument prototypes */
static void CreatePVblock();
static void takeFFTs();
static void PrintBuf();
static void convertToMono();
#endif

#define MINFRMMS	20	/* frame defaults to at least this many ms */
#define MAXFRMPTS	16384
#define MINFRMPTS	16	/* limits on fft size */
#define DFLT_OVLP	4	/* default frame overlap factor */

/* Global variables */
int	    sfErr;
char	    *programName;
int	    debug = 0;		/* tweaked inside ! */

/* Static global variables */
static	int frameSize	= 0;	    /* size of FFT frames */
static	int frameIncr	= 0;	    /* step between successive frames */

complex *basis;     /* LUTable for FFT */

main(argc,argv)
    int argc;
    char *argv[];
    {
    SFSTRUCT	    *inputSound;
    PVSTRUCT	    *outputPVH;
    char    *inputSoundFile = NULL, *outputPvocFile = NULL;
    int     i,ovlp = 4;		/* number of overlapping windows to have */
    int     err = 0;

    /* Check arguments */
    programName = argv[0];
    frameIncr = frameSize/ovlp;
    for(i=1; i<argc && err==0; ++i)
	{
	if(argv[i][0]=='-')
	    switch(argv[i][1])
		{
	    case 'n':
		if(i<argc-1)
		    {
		    frameSize = atoi(argv[++i]);
		    if(frameSize<MINFRMPTS || frameSize>MAXFRMPTS
		       || !IsPowerOfTwo(frameSize))
			{
			fprintf(stderr,"n must be power of 2, %d..%d\n",
				MINFRMPTS, MAXFRMPTS);
			err = 1;
			}
		    }
		else
		    err = 1;
		break;
	    case 'o':
		if(frameIncr == 0 && i<argc-1)
		    ovlp = atoi(argv[++i]);
		else
		    err = 1;
		break;
	    case 'i':
		if(ovlp == 0 && i<argc-1)
		    frameIncr = atoi(argv[++i]);
		else
		    err = 1;
		break;
	    default:
		fprintf(stderr,"unrecog option: %s \n",argv[i-1]);
		err = 1;
		break;
		}
	else{
	    if(inputSoundFile==NULL)
		inputSoundFile = argv[i];
	    else
		outputPvocFile = argv[i];
	    }
	}
    if(err || outputPvocFile==NULL)
	{
	fprintf(stderr,
	  "Usage: %s [-n n] [-o n | -i n] <inputSoundfile> <outputFFTfile>\n",
		programName);
	exit(1);
	}
    /* Read sound file */
    if (sfErr = SFRead(inputSoundFile, &inputSound))
	{
	printf("%s: sound error: %s on %s\n", programName,
	       SFErrMsg(sfErr), inputSoundFile);
	exit(1);
	}
    /* Abort if unsupported format */
    if(debug)
	printf("Read sound at %lx; magic = %lx, srate = %6.1f\n",
	       inputSound, inputSound->magic, inputSound->samplingRate);
    if (inputSound->format != SFMT_SHORT)
	{
	fprintf(stderr, "%s: unsupported sound format: %d\n",
		programName, inputSound->format);
	exit(1);
	}
    /* Convert to mono if required */
    if (inputSound->channels > 1)
	{
	fprintf(stderr,
	  "%s:  Warning: using only first channel of a multi-channel sound\n",
		programName);
	convertToMono(&inputSound);
	}

    /* Abort if not enough samples - just a convenience */
    if (inputSound->dataBsize / sizeof(short) < frameSize)
	{
	fprintf(stderr, "%s: too few samples: %d\n",
		programName, inputSound->dataBsize / sizeof(short));
	exit(1);
	}

    /* setup frame size etc according to sampling rate */
    if(frameSize==0)		/* not specified on command line */
	{
	int	    target;
	target = inputSound->samplingRate * (float)MINFRMMS / (float)1000;
	/* default frame size is > MINFRMMS milliseconds */
	frameSize = MAXFRMPTS;
	while( (frameSize>>1) >= target && frameSize > MINFRMPTS)
	    frameSize >>= 1;	/* divide down until just larger */
	}
    if (ovlp==0 && frameIncr==0)
	{
	ovlp = DFLT_OVLP;	/* default overlap */
	frameIncr = frameSize / ovlp;
	}
    else if(ovlp == 0)
	ovlp = frameSize/frameIncr;
    else
	frameIncr = frameSize/ovlp;

    if ( (ovlp < 2) || (ovlp > 16) )
	{
	fprintf(stderr, "%s: %d is a bad window overlap index\n",
		programName, ovlp);
	exit(1);
	}

    basis = AssignBasis(NULL,frameSize); /* set up FFT tables */
    CreatePVblock(inputSound, &outputPVH);
    takeFFTs(inputSound, outputPVH);
    /* Write pvoc file */
    if (err = PVWriteFile(outputPvocFile, outputPVH))
	{ fprintf(stderr, "%s: %s\n", programName, PVErrMsg(err));
	  exit(1);	}
    }

static void CreatePVblock(inputSound,ppPVH)
    SFSTRUCT	    *inputSound;
    PVSTRUCT	    **ppPVH;
    {
    long    inBSize  = inputSound->dataBsize;
    int     format   = PVFLOAT;
    float   rate     = (float)inputSound->samplingRate;
    int     chans    = (int)inputSound->channels;
    long    numFrames;
    int     frameWords;
    long    inSampls, outBSize;
    int     err;

    if(debug)
	printf("Framesiz %d, framInc %d\n",frameSize, frameIncr);
    inSampls  = inBSize/(chans*sizeof(short));
    numFrames = NumFrames( inSampls, frameSize, frameIncr);
    if(debug)
	printf("insampls = %ld, numFrames = %ld\n",inSampls,numFrames);
    /* If we dealt with frames that hit the ends properly, we'd have */
    /*	 (size/sizeof(short))/frameIncr frames (>= 1/2 inside file) */
    frameWords = frameSize + 2; /* each frame has Mag & phase for n/2 +1 */
    outBSize = numFrames * frameWords * sizeof(float);	/* in bytes! */
    printf("Output: %d chan%s, %ld frames, %d frsize, %d frInc, %ld bytes\n",
       chans, (chans==1)?"":"s", numFrames, frameSize, frameIncr, outBSize);
    /* Allocate a new PV block */
    if (err = PVAlloc(ppPVH, outBSize, format, rate, chans,
	      frameSize, frameIncr, 4))
    { fprintf(stderr, "%s: %s\n", programName, PVErrMsg(err));
      exit(1);	}
    }

/*
 * takeFFTs
 *  Go through the (mono) input sound frame by frame and find the
 *  magnitude and phase change for a string of FFT bins
 */
static void takeFFTs(inputSound,outputPVH)
    SFSTRUCT	    *inputSound;
    PVSTRUCT	    *outputPVH;
    {
    short   *inPtr;
    float   *outPtr;
    int     i, numFrames;
    float   *tmpBuf, *oldInPh, *winBuf;
    long    inSampls;
    float   sampRate = inputSound->samplingRate;

    tmpBuf  = MakeBuf(frameSize * 2);
    oldInPh = MakeBuf(frameSize);
    winBuf  = MakeHalfWin(frameSize,(float)1.0,1);

    inPtr  = (short *) ((char *) inputSound + inputSound->headBsize);
    outPtr = (float *) ((char *) outputPVH  + outputPVH->headBsize);

    inSampls  = inputSound->dataBsize/(inputSound->channels*sizeof(short));
    numFrames = NumFrames(inSampls, frameSize, frameIncr);

/*    printf("%d frames to do (Fsize = %d, increment = %d)\n",
	numFrames, frameSize, frameIncr); */
    printf("frame: ");
    for (i = 0; i < numFrames; i++)
	{
	/* if(i > 680 && i < 700 )
	    debug = 1;	*/
	if((i%20)==0)   printf("%d ", i); fflush(stdout);
	FloatAndCopy(inPtr,tmpBuf,frameSize);
	/* PrintBuf(tmpBuf, frameSize, "floated"); /* */
	ApplyHalfWin(tmpBuf,winBuf,frameSize);
	/* PrintBuf(tmpBuf, frameSize, "windo'd"); /* */
	inPtr += frameIncr;
	UnpackReals(tmpBuf, frameSize);
	/* PrintBuf(tmpBuf, frameSize, "unpacked"); /* */
	FFT2real((complex *)tmpBuf, frameSize, 1,basis);
	/* PrintBuf(tmpBuf, frameSize, "fft'd"); /* */
	Rect2Polar(tmpBuf, frameSize);
	/* PrintBuf(tmpBuf, frameSize, "toPolar"); /* */
	UnwrapPhase(tmpBuf, frameSize, oldInPh);
	/* PrintBuf(tmpBuf, frameSize, "unWrapped"); /* */
	PhaseToFrq(tmpBuf, frameSize, (float)frameIncr, (float)sampRate);
	/* PrintBuf(tmpBuf, frameSize, "toFrq"); /* */
	WriteOut(tmpBuf, &outPtr, frameSize);
	/* store in output in some format, updates pointer itself */
	/* debug = 0;	/* */
	}
    printf("\n");
    }

#define DBGPTS 8
static void PrintBuf(buf,size,msg)
    float *buf;
    int   size;
    char  *msg;
    {
    int   i;

    if(!debug)
    return;
    size = DBGPTS;
    printf("%s:",msg);
    for(i=0; i<size; ++i)
    printf("%7.2f ",buf[i]);
    printf("\n");
    }

/* Convert multi-channel sound to mono by tossing extra channels */
static 	void	convertToMono(theSound)
    SFSTRUCT  **theSound;
    {
    SFSTRUCT *newSound;
    short   *oldPtr, *newPtr;
    int     size     = (*theSound)->dataBsize;
    int     format   = (*theSound)->format;
    int     rate     = (*theSound)->samplingRate;
    int     channels = (*theSound)->channels;
    int     newSize, i, j;

    newSize = size/channels;
    if (sfErr = SFAlloc(&newSound, newSize, format, rate, 1, 4))
	{
	printf("%s: sound error: %s\n", programName,
		    SFErrMsg(sfErr));
	exit(1);
	}
    oldPtr  = (short *)SFDataLoc(*theSound);
    newPtr  = (short *)SFDataLoc(newSound);
    for (i = 0, j = 0; i < newSize/sizeof(short); i++, j+=channels)
	newPtr[i] = oldPtr[j];
    SFFree(*theSound);
    *theSound = newSound;
    }

