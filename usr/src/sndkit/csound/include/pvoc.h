/***************************************************************\
*	pvoc.h							*
*	header defs for pvoc FFT files				*
*	'inspired' by the NeXT SND system			*
*	01aug90 dpwe						*
\***************************************************************/

#ifndef NULL
#define NULL 0L
#endif

#define PVMAGIC 51773	/* look at it upside-down, esp on a 7-seg display */
#define PVSHORT 2	/* for .format .. 16 bit linear data */
#define PVFLOAT 4       /* for .format .. 32 bit float data */

#define PVDFLTBYTS 4

typedef struct pvstruct
    {
    long	magic;			/* magic number to identify */
    long	headBsize;		/* byte offset from start to data */
    long	dataBsize;		/* number of bytes of data */
    int 	format;			/* format specifier */
    float	samplingRate;	/* .. of the source sample */
    int 	channels;		/* mono/stereo etc */
    int 	frameSize;		/* size of FFT frames (2^n) */
    int 	frameIncr;		/* # new samples each frame */
    char	info[PVDFLTBYTS];	/* extendable byte area */
    } PVSTRUCT;

/* Error codes returned by PVOC file functions */
#define PVE_OK  	0	/* no error*/
#define PVE_NOPEN       -1	/* couldn't open file */
#define PVE_NPV  	-2	/* not a PVOC file */
#define PVE_MALLOC	-3	/* couldn't allocate memory */
#define PVE_RDERR	-4	/* read error */
#define PVE_WRERR	-5	/* write error */

/*** yukky ansi bodge ***/
#ifndef FLOATARG
#ifdef __STDC__
#define FLOATARG double		/* you can't have floats in prototypes! */
#else
#define FLOATARG double
#endif /* def __STDC__ */
#endif /* ndef FLOATARG */

/********************************/
/* exported function prototypes */
/********************************/

#ifdef __STDC__
int   PVReadHeader(FILE *fil, PVSTRUCT *phdr);       /* pass in PVH */
FILE *PVOpenAllocRdHdr(char *path, PVSTRUCT **phdr); /* allocs PVH */
int   PVReadFile(char *filename, PVSTRUCT **phdr);   /* allocates new PVH+ */
int   PVWriteFile(char *filename, PVSTRUCT *phdr);   /* write out PVH+  */
int   PVAlloc(PVSTRUCT **pphdr, long dataBsize, int format, FLOATARG srate,
	    int chans, int frSize, int frIncr, int infoBsize);
						     /* create new PVH+ */
void  PVFree(PVSTRUCT *phdr);			     /* release PVH +   */
char *PVErrMsg(int err);		/* return string for error code */

#else /* no prototype arguments */
int   PVReadHeader();        /* pass in PVH */
FILE *PVOpenAllocRdHdr();    /* allocs PVH */
int   PVReadFile();          /* allocates new PVH+ */
int   PVWriteFile();         /* write out PVH+  */
int   PVAlloc();	     /* create new PVH+ */
void  PVFree();		     /* release PVH +   */
char *PVErrMsg();	     /* return string for error code */
#endif


