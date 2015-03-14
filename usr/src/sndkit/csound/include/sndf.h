/***************************************************************\
*   sndf.h							*
*   header defs for sound files					*
*   'inspired' by the NeXT SND system				*
*   10aug90 dpwe						*
\***************************************************************/

#include <stdio.h>

#define SFDFLTBYTS 4

typedef struct sfstruct
    {
    long    magic;	    /* magic number to identify */
    long    headBsize;	    /* byte offset from start to data */
    long    dataBsize;	    /* number of bytes of data */
    int     format;	    /* format specifier */
    int     channels;	    /* mono/stereo etc  */
    float   samplingRate;   /* .. of the data   */
    char    info[SFDFLTBYTS];	/* extendable byte area */
    } SFSTRUCT;


/***********************/
/* Codes for structure */
/***********************/

#define SFMAGIC 107365L

#define SFMT_CHAR  sizeof(char)
#define SFMT_ALAW  64+sizeof(char)
#define SFMT_ULAW  128+sizeof(char)
#define SFMT_SHORT sizeof(short)
#define SFMT_LONG  sizeof(long)
#define SFMT_FLOAT 32+sizeof(float)  /* NOT backward compatible */
#define SFMT_DOUBLE  32+sizeof(double)

/* Error codes returned by PVOC file functions */
#define SFE_OK	    0	/* no error*/
#define SFE_NOPEN   -1	/* couldn't open file */
#define SFE_NSF     -2	/* not a sound file */
#define SFE_MALLOC  -3	/* couldn't allocate memory */
#define SFE_RDERR   -4	/* read error */
#define SFE_WRERR   -5	/* write error */

/*** yukky ANSI bodge (please tell me the right way!) ****/
#ifndef FLOATARG
#ifdef __STDC__
#define FLOATARG double		/* you can't have floats in prototypes! */
#else
#define FLOATARG double
#endif /* def __STDC__ */
#endif /* ndef FLOATARG */

/****************************************/
/**** Function prototypes for export ****/
/****************************************/

#ifdef __STDC__

int  SFInfoSize(SFSTRUCT *snd);
     /* Return number of info bytes in header */

void *SFDataLoc(SFSTRUCT *snd); 
     /* Return pointer to data from header */

int  SFAlloc(SFSTRUCT **psnd, long dataBsize, int format, FLOATARG srate,
	     int chans, int infoBsize);
     /* Allocate memory for a new SFSTRUCT & data block; fill in header
        Returns SFE_MALLOC (& **psnd = NULL) if malloc fails    */

void SFFree(SFSTRUCT *snd);	
     /* destroy in-memory sound */

FILE *SFOpenRdHdr(char *path, SFSTRUCT *snd, int infoBmax);
     /* Opens named file as a stream, reads & checks header, returns stream
        Attempts to read up to infoBmax info bytes, at least 4.
        No error codes; returns NULL on fail (SFE_NOPEN, _NSF, _RDERR) */

int  SFRdXInfo(FILE *file, SFSTRUCT *snd, int done);
     /* fetch any extra info bytes needed in view of newly-read header
        Passed open file handle, snd includes destination for new info,
        done = infobytes already read, other info read from snd */

FILE *SFOpenAllocRdHdr(char *path, SFSTRUCT **psnd);
     /* find sizeof header, alloc space for it, read it in.
        returns NULL on fail (SFE_NOPEN, _NSF, _RDERR) */

FILE *SFOpenWrHdr(char *path, SFSTRUCT *snd);
     /*  Creates file in path & writes out header, returns stream for access
         No error codes, just returns NULL on fail 
         (SFE_NSF, _NOPEN, _WRERR)	*/

void SFCloseWrHdr(FILE *file, SFSTRUCT *snd);			
     /* After opening via SFOpenWrHdr and writing sound to the stream, may
        need to update header with a postieri data (sound length, more info).
        SFCloseWrHdr works 3 ways:
	- snd is NULL	 ->   look at file ptr, rewrite length to hdr
	- snd exists, but dataBlen is 0
			 ->   fill in new dataBlen from file ptr, write hdr
	- snd exists including dataBlen
			 ->   rewrite whole header
        Ignores any fails & just closes file as best it can.    */	

int  SFRead(char *path, SFSTRUCT **psnd);
     /* Opens & checks file specified in path.  Allocs for & reads whole sound.
        Poss errors: SFE_NOPEN, SFE_NSF, SFE_MALLOC, SFE_RDERR */

int  SFWrite(char *path, SFSTRUCT *snd);
     /* Write out the soundfile to the file named in path.
        Poss errors: SFE_NSF, SFE_NOPEN, SFE_WRERR */

char *SFErrMsg(int err);	
     /* convert error code to message */

char *buildsfname(char *name);		/*  build a soundfile name (SFDIR) */

#else /* no argument prototypes */

int  SFInfoSize();
void *SFDataLoc(); 
int  SFAlloc();
void SFFree();	
FILE *SFOpenRdHdr();
FILE *SFOpenAllocRdHdr();
int  SFRdXInfo();
FILE *SFOpenWrHdr();
void SFCloseWrHdr();			
int  SFRead();
int  SFWrite();
char *SFErrMsg();	
char *buildsfname();

#endif /* def __STDC__ */

