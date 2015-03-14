/***************************************************************\
*   sndf.c		    					*
*   implementation of for sound file interface			*
*   just writes files as images of memory block			*
*   'inspired' by the NeXT SND system				*
*   10aug90 dpwe						*
\***************************************************************/

#include <stdio.h>	    /* ANSI file ops */
#include <string.h>	    /* for buildsfname only */
#include <sndf.h>

#ifdef THINK_C
#include <stdlib.h>	    /* malloc stuff */
#define READMODE "rb"
#define WRITEMODE "wb+"     /* "+" allows infoBsize reread */
#else
#include <sys/types.h>
#include <malloc.h>	    /* malloc stuff */
#define READMODE "r"
#define WRITEMODE "w+"	    /* "+" allows infoBsize reread */
#endif

/* static variables */

static int	debug = 1;
static SFSTRUCT tmphdr;     /* scratch space for pre-load */
static int	SFerror;    /* internal error flag */
static char	unspecMsg[] = "Unspecified error : _______";
/* want to 'fill in' at    012345678901234567890... [20..27] */

#ifdef __STDC__
/* static function prototypes */
static void itoa(int n, char *s);   /* for unrecognised error code */

/* more static function prototypes - these fn's in personality #incl */
static void AddFname(char *fname, FILE *file);	/* filename table */
static void RmvFname(FILE *file);	/* implemented if needed */
static int  SFReadHdr(FILE *file, SFSTRUCT *snd, int infoBmax);
static int  SFWriteHdr(FILE *file, SFSTRUCT *snd, int infoBmax);
static long SFHeDaLen(FILE *file);  /* return space used for fi hdr */
#else
/* static function prototypes */
static void itoa();   /* for unrecognised error code */

/* more static function prototypes - these fn's in personality #incl */
static void AddFname();	/* filename table */
static void RmvFname();	/* implemented if needed */
static int  SFReadHdr();
static int  SFWriteHdr();
static long SFHeDaLen();  /* return space used for fi hdr */
#endif

/* ----------------------- implementations ------------------------ */

#ifdef THINK_C
#include "sndfmac.c"	    /* mac versions of header r/w &c */
#else
/* #include "sndfnat.c"	    /* native (direct memory image) headers */
#include "sndfirc.c"	    /* standard IRCAM headers */
#endif

int SFInfoSize(snd)
    SFSTRUCT *snd;	/* return number of info bytes in header */
    {
    return(snd->headBsize - sizeof(SFSTRUCT) + SFDFLTBYTS);
    }

void *SFDataLoc(snd)
    SFSTRUCT *snd;	/* return pointer to data from header */
    {
    return( (void *)((char *)snd + snd->headBsize) );
    }

int  SFAlloc(psnd, dataBsize, format, srate, chans, infoBsize)
    SFSTRUCT **psnd;
    long dataBsize;
    int format;
    FLOATARG srate;
    int chans;
    int infoBsize;
/*  Allocate memory for a new SFSTRUCT+data block;fill in header
    Returns SFE_MALLOC (& **psnd = NULL) if malloc fails    */
    {
    long    bSize;
    long    infoXtra;

    if(infoBsize < SFDFLTBYTS)	infoBsize = SFDFLTBYTS;
    infoBsize = (infoBsize + 3)&-4; /* round up to 4 byte multiple */
    infoXtra = (long)(infoBsize-SFDFLTBYTS);
    bSize = dataBsize + sizeof(SFSTRUCT) + infoXtra;
    if( ( (*psnd) = (SFSTRUCT *)malloc((size_t)bSize)) == NULL)
	return(SFerror = SFE_MALLOC);
    (*psnd)->magic = SFMAGIC;
    (*psnd)->headBsize = sizeof(SFSTRUCT) + infoXtra;
    (*psnd)->dataBsize = dataBsize;
    (*psnd)->format    = format;
    (*psnd)->samplingRate = srate;
    (*psnd)->channels  = chans;
    /* leave info bytes undefined */
    return(SFerror = SFE_OK);
    }

void SFFree(snd)
    SFSTRUCT *snd;	/* destroy in-memory sound */
    {
    free(snd);
    }

FILE *SFOpenRdHdr(path, snd, infoBmax)
    char *path;
    SFSTRUCT *snd;
    int infoBmax;
/*  Opens named file as a stream, reads & checks header, returns stream
    Attempts to read up to infoBmax info bytes, at least SFDFLTBYTS.
    No error codes; returns NULL on fail (SFE_NOPEN, _NSF, _RDERR) */
    {
    FILE    *file;
    int     i;
    long    headBsize, dataBsize, count;
    int     format = 0;
    int     chans  = 0;
    float   srate  = 0.0;
    size_t  num;

    if( (file = fopen(path, READMODE)) == NULL)
	{
	SFerror = SFE_NOPEN;
	return(NULL);
	}
    AddFname(path,file);	/* create filename table entry */
    SFerror = SFReadHdr(file, snd, infoBmax);
    if(SFerror == SFE_OK)
	return(file);
    else
	{
	fclose(file);
	RmvFname(file);		/* remove filename table entry */
	return(NULL);
	}
    }

FILE *SFOpenAllocRdHdr(path, psnd)
    char *path;
    SFSTRUCT **psnd;
     /* find sizeof header, alloc space for it, read it in.
        returns NULL on fail (SFE_NOPEN, _NSF, _RDERR) */
    {
    FILE	*retFP;
    int 	i;
    long	headBsz;

    retFP = SFOpenRdHdr(path, &tmphdr, SFDFLTBYTS);
    if(retFP != NULL)
	{
	headBsz = tmphdr.headBsize;
	*psnd = (SFSTRUCT *)malloc(headBsz);
	if(*psnd != NULL)	/* ..else ukky malloc fail */
	    {
	    for(i = 0; i<sizeof(SFSTRUCT); ++i)	/* bytewise copy */
		((char *)*psnd)[i] = ((char *)&tmphdr)[i];
	    SFRdXInfo(retFP, *psnd, SFDFLTBYTS); 
	    }
	}
    return retFP;
    }

FILE *SFOpenWrHdr(path, snd)
    char *path;
    SFSTRUCT *snd;
/*  Creates file in path & writes out header, returns stream for access
    No error codes, just returns NULL on fail (SFE_NSF, _NOPEN, _WRERR)  */
    {
    FILE    *file;
    int     err = SFE_OK;

    if(snd->magic != SFMAGIC)
	{
	SFerror = SFE_NSF;
	return(NULL);
	}
    if( (file = fopen(path, WRITEMODE)) == NULL)
	{
	SFerror = SFE_NOPEN;
	return(NULL);	
	}
    AddFname(path,file);	/* create a filename table entry */
    if(SFWriteHdr(file,snd,0))
	{
	fclose(file);
	RmvFname(file);		/* remove filename table entry */
	SFerror = SFE_WRERR;
	return(NULL);	
	}
    else
	{
	SFerror = SFE_OK;
	return(file);
	}
    }

void SFCloseWrHdr(file,snd)
    FILE *file;
    SFSTRUCT *snd;
/*  After opening via SFOpenWrHdr and writing sound to the stream, may
    need to update header with a postieri data (sound length, more info).
    SFCloseWrHdr works 3 ways:
	- snd is NULL	    look at file ptr, rewrite length to hdr
	- snd exists, but dataBlen is 0
			    fill in new dataBlen from file ptr, write hdr
	- snd exists including dataBlen
			    rewrite whole header
    Ignores any fails & just closes file as best it can.    */	
    {
    long len;

    len = ftell(file);
    len -= SFHeDaLen(file);    /* knock off written hdr */
    if(snd == NULL)
	{
	SFerror = SFReadHdr(file, &tmphdr, SFDFLTBYTS); /* read original header */
	tmphdr.dataBsize = len;			/* fixup data length */
	/* rewrite modified header, but only as many infobytes as we read */
	if(SFerror == SFE_OK)	/* only write back if we read goo header */
	    SFerror = SFWriteHdr(file, &tmphdr, SFDFLTBYTS);
	}
    else{	/* we were passed a header to rewrite */
	if(snd->dataBsize == 0)     /* .. but does it have a length ? */
	    snd->dataBsize = len;   /* fill in if not */
	SFerror = SFWriteHdr(file, snd, 0); /* write out whole lot (incl. all info) */
	}
    fclose(file);
    RmvFname(file);	/* remove filename table entry */
    }

int SFRead(path,psnd)
    char *path;
    SFSTRUCT **psnd;
/* Opens & checks file specified in path.  Allocs for & reads whole sound.
   Poss errors: SFE_NOPEN, SFE_NSF, SFE_MALLOC, SFE_RDERR */
    {
    FILE    *file;
    int     i, err = SFE_OK;
    int     xinfo;
    int     format = 0;
    int     chans  = 0;
    float   srate  = 0.0;
    size_t  num;

    if( (file = SFOpenRdHdr(path, &tmphdr, SFDFLTBYTS)) == NULL)
	return(SFerror);
    xinfo = tmphdr.headBsize - sizeof(SFSTRUCT);
    err = SFAlloc(psnd, tmphdr.dataBsize, format, srate, chans,
		    xinfo + SFDFLTBYTS);
    if(err == SFE_OK)
	{
	/* copy over what we already read */
	for(i = 0; i < sizeof(SFSTRUCT)/2; ++i)
	    ((short *)*psnd)[i] = ((short *)&tmphdr)[i];
	/* grab remaining info chars (may be 'elsewhere') */
	if( xinfo > 0 )
	    SFRdXInfo(file, *psnd, SFDFLTBYTS);
	/* read all the data */
	if( (num = fread(SFDataLoc(*psnd),
		(size_t)1, (size_t)(*psnd)->dataBsize, file))
			 < (*psnd)->dataBsize )
	    {
	    fprintf(stderr,"SFRead: wanted %ld got %ld\n", num,
				(*psnd)->dataBsize);	
	    err = SFE_RDERR;
	    }
	}
    if( (err != SFE_OK) && (*psnd != NULL))
	{
	SFFree(*psnd);
	*psnd = NULL;
	}
    fclose(file);
    RmvFname(file);	/* remove filename table entry */
    return(SFerror = err);
    }

int SFWrite(path,snd)
    char *path;
    SFSTRUCT *snd;
/* Write out the soundfile to the file named in path.
   Poss errors: SFE_NSF, SFE_NOPEN, SFE_WRERR */
    {
    FILE    *file;
    int     err = SFE_OK;
    long    bSize;

    if(snd->magic != SFMAGIC)
	return(SFerror = SFE_NSF);
    if( (file = SFOpenWrHdr(path, snd)) == NULL)
	return(SFerror);
    bSize = snd->dataBsize;
    if( fwrite(SFDataLoc(snd), (size_t)1, (size_t)bSize, file)
	 < (size_t)bSize )
	err = SFE_WRERR;
    fclose(file);
    RmvFname(file);	/* remove filename table entry */
    return(SFerror = err);
    }

char *SFErrMsg(err)
    int err;     /* convert error code to message */
    {
    switch(err)
	{
    case SFE_OK:	return("No SF error");
    case SFE_NOPEN:	return("Cannot open sound file");
    case SFE_NSF:	return("Object/file not sound");
    case SFE_MALLOC:	return("No memory for sound");
    case SFE_RDERR:	return("Error reading sound file");
    case SFE_WRERR:	return("Error writing sound file");
    default:
	itoa(err, &unspecMsg[20]);
	return(unspecMsg);
	}
    }

/* itoa from K&R p59 */
static void itoa(n,s)
    int n;
    char *s;
    {
    int     i,j,sign;
    char    c;

    if( (sign = n) < 0)     n = -n;
    i = 0;
    do	{
	s[i++] = n%10 + '0';
	} while( (n /= 10) > 0);
    if(sign < 0)
	s[i++] = '-';
    s[i--] = '\0';
    for(j=0; j<i; ++j,--i)	/* reverse the string */
	{ c = s[i]; s[i] = s[j]; s[j] = c; }
    }

/* buildsfname stolen from csound */

#define MAXFULNAME 128

char *buildsfname(name)			/*  build a soundfile name	*/
    register char *name;       		/*  <mod of carl getsfname()>	*/
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

    if (*name == '/' || *name == '.') /* if path already given */
	return(strcpy(fullname,name)); /*	copy as is	*/
    else {			/* else build right here  */
	if ((sfdir = getenv("SFDIR")) != NULL)
	    {
	    sprintf(fullname,"%s/%s",sfdir,name);
	    return(fullname);
	    }
	else
	    {
	    /* die("buildsfname: environment variable SFDIR undefined"); */
	    strcpy(fullname,name);
	    return(fullname);
	    }
	}
#endif
    }
