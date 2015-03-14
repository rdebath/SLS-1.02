/***************************************************************\
*	pvoc.c							*
*	file in/out functions for pvoc FFT files		*
*	'inspired' by the NeXT SND system			*
*	01aug90 dpwe						*
\***************************************************************/

#include <stdio.h>
#ifdef THINK_C
#define SYS5
#include <stdlib.h>
#define READMODE "rb"
#define WRITEMODE "wb"
#else
#include <sys/types.h>
#include <malloc.h>
#define READMODE "r"
#define WRITEMODE "w"
#endif

#ifdef SYS5
#define  index(A,B) strchr(A,B)
#include <fcntl.h>
#include <string.h>
#else
#include <strings.h>
#endif

#include "pvoc.h"

/* static function prototypes */
#ifdef __STDC__
static int  pmy_strlen(char *s);
static void pmy_reverse(char *s);
static void pmy_itoa(int n, char *s);
#else
static int  pmy_strlen();
static void pmy_reverse();
static void pmy_itoa();
#endif

/* static variables */
static PVSTRUCT tmphdr;		/* scratch space for pre-load */
static char unspecMsg[] = "Unspecified error : _______";
/* want to 'fill in' at    012345678901234567890... [20..27] */

int PVReadHeader(fil, phdr)
    FILE *fil;
    PVSTRUCT *phdr;
    /* read just the header from a candidate pvoc file */
    /* returns PVE_RDERR if read fails (or file too small)
            or PVE_NPV   if magic number doesn't fit
	    or PVE_OK     otherwise                     */
    {
    size_t	num;

    phdr->magic = 0L;
    if( (num = fread((void *)phdr, (size_t)1, (size_t)sizeof(PVSTRUCT), fil))
    				 < (size_t)sizeof(PVSTRUCT))
    	{
    	fprintf(stderr, "PVRdH: wanted %ld got %ld\n", 
    			(size_t)sizeof(PVSTRUCT), num);
		return(PVE_RDERR);
		}
    if(phdr->magic != PVMAGIC)
		return(PVE_NPV);
    return(PVE_OK);
    }

FILE *PVOpenAllocRdHdr(path, phdr)
     char *path;
     PVSTRUCT **phdr;
    /* read all of candidate header - variable length header */
    {
    FILE	*pvf;
    long	hSize, rem;
    int 	err = 0;

    if( (pvf = fopen(path,"r"))!= NULL)
	{
	if( PVReadHeader(pvf, &tmphdr) == PVE_OK )
	    {
	    hSize = tmphdr.headBsize;
	    *phdr = (PVSTRUCT *)malloc((size_t)hSize);
	    if( (*phdr)!=NULL)
		{
		**phdr = tmphdr;	/* copies elements of struct ?? */
		rem = hSize - sizeof(PVSTRUCT);
		if(rem > 0)
		    fread((void *)((*phdr)+1),(size_t)1,(size_t)rem,pvf);
		}
	    else
		err = 1;	/* malloc error */
	    }
	else
	    err = 1;		/* header read error - e.g. not pv file */
	}
    if(err)
	{
	fclose(pvf);
	pvf = NULL;
	}
    return(pvf);
    }

int PVReadFile(filename, pphdr)
    char *filename;
    PVSTRUCT **pphdr;
    /* read in the header and all the data */
    /* returns PVE_NOPEN   if can't open
               PVE_NPV     if not a PVOC file
	       PVE_RDERR   if reads fail
	       PVE_MALLOC  if can't get memory
	       PVE_OK      otherwise	*/
    {
    FILE	*fil;
    int 	i, err = PVE_OK;
    long	headBsize, dataBsize, count;
    int 	infoBsize;
    int 	format = 0;
    int 	chans  = 0;
    int 	frSize = 0;
    int 	frIncr = 0;
    float	srate  = 0.0;
    size_t	num;

	if( (fil = fopen(filename, READMODE)) == NULL)
		return(PVE_NOPEN);
    *pphdr = NULL;
    err = PVReadHeader(fil, &tmphdr);
    if(err == PVE_OK)
		{
		headBsize = tmphdr.headBsize;
		infoBsize = headBsize - sizeof(PVSTRUCT) + PVDFLTBYTS;
		if(infoBsize < 0)
		    err = PVE_NPV;
		}
    if(err == PVE_OK)
		{
		dataBsize = tmphdr.dataBsize;
		err = PVAlloc(pphdr, dataBsize, format, srate, chans,
			      frSize, frIncr, infoBsize);
		}
    if(err == PVE_OK)
		{
		/* copy over what we already read */
		for(i = 0; i < sizeof(PVSTRUCT)/2; ++i)
		    ((short *)*pphdr)[i] = ((short *)&tmphdr)[i];
		/* figure how many bytes expected left */
		count = dataBsize + infoBsize - PVDFLTBYTS;
		if( (num = fread( (void *)((*pphdr)+1), 
				(size_t)1, (size_t)count, fil)) < (size_t)count )
			{
			fprintf(stderr,"PVRead: wanted %ld got %ld\n", num, count);	
		    err = PVE_RDERR;
		    }
		}
    if( (err != PVE_OK) && (*pphdr != NULL))
		{
		PVFree(*pphdr);
		*pphdr = NULL;
		}
    fclose(fil);
    return(err);
    }

int PVWriteFile(filename,phdr)
    char *filename;
    PVSTRUCT *phdr;
    /* write out a PVOC block in memory to a file 
       returns PV_NOPEN  if can't open file
               PV_NPV    if *phdr isn't magic
               PV_WRERR  if write fails  */
    {
    FILE	*fil;
    int 	err = PVE_OK;
    long	bSize;

    if(phdr->magic != PVMAGIC)
	return(PVE_NPV);
    if( (fil = fopen(filename, WRITEMODE)) == NULL)
		return(PVE_NOPEN);
    bSize = phdr->dataBsize + phdr->headBsize;
    if( fwrite((void *)phdr, (size_t)1, (size_t)bSize, fil) < (size_t)bSize )
		err = PVE_WRERR;
    fclose(fil);
    return(err);
    }

int PVAlloc(pphdr, dataBsize, format, srate, chans, frSize, frIncr, infoBsize)
    PVSTRUCT **pphdr;
    long dataBsize;
    int format;
    FLOATARG srate;
    int chans;
    int frSize;
    int frIncr;
    int infoBsize;
    /* Allocate memory for a new PVSTRUCT+data block;
       fill in header according to passed in data.
       Returns PVE_MALLOC  (& **pphdr = NULL) if malloc fails
               PVE_OK      otherwise  */
    {
    long	bSize;

    bSize = dataBsize + sizeof(PVSTRUCT) + infoBsize - PVDFLTBYTS;
    if( ( (*pphdr) = (PVSTRUCT *)malloc((size_t)bSize)) == NULL )
		return(PVE_MALLOC);
    (*pphdr)->magic = PVMAGIC;
    (*pphdr)->headBsize = sizeof(PVSTRUCT) + infoBsize - PVDFLTBYTS;
    (*pphdr)->dataBsize = dataBsize;
    (*pphdr)->format    = format;
    (*pphdr)->samplingRate = srate;
    (*pphdr)->channels  = chans;
    (*pphdr)->frameSize = frSize;
    (*pphdr)->frameIncr = frIncr;
    /* leave info bytes undefined */
    return(PVE_OK);
    }

void PVFree(phdr)
    PVSTRUCT *phdr;
    /* release a PVOC block */
    {
    free(phdr);		/* let operating system sort it out */
    }

char *PVErrMsg(err)
    int err;		/* return string for error code */
    {
    switch(err)
	{
    case PVE_OK: 	return("No PV error");
    case PVE_NOPEN:	return("Cannot open PV file");
    case PVE_NPV:	return("Object/file not PVOC");
    case PVE_MALLOC:	return("No memory for PVOC");
    case PVE_RDERR:	return("Error reading PVOC file");
    case PVE_WRERR:	return("Error writing PVOC file");
    default:
	pmy_itoa(err, &unspecMsg[20]);
	return(unspecMsg);
	}
    }

/* itoa from K&R p59 */
static int  pmy_strlen(s)
    char *s;
    {
    int 	i = 0;
    while(s[i] != '\0')
		++i;
    return(--i);
    }

static void pmy_reverse(s)
    char *s;
    {
    int 	i,j;
    char	c;

    for(i = 0, j = pmy_strlen(s); i<j; i++, j--)
		{
		c = s[i];	s[i] = s[j];	s[j] = c;
		}
    }

static void pmy_itoa(n,s)
    int n;
    char *s;
    {
    int 	i, sign;

    if( (sign = n) < 0) 	n = -n;
    i = 0;
    do  {
	s[i++] = n%10 + '0';
	} while( (n /= 10) > 0);
    if(sign < 0)
	s[i++] = '-';
    s[i] = '\0';
    pmy_reverse(s);
    }


