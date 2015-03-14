/***************************************************************\
*   sndfirc.c			IRCAM				*
*   Header functions for SFIRCAM header version of sndf.c	*
*   THIS FILE IS CONDITIONALLY INCLUDED IN sndf.c		*
*   24sep90 after12aug90 dpwe					*
\***************************************************************/

#ifdef THINK_C
#include <unix.h>
#else
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
#include <fcntl.h>

/* from <sfheader.h> .. */

# define SIZEOF_HEADER 1024
# define SF_MAGIC 107364L
# define SF_CHAR  sizeof(char)    /* new sfclass, not SFIRCAM standard */
# define SF_ALAW  sizeof(char)    /* new sfclass, not SFIRCAM standard */
# define SF_ULAW  sizeof(char)    /* new sfclass, not SFIRCAM standard */
# define SF_SHORT sizeof(short)
# define SF_LONG  sizeof(long)
# define SF_FLOAT sizeof(float)
# define SF_BUFSIZE	(16*1024) /* used only in play */

typedef union sfheader {
	struct {
		long	  sf_magic;
		float	  sf_srate;
		long	  sf_chans;
		long	  sf_packmode;
		char	  sf_codes;
	} sfinfo;
	char	filler[SIZEOF_HEADER];
} SFHEADER;


static SFHEADER *psfh = NULL;	    /* just to grab some static space */

/* private type declarations */
typedef struct fnameinfo
    {
    struct fnameinfo *next;
    FILE   *fhan;
    char   fname[80];
    } FNAMEINFO;	/* used to build a map between FILE * & filenames */

/* static variables */
FNAMEINFO   *fniroot = NULL;	/* root of fnameinfo list */

/* static function prototypes */
#ifdef __STDC__
static char *FindFname(FILE *file);
    /* looks up file ptr, returns mapped name if any */
static size_t GetFileSize(FILE *file);	/* return number of bytes in a file */
static size_t fdSize(int fd);
#else
static char *FindFname();
    /* looks up file ptr, returns mapped name if any */
static size_t GetFileSize();	/* return number of bytes in a file */
static size_t fdSize();
#endif

/* ----------------------- implementations ------------------------ */

static void AddFname(fname,file)
    char *fname;
    FILE *file;
    /* associates name 'fname\0' with file ptr 'file' in our lookup table */
    {
    FNAMEINFO *runner = fniroot;

    /* see if FILE is already in our table.. */
    while(runner != NULL && runner->fhan != file)
	runner = runner->next;
    if(runner == NULL)	    /* not found : add new record */
	{
	runner = (FNAMEINFO *)malloc((size_t)sizeof(FNAMEINFO));
	runner->fhan = file;
	runner->next = fniroot;
	fniroot = runner;	/* add to top of list, faster to find */
	}   /* either way, copy in the (new) name */
    strcpy((char *)runner->fname, fname);
    }

static void RmvFname(file)
    FILE *file;
    /* removes any fnameinfo record associated with 'file' */
    {
    FNAMEINFO *node;
    FNAMEINFO **prunner = &fniroot;

    while((*prunner) != NULL && (*prunner)->fhan != file)
	prunner = &(*prunner)->next;
    if((*prunner) != NULL)	/* found : delete record */
	{
	node = (*prunner);
	*prunner = (*prunner)->next;	/* close gap.. */
	free(node);			/* & dispose of memory */
	}
    /* else not found - do nothing */
    }

static char *FindFname(file)
    FILE *file;
    /* looks up file ptr, returns mapped name if any */
    {
    FNAMEINFO *runner = fniroot;

    while(runner != NULL && runner->fhan != file)
	runner = runner->next;
    if(runner != NULL)
	return( (char *)runner->fname );
    else
	return( NULL );     /* fileptr not in our list */
    }

static int SFReadHdr(file, snd, infoBmax)
    FILE *file;
    SFSTRUCT *snd;
    int infoBmax;
/* utility to sample the header of an opened file, at most infoBmax info bytes */
    {
    long    infoBsize, infoXtra, bufXtra;
    long    rdsize;
    size_t  num;
    char    *irc_infp, *sdf_infp;
    int     chans,formt,i;
    float   srate;

    if(psfh == NULL)		    /* check our personal IRCAM sfheader buf */
	if( (psfh = (SFHEADER *)malloc((size_t)sizeof(SFHEADER)))==NULL)
	    return(SFE_MALLOC);     /* couldn't alloc our static buffer */

    if(infoBmax < SFDFLTBYTS)	infoBmax = SFDFLTBYTS;
    infoBmax = (infoBmax)&-4;	/* round DOWN to x4 (can't exceed given bfr) */
    bufXtra = (long)(infoBmax-SFDFLTBYTS);

    psfh->sfinfo.sf_magic = 0L;			/* can test for valid read.. */
    if(fseek(file, (long)0, SEEK_SET) != 0 )	/* move to start of file */
	return(SFerror = SFE_RDERR);
    rdsize = sizeof(SFHEADER);
    /* fill up our buffer */
    num = fread((void *)psfh, (size_t)1, (size_t)rdsize, file);
    if(num != rdsize)	    /* couldn't get that many bytes */
	return(SFerror = SFE_RDERR);		/* .. call it a read error */

    if(psfh->sfinfo.sf_magic != SF_MAGIC) /* compare with IRCAM magic number */
	return(SFerror = SFE_NSF);	  /* got the bytes, but look wrong */
    /* Else must be ok.  Extract rest of sfheader info */
    srate = psfh->sfinfo.sf_srate;
    chans = psfh->sfinfo.sf_chans;
    formt = psfh->sfinfo.sf_packmode;
    switch(formt)
	{
    case SF_CHAR:   formt = SFMT_CHAR;	break;
/*  case SF_ALAW:   formt = SFMT_ALAW;	break;
    case SF_ULAW:   formt = SFMT_ULAW;	break;	*/
    case SF_SHORT:  formt = SFMT_SHORT; break;
/*  case SF_LONG:   formt = SFMT_LONG;	break;	*/  /* not unique cases */
    case SF_FLOAT:  formt = SFMT_FLOAT; break;
    default:	    /* leave formt unchanged ! */
	fprintf(stderr,"sndfirc: unrecog. format %d\n",formt);
	break;
	}
    /* copy across info as read */
    irc_infp = &(psfh->sfinfo.sf_codes);
    sdf_infp = &(snd->info[0]);
    infoBsize = sizeof(SFHEADER) - (irc_infp - (char *)psfh);
    /* by def, SFIRCAM has a lot of potential info area to be hauled about */
    if(infoBmax >= infoBsize)
	i = infoBsize;
    else
	i = infoBmax;	/* can't copy the whole header, just this bit */
    infoXtra = infoBsize - i;	/* bytes of info we couldn't yet read */
    while(i>0)
	{
	--i;
	*sdf_infp++ = *irc_infp++;  /* copy between sep buffers */
	}
    /* rewrite into different data structure */
    snd->magic = SFMAGIC;   /* OUR magic number, not the IRCAM SF_MAGIC */
    snd->headBsize = infoBsize + sizeof(SFSTRUCT) - SFDFLTBYTS;
    snd->dataBsize = GetFileSize(file) - sizeof(SFHEADER);  /* maybe.. */
    snd->format = formt;
    snd->channels = chans;
    snd->samplingRate = srate;

    return(SFerror = SFE_OK);
    }

int SFRdXInfo(file, psnd, done)
    FILE *file;
    SFSTRUCT *psnd;
    int done;
/* fetch any extra info bytes needed in view of read header
    Passes open file handle, existing sound struct
    already read 'done' bytes; rest inferred from structure */
    {
    size_t  num;
    long    subhlen;
    long    file_pos;
    int     rem;

    rem = SFInfoSize(psnd) - done;
    if(rem <= 0)
	return(SFerror = SFE_OK);	/* nuthin more to read */
    /* do not assume seek already at right place.. */
    subhlen = ((&(psfh->sfinfo.sf_codes)) - (char *)psfh) + (long)done;
    file_pos = ftell(file);
    if( (subhlen - file_pos) != 0) 	/* if not at right place.. */
	if(fseek(file, subhlen - file_pos, SEEK_CUR) != 0 )	/* relative */
	    return(SFerror = SFE_RDERR);
    num = fread((void *)(psnd->info+done), (size_t)1, (size_t)rem, file);
    if(num < (size_t)rem)
	return(SFerror = SFE_RDERR);
    else
	return(SFerror = SFE_OK);
    }

static int SFWriteHdr(file, snd, infoBmax)
    FILE *file;
    SFSTRUCT *snd;
    int infoBmax;
/*  Utility to write out header to open file
    infoBmax optionally limits the number of info chars to write.
    infoBmax = 0 => write out all bytes implied by .headBsize field
    infoBmax >= SFDFLTBYS => write out only infoBmax infobytes */
    {
    size_t  num;
    long    hedSiz;
    char    *irc_infp, *sdf_infp;
    int     i;
    long    infoBsize;

    if(psfh == NULL)		    /* check our personal IRCAM sfheader buf */
	if( (psfh = (SFHEADER *)malloc((size_t)sizeof(SFHEADER)))==NULL)
	    return(SFE_MALLOC);     /* couldn't alloc our static buffer */

    if(snd->magic != SFMAGIC)
	return(SFerror = SFE_NSF);
    if(infoBmax < SFDFLTBYTS || infoBmax > SFInfoSize(snd) )
	infoBmax = SFInfoSize(snd);
    if(fseek(file, (long)0, SEEK_SET) != 0 )	/* move to start of file */
	return(SFerror = SFE_WRERR);
    /* copy to private buffer.. */
    psfh->sfinfo.sf_magic = SF_MAGIC;
    psfh->sfinfo.sf_srate  = snd->samplingRate;
    psfh->sfinfo.sf_chans  = snd->channels;
    switch(snd->format)
	{
    case SFMT_CHAR:	psfh->sfinfo.sf_packmode = SF_CHAR;	break;
    case SFMT_ALAW:	psfh->sfinfo.sf_packmode = SF_ALAW;	break;
    case SFMT_ULAW:	psfh->sfinfo.sf_packmode = SF_ULAW;	break;
    case SFMT_SHORT:	psfh->sfinfo.sf_packmode = SF_SHORT;	break;
    case SFMT_LONG:	psfh->sfinfo.sf_packmode = SF_LONG;	break;
    case SFMT_FLOAT:	psfh->sfinfo.sf_packmode = SF_FLOAT;	break;
    default:	    /* leave formt unchanged ! */
	fprintf(stderr,"sndfirc: unrecog. format %d\n",snd->format);
	psfh->sfinfo.sf_packmode = snd->format;
	break;
	}
    /* copy across info provided */
    irc_infp = &(psfh->sfinfo.sf_codes);
    sdf_infp = &(snd->info[0]);
    infoBsize = sizeof(SFHEADER) - (irc_infp - (char *)psfh);
    /* by def, SFIRCAM has a lot of potential info area to be hauled about */
    if(infoBmax >= infoBsize)
	i = infoBsize;
    else
	i = infoBmax;	/* can't copy the whole header, just this bit */
/*  infoXtra = infoBsize - i;	/* bytes of info we couldn't yet read */
    while(i>0)
	{
	--i;
	*irc_infp++ = *sdf_infp++;  /* copy between sep buffers */
	}

    hedSiz = sizeof(SFHEADER);	    /* write a whole header in any case */
    if( (num = fwrite((void *)psfh, (size_t)1, (size_t)hedSiz,
			 file))< (size_t)hedSiz)
	{
    /*	fprintf(stderr, "SFWrH: tried %ld managed %ld\n",
		hedSiz, (long)num);	*/
	return(SFerror = SFE_WRERR);
	}
    return(SFerror = SFE_OK);
    }

static long SFHeDaLen(file)
    FILE *file;
    /* return data space used for header in file - see SFCloseWrHdr */
    {
    long hlen = 0;

    /* always the same for IRCAM soundfiles */
    return(sizeof(SFHEADER));
    }

static size_t GetFileSize(file)
    FILE *file;	    /* return number of bytes in a file */
    {
/*    int fd;	       only known way WAS to use UNIX open() - now have ANSI
    char *name;
    size_t size;

    name = FindFname(file);
    if(name == NULL)
	{
	fprintf(stderr,"GetFileSize: couldn't find filename\n");
	return(-1);
	}
    if( (fd = open(name,O_RDONLY)) < 0)
	{
	fprintf(stderr,"GetFileSize: couldn't open()\n");
	return(-1);
	}
    size = fdSize(fd);
    close(fd);
    return(size);
 */
    long	fpos, flen;

    fpos = ftell(file);
    fseek(file, 0L, SEEK_END); 
    flen = ftell(file);
    fseek(file, fpos, SEEK_SET);	/* back where we started */
    return (size_t)flen;
    }

/* don't need this now ..
static size_t fdSize(fd)
    int fd;
    {
    struct stat b;
    fstat(fd,&b);
    return (size_t)b.st_size;
    }
 */
