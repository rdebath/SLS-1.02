/***************************************************************\
*	sndfmac.c					MACINTOSH						*
*	Extra functions for Macintosh version of sndf.c 			*
*	THIS FILE IS CONDITIONALLY INCLUDED IN sndf.c				*
*	12aug90 dpwe												*
\***************************************************************/

/* private type declarations */
typedef struct fnameinfo
	{
	struct fnameinfo *next;
	FILE   *fhan;
	Str255 fname;
	} FNAMEINFO;		/* used to build a map between FILE * & filenames */

/* static variables */
FNAMEINFO	*fniroot = NULL;	/* root of fnameinfo list */

/* static function prototypes */
static char *FindFname(FILE *file);	
	/* looks up file ptr, returns mapped name if any */

/* ----------------------- implementations ------------------------ */

static void AddFname(char *fname, FILE *file)
	/* associates name 'fname\0' with file pointer 'file' in our lookup table */
	{
	FNAMEINFO *runner = fniroot;
	
	/* see if FILE is already in our table.. */
	while(runner != NULL && runner->fhan != file)
		runner = runner->next;
	if(runner == NULL)		/* not found : add new record */
		{
		runner = (FNAMEINFO *)malloc((size_t)sizeof(FNAMEINFO));
		runner->fhan = file;
		runner->next = fniroot;
		fniroot = runner;		/* add to top of list, faster to find */
		}	/* either way, copy in the (new) name */
	strcpy((char *)runner->fname, fname);
	}

static void RmvFname(FILE *file)
	/* removes any fnameinfo record associated with 'file' */
	{
	FNAMEINFO *node;
	FNAMEINFO **prunner = &fniroot;
	
	while((*prunner) != NULL && (*prunner)->fhan != file)
		prunner = &(*prunner)->next;
	if((*prunner) != NULL)		/* found : delete record */
		{
		node = (*prunner);
		*prunner = (*prunner)->next;	/* close gap.. */
		free(node);						/* & dispose of memory */
		}	
	/* else not found - do nothing */
	}
	
static char *FindFname(FILE *file)
	/* looks up file ptr, returns mapped name if any */
	{
	FNAMEINFO *runner = fniroot;
	
	while(runner != NULL && runner->fhan != file)
		runner = runner->next;
	if(runner != NULL)
		return( (char *)runner->fname );
	else
		return( NULL );		/* fileptr not in our list */
	}

static int SFReadHdr(FILE *file, SFSTRUCT *snd, int infoBmax)
/* utility to sample the header of an opened file, at most infoBmax info byts */
    {
    int 	chans,dsize,infoSize;
    long	len;
    float	srate;
    char	*info;
    char	*fname;
    int 	err,i;

	if(infoBmax < SFDFLTBYTS)	infoBmax = SFDFLTBYTS;
	infoBmax = infoBmax&-4;		/* round down to multiple of four */
	infoSize = infoBmax;
	if( (fname = FindFname(file)) == NULL)
		return(SFerror = SFE_NOPEN);		/* couldn't find a pathname for this */
	CtoPstr(fname);
	err = MacReadHeader(fname,0,&chans,&srate,&dsize,&info,&infoSize);
	/* will only read into info for infoSize byts, updates infoSize to avai bts */

/*	len = MacFileLen(fname,0);
	if(len < 0)					/* couldn't read file length */
/*		printf("SFRdHdr: len read failed\n");
 */
 	len = file->len;			/* cheat by using THINK_C's internal record... */

	PtoCstr(fname);
	if(err&7)					/* just bottom 3 bits critical - optional info */
		return(SFerror = SFE_NSF);		/* reading mac header failed */
	if(infoSize < SFDFLTBYTS)	infoSize = SFDFLTBYTS;	/* don't rock the boat */
	if(infoBmax > infoSize) 	infoBmax = infoSize;	/* actual bts in buf */
    snd->magic = SFMAGIC;
   	snd->headBsize = sizeof(SFSTRUCT) + infoSize - SFDFLTBYTS;
	snd->dataBsize = len;	snd->channels = chans;	snd->samplingRate = srate;
	if(dsize == 1)		snd->format = SFMT_CHAR;
	else if(dsize == 4)	snd->format = SFMT_FLOAT;
	else 				snd->format = SFMT_SHORT;
	for(i=0; i<infoBmax; ++i)
		snd->info[i] = info[i];
    return(SFerror = SFE_OK);

    }

int SFRdXInfo(FILE *file, SFSTRUCT *psnd, int done)
/* fetch any extra info bytes needed in view of read header
    Passes open file handle, existing sound struct
    already read 'done' bytes; rest inferred from structure */
	{
    int 	chans,dsize,infoSize;
    long	len;
    float	srate;
    char	*info;
    char	*fname;
    int 	err,i;
    int 	rem;
	
	if( (fname = FindFname(file)) == NULL)
		return(SFerror = SFE_NOPEN);		/* couldn't find a pathname for this */
	CtoPstr(fname);
	infoSize = SFInfoSize(psnd); /* reread whole of info */
	err = MacReadHeader(fname,0,&chans,&srate,&dsize,psnd->info,&infoSize);
	PtoCstr(fname);
	if(err)
		return(SFerror = SFE_NOPEN);		/* reading mac header failed */
	else
		return(SFerror = SFE_OK);
	}

static int SFWriteHdr(FILE *file, SFSTRUCT *snd, int infoBmax)
/* 	Utility to write out header to open file
	infoBmax optionally limits the number of info chars to write.
	infoBmax = 0 => write out all bytes implied by .headBsize field 
	infoBmax >= SFDFLTBYS => write out only infoBmax infobytes */
    {
    int 	chans,dsize,infoSize;
    float	srate;
    char	*info;
    char	*fname;
    int 	err,i;

    if(snd->magic != SFMAGIC)
		return(SFerror = SFE_NSF);
	if(infoBmax >= SFDFLTBYTS && infoBmax < SFInfoSize(snd) )
		infoSize = infoBmax;
	else
		infoSize = SFInfoSize(snd);
	chans = snd->channels;	srate = snd->samplingRate;
	if(snd->format == SFMT_CHAR || snd->format == SFMT_ALAW ||
			 snd->format == SFMT_ULAW)
		dsize = 1;
	else if(snd->format == SFMT_LONG || snd->format == SFMT_FLOAT)
		dsize = 4;
	else dsize = 2;		/* default short */
	info = snd->info;
	if( (fname = FindFname(file)) == NULL)
		return(SFerror = SFE_NOPEN);		/* couldn't find a pathname for this */
	CtoPstr(fname);
	err=MacAddHeader(fname,0/*vref*/,chans,srate,dsize,info,infoSize);
	MacSetCreator(fname,0/*vref*/);		/* make an Sd2 file */
	PtoCstr(fname);
	if(err)
		return(SFerror = SFE_WRERR);
	else
	    return(SFerror = SFE_OK);
    }

static long SFHeDaLen(FILE *file) /* find header length for a file */
	{
	return(0L);		/* macintosh headers occupy no file data space */
	}
