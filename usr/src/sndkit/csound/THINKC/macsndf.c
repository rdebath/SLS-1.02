/********************************************************/
/*	sfile.c 	(from db.sfile.c)						*/
/*														*/
/* Macintosh soundfile interface - based on Csound/Mac	*/
/* dpwe  02apr90										*/
/********************************************************/

#include <MacTypes.h>
#include <FileMgr.h>
#include <TextEdit.h>
#include <WindowMgr.h>
#include <StdFilePkg.h>
#include <ResourceMgr.h>
#include <MemoryMgr.h>

#ifndef NULL
#define NULL 0L
#endif

#define DDFTYPE  'Sd2f'		/* file type for digidesign files */
#define DDCREAT  'Sd2a'		/* creator for digidesign files */

/* prototypes */

int MacAddHeader(char *fName,int vRef,int chans,float srate,int dsize,
					char *info, int infoSize);
int	MacReadHeader(char *fName,int vRef,int *pchans,float *psrate,
					int *pdsize,char *info, int *pinfoSize /*, long *plen */);
int MacSetCreator(char *fName,int vRef);  /* set creator & type as Digid SF */
int LoadFile(int pathnum, Handle *bufHan,long *bufSize);
static void SetResStr(long type, char *name, int ID,
						 char *str, int len);  /* set or replace string rsrc */
static void SetResNumStr(long type, char *name, int ID,
						 float data, int floatNlong); /* set/rep asc dec str */
static int GetResStr(long type, char *name, char *buf, int *plen);
static int GetResNumStr(long type, char *name, void *rslt, int floatNlong);
static OSErr SetVref(int *pVref, int *pOref);  /* make a good vol reference */
static StringToFloat(char *s,float *pf);  /* scanfloat a string */
static FloatToString(float f,char *s,int dp); /*  sprint a float */
static mystrcat(char *s1,char *s2);		/* sticks s2 on the end of s1  */

/* static globals */

static	char *dSizeStr = "\psample-size";
static	char *sRateStr = "\psample-rate";
static	char *chansStr = "\pchannels";
static	char *sinfoStr = "\psound-info";
static	long strType = (long)'STR ';

int MacAddHeader(fName,vRef,chans,srate,dsize,info,infoSize)
	char	*fName;		/* PASC Name of soundfile - already created */
	int 	vRef;		/* vol reference for that file */
	int 	chans;		/* number of channels */
	float	srate;		/* sampling frequency */
	int 	dsize;		/* bytes per sample */
	char	*info;		/* stream of undesignated bytes */
	int 	infoSize;	/* how many bytes after above pointer */
	{
	char	*str;
	int 	rID;

	int		oVol;
	
	if(SetVref(&vRef,&oVol))
		return(-1);			/* bad volume */

	rID = OpenResFile(fName);
	if(rID == -1)
		{								/* error - no resource file */
		CreateResFile(fName);			/* add a resource fork to the soundfile */
		rID = OpenResFile(fName);		/* open it.. */
		}
	if(rID == -1)
		{
		SetVol("",oVol);
		return(-1);						/* couldn't open a resource file, so bomb out */
		}

	SetResNumStr(strType, dSizeStr, 1000, (float)dsize, 0 /* int */);
	SetResNumStr(strType, sRateStr, 1001, srate, 1 /* float */);
	SetResNumStr(strType, chansStr, 1002, (float)chans, 0);
	SetResStr(strType, sinfoStr, 999, info, infoSize);
	
	CloseResFile(rID);		/* includes releasing all the handles */
	SetVol("",oVol);
	return(0);
	}

static void SetResStr(long type, char *name, int ID,
						 char *str, int len)
	/* replace or add resource as ascii string */
	{
	char	**h;
	int 	i;
	char	*s;
	
	h = GetNamedResource(type,name);
	if(h != NULL)				
		RmveResource(h);		/* delete any existing rsrce */
	h = NewHandle(len);
	HLock(h);
	s = *h;
	while(len--)
		*s++ = *str++;
	HUnlock(h);
	AddResource(h,type,ID,name);
	}

static void SetResNumStr(long type, char *name, int ID,
						 float data, int floatNlong)
	/* replace or add resource as ascii string repr of a number */
	{
	char	**h;
	int 	newr = 0;
	char	*s;
	
	h = GetNamedResource(type,name);
	if(h == NULL)								/* make new resource if there wasn't one */
		{
		h = NewHandle(16);		/* 16 bytes should be enough ??? */
		newr = 1;
		}
	HLock(h);
	s = *h;
	if(floatNlong)
		FloatToString(data,s,4 /* d.p. */);
	else	/* actually an int */
		NumToString((long)data,s);
	HUnlock(h);
	if(newr)
		{
		newr = 0;
		AddResource(h,type,ID,name);
		}
	else
		ChangedResource(h);
	}

long MacFileLen(fName,vRef)
/* attempts to find length of Mac file.  Likely to fail if file already open */
	char	*fName;
	int 	vRef;				/* 0 == default */
	{
	int 	oVol;
	int 	dfd;
	long	len;
	
	if(SetVref(&vRef,&oVol))
		return(-1L);			/* bad volume */
	
	if(FSOpen(fName,vRef,&dfd))	/* open (another path to read-only) data fork */
		len = -1L;				/* error - couldn't open data file to find length */
	else{
		GetEOF(dfd, &len);		/* Find data length */
		FSClose(dfd);			/* release data fork */
		}
	SetVol("",oVol);
	return(len);
	}


int	MacReadHeader(fName,vRef,pchans,psrate,pdsize,info,pinfoSize /* ,plen */)
/* Gets standard stuff from Digidesign-type resource header. Returns error code. */
	char	*fName;			/* PASC Name of file to get it from */
	int		vRef;			/* 0 == default */
	int		*pchans;		/* returns number of channels */
	float	*psrate;		/* returns sampling-rate */
	int 	*pdsize;		/* returns bytes / sample */
	char	*info;			/* where to put (optional) extra bytes */
	int 	*pinfoSize;		/* input: size of info buf/ out: avai info bytes */
/* withdrawn	long	*plen;			/* returns number of bytes in data (sample) */
	{
	int 	rc = 0;			/* return code - couldn't find some resources */
	int 	rfd,err;
/*	int 	dfd;	*/
	long	rslt, *prslt;
	int 	oVol;

	if(SetVref(&vRef,&oVol))
		return(-1);			/* bad volume */
	
/*	if(FSOpen(fName,vRef,&dfd))	/* open (another path to read-only) data fork */
/*		*plen = -1;				/* error - couldn't open data file to find length */
/*	else{
/*		GetEOF(dfd, plen);		/* Find data length */
/*		FSClose(dfd);			/* release data fork */
/*		}
 */
	rfd = OpenResFile(fName);
	if(rfd == -1)
		{
		SetVol("",oVol);
		return(-2);			/* error - no resource file */
		}

	prslt = &rslt;
	
	rc += GetResNumStr(strType,dSizeStr, prslt, 0 /* long */);
	*pdsize = (int)rslt;
	rc += 2 * GetResNumStr(strType,sRateStr, psrate, 1 /* float */);
	rc += 4 * GetResNumStr(strType,chansStr, prslt, 0 /* long */);
	*pchans = (int)rslt;
	rc += 8 * GetResStr(strType,sinfoStr, info, pinfoSize);

	CloseResFile(rfd);
	SetVol("",oVol);
	return(rc);					/* set bits of rc indicate unfound resource */
	}

static int GetResStr(long type, char *name, char *buf, int *plen)
	/* read a resource string */
	/* on entry, *plen is max length of buf.  
	   On exit, it is actual length of resource, although only as much as
	   will fit in the specified buffer will have been copied */
	{
	char	**h,*s;
	int 	rc = 0;
	int 	blen = *plen;

	h = GetNamedResource(type,name);
	if(h == NULL)
		{
		rc = 1;				/* error - no such resource */
		*plen = 0;
		}
	else{
		HLock(h);
		*plen = SizeResource(h);
		if(*plen < blen)	blen = *plen;	/* decide how much to copy */
		s = *h;
		while(blen--)
			*buf++ = *s++;	/* copy it */
		HUnlock(h);
		}
	return(rc);
	}

static int GetResNumStr(long type, char *name, void *rslt, int floatNlong)
	/* read a resource as an ascii decimal string to write to a number */
	{
	char	**h;
	int 	rc = 0;

	h = GetNamedResource(type,name);
	if(h == NULL)
		{
		rc = 1;				/* error - no such resource */
		if(floatNlong)
			*((float *)rslt) = 0.0;
		else
			*((long *)rslt) = 0L;
		}
	else{
		HLock(h);
		if(floatNlong)
			StringToFloat(*h,((float *)rslt));
		else
			StringToNum(*h,((long *)rslt));
		HUnlock(h);
		}
	return(rc);
	}

int MacSetCreator(fName,vRef)	/* set the creator & type of this file for Digidesign SF */
	char 	*fName;				/* PASC name of soundfile - already created */
	int 	vRef;
	{
	FInfo	fInfo;
	int 	oVol;
	
	if(SetVref(&vRef,&oVol))
		return(-1);			/* bad volume */
	
	GetFInfo(fName,vRef,&fInfo);				/* get creator & type */
	fInfo.fdType = DDFTYPE;
	fInfo.fdCreator = DDCREAT;
	SetFInfo(fName,vRef,&fInfo);				/* rewrite modified */
	SetVol("",oVol);
	return(0);
	}

#define noError 0

/*
 * LoadFile - given a  reference number to an open file, load the file
 * into a relocatable block in the heap and return a handle it and its size.
 *	Returns zero if no error occurs, otherwise returns the OS error code.
 */
int LoadFile(pathnum,bufHan,bufSize)
	int pathnum;
	Handle *bufHan;
	long *bufSize;
	{
	OSErr err;

	err=GetEOF(pathnum,bufSize);
	if(err)
		{ /* cannot determine size of file */
		FSClose(pathnum);		
		return(err);
		}
	*bufHan=NewHandle(*bufSize);
	err=MemError();
	if(err)
		{ /* cannot allocate buffer space */
		FSClose(pathnum);
		return(err); 
		}
	HLock(*bufHan);
	err=FSRead(pathnum,bufSize,**bufHan);
	HUnlock(*bufHan);
	if(err)
		{ /* cannot read file */
		FSClose(pathnum);
		DisposHandle(*bufHan);
		*bufHan=NULL;
		*bufSize=0L;
		return(err);
		}
	err=FSClose(pathnum);
	return(noError);
	}

static OSErr SetVref(int *pVref, int *pOref)		/* make a good vol reference */
	{
	char	volName[32];
	OSErr 	rc = noErr;

	rc = GetVol(volName, pOref);	/* remember entry vol ref */
	if(*pVref)
		rc = SetVol("",*pVref);
	else
		*pVref = *pOref;			/* no volume provided - so return actual current */
	return(rc);
	}

static StringToFloat(s,pf)			/* like built-in StringToNum - takes a char *& &float */
	char *	s;
	float	*pf;
	{
	int 	i,j;
	long	l,rslt;
	
	i = (int)(s[0]);
	for(j=1; j<=i && s[j] != '.'; ++j)
		;
	s[0] = (char)j-1;
	StringToNum(s,&rslt);		/* !!!!! Treats as INT string !!! */
	*pf = (float)rslt;
	if(s[j]=='.')
		{
		s[j] = '1';
		s[j-1] = i-j+1;
		StringToNum(s+j-1,&rslt);		/* .xxx becomes (long)1xxx */
		for(l=1;l<=rslt; l *= 10)
			;						/* find next highest pwr of 10 */
		*pf += (10.0*(float)rslt)/((float)l)-1.0;	/* voila - frac part */
		}
	}

static FloatToString(f,s,dp)		/* like built in NumToString, but for floats (& char *) */
	float	f;
	char *	s;
	int 	dp;				/* number of decimal places */
	{
	long	l;
	char *	t;
	float	k = 1.0;
	int 	i;
	float	rup = 0.5;		/* roundup */

	for(i = dp; i>0; --i, rup /= 10.0)
		;					/* form rup as 1/2 smallest digit (per DP) */
	f += rup;				/* Add on, so truncation gives right result */
	l = (long)f;
	NumToString(l,s);		/* convert int part */
	PtoCstr((char *)s);
	if(dp > 0)
		{
		for(i=0; i++ < dp; k *= 10 )	/* K scales for dp */
			;
		l = (long)(k*(1.0+f - (float)l));
		NumToString(l,t);
		PtoCstr((char *)t);
		t[0] = '.';				/* overwrite place holding 1, add dp */
		mystrcat(s,t);
		}
	CtoPstr((char *)s);
	}

static mystrcat(s1,s2)     /* sticks s2 on the end of s1 - best you have enough room */
    char    *s1,*s2;
    {
    while(*s1++)
	;	    /* scan ptr to end of s1 */
    --s1;	    /* *s1 = '\0' */
    while(*s1++ = *s2++)
	;	    /* copy over s2 until you copy a '\0' */
    }


