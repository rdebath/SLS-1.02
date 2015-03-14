/************************************************************************/
/*	tc_files.c   			  						     				*/
/* THINK_C file & Digidesign soundfile support for Csound on the Mac 	*/
/*	dpwe  24mar90, bv 10jun90											*/
/* audiomedia 1.2 support 24feb91 dpwe									*/
/************************************************************************/

#include	<FileMgr.h>
#include	"cs.h"    /* for mmalloc prototypes; includes <string.h> */

#ifndef  NULL
#define  NULL 0L
#endif

static	char *dSizeStr = "\psample-size";
static	char *sRateStr = "\psample-rate";
static	char *chansStr = "\pchannels";
static  long strType   = (long)'STR ';
static  long mrkrType  = (long)'sdML';
static  long fileType  = (long)'Sd2f';
static  long crtrType  = (long)'Sd2b';	/* 'sd2b' is for audiomedia, 'sd2a' is soundDesigner 2 */ 

/*
 * Do a SystemTask() call and check for abort key sequence.
 */
void STasks()
{
	SystemTask();
	if (isAbortEvent()) {
		printf("ABORTING...\n");
		ExitToShell();
	}
}

void AddMacHeader(fName,chans,srate,dsize)
	char	*fName;	/* Name of soundfile - already created */
	int 	chans;		/* number of channels */
	float	srate;		/* sampling frequency */
	int 	dsize;		/* bytes per sample */
	{
	char	**handle1,**handle2,**handle3;	/* temporary strings */
	short	**handle4;
	char	*str;
	int 	rID;
	int 	newr = 0;				/* flag that resource is added */
	
	CtoPstr(fName);
	rID = OpenResFile(fName);
	if(rID == -1)
		{				/* error - no resource file */
		CreateResFile(fName);		/* add a resource fork to the soundfile */
		rID = OpenResFile(fName);		/* open it.. */
		}
	PtoCstr(fName);

	handle1 = GetNamedResource('STR ',dSizeStr);
	if(handle1 == NULL)			/* make new resource if there wasn't one */
		{
		handle1 = NewHandle(4);
		newr = 1;
		}
	HLock(handle1);
	str = *handle1;
	NumToString((long)dsize,str);
	HUnlock(handle1);
	if(newr)
		{
		newr = 0;
		AddResource(handle1,strType,1000 /* UniqueID(strType) */,dSizeStr);
		}		/* Digidesign needs exact ID as well as string */
	else
		ChangedResource(handle1);

	handle2 = GetNamedResource('STR ',sRateStr);
	if(handle2 == NULL)			/* make new resource if there wasn't one */
		{
		handle2 = NewHandle(16);
		newr = 1;
		}
	HLock(handle2);
	str = *handle2;
	FloatToString(srate,str,4 /* d.p. */ );
	HUnlock(handle2);
	if(newr)
		{
		newr = 0;
		AddResource(handle2,strType,1001 /* UniqueID(strType) */,sRateStr);
		}
	else
		ChangedResource(handle2);

	handle3 = GetNamedResource('STR ',chansStr);
	if(handle3 == NULL)			/* make new resource if there wasn't one */
		{
		handle3 = NewHandle(4);
		newr = 1;
		}
	HLock(handle3);
	str = *handle3;
	NumToString( (long)chans, str);
	HUnlock(handle3);
	if(newr)
		{
		newr = 0;
		AddResource(handle3,strType,1002 /* UniqueID(strType) */,chansStr);
		}
	else
		ChangedResource(handle3);
	
	handle4 = GetResource(mrkrType, 1000);	/* must be present for audiomedia */
	if(handle4 == NULL)
		{
		handle4 = (short *)NewHandle(8);		/* meaningless default */
		(*handle4)[0] = 1;
		(*handle4)[1] = 0;
		(*handle4)[2] = 0;
		(*handle4)[3] = 0;
		AddResource(handle4,mrkrType,1000,"\p");
		}

	CloseResFile(rID);		/* includes releasing all the handles */
	}

int	ReadMacHeader(fName,pchans,psrate,pdsize)
/* Gets standard stuff from Digidesign-type resource header. Returns error code. */
	char	*fName;			/* Name of file to get it from */
	int		*pchans;		/* returns number of channels */
	float	*psrate;		/* returns sampling-rate */
	int 	*pdsize;		/* returns bytes / sample */
	{
	int 	rc = 0;			/* return code - couldn't find some resources */
	int 	rfd,err,vRefNum;
	char	**handle,volName[32];
	long	rslt, *prslt;

	CtoPstr(fName);
	GetVol(volName,&vRefNum);
	
	rfd = OpenResFile(fName);
	if(rfd == -1)
		return(-1);			/* error - no resource file */
	
	prslt = &rslt;
	
	handle = GetNamedResource('STR ',dSizeStr);
	if(handle == NULL)
		rc += 1;				/* error - no sample-size resource */
	else{
		HLock(handle);
		StringToNum(*handle,prslt);
		*pdsize = (int)rslt;
		HUnlock(handle);
		}

	handle = GetNamedResource('STR ',sRateStr);
	if(handle == NULL)
		rc += 2;				/* error - no sample-rate resource */
	else{
		HLock(handle);
		StringToFloat(*handle,psrate);
		HUnlock(handle);
		}
 
	handle = GetNamedResource('STR ',chansStr);
	if(handle == NULL)
		rc += 4;				/* error - no channels resource */
	else{
		HLock(handle);
		StringToNum(*handle,prslt);
		*pchans = (int)rslt;
		HUnlock(handle);
		}

	CloseResFile(rfd);
	PtoCstr(fName);

	return(rc);					/* set bits of rc indicate unfound resource */
	}

	
void MacSetCreator(fName)		/* set the creator & type of this file for Digidesign SF */
	char	*fName;				/* Name of soundfile - already created */
	{
	char	volName[32];
	int 	vRefNum;
	FInfo	fInfo;

	CtoPstr(fName);
	GetVol(volName,&vRefNum);
	GetFInfo(fName,vRefNum,&fInfo);				/* get creator & type */
	fInfo.fdType = fileType;
	fInfo.fdCreator = crtrType;
	SetFInfo(fName,vRefNum,&fInfo);				/* rewrite modified */

	PtoCstr(fName);
	}

StringToFloat(s,pf)			/* like built-in StringToNum - takes a Str255 & &float */
	Str255	s;
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
			;					/* find next highest pwr of 10 */
		*pf += (10.0*(float)rslt)/((float)l)-1.0;	/* voila - frac part */
		}
	}

FloatToString(f,s,dp)		/* like built in NumToString, but for floats (& Str255) */
	float	f;
	Str255	s;
	int 	dp;				/* number of decimal places */
	{
	long	l;
	Str255	t;
	float	k = 1.0;
	int 	i;

	l = (long)f;
	NumToString(l,s);       		/* convert int part */
	PtoCstr(s);
	if(dp > 0)
		{
		for(i=0; i++ < dp; k *= 10 )	/* K scales for dp */
			;
		l = (long)(k*(1.0+f - (float)l));
		NumToString(l,t);
		PtoCstr(t);
		t[0] = '.';				/* overwrite place holding 1, add dp */
		strcat((char *)s,(char *)t);
		}
	CtoPstr(s);
	}

/*	(from Lee Boynton's File.c in the Tracker project)
 *
 * LoadFile - given a file name and volume reference number, load the file
 * into a nonrelocatable block in the heap and return a pointer to it and its size.
 *	Returns zero if no error occurs, otherwise returns the OS error code.
 */
short LoadFile(fName,vRefNum,bufPtr,bufSize)
	char *fName;
	int vRefNum;
	Ptr *bufPtr;
	long *bufSize;
{
	int pathnum;
	OSErr err;
	
	CtoPstr(fName);
	err=FSOpen(fName,vRefNum,&pathnum);
	PtoCstr(fName);
	if(err) return(err); /* cannot open file */
	err=GetEOF(pathnum,bufSize);
	if(err){ /* cannot determine size of file */
		FSClose(pathnum);		
		return(err);
	}
	*bufPtr=mmalloc(*bufSize);	/* was NewPtr; malloc so can FREE it later */
	err=MemError();
	if(err){ /* cannot allocate buffer space */
		FSClose(pathnum);
		return(err); 
	}
	err=FSRead(pathnum,bufSize,*bufPtr);
	if(err){ /* cannot read file */
		FSClose(pathnum);
		free(*bufPtr);			/* was DisposPtr */
		*bufPtr=NULL;
		*bufSize=0L;
		return(err);
	}
	err=FSClose(pathnum);
	return(0);             /* presume close ok */
}


