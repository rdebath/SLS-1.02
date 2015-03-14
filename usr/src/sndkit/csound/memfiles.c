#include "cs.h"			/*					MEMFILES.C	*/

#define MAXFILES  10

static MEMFIL memfiles[MAXFILES];

#ifndef THINK_C
static struct stat statbuf;
#endif

 MEMFIL *
ldmemfile(filnam)
 char *filnam;
{
	MEMFIL	*mfp, *endp;
	char	*allocp;
	long	len;
	int	fd;

	for (mfp=memfiles,endp=mfp+MAXFILES; mfp<endp; mfp++) {
	    if (strcmp(mfp->filename,"") == 0)              /* if empty slot */
	        goto ldopn;                                 /*   go readfile */
	    else if (strcmp(mfp->filename,filnam) == 0)     /* else if match */
	        return(mfp);                                /*   we have it  */
	}
	sprintf(errmsg,"memfiles: MAXFILES exceeded");      /* else overflow */
	goto lderr;

#ifdef THINK_C
ldopn:  if (LoadFile(filnam,0,&allocp,&len)) {
	    sprintf(errmsg,"cannot load %s",filnam);
	    goto lderr;
	}
#else
ldopn:	if ((fd = open(filnam,O_RDONLY)) < 0) {             /* open the file, */
	    sprintf(errmsg,"cannot open %s",filnam);
	    goto lderr;
	}
	fstat(fd,&statbuf);				    /*   get its length */
	len = statbuf.st_size;
	allocp = mmalloc((long)len);            	    /*   alloc as reqd  */
	if ((len = read(fd,allocp,(int)len)) <= 0)	    /*   read file in   */
		dies("read error on %s",filnam);
	close(fd);					    /*   and close it   */
#endif
	strcpy(mfp->filename,filnam);                       /* init the struct  */
	mfp->beginp = allocp;
	mfp->endp = allocp + len;
	mfp->length = len;
	printf("file %s (%ld bytes) loaded into memory\n",filnam,len);
 	return(mfp);

lderr:	initerror(errmsg);
	return(NULL);
}

rlsmemfiles()            /* clear the memfile array, & free all allocated space */
{
register MEMFIL  *mfp, *endp;
register int     memcount = 0;

	for (mfp=memfiles,endp=mfp+MAXFILES; mfp<endp; mfp++)
	    if (strcmp(mfp->filename,"") != 0) {            /* if slot taken    */
	        strcpy(mfp->filename,"");                   /*   clr the name & */
	        free(mfp->beginp);                          /*   free the space */
		memcount++;
	    }
        if (memcount)
	    printf("%d memfile%s deleted\n", memcount, (memcount>1)? "s":"");
}
