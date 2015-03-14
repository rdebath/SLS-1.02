/*
 * Function getPath(path,fname,vrefnum) that takes a fname and vrefnum
 * and returns the full pathname from the root volume.
 *
 * Function getFV(path,fname,vrefnum) does the opposite.
 *
 * Works with both HFS and MFS systems.
 *
 * Bill Gardner, 3/16/89
 * Revision history:
 *	Aug 26, 1990: If fname is NULL, getPath() now returns directory path.
 *		Similarly, if fname is NULL, getFV() assumes path is a directory.
 */
#include	<HFS.h>
#include	<pascal.h>

#define ROOT_DIR_ID	2

/*
 * Returns TRUE if running HFS, else FALSE.
 */
int isHFS()
{
	return (FSFCBLen > 0);
}

/*
 * Concatenates name, ':', and path. Returns in path.
 */
concatPath(path,name)
char *path;
char *name;
{
char buf[256];
	sprintf(buf,"%s%s%s",name,*path ? ":" : "",path);
	strcpy(path,buf);
}

/*
 * Call this regardless of file system type.
 * Returns noErr if success, else FileMgr error.
 */
int getPath(path,fname,vrefnum)
char *path;		/* C string */
char *fname;	/* C string */
short vrefnum;
{
	return isHFS() ? getHFSPath(path,fname,vrefnum) : getMFSPath(path,fname,vrefnum);
}

/*
 * getHFSPath() function builds full pathname from fname and vrefnum.
 * Path is limited to 255 characters. If fname is NULL, then builds
 * directory pathname of vrefnum. Returns noErr is success, else error.
 * Will crash if run on non-HFS system.
 */
OSErr getHFSPath(path,fname,vrefnum)
char *path;		/* C string */
char *fname;	/* C string */
short vrefnum;
{
HFileInfo pb;
WDParam wd;
int stat;
long curDir;
char buf[256];
	*path = 0;
	bufclr(&pb,sizeof(HFileInfo));
	if (fname) {
		strcpy(buf,fname);
		CtoPstr(buf);
		pb.ioNamePtr = (StringPtr) buf;
		pb.ioVRefNum = vrefnum;
		pb.ioFDirIndex = 0;
	}
	else {
		bufclr(&wd,sizeof(WDParam));
		wd.ioVRefNum = vrefnum;
		if ((stat = PBGetWDInfo(&wd,FALSE)) != noErr) return stat;
		buf[0] = 0;
		pb.ioNamePtr = (StringPtr) buf;
		pb.ioDirID = wd.ioWDDirID;
		pb.ioFDirIndex = -1;
	}
	do {
		if ((stat = PBGetCatInfo(&pb,FALSE)) != noErr) return stat;
		PtoCstr((char *)pb.ioNamePtr);
		concatPath(path,pb.ioNamePtr);
		CtoPstr((char *)pb.ioNamePtr);
		curDir = pb.ioDirID;
		pb.ioFDirIndex = -1;
		pb.ioDirID = pb.ioFlParID;
	} while (curDir != ROOT_DIR_ID);
	return noErr;
}

/*
 * getMFSPath() function builds full pathname from fname and vrefnum.
 * On MFS systems, this is easily done my appending the filename to the
 * volume name. Path is limited to 255 characters. If fname is NULL,
 * then builds directory pathname of vrefnum (the volume name).
 * Returns noErr is success, else error. Changes default volume!
 */
int getMFSPath(path,fname,vrefnum)
char *path;		/* C string */
char *fname;	/* C string */
short vrefnum;
{
char volname[64];
long free;
int stat;
	volname[0] = 0;
	SetVol(0L,vrefnum);
	if ((stat = GetVInfo(0,volname,&vrefnum,&free)) != noErr) return stat;
	PtoCstr(volname);
	if (fname) strcpy(path,fname);
	concatPath(path,volname);
	return noErr;
}

/*
 * Splits a full pathname into volume (directory) pathname and filename.
 */
splitPath(path,volname,fname)
char *path;
char *volname;
char *fname;
{
char *s;
	strcpy(volname,path);
	s = &volname[strlen(volname)];
	while (s > &volname[0] && *s != ':') s--;
	s++;
	strcpy(fname,s);
	*s = 0;
}

/*
 * Returns TRUE if s is a heirarchical pathname (i.e. contains a colon),
 * else FALSE.
 */
int isHierarchical(s)
char *s;
{
	while (*s) if (*s++ == ':') return TRUE;
	return FALSE;
}

/*
 * Returns filename and volume reference number for given full pathname.
 * If fname is NULL, assume pathname is a directory specification.
 * Works on HFS and MFS systems. Returns FileMgr error, noErr if success.
 */
int getFV(path,fname,vrefnum)
char *path;
char *fname;
short *vrefnum;
{
char volname[256];
	if (fname) splitPath(path,volname,fname);
	else strcpy(volname,path);
	return (isHFS() && isHierarchical(volname)) ?
		getHFSVRefNum(volname,vrefnum) : getMFSVRefNum(volname,vrefnum);
}

/*
 * Returns VRefNum of volname, returns FileMgr error, noErr if success.
 */
int getMFSVRefNum(volname,vrefnum)
char *volname;
short *vrefnum;
{
int stat;
long free;
	CtoPstr(volname);
	if ((stat = SetVol(volname,0)) != noErr) return stat;
	if ((stat = GetVInfo(0,volname,vrefnum,&free)) != noErr) return stat;
	PtoCstr(volname);
	return stat;
}

/*
 * Returns vrefnum of volname. Actually opens a working directory and returns
 * the WDVRefNum. Returns FileMgr error, noErr if success.
 */
int getHFSVRefNum(volname,vrefnum)
char *volname;
short *vrefnum;
{
WDPBRec pb;
int stat;
	bufclr(&pb,sizeof(WDPBRec));
	CtoPstr(volname);
	pb.ioNamePtr = (StringPtr) volname;
	pb.ioWDProcID = 'ERIK';
	stat = PBOpenWD(&pb,FALSE);
	PtoCstr(volname);
	*vrefnum = pb.ioVRefNum;
	return stat;
}
