/*
 * C interface to Macintosh Standard File Package.
 *
 * Bill Gardner, Sept, 1990.
 */
#include	<MacTypes.h>
#include	<StdFilePkg.h>
#include	<HFS.h>
#include	<pascal.h>
#include	"tc_misc.h"

long lastType;		/* type of last file opened */

/*
 * Gets name and vrefnum of Mac file for opening.
 * Returns TRUE if OK, FALSE if CANCEL.
 */
int getFile(fname, vRefNum, nType, types)
char *fname;	/* C string */
short *vRefNum;	/* pointer to vrefnum */
int nType;		/* number of types passed */
OSType types;	/* variable number of arguments! */
{
	SFReply sfr;
	Point pt;

	pt.h = 80;
	pt.v = 80;
	cursNorm();
	if (nType > 0)
		SFGetFile(pt, "\p", NULL, nType, &types, NULL, &sfr);
	else
		SFGetFile(pt, "\p", NULL, -1, NULL, NULL, &sfr);
	cursPop();
	if (!sfr.good)
		return FALSE;
	strcpy(fname, PtoCstr((char *)sfr.fName));
	*vRefNum = sfr.vRefNum;
	SetVol(NULL,sfr.vRefNum);
	lastType = sfr.fType;
	return TRUE;
}

/*
 * Gets output file name and vrefnum, uses fname as default.
 * Returns TRUE if OK, FALSE if CANCEL.
 */
int putFile(prompt,fname,vRefNum)
char *prompt;	/* pascal string, use default if NULL */
char *fname;	/* C string */
short *vRefNum;	/* pointer to vrefnum */
{
SFReply sfr;
Point pt;

	pt.h = 80;
	pt.v = 80;
	cursNorm();
	SFPutFile(pt, prompt ? prompt : "\pSave file as:", CtoPstr(fname), NULL, &sfr);
	cursPop();
	if (!sfr.good) {
		PtoCstr(fname);
		return FALSE;
	}
	strcpy(fname, PtoCstr((char *)sfr.fName));
	*vRefNum = sfr.vRefNum;
	SetVol(NULL, sfr.vRefNum);
	return TRUE;
}

/*
 * Changes type and creator of specified file.
 * Returns TRUE if success, FALSE if error.
 */
chgFInfo(fname,vrefnum,type,creator)
char *fname;	/* C */
short vrefnum;
OSType type;
OSType creator;
{
FInfo finfo;
char buf[64];
	strcpy(buf,fname);
	CtoPstr(buf);
	if (GetFInfo(buf,vrefnum,&finfo) != noErr) return FALSE;
	finfo.fdType = type;
	finfo.fdCreator = creator;
	if (SetFInfo(buf,vrefnum,&finfo) != noErr) return FALSE;
	return TRUE;
}
