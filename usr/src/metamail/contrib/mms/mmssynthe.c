/*////////////////////////////////////////////////////////////////////////
Copyright (c) 1992 Electrotechnical Laboratry (ETL)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of ETL not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of ETL.
ETL MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
/////////////////////////////////////////////////////////////////////////
Content-Type: program/C; charset=US-ASCII
Program:      mmssynth.c  (MIME message synthesizer)
Author:       Yutaka Sato <ysato@etl.go.jp>

History:
  v0.1  92.05.03  extracted from mms.c (mailto dependent version)
///////////////////////////////////////////////////////////////////////*/

#include <stdio.h>
char *getenv();

/*////////////////////////////////////////////////////////////////////////
 *	interface to "mailto"
 */
char *MMS_freshcopy();

struct mailpart {
    int istext;
    int isrich;
    char *content_type;
    int encoding_type_needed;
    char *filename;
    struct MailcapEntry *mc;
    struct mailpart *next, *prev;
};

#define ENC_NONE	0
static struct mailpart *CurrentPart;

extern FILE *fpout;
extern char *CharacterSet;
extern struct mailpart *FirstPart,*NewPart();

static char *Subject = NULL;
static char *ToList = NULL;
static char *CCList= NULL;
static char *NGList= NULL;
static int EightBitMode = 0;
static int RightToLeftMode = 0;

struct {
	char	*a_name;
	char	 a_on[50];
	char	 a_off[50];
} char_attrs[] = {
	0,
	{"underline"},
	{"italic"},
	{"bold"},
	{"excerpt"},
	0
};
static known_chattr(chattr)
	char *chattr;
{	int ai;
	char *name;

	for(ai = 1; name = char_attrs[ai].a_name; ai++)
		if( strcmp(chattr,name) == 0 )
			return ai;
	return 0;
}

MMS_mailtoReSetup(){
	ProcessInitFiles();
}
MMS_mailtoSetup(){
	ProcessInitFiles();
	InitSignals();
	InitTerminal();

	CharacterSet = getenv("MM_CHARSET");
	if( !CharacterSet )
		CharacterSet = "us-ascii";

	FirstPart = NewPart();
	CurrentPart = FirstPart;
	fpout = fopen((CurrentPart)->filename,"w");

	MMS_ProcessMailcap(0);
}

MMS_synthAddPart(p,errmsg)
	struct mailpart *p;
	char *errmsg;
{
	TempCloseStyles(fpout);
	fclose(fpout);
	CurrentPart->next = p;
	CurrentPart->next->prev = CurrentPart;
	CurrentPart = CurrentPart->next;
	CurrentPart->next = NewPart();
	CurrentPart->next->prev = CurrentPart;
	CurrentPart = CurrentPart->next;
	fpout = fopen((CurrentPart)->filename,"w");
	if( fpout == NULL )
		return -1;
	ReopenStyles(fpout, CurrentPart);
	return 0;
}

MMS_synthSubject(subject)
	char *subject;
{
	Subject = MMS_freshcopy(subject);
}

char *AddToList();
MMS_synthAddHead(field,entry,errmsg)
	char *field,*entry,*errmsg;
{
	MMS_touppers(field);
	if( strcmp(field,"TO") == 0 ){
		ToList = AddToList(ToList,entry);
	}else
	if( strcmp(field,"CC") == 0 ){
		CCList = AddToList(CCList,entry);
	}else
	if( strcmp(field,"NEWSGROUPS") == 0 ){
		NGList = AddToList(NGList,entry);
	}else{
		sprintf(errmsg,"unknown header [%s].",field);
		return -1;
	}
	return 0;
}

struct mailpart *
MMS_synthEncloseFile(ctype,ifp,errmsg)
	char *ctype;
	FILE *ifp;
	char *errmsg;
{	FILE *ofp;
	struct mailpart *mp;

	mp = NewPart();
	ofp = fopen(mp->filename,"w");
	if( ofp == NULL ){
		sprintf(errmsg,"cannot open tempfile: %s",mp->filename);
		fclose(ifp);
		return 0;
	}
	TranslateInputToEncodedOutput(ifp,ofp,ENC_NONE);
	fclose(ofp);
	mp->istext = 0;
	mp->content_type = MMS_freshcopy(ctype);
	mp->encoding_type_needed = WhichEncodingForFile(mp->filename);
	return mp;
}

MMS_synthAddText(ifp)
	FILE *ifp;
{	int lines = 0;
	char xline[2048];

	while( MMS_getline(xline,sizeof(xline),ifp) != NULL ){
		FputsQuotingLT(xline, fpout, CurrentPart,
			EightBitMode, RightToLeftMode);
		lines++;
	}
	return lines;
}

MMS_synthToggleTextAttr(okcode,errcode,okmsg,chattrs)
	char *okmsg,*chattrs;
{	char chattr[256];
	int ai;

	if( *chattrs == 0 || sscanf(chattrs,"%s",chattr) == 0 ){
		MMS_status0(okcode,okmsg);
	}else
	if( ai = known_chattr(chattr) ){
		MMS_statusN(okcode,okmsg);
		ToggleStyle(chattr,fpout,CurrentPart,
			char_attrs[ai].a_on, char_attrs[ai].a_off);
		MMS_response_done();
		return okcode;
	}else{
		MMS_status0(errcode,"UNKNOWN character attribute(%s)",chattr);
		return errcode;
	}
}

/*////////////////////////////////////////////////////////////////////////
 *	list Content-Type  (using mailto.o)
 */
struct MailcapEntry {
	char *contenttype;
	char *command;
	char *testcommand;
	char *editcommand;
	char *composecommand;
	char *label;
	int needsterminal;
	int copiousoutput;
	struct MailcapEntry *next;
};
extern struct MailcapEntry *FirstMailcapEntry;

char *MailcapPath =
"%s/.mailcap:%s/mailcap:/etc/mailcap:/usr/etc/mailcap:/usr/local/etc/mailcap";

MMS_ProcessMailcap(force)
{	static int initdone;
	char *home,*lib;

	if( force || !initdone ){
		initdone = 1;
		home = getenv("HOME");
		lib = getenv("SYSLIB");
		MMS_putenv("MAILCAPS",MailcapPath,home?home:"",lib?lib:"");
		ProcessMailcapFiles();
	}
}


MMS_mailtoAvailableCtype(actype)
	char *actype;
{	struct MailcapEntry *mc;
	char *ctype;

	MMS_ProcessMailcap(0);
	for(mc = FirstMailcapEntry; mc; mc = mc->next ){
		ctype = mc->contenttype;
		if(ctype && index(ctype,'/') && !index(ctype,'*'))
			if( strcmp(ctype,actype) == 0 )
				return 1;
	}
	return 0;
}
MMS_mailtoPrintTypes(out)
	FILE *out;
{	struct MailcapEntry *mc;
	char *ctype;

	MMS_ProcessMailcap(0);
	for(mc = FirstMailcapEntry; mc; mc = mc->next ){
		ctype = mc->contenttype;
		if(ctype && index(ctype,'/') && !index(ctype,'*'))
			fprintf(out,"%s\n",mc->contenttype);
	}
}

char *
MMS_mailtoGetCommand(ctype)
	char *ctype;
{	char maintype[128],subtype[128],amaintype[128],asubtype[128];
	char *actype;
	struct MailcapEntry *mc;

	MMS_ProcessMailcap(0);

	strcpy(subtype,"*");
	sscanf(ctype,"%[^/]/%s",maintype,subtype);

	for(mc = FirstMailcapEntry; mc; mc = mc->next ){
		if( actype = mc->contenttype ){
			if( strcmp(ctype,actype) == 0 )
				return mc->command;

			sscanf(actype,"%[^/]/%s",amaintype,asubtype);
			if( strcmp(maintype,amaintype) == 0 ){
				if( strcmp(asubtype,"*") == 0 )
					return mc->command;
			}
		}
	}
	return 0;
}

MMS_mailtoFinalize(){
	return finalize();
}

char *
MMS_freshcopy(str){
	extern char *freshcopy();
	return freshcopy(str);
}


static write_to_file(fp)
	FILE *fp;
{	int size;

	fflush(fpout);
	if( NGList ) fprintf(fp,"Newsgroups: %s\n",NGList);

	WriteOutMessage(fp, ToList, Subject, CCList, FirstPart);
	TempCloseStyles(fp);

	fflush(fp);
	size = ftell(fp);

	if( fp != stdout )
		MMS_fseek(fp,0,0);
	
	return size;
}
FILE *
MMS_synthWriteFile(file,sizep,encode_head)
	char *file;
	int *sizep;
{	FILE *tfp,*fp;
	int size = -1;

	if( file == 0 )
		fp = stdout;
	else{
		if( *file == 0 )
			sprintf(file,"%s",tmpname());

		fp = fopen(file,"w+");
		if( fp == NULL )
			goto EXIT;
	}
	if( encode_head ){
		tfp = tmpfile();
		write_to_file(tfp);
		fflush(tfp);
		fseek(tfp,0,0);
		MIME_headerEncode(tfp,fp);
		fclose(tfp);
		fflush(fp);
		size = ftell(fp);
		fseek(fp,0,0);
	}else	size = write_to_file(fp);

EXIT:
	if( sizep )
		*sizep = size;
	return fp;
}
