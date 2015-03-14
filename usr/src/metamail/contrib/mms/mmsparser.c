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
Program:      mmsparser.c (MIME message parser)
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:

History:
  v0.0  92.05.02     created a small prototype
///////////////////////////////////////////////////////////////////////*/

#include <stdio.h>
#include <ctype.h>
char *index();

#define STREQ(a,b)	(strcmp(a,b)==0)
#define STRCASEEQ(a,b)	(strcasecmp(a,b)==0)
char *RFC822_getHeader1();


typedef char *FieldName;

FieldName H_SUBJECT		="subject";
FieldName H_FROM		="from";
FieldName H_DATE		="date";
FieldName H_MIME_VERSION	="mime-version";
FieldName H_CONTENT_ID		="content-id";
FieldName H_CONTENT_DESC	="content-description";
FieldName H_CONTENT_ENCODE	="content-transfer-encoding";
FieldName H_CONTENT_TYPE	="content-type";

FieldName HC_TYPE		="type";
FieldName HC_CHARSET		="charset";
FieldName HC_BOUNDARY		="boundary";
FieldName HC_ACCESS_TYPE	="access-type";
FieldName HC_DIRECTORY		="directory";
FieldName HC_NAME		="name";
FieldName HC_SITE		="site";
FieldName HC_MODE		="mode";


int XH_HEAD = 0;
int XH_SUBJECT;
int XH_FROM;
int XH_DATE;
int XH_MIME_VERSION;
int XH_CONTENT_ID;
int XH_CONTENT_DESC;
int XH_CONTENT_ENCODE;
int XH_CONTENT_TYPE;

int XC_TYPE;
int XC_CHARSET;
int XC_BOUNDARY;
int XC_ACCESS_TYPE;
int XC_DIRECTORY;
int XC_NAME;
int XC_SITE;
int XC_MODE;

typedef int (*IFUNCP)();
typedef struct {
	int	h_nent;
	struct {
		int	h_parent;
		int	h_dumpval;
		char   *h_name;
		IFUNCP	h_parser;
	} h_ents[256];
} MimeParams;
MimeParams headers;

static add_parser1(parent,fieldname,parser,dumpval)
	char *fieldname;
	IFUNCP parser;
{	int fid;
	MimeParams *tab = &headers;

	fid = ++tab->h_nent;
	tab->h_ents[fid].h_parent = parent;
	tab->h_ents[fid].h_name = fieldname;
	tab->h_ents[fid].h_dumpval = dumpval;
	tab->h_ents[fid].h_parser = parser;
	return fid;
}

int MMS_parseContentType();
static init_headers(){
  if( headers.h_nent )
	return;
 XH_FROM		= add_parser1(XH_HEAD,	H_FROM,		0,1);
 XH_DATE		= add_parser1(XH_HEAD,	H_DATE,		0,1);
 XH_SUBJECT		= add_parser1(XH_HEAD, 	H_SUBJECT,	0,1);
 XH_MIME_VERSION	= add_parser1(XH_HEAD,	H_MIME_VERSION,	0,1);
 XH_CONTENT_ID		= add_parser1(XH_HEAD, 	H_CONTENT_ID,	0,0);
 XH_CONTENT_DESC	= add_parser1(XH_HEAD,	H_CONTENT_DESC,	0,1);
 XH_CONTENT_ENCODE	= add_parser1(XH_HEAD,	H_CONTENT_ENCODE,0,0);
 XH_CONTENT_TYPE	= add_parser1(XH_HEAD,	H_CONTENT_TYPE,
				  	     MMS_parseContentType,0);

 XC_TYPE	= add_parser1(XH_CONTENT_TYPE,	HC_TYPE,	0,1);
 XC_CHARSET	= add_parser1(XH_CONTENT_TYPE,	HC_CHARSET,	0,1);
 XC_BOUNDARY	= add_parser1(XH_CONTENT_TYPE,	HC_BOUNDARY,	0,0);
 XC_ACCESS_TYPE	= add_parser1(XH_CONTENT_TYPE,	HC_ACCESS_TYPE, 0,0);
 XC_DIRECTORY	= add_parser1(XH_CONTENT_TYPE,	HC_DIRECTORY,	0,0);
 XC_NAME	= add_parser1(XH_CONTENT_TYPE,	HC_NAME,	0,0);
 XC_SITE	= add_parser1(XH_CONTENT_TYPE,	HC_SITE,	0,0);
 XC_MODE	= add_parser1(XH_CONTENT_TYPE,	HC_MODE,	0,0);
}

MIME_fieldid(parent,fieldname)
	char *fieldname;
{	MimeParams *htab = &headers;
	int fi;

	for( fi = 1; fi <= htab->h_nent; fi++ ){
		if( htab->h_ents[fi].h_parent == parent ){
			if( STRCASEEQ(fieldname,htab->h_ents[fi].h_name) )
				return fi;
		}
	}
	return 0;
}
char *
MIME_fieldname(fid,fname)
	char *fname;
{	char *dp;

	strcpy(fname,headers.h_ents[fid].h_name);
	if( islower(*fname) )
		*fname = toupper(*fname);
	for(dp = fname; dp = index(dp,'-'); dp++)
		if( dp[1] && islower(dp[1]) )
			dp[1] = toupper(dp[1]);

	return fname;
}

/*////////////////////////////////////////////////////////////////////////
 *	environment of MIME parts
 */
typedef struct {
	int	m_type;
	char	*m_value;
} MimeValue;
typedef struct MimePart {
	struct MimePart *p_parent;
	struct MimePart	*p_child1;
	struct MimePart	*p_next,*p_prev;
	int		 p_headoffset;
	int		 p_bodyoffset;
	int		 p_tailoffset;
	int		 p_begln,p_endln;
	int		 p_serial;
	int		 p_nchildren;
	int		 p_nthchild;
	MimeValue	 p_attributes[256];
} MimePart;

typedef struct {
	FILE	*in;
	FILE	*out_file;
	int	 in_lines;
	int	 in_bytes;
	int	 in_count;
} STREAM;

static char *
FGETS(s,z,io)
	char *s;
	STREAM *io;
{	char *r;

	if( io->in_bytes )
		if( io->in_bytes < io->in_count )
			return NULL;
	r = fgets(s,z,io->in);
	if( r != NULL ){
		io->in_lines++;
		io->in_count += strlen(s);
	}
	return r;
}

MIME_InitPart(io,mp,parent)
	STREAM *io;
	MimePart *mp,*parent;
{
	mp->p_headoffset = MMS_ftell(io->in);
	mp->p_begln = io->in_lines;
	mp->p_parent = parent;
	if( parent ){
		if( parent->p_child1 == 0 )
			parent->p_child1 = mp;
		parent->p_nchildren++;
		mp->p_nthchild = parent->p_nchildren;
	}
}

MimePart *Mime_parts[256];
int MIME_part_serial;
static MimePart *Mime_root;

static
MIME_set_default_values(mp)
	MimePart *mp;
{
	MIME_setvalue(mp,XC_TYPE,"text/plain");
	MIME_setvalue(mp,XC_CHARSET,"us-ascii");
}

MimePart *
MIME_NewPart(io,parent,prev)
	STREAM *io;
	MimePart *parent,*prev;
{	MimePart *newpart;

	newpart = (MimePart*)calloc(sizeof(MimePart),1);
	newpart->p_serial = MIME_part_serial++;
	Mime_parts[newpart->p_serial] = newpart;

	MIME_set_default_values(newpart);
	MIME_InitPart(io,newpart,parent);
	if( prev ){
		prev->p_next = newpart;
		newpart->p_prev = prev;
	}
	return newpart;
}
static
MimePart TmpPart;
MimePart *
MIME_partTmp(parent)
	MimePart *parent;
{
	TmpPart.p_parent= parent;
	MIME_set_default_values(&TmpPart);
	return &TmpPart;
}

MimePart *MMS_parserBytes();
MimePart *
MMS_parsePartBody(in,part)
	FILE *in;
	MimePart *part;
{	MimePart *tmp;
	int bodysize;
	STREAM iob,*io=&iob;

	tmp = MIME_partTmp(part);
	fseek(in,part->p_bodyoffset,0);
	bodysize = part->p_tailoffset-part->p_bodyoffset;

	MMS_setIO(io,in,NULL,bodysize);

	tmp->p_headoffset = MMS_ftell(in);
	MMS_parseHeader(io,tmp);
	tmp->p_bodyoffset = MMS_ftell(in);
	MMS_parseBody(io,tmp);
	tmp->p_tailoffset = MMS_ftell(in);
	return tmp;
}

MIME_parserInit(io)
	STREAM *io;
{
	init_headers();
	MIME_part_serial = 0;
	Mime_root = MIME_NewPart(io,0,0);
}

char *
MIME_getvalue(mp,mtype)
	MimePart *mp;
{	int vi,xtype;

	for(vi = 0; xtype = mp->p_attributes[vi].m_type; vi++)
		if( xtype == mtype )
			break;
	return mp->p_attributes[vi].m_value;
}
MIME_setvalue(mp,mtype,value)
	MimePart *mp;
	char *value;
{	int vi,xtype;
	char *ovalue;

	for(vi = 0; xtype = mp->p_attributes[vi].m_type; vi++)
		if( xtype == mtype )
			break;
	mp->p_attributes[vi].m_type = mtype;
	if( ovalue = mp->p_attributes[vi].m_value )
		free(ovalue);
	mp->p_attributes[vi].m_value = (char*)malloc(strlen(value)+1);
	strcpy(mp->p_attributes[vi].m_value,value);
}
static sectionNo(mp,sec)
	MimePart *mp;
	char *sec;
{
	if( mp->p_parent ){
		sectionNo(mp->p_parent,sec);
		sprintf(sec+strlen(sec),"%d.",mp->p_nthchild);
	}
}
MMS_partSectionNO(sec,mp)
	char *sec;
{
	*sec = 0;
	sectionNo(mp,sec);
	if( *sec )
		sec[strlen(sec)-1] = 0;
}
MimePart *MMS_partNext(mp) MimePart *mp; {
	int partid = mp->p_serial + 1;

	if( MIME_part_serial <= partid )
		return 0;
	else	return Mime_parts[partid];
}
MimePart *MMS_partPrev(mp) MimePart *mp; { return Mime_parts[mp->p_serial-1]; }
MimePart *MMS_partNextPeer(mp) MimePart *mp; { return mp->p_next; }
MimePart *MMS_partPrevPeer(mp) MimePart *mp; { return mp->p_prev; }
MimePart *MMS_partParent(mp)   MimePart *mp; { return mp->p_parent; }
MimePart *MMS_part1stChild(mp) MimePart *mp; { return mp->p_child1; }
MimePart *MMS_partNthSection(mp,nth) MimePart *mp; {
	int nc;

	if( nth <= 1 )
		return mp;

	if( mp = mp->p_child1 ){
		for(nc = 2; mp && nc < nth; nc++)
			mp = MMS_partNext(mp);
	}
	return mp;
}
MimePart *MMS_partBySectionNO(section)
	char *section;
{	char sec[32];
	MimePart *mp;

	for( mp = Mime_root; mp; mp = MMS_partNext(mp) ){
		MMS_partSectionNO(sec,mp);
		if( strcmp(sec,section) == 0 )
			return mp;
	}
	return 0;
}

MimePart *MMS_partNthPart(mp,nth){ return MMS_partNthSection(mp,nth); }
int	  MMS_partIsAncestorOf(amp,mp) MimePart *amp,*mp; {
	int i;

	for(i = 0; mp; i++){
		if( mp == amp )
			return i;
		mp = mp->p_parent;
	}
	return 0;
}

int       MMS_partSerialNO(mp) MimePart *mp; { return mp->p_serial; }
int       MMS_partHeadOffset(mp) MimePart *mp; { return mp->p_headoffset; }
int       MMS_partBodyOffset(mp) MimePart *mp; { return mp->p_bodyoffset; }
int       MMS_partTailOffset(mp) MimePart *mp; { return mp->p_tailoffset; }
int	  MMS_partSize(mp) MimePart *mp; {
				return mp->p_tailoffset - mp->p_headoffset; }
char	 *MMS_partContentType(mp) MimePart *mp; {
		return MIME_getvalue(mp,XC_TYPE); }
char	 *MMS_partContentTypeParams(mp) MimePart *mp; {
		return MIME_getvalue(mp,XH_CONTENT_TYPE); }
char	 *MMS_partContentEncode(mp) MimePart *mp; {
		return MIME_getvalue(mp,XH_CONTENT_ENCODE); }
char	 *MMS_partContentDescription(mp) MimePart *mp; {
		return MIME_getvalue(mp,XH_CONTENT_DESC); }


MIME_dumpStat(out,mp)
	FILE *out;
	MimePart *mp;
{	char sec[128];

	MMS_partSectionNO(sec,mp);
	fprintf(out,"%d section=%s head=%d tail=%d line=%d parent=%d",
		mp->p_serial,
		sec,
		mp->p_headoffset,
		mp->p_tailoffset,
		mp->p_begln,
		mp->p_parent ? mp->p_parent->p_serial : 0);

	if( mp->p_nchildren )
		fprintf(out," nchildren=%d 1stchild=%d",
			mp->p_nchildren,
			mp->p_child1->p_serial);

	if( mp->p_next )
		fprintf(out," next=%d\n",mp->p_next->p_serial);
	else	fprintf(out,"\n");
}
MIME_dumpValues(out,mp,prefix)
	FILE *out;
	MimePart *mp;
	char *prefix;
{	int vi,xtype,parent;
	char buf[128];

	for(vi = 0; xtype = mp->p_attributes[vi].m_type; vi++){
		if( headers.h_ents[xtype].h_dumpval == 0 )
			continue;

		fprintf(out,"%s",prefix);
		if( parent = headers.h_ents[xtype].h_parent )
			fprintf(out,"%s/",MIME_fieldname(parent,buf));

		fprintf(out,"%s: %s\n",
			MIME_fieldname(xtype,buf),
			mp->p_attributes[vi].m_value);
	}
	fflush(out);
}

MMS_partIsMultipart(mp)
	MimePart *mp;
{
	return MIME_getvalue(mp,XC_BOUNDARY) != 0;
}
MMS_partIsMultipartAlternative(mp)
	MimePart *mp;
{	char *ctype;

	ctype = MMS_partContentType(mp);
	if( strcasecmp(ctype,"multipart/alternative") == 0 )
		return 1;
	else	return 0;
}
char*
MMS_partIsText(mp)
	MimePart *mp;
{	char *ctype;

	ctype = MMS_partContentType(mp);
	if( strncmp(ctype,"text/",5) == 0 )
		return ctype+5;
	return 0;
}
MMS_partIsPlainText(mp)
	MimePart *mp;
{	char *subtype;

	if( subtype = MMS_partIsText(mp) ) 
		return strcmp(subtype,"plain") == 0;
	return 0;
}
MMS_partIsAlternativePlainText(mp)
	MimePart *mp;
{	MimePart *parent;

	if( MMS_partIsPlainText(mp) ){
		parent = MMS_partParent(mp);
		return parent && MMS_partIsMultipartAlternative(parent);
	}
	return 0;
}
MMS_partIsExternalBody(mp)
	MimePart *mp;
{	char *ctype;

	ctype = MMS_partContentType(mp);
	return STRCASEEQ(ctype,"message/external-body");
}
char *
MMS_partAccessType(mp)
	MimePart *mp;
{
	return MIME_getvalue(mp,XC_ACCESS_TYPE);
}

MIME_active_boundary(current)
	MimePart *current;
{	MimePart *mp;
	char *boundary;

	for( mp = current; mp; mp = mp->p_parent ){
		if( boundary = MIME_getvalue(mp,XC_BOUNDARY) )
			printf("%d: %s\n",mp->p_serial,boundary);
	}
}
static
MIME_on_boundary(current,line)
	MimePart *current;
	char *line;
{	MimePart *mp;

	if( line[0] == '-' && line[1] == '-' ){
		line += 2;
		for( mp = current; mp; mp = mp->p_parent ){
			char *boundary;
			int len;

			if( boundary = MIME_getvalue(mp,XC_BOUNDARY) ){
				len = strlen(boundary);
				if( strncmp(line,boundary,len)==0 ){
					if( line[len] == 0 )
						return  mp->p_serial;
					if( strcmp(&line[len],"--") == 0 )
						return -mp->p_serial;
				}
			}
		}
	}
	return 0;
}
char *
MMS_partInheritHeader(mp,field)
	MimePart *mp;
	char *field;
{	int parent,fid;
	char fieldname[256],*val;

	strcpy(fieldname,field);
	parent = XH_HEAD;
	fid = MIME_fieldid(parent,fieldname);

	for(; mp; mp = mp->p_parent )
		if( val = MIME_getvalue(mp,fid) )
			return val;
	return 0;
}
char *
MMS_partContentParam(mp,param)
	MimePart *mp;
	char *param;
{	int fid;
	
	fid = MIME_fieldid(XH_CONTENT_TYPE,param);
	if( fid == 0 )
		return (char*)-1;
	return MIME_getvalue(mp,fid);
}

MIME_call_parser(env,fieldid,body)
	MimePart *env;
	char *body;
{	IFUNCP parser;

	if( parser = headers.h_ents[fieldid].h_parser ){
		(*parser)(env,body);
	}
}

/*////////////////////////////////////////////////////////////////////////
 *
 */

MMS_setIO(io,in,out,bytes)
	STREAM *io;
	FILE *in,*out;
{
	io->in = in;
	io->in_lines = 0;
	io->in_bytes = bytes;
	io->in_count = 0;
	io->out_file = out;
}

MimePart *
MMS_parserBytes(in,out,bytes)
	FILE *in,*out;
{	MimePart *current;
	STREAM iob,*io=&iob;

	MMS_setIO(io,in,out,bytes);
	MIME_parserInit(io);
	current = MIME_NewPart(io,Mime_root,0);
	MMS_parsePart(io,Mime_root,current);
	return current;
}
MimePart *
MMS_parser(in,out)
	FILE *in,*out;
{
	return MMS_parserBytes(in,out,0);
}

MMS_parseParts(io,parts)
	STREAM *io;
	MimePart *parts;
{	MimePart *part1,*prev = 0;
	int exitto;

	for(;;){
		part1 = MIME_NewPart(io,parts,prev);
		exitto = MMS_parsePart(io,parts,part1);
		if( exitto != parts->p_serial )
			break;
		prev = part1;
	}
	return exitto;
}
MMS_parsePart(io,parent,current)
	STREAM *io;
	MimePart *parent,*current;
{	int exitto = 0;

	MMS_parseHeader(io,current);
	current->p_bodyoffset = MMS_ftell(io->in);

	if( MMS_partIsMultipart(current) ){
		exitto = MMS_parseBody(io,current);
		if( exitto == current->p_serial )
			exitto = MMS_parseParts(io,current);
		if( exitto < 0 && -exitto == current->p_serial )
			exitto = MMS_parseBody(io,current);
	}else	exitto = MMS_parseBody(io,current);
	current->p_tailoffset = MMS_ftell(io->in);
	return exitto;
}
MMS_parseBody(io,current)
	STREAM *io;
	MimePart *current;
{	char line[256];
	int nline;
	int exitto = 0;

	for(nline = 1;;nline++){
		if( FGETS(line,sizeof(line),io) == NULL )
			break;

		line[strlen(line)-1] = 0;
		if( exitto = MIME_on_boundary(current,line) )
			break;
	}
	return exitto;
}

MMS_parseHeader(io,env)
	STREAM *io;
	MimePart *env;
{	char fname[256],fbody[0x8000];
	int fid;

	while( RFC822_getHeader1(io,fname,fbody,sizeof(fbody)) != NULL ){
		if( fid = MIME_fieldid(XH_HEAD,fname) ){
			MIME_setvalue(env,fid,fbody);
			MIME_call_parser(env,fid,fbody);
		}
	}
}

MMS_parseContentType(env,values)
	MimePart *env;
	char *values;
{	char *sp,*nsp,value1[256],name[256];

	if( nsp = index(values,';') )
		*nsp++ = 0;

	sscanf(values,"%s",value1);
	MIME_setvalue(env,XC_TYPE,value1);
	while( nsp ){
		sp = nsp;
		if( nsp = index(sp,';') )
			*nsp++ = 0;

		if( sscanf(sp," %[^=]=%s",name,value1) == 2 ){
			int fid;
			if( fid = MIME_fieldid(XH_CONTENT_TYPE,name) ){
				if( value1[0] == '"' )
					strcpy(value1,value1+1);
				if( value1[strlen(value1)-1] == '"' )
					value1[strlen(value1)-1] = 0;
				MIME_setvalue(env,fid,value1);
			}
		}
	}
}


/*////////////////////////////////////////////////////////////////////////
 *	RFC822
 */
static char *
RFC822_getHeader1(io,fname,fbody,bsize)
	STREAM *io;
	char *fname,*fbody;
{	char line[128],*tail,peekc;
	FILE *in = io->in;

	*fname = *fbody = 0;
	if( FGETS(line,sizeof(line),io) == NULL )
		return NULL;

	if( *line == '\n' )
		return NULL;

	if( sscanf(line,"%[^: ]%*[: ]%[^\377]",fname,fbody) != 2 )
		return NULL;

	tail = fbody;
	for(;;){
		tail = tail + strlen(tail);
		peekc = fgetc(in);
		if( peekc != ' ' && peekc != '\t' )
			break;
		*tail++ = peekc;
		if( FGETS(tail,bsize,io) == NULL )
			break;
	}
	ungetc(peekc,in);

	if( fbody < tail && tail[-1] == '\n' )
		tail[-1] = 0;

	return fbody;
}
