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
Program:      mmstransl.c (MIME message translator)
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:

History:
  v0.0  92.05.05     created a small prototype
///////////////////////////////////////////////////////////////////////*/

#include <stdio.h>
typedef int MimePartP;
typedef int (*IFUNCP)();
#define STREQ(a,b)	(strcmp(a,b)==0)

/*/////////////////////////////////////////////////////////////////////*/

int MMS_translSh();
int MMS_translDir();
int MMS_translRTF();
int MMS_translVin();

MMS_E_transl(ac,av,in,out)
	char *av[];
{	IFUNCP translator;

	translator = MMS_translSh; /* :-) */
	if( 1 < ac ){
		char *lang;

		lang = &av[1][1];
		if(STREQ(lang,"sh" )) translator = MMS_translSh;  else
		if(STREQ(lang,"dir")) translator = MMS_translDir; else
		if(STREQ(lang,"rtf")) translator = MMS_translRTF; else
		if(STREQ(lang,"vin")) translator = MMS_translVin;
	}
	MMS_transl(in,out,translator);
}
int (*MMS_E_TRANSL)() = MMS_E_transl;

/*/////////////////////////////////////////////////////////////////////*/
#define MMS_T_START		0
#define MMS_T_END		1
#define MMS_T_PART		2
#define MMS_T_MULTIPART_BEGIN	3
#define MMS_T_MULTIPART_END	4

MMS_transl(in,out,translator)
	FILE *in,*out;
	IFUNCP translator;
{	MimePartP root;

	if( !MMS_seekable(in) )
		in = (FILE*)MMS_saveTempfile(in,0);
	root = MMS_parser(in,out);
	(*translator)(in,out,root,MMS_T_START);
	MMS_translPart(in,out,root,translator);
	(*translator)(in,out,root,MMS_T_END);
}


MMS_translPart(in,out,mp,translator)
	FILE *in,*out;
	MimePartP mp;
	IFUNCP translator;
{
	if( MMS_partIsMultipart(mp) ){
		(*translator)(in,out,mp,MMS_T_MULTIPART_BEGIN);
		mp = MMS_translParts(in,out,mp,translator);
		(*translator)(in,out,mp,MMS_T_MULTIPART_END);
	}else{
		(*translator)(in,out,mp,MMS_T_PART);
	}
	return mp;
}

MMS_translParts(in,out,mp,translator)
	FILE *in,*out;
	IFUNCP translator;
{
	mp = MMS_part1stChild(mp);
	while( mp ){
		MMS_translPart(in,out,mp,translator);
		mp = MMS_partNextPeer(mp);
	}
	return mp;
}

/*/////////////////////////////////////////////////////////////////////
 *	MIME to Sh (shell script)
 */
static char *MMS_T_END_Sh = "\
#####################\n\
PARTq(){ rm -rf $MMS_TMPFILES; exit; }\n\
NEXTPART=1\n\
echo -n \"[enter part number or RETURN($NEXTPART) or q]\"\n\
while read COMMAND\n\
do\n\
	if [ \"$COMMAND\" = '' ]; then\n\
		COMMAND=$NEXTPART\n\
		echo \"($COMMAND)\"\n\
	fi\n\
	eval \"PART$COMMAND\"\n\
	echo -n \"[enter part number or RETURN($NEXTPART) or q]\"\n\
done\n\
PARTq\n\
";
MMS_translSh(in,out,mp,where)
	FILE *in,*out;
	MimePartP mp;
{	char holophrast[128];
	char *decoder;
	char viewer[1024];
	char tmpfile[1024];
	static char tmpid;
	int usetemp;
	int serial;

    switch( where ){
	case MMS_T_START: break;
	case MMS_T_END: fprintf(out,"%s",MMS_T_END_Sh); break;

	case MMS_T_PART:
	case MMS_T_MULTIPART_BEGIN:
		serial = MMS_partSerialNO(mp);
		MMS_holophrast(holophrast,mp);
		fprintf(out,"########### (%d)  %s\n",serial,holophrast);
		MIME_dumpValues(out,mp,"# ");
		fprintf(out,"echo '(%d)  %s'\n",serial,holophrast);
		fprintf(out,"PART%d(){\n",serial);

		if( where == MMS_T_PART ){
			sprintf(tmpfile,"/tmp/mms.$$.%d",tmpid);
			if( usetemp = MMS_BuildViewer(in,mp,viewer,tmpfile) ){
				fprintf(out,
					"MMS_TMPFILES=\"$MMS_TMPFILES %s\"\n",
					tmpfile);
				tmpid++;
			}
			decoder = (char*)MIME_Decoder(mp);
			if( decoder )
				fprintf(out,"(%s << EOT\n",decoder);
			else	fprintf(out,"(cat << EOT\n");

			MMS_partWriteBody(in,out,mp,0,0);

			if( usetemp )
				fprintf(out,"EOT\n)>%s\n%s\n",tmpfile,viewer);
			else	fprintf(out,"EOT\n) | %s\n\n",viewer);
		}else{
			fprintf(out,"echo '%s'\n",holophrast);
		}
		if( MMS_partNext(mp) )
			fprintf(out,"NEXTPART=%d\n",serial+1);
		else	fprintf(out,"NEXTPART=q\n");
		fprintf(out,"}\n");
		break;
    }
}

/*/////////////////////////////////////////////////////////////////////
 *	MIME to Directory Tree
 */
MMS_translDir(in,out,mp,where)
	FILE *in,*out;
	MimePartP mp;
{
}

/*/////////////////////////////////////////////////////////////////////
 *	MIME to RTF (Rich Text Format)
 */
MMS_translRTF(in,out,mp,where)
	FILE *in,*out;
	MimePartP mp;
{
}

/*/////////////////////////////////////////////////////////////////////
 *	MIME to Vin
 */
MMS_translVin(in,out,mp,where)
	FILE *in,*out;
	MimePartP mp;
{
}
