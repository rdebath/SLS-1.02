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
Program:      mmsviewer.c (MIME message viewer)
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:

History:
        92.05.03     created a small prototype
        92.05.13     added HOLOPHRASTING function
///////////////////////////////////////////////////////////////////////*/

#include <stdio.h>
#include <ctype.h>
char *getenv();

typedef int MimePartP;
char *MMS_partContentType();
char *MMS_partContentDescription();

/*//////////////////////////////////////////////////////////////////////*/

MMS_E_viewer(ac,av,in,out)
	char *av[];
	FILE *in,*out;
{	FILE *in1;

	if( 1 < ac ){
		in1 = fopen(av[1],"r");
		if( in1 == NULL ){
			fprintf(stderr,"mmsviewer: unknown file '%s'\n",av[1]);
			exit(-1);
		}
		in = in1;
	}
	MMS_viewer(in,out);
}
int (*MMS_E_VIEWER)() = MMS_E_viewer;

/*//////////////////////////////////////////////////////////////////////*/

int MMS_V_HEAD		= 1;
int MMS_V_BODY		= 2;
int MMS_V_ARTICLE	= 3;

/*////////////////////////////////////////////////////////////////////////
 *	SIMPLE VIEWER
 */
static winclear(mp){
	char *cd;

	if( cd = getenv("MMS_CLEARDISPLAY") ){
		printf("%s",cd);
		fflush(stdout);
	}else	system("clear");
}
int (*MMS_BEFORE_viewZoom1)();

MMS_viewer(in,out)
	FILE *in,*out;
{	MimePartP root;
	FILE *tty;

	if( !MMS_seekable(in) )
		in = (FILE*)MMS_saveTempfile(in,0);
	root = MMS_parser(in,out);

	tty = fopen("/dev/tty","r");
	if( tty == NULL )
		return;

	if( in == stdin ){
		in = fdopen(dup(fileno(stdin)),"r");
		close(fileno(stdin));
		dup(fileno(tty));
	}

	MMS_BEFORE_viewZoom1 = winclear;
	MMS_Viewer(in,out,root,0,tty);
	fclose(tty);
}

MMS_viewNthPart(in,out,root,nth)
	FILE *in,*out;
	MimePartP root;
{	MimePartP mp;

	if( root )
	if( mp = MMS_partNthPart(root,nth) )
		MMS_viewCocked(in,out,mp);
}

static char *
MMS_std_comget(msg,command,size,comin)
	char *msg,*command;
	FILE *comin;
{
	fprintf(stdout,"[ %s ] ",msg);
	fflush(stdout);
	return fgets(command,size,comin);
}
#define STRTAIL(s)	(s+strlen(s))
MMS_Viewer(in,out,rootmp,comfunc,comin)
	FILE *in,*out,*comin;
	MimePartP rootmp;
	char *(*comfunc)();
{	char command[128],msg[256],*comp;
	MimePartP mp,pmp;
	int numarg = 0;

	if( comfunc == 0 )
		comfunc = MMS_std_comget;

	mp = rootmp;
	MMS_viewZoom1(in,out,rootmp,mp);
	for(;;){
		printf(">>>> "); MIME_dumpStat(out,mp);
		sprintf(msg,"q:quit s:skeleton ");
		if(!MMS_partIsMultipart(mp))sprintf(STRTAIL(msg),"v:view ");
		if( MMS_partNextPeer(mp)   )sprintf(STRTAIL(msg),"n:next ");
		if( MMS_partPrevPeer(mp)   )sprintf(STRTAIL(msg),"p:prev ");
		if( MMS_part1stChild(mp)   )sprintf(STRTAIL(msg),"d:down ");
		if( MMS_partParent(mp)     )sprintf(STRTAIL(msg),"u:up ");
		sprintf(STRTAIL(msg),"Ng:goto-Nth ");
		if( MMS_partNext(mp) )
			sprintf(STRTAIL(msg),"RETURN:print-next");
		else	sprintf(STRTAIL(msg),"RETURN:quit");

		(*comfunc)(msg,command,sizeof(command),comin);
		if( *command == '\n' ){
			if( MMS_partNext(mp) == 0 )
				break;
			else	strcpy(command,"+a");
		}

		for(comp = command; *comp; comp++ ){
		    pmp = mp;
		    numarg = 0;
		    if( isdigit(*comp) ){
			do {
				numarg = numarg * 10 + (*comp - '0');
				comp++;
			} while( isdigit(*comp) );
			if( *comp == '\n' )
				strcpy(comp,"g");
		    }
		    switch( *comp ){
			case '+': mp = MMS_partNext(mp); break;
			case '-': mp = MMS_partPrev(mp); break;

			case 'j': case 'n':
				if( mp = MMS_partNextPeer(mp) )
					MMS_viewZoom1(in,out,rootmp,mp);break;
			case 'k': case 'p':
				if( mp = MMS_partPrevPeer(mp) )
					MMS_viewZoom1(in,out,rootmp,mp);break;
			case 'i': case 'd':
				if( mp = MMS_part1stChild(mp) )
					MMS_viewZoom1(in,out,rootmp,mp);break;
			case 'o': case 'u':
				if( mp = MMS_partParent(mp) )
					MMS_viewZoom1(in,out,rootmp,mp);break;
			case 'g':
				if( mp = MMS_partNthSection(rootmp,numarg) )
				  MMS_viewZoom1(in,out,rootmp,mp);	break;

			case 'h': MMS_viewAsis(in,out,mp,MMS_V_HEAD);	break;
			case 'b': MMS_viewAsis(in,out,mp,MMS_V_BODY);	break;
			case 'a': MMS_viewZoom1(in,out,rootmp,mp);	break;
			case 'v': MMS_viewCocked(in,out,mp);		break;
			case 'w': MMS_writetotmp(in,mp);		break;

			case 's': MMS_viewSkeleton(in,out,mp); break;

			case 'q': goto EXIT;
		    }
		    if( mp == 0 )
			mp = pmp;
		}
	}
	EXIT:;
}

MMS_BuildCommand(command,tmpfile,comspec,in,mp)
	char *command,*tmpfile,*comspec;
	FILE *in;
	MimePartP mp;
{	char *sp,*cp,*tp;
	int usetemp = 0;
	char *tmpfn;
	FILE *tmpfp;

	command[0] = 0; 
	sp = comspec;
	cp = command;
	while( *sp ){
	    if( *sp == '%' ){
		sp++;
		switch(*sp){
		    case 's': /* FILENAME */
			if( usetemp == 0 ){
			    if( *tmpfile == 0 ){
				strcpy(tmpfile,tmpname());
				tmpfp = fopen(tmpfile,"w+");
				MMS_partWriteBody(in,tmpfp,mp,0,1);
			    }
			    usetemp = 1;
			}
			strcpy(cp,tmpfile);
			cp += strlen(cp);
			sp++;
			break;

	/* PARAMETER */
		    case '{':
			{	char *fp;

			tp = (char*)index(sp,'}');
			if( tp == 0 )
				break;
			sscanf(sp+1,"%[^}]",cp);
			MMS_tolowers(cp);

			fp = (char*)MMS_partContentParam(mp,cp);
			if( fp < 0 ){
				fprintf(stderr,"Unknown Param: %s\n",cp);
			}else{
				if( fp == 0 )
					strcpy(cp,"\"\"");
				else	strcpy(cp,fp);
				cp += strlen(cp);
			}
			sp = tp + 1;
			break;
			}

		    case 't':
			strcpy(cp,MMS_partContentType(mp));
			cp += strlen(cp);
			sp++;
			break;

		    default:
			break;
		}
	    }else{
		*cp++ = *sp++;
	    }
	}
	*cp = 0;
	return usetemp;
}

MMS_BuildViewer(in,mp,command,tmpfile)
	MimePartP mp;
	char *command,*tmpfile;
{	char *ctype,*viewer;

	ctype = MMS_partContentType(mp);
	if( ctype == 0 ){
		printf("cannot get content-type\n");
		return -1;
	}
	viewer = (char*)MMS_mailtoGetCommand(ctype);
	if( viewer == 0 ){
		printf("cannot get viewer for %s\n",ctype);
		return -1;
	}
	return MMS_BuildCommand(command,tmpfile,viewer,in,mp);
}

#include <signal.h>
MMS_viewCocked(in,out,mp)
	FILE *in,*out;
	MimePartP mp;
{	FILE *pfp;
	void (*osig)();
	char command[1024],tmpfile[1024];
	int usetemp;

	*tmpfile = 0;
	if( (usetemp = MMS_BuildViewer(in,mp,command,tmpfile)) < 0 )
		return -1;

	if( isatty(fileno(stdout)) ){
		printf(">>>> command: %s\n",command);
		printf(">>>> input size: %dbytes Wait ...\n",MMS_partSize(mp));
	}

	if( out != stdout ){
		fflush(out);
		if( fork() ){
			wait(0);
			return 0;
		}
		close(fileno(stdout));
		dup(fileno(out));
	}

	if( usetemp ){
		system(command);
		unlink(tmpfile);
	}else{
		osig = (void*)signal(SIGPIPE,SIG_IGN);
		pfp = popen(command,"w");
		MMS_partWriteBody(in,pfp,mp,0,1);
		pclose(pfp);
		signal(SIGPIPE,osig);
	}
	if( out != stdout ){
		fflush(stdout);
		_exit(0);
	}

	return 0;
}


int MMS_WINCOLS = 76;
MMS_viewAsis(in,out,mp,headbody)
	FILE *in,*out;
	MimePartP mp;
{	int maxlines,col;

	for(col = 0; col < MMS_WINCOLS; col++) printf("="); printf("\n");

	if( MMS_partIsMultipart(mp) ){
		MMS_viewSkeleton(in,out,mp);
	}else{
		maxlines = 10;
		MMS_partWrite(in,out,mp,headbody,maxlines);
	}
	for(col = 0; col < MMS_WINCOLS; col++) printf("-"); printf("\n");
}

MMS_holophrast(holo,mp)
	char *holo;
	MimePartP mp;
{	char sec[32],*ctype,*desc;

	MMS_partSectionNO(sec,mp);
	ctype = MMS_partContentType(mp);
	sprintf(holo,"%s    %s ",sec,ctype);
	if( MMS_partIsExternalBody(mp) )
		sprintf(holo+strlen(holo),"(%s)",MMS_partAccessType(mp));
	else	sprintf(holo+strlen(holo),"(%dbytes)",MMS_partSize(mp));
}
MMS_viewZoom1(in,out,rootmp,cmp)
	MimePartP rootmp,cmp;
{
	if( MMS_BEFORE_viewZoom1 )
		(*MMS_BEFORE_viewZoom1)(cmp);
	MMS_viewHolophrast(in,out,rootmp,cmp,"",10);
	fflush(out);
}

MMS_viewHolophrast(in,out,rootmp,cmp,form,lines)
	MimePartP rootmp,cmp;
	FILE *in,*out;
	char *form;
{	MimePartP mp;
	char holo[256],sec[32],*desc;
	int col;

	for(mp = rootmp; mp; mp = MMS_partNext(mp) ){
		if( mp != rootmp && !MMS_partIsAncestorOf(rootmp,mp) )
			break;
		MMS_holophrast(holo,mp);

		col = fprintf(out,"%s(%2d) %s",form,MMS_partSerialNO(mp),holo);
		desc = MMS_partContentDescription(mp);
		if( desc ){
			if( MMS_WINCOLS < col+strlen(desc) )
				fprintf(out,"\\\n %*s\n",MMS_WINCOLS,desc);
			else	fprintf(out," %s\n",desc);
		}else	fprintf(out,"\n");

		if( mp == cmp ){
			MMS_partSectionNO(sec,mp);
			fprintf(out,
				"======== content of section %s ========\n",
				sec);

			MMS_partWrite(in,out,mp,MMS_V_ARTICLE,lines);

			for(col = 0; col < MMS_WINCOLS; col++)
				fprintf(out,"-");
			fprintf(out,"\n");
		}
	}
}

/*////////////////////////////////////////////////////////////////////////
 *
 */

MMS_writetotmp(in,mp)
	FILE *in;
	MimePartP mp;
{	FILE *out;

	out = fopen("mms.out","w+");
	MMS_partWrite(in,out,mp,MMS_V_BODY,0);
	fflush(out);
}

MMS_partWrite(in,out,mp,headbody,maxlines)
	FILE *in,*out;
	MimePartP mp;
{	char line[1024];
	int lines;
	int tail;

	MMS_seekpart(in,mp,headbody);
	if( headbody == MMS_V_HEAD )
		tail = MMS_partBodyOffset(mp);
	else	tail = MMS_partTailOffset(mp);

	for(lines = 1; ;lines++){
		if( fgets(line,sizeof(line),in) == NULL )
			break;
		if( tail <= MMS_ftell(in) )
			break;
		if( maxlines && maxlines <= lines )
			break;
		fprintf(out,"%s",line);
	}
	fflush(out);
}

MMS_partWriteHead(in,out,mp,maxlines)
	FILE *in,*out; MimePartP mp;
{	return MMS_partWrite(in,out,mp,MMS_V_HEAD,maxlines); }

MMS_partWriteBody(in,out,mp,maxlines,do_decode)
	FILE *in,*out; MimePartP mp;
{	char *encode;

	if( do_decode )
	if( encode = (char*)MMS_partContentEncode(mp) ){
		FILE *tmp = tmpfile();

		MMS_partWrite(in,tmp,mp,MMS_V_BODY,maxlines);
		MMS_fseek(tmp,0,0);
		MIME_Decode(tmp,out,encode);
		fclose(tmp);
		return;
	}
	return MMS_partWrite(in,out,mp,MMS_V_BODY,maxlines);
}

MMS_partWriteArticle(in,out,mp,maxlines)
	FILE *in,*out; MimePartP mp;
{	return MMS_partWrite(in,out,mp,MMS_V_ARTICLE,maxlines); }

MMS_seekpart(in,mp,headbody)
	FILE *in;
	MimePartP mp;
{	int off;

	if( headbody == MMS_V_BODY )
		off = MMS_partBodyOffset(mp);
	else	off = MMS_partHeadOffset(mp);
	clearerr(in);
	MMS_fseek(in,off,0);
	return off;
}

/*////////////////////////////////////////////////////////////////////////
 *
 */
MMS_viewSkeleton(in,out,mp)
	FILE *in,*out;
	MimePartP mp;
{
	while( mp ){
		MIME_dumpStat(out,mp);
		MIME_dumpValues(out,mp,"    ");
		mp = MMS_partNext(mp);
	}
}

MMS_getCockedText(in,out,mp,maxlines)
	FILE *in,*out;
	MimePartP mp;
{	char *ctype;

	ctype = MMS_partContentType(mp);
	if( !MMS_partIsText(mp) )
		return 0;

	if( MMS_partIsPlainText(mp) )
		MMS_partWriteBody(in,out,mp,maxlines,1);
	else{
		fflush(out);
		if( fork() == 0 ){
			FILE *tfp;
			static char *av[] = {"richtext","-f",0};

			tfp = tmpfile();
			MMS_partWriteBody(in,tfp,mp,maxlines,1);
			fflush(tfp);
			fseek(tfp,0,0);
			close(0); dup(fileno(tfp));
			close(1); dup(fileno(out));
			close(2); dup(fileno(out));
			richtext_main(2,av);
			_exit(0);
		}
		wait(0);
	}
	return 1;
}


MMS_partPrintf(in,out,mp)
{
}

