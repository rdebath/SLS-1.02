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
Program:      mmsserver.c  (metamail server)
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:
     This is a program that allows programs to compose/decompsea
     a MIME-format mail with the commands via socket.
History:
  v0.0  92.04.21  created a small prototype
///////////////////////////////////////////////////////////////////////*/
typedef int (*IFUNCP)();

char *MMS_versionNO = "0.5";
char *MMS_versionDate = "3 June 1992";

MMS_E_version(ac,av,in,out){
	fprintf(out,"%s\n",MMS_versionNO);
}
IFUNCP MMS_E_VERSION = MMS_E_version;

/*/////////////////////////////////////////////////////////////////////*/

MMS_E_server(ac,av,in,out)
	char *av[];
{
	MMS_server(ac,av);
}
IFUNCP MMS_E_SERVER = MMS_E_server;

int MMS_ACCESS;

/*//////////////////////////////////////////////////////////////////////*/
#include <stdio.h>
#include <sys/param.h>
#include <pwd.h>
#include <signal.h>

char *getenv(),*index(),*rindex(),*strstr();
FILE *MMS_synthWriteFile();
char *MMS_getline();
#define MMS_COMMANDIN stdin
struct passwd *MMS_user();
FILE *MMS_getbody();

/*//////////////////////////////////////////////////////////////////////*/
struct comdesc {
	int	numarg;
	int	haveresp;
	char	*shelp;
	char	*lhelp;
};
#define DESC(name,args,resp,shelp,lhelp) \
static struct comdesc name = {args,resp,shelp,lhelp};
extern struct comdesc
	MMS_H_HELP,	MMS_H_QUIT;

#define NO	0
#define RO	1
#define WO	2
#define RW	3
#define XO	4
#define RX	5
#define WX	6
#define RWX	7
#define ALL	0xFF

typedef struct {
	char	 *mms_name;
	int	(*mms_func)();
	int	  mms_argc;
	int	  mms_argbody;	/* argument body follows */
	int	  mms_access;	/* necessary access permission */
	char	 *mms_desc;
} MMS_Command;

typedef struct {
	MMS_Command	*commands;
	char		*c_name;
	int		 c_done;
	int		 c_error;
	FILE		*c_partfp;
	int		 c_partid;
	int		 c_curpartid;
} MMS_Commands;


int MMS_HELP(),      MMS_QUIT(),     MMS_USER();
int MMS_ENV(),       MMS_ECHO(),     MMS_CHDIR();
int MMS_ADDHEAD(),   MMS_SUBJECT();
int MMS_ADDTEXT();
int MMS_ENCLOSE(),   MMS_TYPES();
int MMS_PRINT(),     MMS_WRITE(),    MMS_SKELETON();
int MMS_VIEW(),      MMS_SHOW(),     MMS_COMMAND();
int MMS_PARSE();
int MMS_SEND();
int MMS_SH();
int MMS_BLOCK();
int MMS_HENCODE(),   MMS_HDECODE();

MMS_Command MMS_main_command_set[] = {
    {"# commands with mark '*' are available for you"},
    {"HELP",	MMS_HELP,    1,0,NO,  "show this help"},
    {"QUIT",	MMS_QUIT,    1,0,NO,  "quit"},
    {"USER",	MMS_USER,    3,0,NO,  "set effective user ID"},
    {"ENV",	MMS_ENV,     1,0,RO,  "get/set environment"},
    {"ECHO",	MMS_ECHO,    1,0,NO,  "echo given string"},
    {"CHDIR",   MMS_CHDIR,   1,0,RX,  "change working directory"},
    {"ADDHEAD",	MMS_ADDHEAD, 2,0,RO,  "add to To/Cc list"},
    {"SUBJECT",	MMS_SUBJECT, 1,0,NO,  "set the subject"},
    {"ADDTEXT",	MMS_ADDTEXT, 1,1,NO,  "add text to the body"},
    {"ENCLOSE",	MMS_ENCLOSE, 1,1,RW,  "enclose nontext body"},
    {"TYPES",	MMS_TYPES,   0,0,NO,  "list available content-types"},
    {"PRINT",	MMS_PRINT,   0,0,NO,  "print the current message"},
    {"WRITE",	MMS_WRITE,   0,0,WO,  "write the the current message to file"},
    {"SKELETON",MMS_SKELETON,0,0,NO,  "print the skeleton of the message"},
    {"VIEW",	MMS_VIEW,    0,0,RWX, "interpret with the Metamail"},
    {"COMMAND",	MMS_COMMAND, 0,1,RO,  "get commands for the content-type"},
    {"SHOW",	MMS_SHOW,    0,1,RWX, "display it"},
    {"PARSE",	MMS_PARSE,   0,0,NO,  "enter PARSE subcommands"},
    {"SEND",	MMS_SEND,    0,0,RWX, "send the message"},
    {"HENCODE",	MMS_HENCODE, 0,0,NO,  "encode MIME header"},
    {"HDECODE",	MMS_HDECODE, 0,0,NO,  "decode MIME header"},
    {"SH",	MMS_SH,	     1,0,RWX, "sh (for debug)"},
    {"BLOCK",	MMS_BLOCK,   0,0,RWX, "command sequence"},
    {"# Direct comments to <ysato@etl.go.jp>"},
    0
};
MMS_Commands MMS_main_command_env = { MMS_main_command_set, "MAIN" };


struct {
	int	(*mmc_func)();
	char	 *mmc_args;
} MMS_args[] = {
	{MMS_ENCLOSE,	"content-type file-name"},
	0
};

int MMS_P_PRINT();
int MMS_P_VIEW();
int MMS_P_CHILD();
int MMS_P_NEXT();
int MMS_P_PREV();
int MMS_P_PARENT();

MMS_Command MMS_PARSE_command_set[] = {
    {"# PARSE subcommands:"},
    {"HELP",    MMS_HELP,    1,0,NO,  "show this help"},
    {"QUIT",    MMS_QUIT,    1,0,NO,  "quit PARSE command mode"},
    {"PRINT",	MMS_P_PRINT, 1,0,NO,  "print current part as is"},
    {"VIEW",	MMS_P_VIEW,  1,0,NO,  "view current part"},
    {"CHILD",   MMS_P_CHILD, 1,0,NO,  "goto child part"},
    {"PARENT",	MMS_P_PARENT,1,0,NO,  "goto parent part"},
    {"NEXT",    MMS_P_NEXT,  1,0,NO,  "goto next part"},
    {"PREV",	MMS_P_PREV,  1,0,NO,  "goto previous part"},
    0
};
MMS_Commands MMS_PARSE_command_env = { MMS_PARSE_command_set, "PARSE" };

/*////////////////////////////////////////////////////////////////////////
 *	Response status codes
 */
#define OK_GOODBYE	205
#define OK_ARTICLE	220
#define OK_POSTED	240

#define OK_START	200
int MMS_OK_ACCESS =	201;
#define OK_CMD		290
#define OK_ADDHEAD	291
#define OK_ADDCC	292
#define OK_SUBJECT	293
#define OK_ADDTEXT	294
#define OK_ENCLOSE	296
#define OK_VIEW		297
#define OK_CONTENTTYPE	298

#define CONT_INPUT	390

int MMS_ERR_CMDSYN =	500;
#define ERR_UNSUPPORTED 501
#define ERR_CMD		590
int MMS_ERR_ACCESS =	591;
#define ERR_CONTENTTYPE	592
#define ERR_INFILE	593
#define ERR_TEMPFILE	594
#define ERR_CONT_INPUT	596


static
mms_statusX(response_type,code,form,a,b,c,d){
	printf("%d %c ",code,response_type);
	if( code == MMS_ERR_CMDSYN )
		printf("Usage: ");
	printf(form,a,b,c,d);
	printf("\n");
	fflush(stdout);
}
MMS_status0(code,form,a,b,c,d){
	mms_statusX('0',code,form,a,b,c,d);
}
MMS_statusI(code,form,a,b,c,d){
	mms_statusX('I');
}
MMS_statusN(code,form,a,b,c,d){
	mms_statusX('N',code,form,a,b,c,d);
}
MMS_statusB(code,form,a,b,c,d){
	mms_statusX('B',code,form,a,b,c,d);
}
MMS_response(form,a,b,c,d) char *form,*a,*b,*c,*d; {
	if(form[0] == '.' && form[1] == '\n')
		printf("..\n");
	else	printf(form,a,b,c,d);
	fflush(stdout);
}
MMS_response_done(){
	printf(".\n");
	fflush(stdout);
}

/*////////////////////////////////////////////////////////////////////////
 */

extern char *MMS_versionNO;
extern char *MMS_versionDate;
char *MMS_start_message = "MetaMail Server version MMS-%s (%s)";

MMS_start_server(){
	int access;
	char msg[256];

	access = setup_access();
	sprintf(msg,MMS_start_message,MMS_versionNO,MMS_versionDate);
	MMS_status0(OK_START,"%s @%s ACCESS=%x",msg,HostName(),access);
	MMS_mailtoSetup();
}
MMS_server_interpreter(in)
	FILE *in;
{
	if( in != stdin )
		*stdin = *in;
	return MetaMetamail(&MMS_main_command_env);
}

MMS_server(ac,av)
	char *av[];
{
	MMS_start_server();
	return MMS_server_interpreter(stdin);
}

MetaMetamail(comenv)
	MMS_Commands *comenv;
{	char LineBuf[MAXPATHLEN],Command[MAXPATHLEN],ArgList[MAXPATHLEN];
	int ci,sargc;
	char *name;
	int (*func)();

	comenv->c_done = 0;
	while( MMS_getline(LineBuf,sizeof(LineBuf),MMS_COMMANDIN) != NULL ){
		if( *LineBuf == 0 )
			continue;

		*ArgList = 0;
		sargc = sscanf(LineBuf,"%s %[^\377]",Command,ArgList);
		if( sargc == 0 )
			continue;

		MMS_touppers(Command);
		for(ci = 0; name = comenv->commands[ci].mms_name; ci++)
		    if( strcmp(Command,name) == 0 ){
			if( !permitted(comenv,ci) ){
				MMS_status0(MMS_ERR_ACCESS,
					"You must do USER first.");
			}else{
				func = comenv->commands[ci].mms_func;

				if( func == 0 )
	MMS_status0(ERR_UNSUPPORTED,"%s not supported yet.",Command);
				else	(*func)(comenv,ArgList);
				if( comenv->c_done )
					goto done;
			}
			break;
		    }
		if( name == 0 )
			MMS_status0(MMS_ERR_CMDSYN,"Invalid command(%s).",
				Command);
	}
done:
	return 0;
}


/*////////////////////////////////////////////////////////////////////////
 *	PARSE command mode
 */
MMS_PARSE(_){
	FILE *fp;
	char file[MAXPATHLEN];

	*file = 0;
	if( fp = MMS_synthWriteFile(file,0,0) ){
		MMS_PARSE_command_env.c_partfp = fp;
		MMS_PARSE_command_env.c_curpartid =
		MMS_PARSE_command_env.c_partid = MMS_parser(fp,stdout);
		MMS_status0(OK_CMD,"PARSE command mode");
		MetaMetamail(&MMS_PARSE_command_env);
                unlink(file);
	}else	MMS_status0(ERR_CMD,"cannot open tempfile.");
}
MMS_P_NEXT(comenv)
	MMS_Commands *comenv;
{	int npartid;

	if( npartid = MMS_partNext(comenv->c_curpartid) ){
		comenv->c_curpartid = npartid;
		MMS_status0(OK_CMD,"partid=%d",npartid);
	}else	MMS_status0(ERR_CMD,"no more next.");
}
MMS_P_PREV(comenv)
	MMS_Commands *comenv;
{	int npartid;

	if( npartid = MMS_partPrev(comenv->c_curpartid) ){
		comenv->c_curpartid = npartid;
		MMS_status0(OK_CMD,"partid=%d",npartid);
	}else	MMS_status0(ERR_CMD,"no more previous.");
}
MMS_P_CHILD(comenv)
	MMS_Commands *comenv;
{	int npartid;

	if( npartid = MMS_part1stChild(comenv->c_curpartid) ){
		comenv->c_curpartid = npartid;
		MMS_status0(OK_CMD,"partid=%d",npartid);
	}else	MMS_status0(ERR_CMD,"no children.");
}
MMS_P_PARENT(comenv)
	MMS_Commands *comenv;
{	int npartid;

	if( npartid = MMS_partParent(comenv->c_curpartid) ){
		comenv->c_curpartid = npartid;
		MMS_status0(OK_CMD,"partid=%d",npartid);
	}else	MMS_status0(ERR_CMD,"no parent.");
}
MMS_P_PRINT(comenv)
	MMS_Commands *comenv;
{	int partid;

	partid = comenv->c_curpartid;
	MMS_statusN(OK_CMD,"body of partid=%d follows:",partid);
	MMS_viewAsis(comenv->c_partfp,stdout,partid,3);
	MMS_response_done();
}
MMS_P_VIEW(comenv)
	MMS_Commands *comenv;
{
	MMS_putenv("MM_NOASK","1");
	MMS_putenv("MM_NOTTY","1");
	MMS_statusN(OK_CMD,"body of partid=%d follows:",comenv->c_curpartid);
	MMS_viewCocked(comenv->c_partfp,stdout,comenv->c_curpartid);
	MMS_response_done();
}

/*////////////////////////////////////////////////////////////////////////
 */
MMS_USER(_,user_file_passwd)
	char *user_file_passwd;
{	char user[MAXPATHLEN],file[MAXPATHLEN],passwd[MAXPATHLEN];
	char errmsg[MAXPATHLEN];
	struct passwd *pw;
	int uid;

	if(sscanf(user_file_passwd,"%s %s %[^\n]",user,file,passwd) != 3){
		MMS_status0(MMS_ERR_CMDSYN,"USER username filename passwd");
		return 0;
	}
	pw = MMS_user(user,file,passwd,errmsg);
	if( pw == 0 ){
		MMS_status0(MMS_ERR_ACCESS,"%s",errmsg);
                return 0;
	}
	MMS_ACCESS = ALL;

	MMS_status0(MMS_OK_ACCESS,"Cirtainly, you are %s",user);

	MMS_putenv("USER","%s",user);
	MMS_putenv("HOME","%s",pw->pw_dir);
	MMS_putenv("PATH","%s",
		"/usr/ucb:/bin:/usr/bin:/usr/local/bin:/usr/bin/X11");
	MMS_putenv("DISPLAY","%s:0",HostName());
	MMS_mailtoReSetup();
}

static permitted(comenv,ci)
	MMS_Commands *comenv;
{	int i,mask;

	for(i = 0; i < 32; i++){
		mask = 1 << i;
		if( mask & comenv->commands[ci].mms_access )
		if((mask & MMS_ACCESS) == 0)
			return 0;
	}
	return 1;
}
static setup_access(){
	if( MMS_privileged_user(geteuid(),getegid()) )
		return MMS_ACCESS = NO;
	else	return MMS_ACCESS = RWX;
}

/*////////////////////////////////////////////////////////////////////////
 */
DESC(MMS_H_HELP,0,0,"show this help",0);
MMS_HELP(comenv,arg)
	MMS_Commands *comenv;
	char *arg;
{	int ci,col,long_form;
	char *name,*mark;

	MMS_statusN(OK_CMD,"This server accepts the following commands:");

	long_form = *arg != 0;
	col = 0;

	for(ci = 0; name = comenv->commands[ci].mms_name; ci++){
		if( *name == '#' ){
			if( !long_form && col != 0 ){
				printf("\n");
				col = 0;
			}
			MMS_response("%s\n",name);
		}else{
			mark = permitted(comenv,ci)?"*":" ";
			MMS_response("%s%-10s ",mark,name);
			col += 12;

			if( long_form )
				printf("%s\n",comenv->commands[ci].mms_desc);
			else{
				if( 72 <= col ){
					printf("\n");
					col = 0;
				}
			}
		}
	}
	if( !long_form ){
		if( col != 0 )
			MMS_response("\n");
		MMS_response("# enter HELP LONG to get more help.\n");
	}
	MMS_response_done();
}

DESC(MMS_H_QUIT,0,0,"quit",0);
MMS_QUIT(mce) MMS_Commands *mce; {
	if( mce == &MMS_main_command_env )
		MMS_status0(OK_GOODBYE,"closing connection.  Goodbye.");
	else	MMS_status0(OK_CMD,"%s command mode done.",mce->c_name);
	mce->c_done = 1;
}

MMS_PRINT(_){
	MMS_statusN(OK_ARTICLE,"Current content of article follows:");
	MMS_synthWriteFile(0,0,0);
	MMS_response("\n");
	MMS_response_done();
}

MMS_WRITE(_,file) char *file; {
	int size;

	if( *file == 0 ){
		MMS_status0(MMS_ERR_CMDSYN,"WRITE filenmae");
	}else{
		char filebuf[MAXPATHLEN];

		if( *file != '/' ){
			getwd(filebuf);
			sprintf(filebuf+strlen(filebuf),"/%s",file);
			file = filebuf;
		}
		MMS_synthWriteFile(file,&size,1);
		if( 0 < size )
			MMS_status0(OK_CMD,"%d bytes written to %s",size,file);
		else	MMS_status0(ERR_CMD,"cannot write to %s",file);
	}
}
MMS_SKELETON(_){
	FILE *fp;
	char file[MAXPATHLEN];
	int rootid;

	*file = 0;
	if( fp = MMS_synthWriteFile(file,0,0) ){
		rootid = MMS_parser(fp,stdout);
		MMS_statusN(OK_CMD,"skeleton of the message follows:");
		MMS_viewSkeleton(fp,stdout,rootid);
		MMS_response_done();
		unlink(file);
	}else	MMS_status0(ERR_CMD,"cannot open tempfile.");
}
MMS_VIEW(_){
	static char *av[4] = {"metamail","-z",0};
	char file[2048];
	FILE *fp;
	char command[1024];

	*file = 0;
	fp = MMS_synthWriteFile(file,0,0);
	if( fp == NULL ){
		MMS_status0(ERR_TEMPFILE,"cant create temporary file.");
		return NULL;
	}
	fclose(fp);
	av[2] = file;

	MMS_putenv("MM_NOASK","1");
	MMS_putenv("MM_NOTTY","1");
	MMS_statusN(OK_VIEW,"view start.");
	sprintf(command,"exec %s -z %s","/usr/local/bin/metamail",file);

	if( vfork() == 0 ){
		system(command);
		_exit(0);
	}
	wait(0);
	unlink(file);

printf("\n");
	MMS_response_done();
}

MMS_SHOW(_,type)
	char *type;
{	FILE *body,*player;
	char command[MAXPATHLEN];
	int bytes;
	void (*osig)();

	if( strcasecmp(type,"audio/basic") == 0 )
		sprintf(command,"cat > /dev/audio");
	else
	if( strcasecmp(type,"image/xwd") == 0 )
		sprintf(command,"xloadimage stdin");
	else{
		MMS_status0(ERR_CONT_INPUT,
			"unsupported type [%s] cannot display",type);
		return;
	}

	player = popen(command,"w");
	if( player == NULL ){
		MMS_status0(ERR_CONT_INPUT,"cannot exec %s",command);
		return;
	}
	MMS_status0(CONT_INPUT,"send data in BASE64 coding:");
	MMS_statusN(OK_CMD,"showing...");

	osig = signal(SIGPIPE,SIG_IGN);
	body = MMS_getbody(MMS_COMMANDIN,&bytes);
	MMS_response("Transmission done: %d bytes.\n",bytes);
	from64(body,player,0,NULL);
	fclose(body);

	pclose(player);
	signal(SIGPIPE,osig);

	MMS_response_done();
}

MMS_COMMAND(_,ctype)
	char *ctype;
{	char *command;

	command = (char*)MMS_mailtoGetCommand(ctype);
	if(command)
		MMS_status0(OK_CMD, "%s",command);
	else	MMS_status0(ERR_CMD,"%s","NOCOMMAND");
}


MMS_SEND(_){
	char file[2048],command[2048];
	FILE *fp;

	*file = 0;
	fp = MMS_synthWriteFile(file,0,1);
	if( fp == NULL ){
		MMS_status0(ERR_TEMPFILE,"cant create temporary file");
		return NULL;
	}
	unlink(file);

	sprintf(command,"exec /usr/lib/sendmail -t -om");
	if( vfork() == 0 ){
		close(0);
		dup(fileno(fp));
		system(command);
		_exit(0);
	}
	wait(0);
	fclose(fp);
	MMS_status0(OK_POSTED,"Article posted successfully.");
}

MMS_ADDHEAD(_,field_entry)
	char *field_entry;
{	char field[128],entry[2048],errmsg[256];

	if( sscanf(field_entry,"%s %[^\377]",field,entry) != 2 ){
		MMS_status0(MMS_ERR_CMDSYN,"ADDHEAD fieldname entyr");
		return 0;
	}
	if( MMS_synthAddHead(field,entry,errmsg) != 0 )
		MMS_status0(ERR_CMD,errmsg);
	else	MMS_status0(OK_ADDHEAD,"added to %s list.",field);
}
MMS_SUBJECT(_,subject)
	char *subject;
{
	MMS_synthSubject(subject);
	MMS_status0(OK_SUBJECT,"subject set.");
}


MMS_ADDTEXT(_,arg)
	char *arg;
{	char msg[2048];
	int stcode,lines;

	stcode = MMS_synthToggleTextAttr(CONT_INPUT,ERR_CONT_INPUT,
			"send text then end with \".\":",arg);
	if( stcode == ERR_CONT_INPUT )
		return;

	lines = MMS_synthAddText(MMS_COMMANDIN);
	sprintf(msg,"text added (%d lines).",lines);
	MMS_synthToggleTextAttr(OK_ADDTEXT,OK_ADDTEXT,msg,arg);
}
MMS_HDECODE(_,arg)
	char *arg;
{	FILE *tmp;
	char line[256];

	MMS_status0(CONT_INPUT,"send header.");
	tmp = tmpfile();
	while( MMS_getline(line,sizeof(line),MMS_COMMANDIN) != NULL ){
		fputs(line,tmp);
		fputs("\n",tmp);
	}
	fseek(tmp,0,0);

	MMS_status0(OK_CMD,"decoded message follows:");
	MIME_headerDecode(tmp,stdout);
	MMS_response_done();
	fclose(tmp);
}
MMS_HENCODE(_,arg)
	char *arg;
{	FILE *tmp;
	char line[256];

	MMS_status0(CONT_INPUT,"send header.");
	tmp = tmpfile();
	while( MMS_getline(line,sizeof(line),MMS_COMMANDIN) != NULL ){
		fputs(line,tmp);
		fputs("\n",tmp);
	}
	fseek(tmp,0,0);

	MMS_status0(OK_CMD,"encoded message follows:");
	MIME_headerEncode(tmp,stdout);
	MMS_response_done();
	fclose(tmp);
}

MMS_ENCLOSE(_,args)
	char *args;
{	int mailpart;
	char ctype[2048],file[2048],errmsg[256];
	FILE *body,*ifp;
	int bytes;

	file[0] = 0;
	if( sscanf(args,"%s %s",ctype,file) < 1 ){
		MMS_status0(MMS_ERR_CMDSYN,"ENCLOSE content-type [filename]");
		return MMS_ERR_CMDSYN;
	}
	if( !MMS_mailtoAvailableCtype(ctype) ){
		MMS_status0(ERR_CONTENTTYPE,"unknown content-type: %s",ctype);
		return ERR_CONTENTTYPE;
	}

	if( file[0] ){
		ifp = fopen(file,"r");
		if( ifp == NULL ){
			MMS_status0(ERR_INFILE,"can't open file: %s",file);
			return 0;
		}
	}else{
		MMS_status0(CONT_INPUT,"send data in BASE64 coding:");
		ifp = tmpfile();
		body = MMS_getbody(MMS_COMMANDIN,&bytes);
		from64(body,ifp,0,NULL);
		fclose(body);
		MMS_fseek(ifp,0,0);
		sprintf(file,"INPUT(%d bytes)",bytes);
	}
        if( mailpart = MMS_synthEncloseFile(ctype,ifp,errmsg) ){
                MMS_statusN(OK_ENCLOSE,"%s enclosed as %s",file,ctype);
                MMS_synthAddPart(mailpart);
                MMS_response_done();
        }else{
		MMS_status0(ERR_CMD,"%s",errmsg);
	}
	fclose(ifp);
}

MMS_TYPES(_){
	MMS_statusN(OK_CONTENTTYPE,"known content-types:");
	MMS_mailtoPrintTypes(stdout);
	MMS_response_done();
}

/*////////////////////////////////////////////////////////////////////////
 *	MISC
 */

extern char **environ;
MMS_ENV(_,envarg)
	char *envarg;
{	char name[1024],body[4096];
	char *oldenv;
	int sc,envi;

	sc = sscanf(envarg,"%s %[^\377]",name,body);
	switch(sc){
		default:
		case 0:	MMS_statusN(OK_CMD,"environments follow:");
			for(envi = 0; environ[envi]; envi++)
				MMS_response("%s\n",environ[envi]);
			MMS_response_done();
			break;
		case 1:
			if( oldenv = getenv(name) )
				MMS_status0(OK_CMD,"%s=%s",name,oldenv);
			else	MMS_status0(OK_CMD,"%s undefined.",name);
			break;
		case 2:
			oldenv = getenv(name);
			MMS_putenv(name,"%s",body);
			MMS_status0(OK_CMD,"env %s",oldenv?"reset":"set");
	}
}
MMS_ECHO(_,string)
	char *string;
{
	MMS_status0(OK_CMD,"%s",string);
}
MMS_SH(_,com){
	MMS_statusN(OK_CMD,"START %s",com);
	system(com);
	MMS_response_done();
}

MMS_CHDIR(_,dir)
	char *dir;
{	char wd[2048];

	if( chdir(dir) == 0 ){
		getwd(wd);
		MMS_status0(OK_CMD, "ok. %s",wd);
	}else	MMS_status0(ERR_CMD,"cant go to %s",dir);
}

MMS_BLOCK(_,arg)
	char *arg;
{
}
