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
Program:      mmsclient (client library to access remote mms server)
Author:       Yutaka Sato <ysato@etl.go.jp>

History:
  v0.1  92.04.29     created a small prototype
///////////////////////////////////////////////////////////////////////*/

#include <stdio.h>
char *index(),*getenv();

/*//////////////////////////////////////////////////////////////////////*/

MMS_E_client(ac,av,in,out){
	MMS_client(ac,av,in);
}
int (*MMS_E_CLIENT)() = MMS_E_client;

/*//////////////////////////////////////////////////////////////////////*/
#define MMS_PORT_NAME	"mms"

#define OK_GOODBYE 205
MMS_goodbye(stat){ return stat == OK_GOODBYE; }
#define CONT_INPUT 390
MMS_sendbody(stat){ return stat == CONT_INPUT; }
#define ERR_CONT_INPUT 596
MMS_discardbody(stat){ return stat == ERR_CONT_INPUT; }

typedef struct {
	char	*host;
	FILE	*serv;
} MMS_Server;

int MMS_DEBUG;
MMS_client(ac,av,in)
	char *av[];
	FILE *in;
{	char line[2048];
	FILE *serv[2];
	int stat = 0;
	static int IsaTty;

	ac = MMS_clientstart(ac,av,serv);
	if( 1 < ac ){
		int ai;

		for( ai = 1; ai < ac; ai++ ){
			if( MMS_discardbody(stat) ){}else
			if( MMS_sendbody(stat) ){
				MMS_putserver_file(serv,in,av[ai]);
				stat = MMS_getserver(serv);
			}else{
				if( MMS_putserver(serv,"%s\n",av[ai]) != 0 )
					break;
				stat = MMS_getserver(serv);
			}
			if( MMS_goodbye(stat) )
				break;
		}
		return 0;
	}
	for(;;){
		IsaTty = isatty(fileno(in));
		if( IsaTty ){
			printf("command> ");
			fflush(stdout);
		}
		if( fgets(line,sizeof(line),in) == NULL )
			break;
		if( *line == '\n' )
			continue;

		if( MMS_putserver(serv,"%s",line) != 0 )
			break;

		stat = MMS_getserver(serv);
		if( MMS_sendbody(stat) ){
			send_body(serv,in);
			stat = MMS_getserver(serv);
		}
		if( MMS_goodbye(stat) )
			break;
	}
	return 0;
}
static send_body(serv,in)
	FILE *serv[];
	FILE *in;
{	int Isatty = isatty(fileno(in));
	char line[2048];
	int lines = 0;

	for(;;){
		lines++;
		if( Isatty ){
			printf("input> ");
			fflush(stdout);
		}
		if( fgets(line,sizeof(line),in) == NULL )
			break;

		if( lines == 1 ){
			if( line[0] == '<' || line[0] == '!' ){
				char file[1024];

				if( line[0] == '<' )
					sscanf(line+1,"%s",file);
				else	strcpy(file,line);
				MMS_putserver_file(serv,in,file);
				break;
			}
		}

		MMS_putserver(serv,"%s",line);
		if( strcmp(line,".\n") == 0 )
			break;
	}
}
MMS_putserver_file(serv,in,srcfile)
	FILE *serv[],*in;
	char *srcfile;
{	FILE *srcfp;

	if( strcmp(srcfile,"-") == 0 )
		srcfp = fdopen(fileno(in),"r");
	else
	if( *srcfile == '!' )
		srcfp = popen(srcfile+1,"r");
	else	srcfp = fopen(srcfile,"r");

	if( srcfp != NULL ){
/*
		MMS_encodeBASE16(srcfp,serv[1]);
*/
		to64(srcfp,serv[1]);
		fprintf(serv[1],".\n");
	}else	fprintf(serv[1],".\n");
	fflush(serv[1]);
	fclose(srcfp);
}

/*////////////////////////////////////////////////////////////////////////
 */
MMS_clientstart(ac,av,serv)
	char *av[];
	FILE *serv[];
{	int ai;
	int sock;
	char *hostname[256],*hn;

	ac = MMS_gethostarg(ac,av,hostname);
	if( *hostname == 0 ){
		if( hn = getenv("MMSERVER") )
			strcpy(hostname,hn);
		else	gethostname(hostname,sizeof hostname);
	}
	sock = open_MMS_server(hostname);
	serv[0] = fdopen(sock,"r"); setlinebuf(serv[0]);
	serv[1] = fdopen(sock,"w"); setlinebuf(serv[1]);
	MMS_getserver(serv);
	MMS_authenticate(serv);
	return ac;
}
MMS_gethostarg(ac,av,hostnameb)
	char *av[],*hostnameb;
{	int ai;

	*hostnameb = 0;
	for(ai = 0; ai < ac; ai++){
		if( strcmp(av[ai],"-host") == 0 ){
			if(ai+1 < ac){
				strcpy(hostnameb,av[ai+1]);
				ac -= 2;
				for(;ai < ac; ai++)
					av[ai] = av[ai+2];
			}
			break;
		}
	}
	return ac;
}
static mmsc_error_exit(message)
	char *message;
{
	perror(message);
	exit(1);
}

/*////////////////////////////////////////////////////////////////////////
 *	put / get server
 */
MMS_putserver(serv,form,a,b,c,d,e,f,g)
	FILE *serv[];
	char *form;
{	char command[2048];
	int eof;

	sprintf(command,form,a,b,c,d,e,f,g);
	fprintf(serv[1],"%s",command);
	eof = fflush(serv[1]);

	if( MMS_DEBUG )
		printf("%s\n",command);
	return eof;
}
MMS_getserver(serv)
	FILE *serv[];
{	char status[2048],body[2048];
	int stat;
	char resp_type;

	fgets(status,sizeof(status),serv[0]);
	printf("%s",status);
	fflush(stdout);

	sscanf(status,"%d %c",&stat,&resp_type);
	if( resp_type && resp_type != '0' ){
		while( fgets(body,sizeof(body),serv[0]) != NULL ){
			printf("%s",body);
			fflush(stdout);
/*
{
int i;
for(i = 0; i < 10 && body[i]; i++)
fprintf(stderr,"[%c]",body[i]);
fprintf(stderr,"\n");
}
*/
			if( body[0]=='.' && (body[1]=='\n'||body[1]=='\r') )
				break;
		}
	}
	return stat;
}
MMS_getresponse1(serv)
	FILE *serv[];
{
}
char *
MMS_getline(line,size,ifp)
	char *line;
	FILE *ifp;
{	char *r,*p;

	r = fgets(line,size,ifp);
	if( r != NULL ){
		if(p = index(line,'\r')) *p = 0;
		if(p = index(line,'\n')) *p = 0;
		if( strcmp(line,".") == 0 )
			return NULL;
		if( strcmp(line,"..") == 0 )
			strcpy(line,line+1);
	}
	return r;
}
FILE *MMS_getbody(ifp,sizep)
	FILE *ifp;
	int *sizep;
{	FILE *tmp;
	char line[256];
	int bytes;

	if( (tmp = tmpfile()) != NULL ){
		bytes = 0;
		while( MMS_getline(line,sizeof(line),ifp) != NULL ){
			fputs(line,tmp);
			bytes += strlen(line);
		}
		fflush(tmp);
		fseek(tmp,0,0);
		if( sizep )
			*sizep = bytes;
	}
	return tmp;
}

/*////////////////////////////////////////////////////////////////////////
 *	internet socket
 */
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
char *ERRMSG_open_MMS = "open_MMS_server";

static struct sockaddr_in in_sockaddr;
static open_MMS_server(hostname)
	char *hostname;
{	int port,sock;
	struct hostent *host;
	struct servent *serv;

	sock = socket(AF_INET,SOCK_STREAM,6);
	if( sock == -1 ) mmsc_error_exit(ERRMSG_open_MMS);

	serv = getservbyname(MMS_PORT_NAME,"tcp");
	if( serv == NULL ) mmsc_error_exit(ERRMSG_open_MMS);

	port = serv->s_port;
	host = gethostbyname(hostname);
	if( host == NULL ) mmsc_error_exit(ERRMSG_open_MMS);

	in_sockaddr.sin_family = AF_INET;
	in_sockaddr.sin_port = port;
	bcopy(host->h_addr,&in_sockaddr.sin_addr,host->h_length);

	if( connect(sock,&in_sockaddr,sizeof(in_sockaddr)) == -1 )
		mmsc_error_exit(ERRMSG_open_MMS);

	return sock;
}

/*////////////////////////////////////////////////////////////////////////
 *
 */
char *MMS_freshcopy();

extern char **environ;
char *MMS_environ[200];
MMS_putenv(name,form,a,b,c,d)
	char *name,*form;
{	int ei;
	char oname[1024],env[4096],*ep;

	if( MMS_environ[0] == 0 ){
		for(ei = 0; environ[ei]; ei++)
			MMS_environ[ei] = MMS_freshcopy(environ[ei]);
		MMS_environ[ei] = 0;
		environ = MMS_environ;
	}
	for(ei = 0; environ[ei]; ei++){
		sscanf(environ[ei],"%[^=]",oname);
		if( strcmp(oname,name) == 0 )
			break;
	}
	if( ep = environ[ei] )
		free(ep);

	sprintf(env,"%s=",name);
	sprintf(env+strlen(env),form,a,b,c,d);
	environ[ei] = MMS_freshcopy(env);
}


FILE *
MMS_saveTempfile(fp,size)
	FILE *fp;
{	FILE *tmp;

	tmp = tmpfile();
	MMS_copyFile(fp,tmp);
	MMS_fseek(tmp,0,0);
	MMS_fseek(fp,0,0);
	return tmp;
}
MMS_copyFile(in,out)
	FILE *in,*out;
{	char buf[4096];
	int rsize,rc;

	while( 0 < (rc = fread(buf,1,sizeof(buf),in)) )
		fwrite(buf,rc,1,out);
	fflush(out);
}

char *HostName(){
	static char hostname[128];

	if( hostname[0] == 0 )
		gethostname(hostname,sizeof(hostname)-1);
	return hostname;
}

int PSEUDO_FILE_FD;

#ifdef hpux
getwd(dir)
	char *dir;
{
	return getcwd(dir);
}
setreuid(ruid,euid)
{
	setuid(euid);
}
setlinebuf(fp)
	FILE *fp;
{
	setbuf(fp,NULL);
}
#endif

#if defined(_IODUMMY) || !defined(_IOSTRG)
static char *dummyptr;
static char *dummybase;
static int dummycnt;
#define PSEUDOFILE(fp)	0
#define BUFSIZE(fp)	0
#define BASE(fp)	dummybase
#define PTR(fp)		dummyptr
#define CNT(fp)		dummycnt
#else
#define PSEUDOFILE(fp)	(fileno(fp) < 0 && fileno(fp) == PSEUDO_FILE_FD)
#define BUFSIZE(fp)	fp->_bufsiz
#define BASE(fp)	fp->_base
#define PTR(fp)		fp->_ptr
#define CNT(fp)		fp->_cnt
#endif

MMS_fseek(fp,off,from)
	FILE *fp;
{	int bufsize,base,disp;

	if( !PSEUDOFILE(fp) )
		return fseek(fp,off,from);

	bufsize = BUFSIZE(fp);
	switch(from){
		case 0: base = 0;		break;
		case 1: base = PTR(fp)-BASE(fp);break;
		case 2: base = bufsize;		break;
	}
	disp = base + off;
	if( disp < 0 || bufsize < disp )
		return -1;

	PTR(fp) = &BASE(fp)[disp];
	CNT(fp) = bufsize - disp;
	return 0;
}
MMS_ftell(fp)
	FILE *fp;
{
	if( !PSEUDOFILE(fp) )
		return ftell(fp);
/*
fprintf(stderr,"ptr=%x / base=%x / off=%d / cnt=%d\n",
PTR(fp),BASE(fp),PTR(fp)-BASE(fp),CNT(fp));
sleep(1);
*/
	if( CNT(fp) == 0 )
		return BUFSIZE(fp);
	return PTR(fp) - BASE(fp);
}

MMS_seekable(fp)
	FILE *fp;
{
	if( PSEUDOFILE(fp) )
		return 1;
	else	return fseek(fp,0,1) == 0;
}


char *
MIME_Decoder(mp){
	char *encode;

	if( encode = (char*)MMS_partContentEncode(mp) ){
		if( strcasecmp(encode,"base64") == 0 )
			return "mmencode -u";
		if( strcasecmp(encode,"quoted-printable") == 0 )
			return "mmencode -u -q";
	}
	return 0;
}
