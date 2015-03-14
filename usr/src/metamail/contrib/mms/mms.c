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
Program:      mms.c  (metamail server)
Author:       Yutaka Sato <ysato@etl.go.jp>
Description:
     This is a program that allows programs to compose/decompsea
     a MIME-format mail with the commands via socket.
History:
  v0.0  92.04.21  created a small prototype
  v0.1	92.04.24  added authentication (mmsauth.c)
  v0.1  92.04.29  added client interface (mmsclient.c)
  v0.1  92.05.02  added a MIME parser (mmsparser.c)
  v0.1  92.05.03  added a MIME viewer (mmsviewer.c)
  v0.2  92.05.05  added a MIME translator (mmstransl.c)
  v0.2  92.05.13  separated mms.c(main) and mmsserver.c
  v0.3  92.05.19  added MIME HEADER composer/decomposer (mmsencode.c)
  v0.3  92.05.20  implanted mmsencode into NNTP server (nntp/)
///////////////////////////////////////////////////////////////////////*/

char *rindex();
typedef int (*IFUNCP)();
#include <stdio.h>

/*//////////////////////////////////////////////////////////////////////*/
MMS_E_help(ac,av,in,out)
	char *av[];
{
	fprintf(out,"Usage: %s [ENTRY] [arguments]\n",av[0]);
	fprintf(out," ENTRY:");
	print_entries(out);
}
IFUNCP MMS_E_HELP = MMS_E_help;

MMS_E_script(ac,av,in,out)
	char *av[];
{	char *file;
	int ai;
	FILE *sfp;

	if( ac < 2 )
		exit(-1);
	MMS_start_server();

	for( ai = 1; ai < ac; ai++ ){
		sfp = fopen(av[ai],"r");
		if( sfp == NULL )
			exit(-1);
		MMS_server_interpreter(sfp);
		fclose(sfp);
	}
}
IFUNCP MMS_E_SCRIPT = MMS_E_script;

/*//////////////////////////////////////////////////////////////////////*/
IFUNCP MMS_E_VERSION;
IFUNCP MMS_E_SERVER;
IFUNCP MMS_E_CLIENT;
IFUNCP MMS_E_VIEWER;
IFUNCP MMS_E_TRANSL;
IFUNCP MMS_E_COMPOSE;
IFUNCP MMS_E_ENCODEBASE16;
IFUNCP MMS_E_DECODEBASE16;
IFUNCP MMS_E_HEAD_ENCODER;
IFUNCP MMS_E_HEAD_DECODER;

struct {
	char	*e_name;
	IFUNCP	*e_funcp;

} MMS_entries[] = {
	{"version",	&MMS_E_VERSION},
	{"help",	&MMS_E_HELP},
	{"server",	&MMS_E_SERVER},
	{"client",	&MMS_E_CLIENT},
	{"script",	&MMS_E_SCRIPT},
	{"viewer",	&MMS_E_VIEWER},
	{"transl",	&MMS_E_TRANSL},
	{"compose",	&MMS_E_COMPOSE},
	{"encode",	&MMS_E_ENCODEBASE16},
	{"decode",	&MMS_E_DECODEBASE16},
	{"hencode",	&MMS_E_HEAD_ENCODER},
	{"hdecode",	&MMS_E_HEAD_DECODER},
	0
};

int arg_infile();
struct {
	char	*a_name;
	int	 a_count;
	IFUNCP	 a_funcp;
} MMS_argments[] = {
	{"-f",	2,	arg_infile},
	0
};


IFUNCP *MMS_E_default = &MMS_E_CLIENT;


print_entries(out)
	FILE *out;
{	int ei;
	char *name;

	for(ei = 0; name = MMS_entries[ei].e_name; ei++){
		if( MMS_entries[ei].e_funcp == MMS_E_default )
			fprintf(out,"\t[-%s]\n",name);
		else	fprintf(out,"\t-%s\n",name);
	}
}

remove_arg(ac,av,delai,delcnt)
	char *av[];
{	int ai;

	for( ai = delai; ai < ac; ai++ )
		av[ai] = av[ai+delcnt];
	return ac - delcnt;
}

main(ac,av)
	char *av[];
{	char *file_ename,*arg1_ename,*ename;
	int ei;
	IFUNCP efunc;
	FILE *input;

/*
{ int ai; for(ai=0; ai<ac; ai++){ fprintf(stderr,"[%d]%s\n",ai,av[ai]); } }
*/

	input = stdin;
	if( file_ename = rindex(av[0],'/') )
		file_ename++;
	else	file_ename = av[0];
	if( strncmp(file_ename,"mms",3) == 0 )
		file_ename += 3;

	if( 1 < ac && av[1][0] == '-' )
		arg1_ename = &av[1][1];
	else	arg1_ename = "";

	for(ei = 0; ename = MMS_entries[ei].e_name; ei++ ){
		if( strcmp(ename,file_ename) == 0 )
			break;
		if( strcmp(ename,arg1_ename) == 0 ){
			ac = remove_arg(ac,av,1,1);
			break;
		}
	}
	if( 2 < ac ){
		if( strcmp("-f",av[1]) == 0 ){
			char *file = av[2];

			ac = remove_arg(ac,av,1,2);
			input = fopen(file,"r");
		}
	}

/*
{ int ai; for(ai=0; ai<ac; ai++){ fprintf(stderr,"[%d]%s\n",ai,av[ai]); } }
*/


	if( ename ){
		if( efunc = *MMS_entries[ei].e_funcp ){
			(*efunc)(ac,av,input,stdout);
		}else{
			fprintf(stderr,
				"This program does not include '%s'.\n",ename);
		}
	}else	MMS_client(ac,av,input);

	MMS_mailtoFinalize();
	return 0;
}

static
arg_infile(ac,av)
	char *av[];
{
}
