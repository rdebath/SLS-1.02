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
Program:      mmsauth.c (metamail client authentication)
Author:       Yutaka Sato <ysato@etl.go.jp>

Description:
   When mms invoked as remote server, it must run under someone's access
permission. This module authenticate the remote client based on the set
of {user, password file name, password}.

History:
  v0.1	92.04.24     created a small prototype
///////////////////////////////////////////////////////////////////////*/


/*////////////////////////////////////////////////////////////////////////
 *	Server side authentication
 */
#include <stdio.h>
#include <sys/param.h>
#include <pwd.h>
#include <sys/stat.h>
#define MMS_SALT "MM"

struct passwd *
MMS_user(user,file,passwd,errmsg)
	char *user,*file,*passwd,*errmsg;
{	int euid;
	char hostname[MAXPATHLEN];
	char tmp[MAXPATHLEN],xpasswd[MAXPATHLEN],*upasswd;
	struct passwd *pw;
	FILE *fp;
	struct stat Stat;
	int rcc;

	pw = getpwnam(user);
	if( pw == NULL ){
		sprintf(errmsg,"unknown user(%s)",file);
		return 0;
	}
	euid = pw->pw_uid;

	if( MMS_privileged_user(euid,pw->pw_gid) ){
		sprintf(errmsg,"don't use in privileged user(uid=%d,gid=%d)",
			pw->pw_uid,pw->pw_gid);
		return 0;
	}

	if( *file != '/' ){
		sprintf(tmp,"%s/%s",pw->pw_dir,file);
		strcpy(file,tmp);
	}
	fp = fopen(file,"r");
	if( fp == NULL ){
		sprintf(errmsg,"unknown file(%s)",file);
		return 0;
	}
	fstat(fileno(fp),&Stat);
	if( Stat.st_uid != euid ||(Stat.st_mode & 0777) & ~0700 ){
		sprintf(errmsg,
			"file must be readable only for %s (%s) %d/%d",
			user,file,Stat.st_uid,euid);
		return 0;
	}
	rcc = read(fileno(fp),xpasswd,11);
	fclose(fp);
	if( rcc < 11 ){
		sprintf(errmsg,"encoded passwd too short ? (rcc)",rcc);
		return 0;
	}

	upasswd = (char*)crypt(passwd,MMS_SALT) + 2;
	if( strncmp(upasswd,xpasswd,11) != 0 ){
		sprintf(errmsg,"incorrect passwd %s (%s)",passwd,upasswd);
		return 0;
	}
	if( setreuid(euid,euid) != 0 ){
		sprintf(errmsg,"cannot set uid (%s:%d).",user,euid);
		return 0;
	}
	return pw;
}

/*////////////////////////////////////////////////////////////////////////
 *	Client side default authentication
 */
#define MMS_CLNT_AUTH	".mmsprofile"
#define MMS_SERV_AUTH	".mmspasswd"
MMS_authenticate(serv)
	FILE *serv[];
{	FILE *afp;
	char *user,authfile[MAXPATHLEN],password[MAXPATHLEN];
	char *getenv();

	sprintf(authfile,"%s/%s",getenv("HOME"),MMS_CLNT_AUTH);
	afp = fopen(authfile,"r");
	if( afp == NULL )
		return;
	password[0] = 0;
	fscanf(afp,"%[^\n]",password);
	fclose(afp);

	user = getenv("USER");
	MMS_putserver(serv,"USER %s %s %s\n",user,MMS_SERV_AUTH,password);
	MMS_getserver(serv);
}

/*////////////////////////////////////////////////////////////////////////
 */
MMS_privileged_user(uid,gid){
	if( uid == 0 || gid == 0 )
		return 1;
	return 0;
}
