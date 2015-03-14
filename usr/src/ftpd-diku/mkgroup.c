/* Written from scratch by me Mitch 1992 m.dsouza@mrc-apu.cam.ac.uk */

#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>
#ifdef SHADOW_PWD
#	include <shadow.h>
#endif
#include <unistd.h>
#include <limits.h>
#include <dirent.h>
#define GROUP_FILE "groupaccess"

#ifndef MAXPATHLEN
#define MAXPATHLEN _POSIX_PATH_MAX
#endif

#define bin_to_ascii(c) ((c)>=38?((c)-38+'a'):(c)>=12?((c)-12+'A'):(c)+'.')

void main (int argc, char **argv)
{
	FILE *fp,*ftpp;
	char *cryptstr, salt[2], ftp_path[MAXPATHLEN];
	time_t tm;
	struct group *grp;
	struct passwd *pwd;

	/* check for errors */
	if (argc!=4)
	{
		printf("Usage: mkgroup group pass real_group\n");
		exit(1);
	}
	if ((pwd=getpwnam("ftp"))==NULL)
	{
		printf("Can't find `ftp' user entry in /etc/passwd\n");
		exit(1);
	}
	strcat(ftp_path,pwd->pw_dir);
	strcat(ftp_path,"/etc/group");
	if ((ftpp=fopen(ftp_path,"r"))==NULL)
	{
		printf("The anonymous ftp directory `%s' doesn't exist\n",ftp_path);
		exit(1);
	}
	while ((grp=(struct group *)fgetgrent(ftpp)) && strcmp(argv[3],grp->gr_name));
	if (grp==(struct group *)NULL)
	{
		printf("Can't find `%s' group entry in %s\n",argv[3],ftp_path);
		exit(1);
	}
	if ((fp=fopen(GROUP_FILE,"r"))==NULL)
	{
		printf("You must be in the same directory as the %s file\n",GROUP_FILE);
		exit(1);
	}
	if (getgrnam(argv[3]) == (struct group *)NULL)
	{
		printf("Can't find `%s' group entry in /etc/group\n",argv[3]);
		exit(1);
	}
	freopen(GROUP_FILE,"a",fp);
	time(&tm);
	salt[0] = bin_to_ascii(tm & 0x3f);
	salt[1] = bin_to_ascii((tm >> 5) & 0x3f);
	cryptstr = crypt(argv[2], salt);
	fprintf(fp,"%s:%s:%s\n",argv[1],cryptstr,argv[3]);

	fclose(fp);
	fclose(ftpp);
}
