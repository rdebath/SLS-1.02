/* ---------------------------------------------------------------------
   tarsh.c					(c) 91 Michael Koehne


   Dies ist ein Tar-Extrakter, der fuer jede Datei Prozesse
   erzeugt, welche die Datei als stdin und den Namen als Argument
   bekommen. Ziemlich praktisch wenn auf einem Tape ein Tar
   mit vielen tar's ist :-)

   Usage : tarsh /dev/TAPE shell-sript
           lets try a : tarsh /dev/fd0 "wc -c;echo $*"

--------------------------------------------------------------------- */

#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <memory.h>

/* ------------------------------------------------------------------ */
/* Dieser Header stammt aus Minix von Tannenbaum */

/* The <tar.h> header is used with the tape archiver, tar. */

#ifndef _TAR_H
#define _TAR_H

#define TBLOCK 		512
#define NAMSIZ		100
#define PFXSIZ		155

#define TMODLEN 	8
#define TUIDLEN		8
#define TGIDLEN		8
#define TSIZLEN		12
#define TMTMLEN		12
#define TCKSLEN		8

#define TMAGIC		"ustar"
#define TMAGLEN		6
#define TVERSION	"00"
#define TVERSLEN	2
#define TUNMLEN		32
#define TGNMLEN		32
#define TDEVLEN		8

#define REGTYPE		'0'
#define AREGTYPE	'\0'
#define LNKTYPE		'1'
#define SYMTYPE		'2'
#define CHRTYPE		'3'
#define BLKTYPE		'4'
#define DIRTYPE		'5'
#define FIFOTYPE	'6'
#define CONTTYPE	'7'

#define TSUID		04000
#define TSGID		02000
#define TSVTX		01000

#define TUREAD		00400
#define TUWRITE		00200
#define TUEXEC		00100
#define TGREAD		00040
#define TGWRITE		00020
#define TGEXEC		00010
#define TOREAD		00004
#define TOWRITE		00002
#define TOEXEC		00001

struct Thead {
	char name[NAMSIZ];
	char mode[TMODLEN];
	char uid[TUIDLEN];
	char gid[TGIDLEN];
	char size[TSIZLEN];
	char mtime[TMTMLEN];
	char chksum[TCKSLEN];
	char typeflag;
	char linkname[NAMSIZ];
	char magic[TMAGLEN];
	char version[TVERSLEN];
	char uname[TUNMLEN];
	char gname[TGNMLEN];
	char devmajor[TDEVLEN];
	char devminor[TDEVLEN];
	char prefix[PFXSIZ];
	} ;

#endif /* _TAR_H */

char tblock[TBLOCK];
#define thead ((struct Thead *)tblock)

int uid,gid;
int ndots;

/* ------------------------------------------------------------------ */

void error(s)
char *s;
{	fprintf(stderr,"Error : %s\n",s);
	fflush(stderr);

	exit(0);
	}

void usage()
{	fprintf(stderr,"tarsh - Tar Extracter to a shell\n");
	fprintf(stderr,"Usage: tarsh DEVICE COMMAND\n");

	exit(0);
	}

/* ------------------------------------------------------------------ */


long otol(s)
char *s;
{	long val = 0;

	while ((*s==' ') || (*s=='\t')) s++;
	while ((*s>='0') && (*s<='8')) {
		val = val*8 + (*s++)-'0';
		} 
	return(val);
	}

char *ltoo(val,count)
long val;
int count;
{	static char buffer[16];

	if (count > 15) count=15;

	sprintf(buffer,"%0*.*lo",count,count,val);

	return(buffer);
	}

ltoo_patch(p,val,count)
char *p;
long val;
int count;
{	char buffer[32];

	if (count > 15) count=15;

	sprintf(buffer,"%0*.*lo",count-1,count-1,val);

	memcpy(p,buffer,count);
	}

/* ------------------------------------------------------------------ */

int checksum()
{	char *ptr;
	int  ac;

	ptr = thead->chksum;
	while (ptr < thead->chksum+8) *ptr++ = ' ';

	ptr = thead->name; ac = 0;
	while (ptr < ((char*)thead)+TBLOCK) ac += (int)(*ptr++);

	return ac;
	}

/* ------------------------------------------------------------------ */

tar_xtract(tar,cmd)
char *cmd,*tar;
{	int tfd;
	FILE *ofp;
	int nskip;
	long size;
	char pcmd[64];
	
	if ((tfd=open(tar,O_RDONLY)) < 0) 
		error("Tar-File kann nicht geoeffnet werden");

	while(1) {
		if (read(tfd,thead,TBLOCK) <= 0)
			error("Tar-Header kann nicht gelesen werden");

		if (!*thead->name) {
			break;
			}

		size=otol(thead->size);
		nskip = (size+TBLOCK-1l)/TBLOCK;

		if (thead->typeflag == LNKTYPE) {
			fprintf(stderr,"DTAR:Links werden nicht untersuetzt\n");
			}
		else
		if ((thead->typeflag == REGTYPE)
		|| (thead->typeflag == AREGTYPE)) {
		    int rws;

		    sprintf(pcmd,"%s %s",cmd,thead->name);
		    ofp=popen(pcmd,"w");
	
		/*
		    if (ofp) fprintf(stderr,"Extract to %s \n",pcmd);
		*/
		    if (!ofp) fprintf(stderr,"Can not open pipe %s \n",pcmd);

		    while (size>0) {
			if ((rws=read(tfd,thead,TBLOCK)) <= 0)
			   error("Tar-Block kann nicht gelesen werden");

			if (size > 512) size-=512;
			else {	rws=size;
				size=0;
				}

			if ((ofp) && (fwrite(thead,rws,1,ofp)<=0)) {
				pclose(ofp);
				ofp=0;
				}
			}
		    if (ofp) pclose(ofp);
		    }
		else {
		    while (nskip--)  if (read(tfd,thead,TBLOCK) <= 0)
			error("Tar-Block kann nicht gelesen werden");
		    }
		}

	fprintf(stderr,"\nEnd of Tar :-)\n\n");
	fflush(stderr);
	close(tfd);
	}

/* ------------------------------------------------------------------ */

main(argc,argv)
int argc;
char **argv;
{	char *tcmd,*tar;
	int todo=0;

	if (argc!=3) usage();
	tar=argv[1];
	tcmd=argv[2];

	fprintf(stderr,"\nTarSh (c) Michael_Kraehe@hb.maus.de\n\n");
	fflush(stderr);

	tar_xtract(tar,tcmd);
	}
