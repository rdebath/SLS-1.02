/*
Date: 11 Mar 92 21:37:37 GMT
Subject: rdev - query/set root device
From: almesber@nessie.cs.id.ethz.ch (Werner Almesberger)
Organization: Swiss Federal Institute of Technology (ETH), Zurich, CH

With all that socket, X11, disk driver and FS hacking going on, apparently
nobody has found time to address one of the minor nuisances of life: set-
ting the root FS device is still somewhat cumbersome. I've written a little
utility which can read and set the root device in boot images:

rdev accepts an optional offset argument, just in case the address should
ever move from 508. If called without arguments, rdev outputs an mtab line
for the current root FS, just like /etc/rootdev does.

ramsize sets the size of the ramdisk.  If size is zero, no ramdisk is used.

vidmode sets the default video mode at bootup time.  -1 uses default video
mode, -2 uses menu.
ram
raia - Werner
   _________________________________________________________________________
  / Werner Almesberger, ETH Zuerich, CH      almesber@nessie.cs.id.ethz.ch /
 / IFW A44  Tel. +41 1 254 7213                 almesberger@rzvax.ethz.ch /
/_BITNET:_ALMESBER@CZHETH5A__HEPNET/CHADNET:_[20579::]57414::ALMESBERGER_/

*/

/* rdev.c  -  query/set root device. */

usage()
{

    puts("usage: rdev [ -orsv ] IMAGE [ DEV [ OFFSET ] ]");
    puts("  rdev /dev/at0  (or rdev /linux, etc.) displays the current ROOT device");
    puts("  rdev /dev/at0 /dev/hda2         sets ROOT to /dev/hda2");
    puts("  rdev -s /dev/at0 /dev/hda2      set the SWAP device");
    puts("  rdev -r /dev/at0 627            set the RAMDISK size");
    puts("  rdev -v /dev/at0 1              set the bootup VIDEOMODE");
    puts("  rdev -o N ...                   use the byte offset N");
    puts("  sdev ...                        same as rdev -s");
    puts("  ramsize ...                     same as rdev -r");
    puts("  vidmode ...                     same as rdev -v");
    puts("Note: video modes are: -3=Ask, -2=Extended, -1=NormalVga, 1=key1, 2=key2,...");
    exit(-1);
}

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>


#define DEFAULT_OFFSET 508


static void die(char *msg)
{
    perror(msg);
    exit(1);
}


static char *find_dev(int number)
{
    DIR *dp;
    struct dirent *dir;
    static char name[PATH_MAX+1];
    struct stat s;

    if (!number) return "Boot device";
    if ((dp = opendir("/dev")) == NULL) die("opendir /dev");
    strcpy(name,"/dev/");
    while (dir = readdir(dp)) {
	strcpy(name+5,dir->d_name);
	if (stat(name,&s) < 0) die(name);
	if ((s.st_mode & S_IFMT) == S_IFBLK && s.st_rdev == number) return name;
    }
    sprintf(name,"0x%04x",number);
    return name;
}

/* enum { RDEV, SDEV, RAMSIZE, VIDMODE }; */
enum { RDEV, VIDMODE, RAMSIZE, SDEV }; 
char *cmdnames[4] = { "rdev", "vidmode",  "ramsize", "swapdev" }; 
char *desc[4] = { "Root device", "Video mode",  "Ramsize",  "Swap device" };
#define shift(n) argv+=n,argc-=n

int main(int argc,char **argv)
{
    int image,offset,dev_nr, i, newoffset=-1;
    char *device, cmd = 0, *ptr, tmp[40];
    struct stat s;

    device = NULL;
    if (ptr = strrchr(argv[0],'/'))
    	ptr++;
    else
    	ptr = argv[0];
    for (i=0; i<=3; i++)
    	if (!strcmp(ptr,cmdnames[i]))
	    break;
    cmd = i;
    if (cmd>3)
    	cmd=RDEV;
    offset = DEFAULT_OFFSET-cmd*2;

    while (1)
    { 
    	if (argv[1][0] != '-')
   	    break;
	else
	    switch (argv[1][1])
	    {
	    	case 'r': 
	    		cmd=RAMSIZE;
		        offset = DEFAULT_OFFSET-cmd*2;
	    		shift(1);
			break;
		case 'v':
	    		cmd=VIDMODE;
		        offset = DEFAULT_OFFSET-cmd*2;
	    		shift(1);
			break;
		case 's':
	    		cmd=SDEV;
		        offset = DEFAULT_OFFSET-cmd*2;
	    		shift(1);
			break;
		case 'o':
			if (argv[1][2])
			{
				newoffset=atoi(argv[1]+2);
				shift(1);
			} else {
				newoffset=atoi(argv[2]);
				shift(2);
			}
			break;
		default:
			usage();
	     }
    }
    if (newoffset >= 0)
	offset = newoffset;

    if  ((cmd==RDEV) && (argc == 1 || argc > 4)) {
	if (stat("/",&s) < 0) die("/");
	printf("%s /\n",find_dev(s.st_dev));
	exit(0);
    }
    if ((cmd==RDEV) || (cmd==SDEV))
    {	
	    if (argc == 4) {
		device = argv[2];
		offset = atoi(argv[3]);
	    }
	    else {
		if (argc == 3) {
		    if (isdigit(*argv[2])) offset = atoi(argv[2]);
		    else device = argv[2];
		}
	    }
    }
    else
    {
    	if (argc>=3)
		device = argv[2];
    	if (argc==4)
		offset = atoi(argv[3]);
    }
    if (device) {
    	if ((cmd==SDEV) || (cmd==RDEV))
	{	if (stat(device,&s) < 0) die(device);
	} else
		s.st_rdev=atoi(device);
	if ((image = open(argv[1],O_WRONLY)) < 0) die(argv[1]);
	if (lseek(image,offset,0) < 0) die("lseek");
	if (write(image,(char *)&s.st_rdev,2) != 2) die(argv[1]);
	if (close(image) < 0) die("close");
    }
    else {
	if ((image = open(argv[1],O_RDONLY)) < 0) die(argv[1]);
	if (lseek(image,offset,0) < 0) die("lseek");
	dev_nr = 0;
	if (read(image,(char *)&dev_nr,2) != 2) die(argv[1]);
	if (close(image) < 0) die("close");
	printf(desc[cmd]);
	if ((cmd==SDEV) || (cmd==RDEV))
		printf(" %s\n", find_dev(dev_nr));
	else
		printf(" %d\n", dev_nr);
    }
    return 0;
}
