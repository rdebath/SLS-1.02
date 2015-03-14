/* mmap for Coherent 4.0 by Harry C. Pulley */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "bm.h"

extern graphics_fd;

char *mmap(useraddr,videoaddr,videosize)
int useraddr,videoaddr,videosize;
{
	struct bmform bmstruc;
	
	errno=0;

	bmstruc.arg.map.userAddr=useraddr;

	bmstruc.arg.map.videoAddr=videoaddr;

	bmstruc.arg.map.videoLen=videosize;

	ioctl(graphics_fd,KDMAPDISP,&bmstruc);

	if (errno==0)
		return (char *)bmstruc.arg.map.userAddr;
	else
	{
		fprintf(stderr,"mmap: ioctl: %s\n",strerror(errno));
		return (char *)-1;
	}
}
