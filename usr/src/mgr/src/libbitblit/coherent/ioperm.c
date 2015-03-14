/* ioperm for Coherent 4.0 by Harry C. Pulley */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "bm.h"

extern graphics_fd;

int ioperm(offset,flag)
unsigned offset; 
int flag;
{
	struct bmform bmstruc;

	errno=0;

	bmstruc.arg.io.offset=offset;

	ioctl(graphics_fd,flag?KDADDIO:KDDELIO,&bmstruc);

	if (errno==0)
		return bmstruc.arg.io.val;
	else
	{
		fprintf(stderr,"ioperm: ioctl: %s\n",strerror(errno));
		return 0;
	}
}
