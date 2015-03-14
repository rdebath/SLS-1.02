/* $Header: /home/x_cvs/mit/server/ddx/x386/etc/kbd_mode.c,v 1.4 1992/08/25 09:02:29 dawes Exp $ */

/* Keyboard mode control program for 386BSD */

#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "x386OSD.h"

static int fd;

void
msg (char* s)
{
  perror (s);
  close (fd);
  exit (-1);
}
	
int
main(int argc, char** argv)
{

    if ((fd = open("/dev/vga",O_RDONLY,0)) <0)
      msg ("Cannot open /dev/vga");

    if (0 == strcmp (argv[1], "-u"))
      {
	if (ioctl (fd, CONSOLE_X_MODE_ON, 0))
	  msg ("Cannot change /dev/vga to X mode");
      }
    else if (0 == strcmp (argv[1], "-a"))
      {
	if (ioctl (fd, CONSOLE_X_MODE_OFF, 0))
	  msg ("Cannot change /dev/vga to X ascii");
#if 0
	close (fd);
	/* XXX open and close /dev/console.  This should not be needed, but
	   for some reason the io doesn't get directed back to /dev/console
	   otherwise. */
	if ((fd = open("/dev/console",O_RDONLY,0)) <0)
	  msg ("Cannot open /dev/console");
#endif
      }
    else
      {
#if 0
	close (fd);
	/* XXX open and close /dev/console.  This should not be needed, but
	   for some reason the io doesn't get directed back to /dev/console
	   otherwise. */
	if ((fd = open("/dev/console",O_RDONLY,0)) <0)
	  msg ("Cannot open /dev/console");
#endif
	close (fd);
	fprintf (stderr,"Usage: %s [-u|-a]\n",argv[0]);
	fprintf (stderr,"-u for sending up down key events in x mode.\n");
	fprintf (stderr,"-a for sending ascii keys in normal use.\n");
	exit (-1);
      }
    close (fd);
    return (0);
}
