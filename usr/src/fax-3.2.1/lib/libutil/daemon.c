/*
  This file is part of the NetFax system.

  (c) Copyright 1989 by David M. Siegel and Sundar Narasimhan.
      All rights reserved.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation.

    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of 
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <fcntl.h>
#include <sys/ioctl.h>

int
start_daemon()
{
    int s;
    int pid;

    if ((pid = fork()) != 0) {
      /*
	If the for returns less than zero, than it failed.  
      */
      if (pid < 0) {
	  perror("start_daemon: fork");
	  return (-1);
      }

      /*
	Otherwise this is the parent.  We simply exit.
      */
      exit(0);
    }
    
    /*
      At this point we are the child...
    */

    for (s = getdtablesize()-1; s >= 0; --s)
      close(s);

    open("/dev/null", O_RDONLY);
    dup2(0, 1);
    dup2(0, 2);
	
    /* 
      Disassociate server from controlling terminal...
    */
    if ((s = open("/dev/tty", O_RDWR)) >= 0) {
	ioctl(s, TIOCNOTTY, 0);
	close(s);
    }
    
    return (0);
}
