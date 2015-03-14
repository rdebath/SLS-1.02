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
#include <termios.h>
#include <unistd.h>
#include <sys/types.h>

#include "log.h"
#include "tty.h"

int tty_fc(fd, state)
     int fd;
     fc_state state;
{
    struct termios t;
    
    if (tcgetattr(fd, &t) < 0) {
	log(L_EMERG, "tty_fc: can't get attrs: %m");
	return (-1);
    }

    switch (state) {
      case FC_OUTPUT_ON:
	t.c_iflag |= IXON;
	t.c_iflag &= ~IXOFF;
	break;
      case FC_INPUT_ON:
	t.c_iflag |= IXOFF;
	t.c_iflag &= ~IXON;
	break;
      case FC_BOTH_ON:
	t.c_iflag |= (IXON|IXOFF);
	break;
      case FC_BOTH_OFF:
	t.c_iflag &= ~(IXON|IXOFF);
	break;
    }

    if (tcsetattr(fd, TCSANOW, &t) < 0) {
	log(L_EMERG, "tty_fc: can't set attrs: %m");
	return (-1);
    }

    return (0);
}    

static int condition_tty(fd)
     int fd;
{
    struct termios t;

    if (tcgetattr(fd, &t) < 0) {
	log(L_EMERG, "condition_tty: can't get attrs: %m");
	return (-1);
    }

    t.c_iflag = IXON|IXOFF;
    t.c_oflag = 0;
    t.c_cflag = CS8|CREAD|CLOCAL;
    t.c_lflag = 0;


    cfsetispeed(&t, B19200);
    cfsetospeed(&t, B19200);

    if (tcsetattr(fd, TCSANOW, &t) < 0) {
	log(L_EMERG, "condition_tty: can't set attrs: %m");
	return (-1);
    }

    return (0);
}

int tty_open(filename)
     char *filename;
{
    int fd;
    
    if ((fd = open(filename, O_RDWR|O_NDELAY)) < 0) {
	log(L_EMERG, "can't open fax modem file: %m");
	return (-1);
    }
    
    if (condition_tty(fd) < 0) {
	close(fd);
	return (-1);
    }

    return (fd);
}

int tty_close(fd)
     int fd;
{
    return (close(fd));
}
