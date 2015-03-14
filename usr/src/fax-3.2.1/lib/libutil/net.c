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
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include "net.h"
#include "bool.h"

/*
  Given a service and a protocol, return the port number to use.
*/
int
service_get_portnum(service, proto)
char *service;
char *proto;
{
    int port;
    struct servent *se;

    /* lookup the port number */
    if (isdigit(service[0]))
      port = atoi(service);
    else {
	if ((se = getservbyname(service, proto)) == NULL)
	  return (-1);
	port = se->s_port;
    }

    return (port);
}
