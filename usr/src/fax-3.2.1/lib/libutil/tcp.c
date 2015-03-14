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
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "tcp.h"

/*
  Make a socket that will listen for connection on the given
  services.  Return the fd of that listener socket.
*/
int
tcp_make_listener(service)
char *service;
{
    struct sockaddr_in sin;
    int on = 1;
    int port;
    int s;

    /* get the port */
    if ((port = service_get_portnum(service, "tcp")) < 0)
      return (-1);

    /* accept connections from any address on the given port */
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = port;
    
    /* create the socket for listening */
    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
      return (-1);

    if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &on, sizeof (on)) < 0) {
        close(s);
        return (-1);
    }
    
    /* bind address to the socket */
    if (bind(s, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
	close(s);
	return (-1);
    }

    /* listen for backlog of max 5 connection */
    listen(s, 5);

    return (s);
}

/*
  When given a listener socket, accept a new client connection on
  that socket.  Return the fd to the new client.  Enable sending
  keepalive packets to insure that client is alive.
*/
int
tcp_accept_connection(s)
int s;
{
    struct sockaddr_in from;
    int len = sizeof (from);
    int new;
    int on = 1;

    /* accept a connection from the listener, and get new fd */
    if ((new = accept(s, (struct sockaddr *)&from, &len)) < 0)
      return (-1);

    /* enable keepalive messages */
    if (setsockopt(new, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0) {
	close(new);
	return (-1);
    }

    return (new);
}

/*
  Establish a stream connection to the given host with the given
  services.  Return an fd to the connection.
*/
int
tcp_make_connection(host, service)
char *host;
char *service;
{
    int port;
    struct hostent *hp;
    struct sockaddr_in sin;
    int s;

    /* get the port */
    if ((port = service_get_portnum(service, "tcp")) < 0)
      return (-1);

    /* find the host */
    if ((hp = gethostbyname(host)) == NULL)
      return (-1);

    /* set address and port for the connection */
    bcopy(hp->h_addr, (char *)&sin.sin_addr, hp->h_length);
    sin.sin_port = port;
    sin.sin_family = hp->h_addrtype;
    
    /* create the socket for the connection */
    if ((s = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0)
      return (-1);
    
    /* connect to the host */
    if (connect(s, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
	close(s);
	return (-1);
    }

    return (s);
}
