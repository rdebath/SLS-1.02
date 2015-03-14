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

#ifndef INCneth
#define INCneth 1

/*
  Prototypes:
*/

int is_my_in_addr(
#ifdef _PROTO
    struct in_addr *addr
#endif
);

int service_get_portnum(
#ifdef _PROTO
    char *service,
    char *proto
#endif
);

int hostname_get_addr(
#ifdef _PROTO
    char *hostname,
    char *addr,
    int len,
    int type
#endif
);

int addr_get_hostname(
#ifdef _PROTO
    char *addr,
    int len,
    int type,
    char *buf,
    int buflen
#endif
);

char *in_addr_hostname(
#ifdef _PROTO
    struct in_addr *addr
#endif
);

#endif
