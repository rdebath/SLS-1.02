/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles setting the interface and
 *		its routing table entry.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


#define IP_MAXNAME	20
#define IP_SET_DEV	0x2401
#define IP_ADD_ROUTE	0x2402
#define IP_HANDOFF	0x2403

#define IP_FDEVUP	0xFFFFFFFFL

struct ip_config {
   char		 name[IP_MAXNAME];
   unsigned long paddr;
   unsigned long router;
   unsigned long net;
   unsigned long flags;
};


int rt_add(name, dest)
char *name;
struct in_addr *dest;
{
  char myname[128];
  struct ip_config ifa;
  struct in_addr myaddr;
  struct hostent *hp;
  int sock;

  /* Determine our own IP address. */
  if (gethostname(myname, 128) < 0) return(-1);
  if ((hp = gethostbyname(myname)) == (struct hostent *)NULL) return(-1);
  memcpy((char *) &myaddr, (char *) hp->h_addr_list[0], hp->h_length);

  /* Set up for "upping" the desired SLIP interface. */
  strncpy(ifa.name, name, IP_MAXNAME);
  ifa.flags = IP_FDEVUP;
  ifa.paddr = myaddr.s_addr;
/*  ifa.net = dest->s_addr;
  ifa.router = -1L; */
  ifa.net = 0L;
  ifa.router = 0L;
 
  /* Create a socket to the INET kernel. */
  if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror("socket");
	return(-1);
  }

  /* Fire up the new interface. */
  if (ioctl(sock, IP_SET_DEV, &ifa) < 0) {
	perror("ioctl SETDEV");
	(void) close(sock);
	return(-1);
  }

  /* Close the socket. */
  (void) close(sock);
  return(0);
}


void rt_del(name)
char *name;
{
  struct ip_config ifa;
  int sock;

  /* Set up for "downing" the desired SLIP interface. */
  strncpy(ifa.name, name, IP_MAXNAME);
  ifa.flags = 0L;
  ifa.paddr = -1L;
  ifa.net = -1L;
  ifa.router = -1L;
 
  /* Create a socket to the INET kernel. */
  if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	perror("socket");
	return;
  }

  if (ioctl(sock, IP_SET_DEV, &ifa) < 0) {
	perror("ioctl SETDEV");
	(void) close(sock);
	return;
  }

  /* Close the socket. */
  (void) close(sock);
}
