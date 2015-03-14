/*
 * dip		A program for handling dialup IP connecions.
 *		This module handles the PPP protocol.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"

#ifdef DIP_PPP
int do_ppp(fd, addr, mtu)
int fd;
struct in_addr addr;
int mtu;
{
  fprintf(stderr, "PPP protocol not available yet.\n");
  return(-1);
}
#endif	/* DIP_PPP */
