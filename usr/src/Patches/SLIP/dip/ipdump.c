/*
 * dip		A program for handling dialup IP connecions.
 *		IP Datagram dumping routines.
 *
 * Author:      Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988-1992 MicroWalt Corporation
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */
#include "dip.h"


/* Dump the contents of an IP datagram. */
void ip_dump(ptr, len)
char *ptr;
int len;
{
  int hdr_ver;
  int hdr_len;
  int dta_len;
  int dta_off;
  struct in_addr src, dst;
  IP *ip;

  ip = (IP *) ptr;
  hdr_ver = (ip->v_ihl & 0xF0) >> 4;
  hdr_len = (ip->v_ihl & 0x0F) * sizeof(long);
  dta_len = ntohs(ip->length);
  dta_off = (ntohs(ip->fl_offs) & IPF_F_OFFSET) << 3 ;

  src.s_addr = ip->source;
  dst.s_addr = ip->dest;

  fprintf(stderr, "\r*****\n");
  fprintf(stderr, "IP: %s->", inet_ntoa(src));
  fprintf(stderr, "%s\n", inet_ntoa(dst));
  fprintf(stderr, " len %u ihl %u ttl %u prot %u",
	dta_len, ip->v_ihl & 0xFF, ip->ttl & 0xFF, ip->protocol & 0xFF);

  if (ip->tos != 0) fprintf(stderr, " tos %u", ip->tos);
  if (dta_off != 0 || (ntohs(ip->fl_offs) & IPF_MF))
	fprintf(stderr, " id %u offs %u", ntohs(ip->id), dta_off);

  if (ntohs(ip->fl_offs) & IPF_DF) fprintf(stderr, " DF");
  if (ntohs(ip->fl_offs) & IPF_MF) fprintf(stderr, " MF");
  fprintf(stderr, "\n*****\n");
  (void) fflush(stderr);
}
