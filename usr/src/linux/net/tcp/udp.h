/* udp.h */
/*
    Copyright (C) 1992  Ross Biro

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 

    The Author may be reached as bir7@leland.stanford.edu or
    C/O Department of Mathematics; Stanford University; Stanford, CA 94305
*/
/* $Id: udp.h,v 0.8.4.1 1992/11/10 00:17:18 bir7 Exp $ */
/* $Log: udp.h,v $
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.2  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added $iId$ and $Log: udp.h,v $
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *.
 * */

#ifndef _TCP_UDP_H
#define _TCP_UDP_H

struct udp_header
{
	unsigned short source;
	unsigned short dest;
	unsigned short len;
	unsigned short check;
};

extern struct proto udp_prot;
#define UDP_NO_CHECK 1

#endif
