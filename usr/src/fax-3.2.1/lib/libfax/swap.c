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

#include "c2proto.h"
#include "swap.h"

/*
  Builds table to swap the low order 4 bitswith the high order.
*/
static void init_swaptable(swaptable)
     unsigned char *swaptable;
{
    int i, j;

    for (i = 0; i < 256; i++) {
	j = ( ((i & 0x01) << 7) |
	     ((i & 0x02) << 5) |
	     ((i & 0x04) << 3) |
	     ((i & 0x08) << 1) |
	     ((i & 0x10) >> 1) |
	     ((i & 0x20) >> 3) |
	     ((i & 0x40) >> 5) |
	     ((i & 0x80) >> 7) );
	swaptable[i] = j;
    }
}

/* 
  Reverses the low order 8 bits of a byte
*/
unsigned char swap_bits(c)
     unsigned char c;
{
    static unsigned char swaptable[256];
    static int swaptable_init = FALSE;

    if (!swaptable_init) {
	init_swaptable(swaptable);
	swaptable_init = TRUE;
    }

    return(swaptable[c]);
}
