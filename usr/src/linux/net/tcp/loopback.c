/* loopback.c contains the loopback device functions. */
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
/* $Id: loopback.c,v 0.8.4.8 1993/01/23 18:00:11 bir7 Exp $ */
/* $Log: loopback.c,v $
 * Revision 0.8.4.8  1993/01/23  18:00:11  bir7
 * Fixed problems introduced by merge.
 *
 * Revision 0.8.4.7  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.6  1993/01/22  22:58:08  bir7
 * Changed so transmitting takes place in bottom half of interrupt routine.
 *
 * Revision 0.8.4.5  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.4  1992/12/05  21:35:53  bir7
 * changed dev->init to return an int.
 *
 * Revision 0.8.4.3  1992/11/18  15:38:03  bir7
 * Fixed bug in start_xmit.
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.2  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added Id and Log
 * */

#include <linux/config.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/fs.h>
#include <linux/tty.h>
#include <linux/types.h>
#include <linux/ptrace.h>
#include <linux/string.h>
#include <asm/system.h>
#include <asm/segment.h>
#include <asm/io.h>
#include <errno.h>
#include <linux/fcntl.h>
#include <netinet/in.h>

#include "dev.h"
#include "eth.h"
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include "arp.h"

#ifdef PRINTK
#undef PRINTK
#endif

#ifdef LOOPBACK_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

static int
loopback_xmit(struct sk_buff *skb, struct device *dev)
{
  int done;
  if (!skb || !dev) return 0;
  PRINTK (("loopback_xmit (dev = %X)\n", dev));
  cli();
  if (dev->tbusy != 0)
    {
       sti();
       return (1);
    }
  dev->tbusy = 1;
  sti();

  done = dev_rint ((unsigned char *)(skb+1), skb->len, 0, dev);

  if (skb->free)
    kfree_skb (skb, FREE_WRITE);

  while (done != 1)
	 {
	   done = dev_rint (NULL, 0, 0, dev);
	 }

  dev->tbusy = 0;

  return (0);
}

int
loopback_init(struct device *dev)
{
   printk ("Loopback device init\n");
  /* initialize the rest of the device structure. */
  dev->mtu = 2000; /* mtu */
  dev->tbusy = 0;
  dev->hard_start_xmit = loopback_xmit;
  dev->open = NULL;
  dev->hard_header = eth_hard_header;
  dev->add_arp = NULL;
  dev->hard_header_len = sizeof (struct enet_header);
  dev->addr_len = ETHER_ADDR_LEN;
  dev->type = ETHER_TYPE;
  dev->queue_xmit = dev_queue_xmit;
  dev->rebuild_header = eth_rebuild_header;
  dev->type_trans = eth_type_trans;
  dev->loopback = 1;
  return (0);
}
