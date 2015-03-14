/* dev.c */
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
/* $Id: dev.c,v 0.8.4.13 1993/01/23 18:00:11 bir7 Exp $ */
/* $Log: dev.c,v $
 * Revision 0.8.4.13  1993/01/23  18:00:11  bir7
 * Fixed problems from merging.
 *
 * Revision 0.8.4.12  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.11  1993/01/22  22:58:08  bir7
 * Changed so transmitting takes place in bottom half of interrupt routine.
 *
 * Revision 0.8.4.10  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.9  1992/12/12  01:50:49  bir7
 * *** empty log message ***
 *
 * Revision 0.8.4.8  1992/12/08  20:49:15  bir7
 * Edited ctrl-h's out of log messages.
 *
 * Revision 0.8.4.7  1992/12/06  23:29:59  bir7
 * Converted to using lower half interrupt routine.
 *
 * Revision 0.8.4.6  1992/12/05  21:35:53  bir7
 * Updated dev->init type.
 *
 * Revision 0.8.4.5  1992/12/03  19:52:20  bir7
 * Added paranoid queue checking.
 *
 * Revision 0.8.4.4  1992/11/18  15:38:03  bir7
 * Fixed bug in copying packets and changed some printk's
 *
 * Revision 0.8.4.3  1992/11/15  14:55:30  bir7
 * More sanity checks.
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.5  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added Id and Log
 *
 */

#include <asm/segment.h>
#include <asm/system.h>
#include <linux/config.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/socket.h>
#include <netinet/in.h>
#include "dev.h"
#include "eth.h"
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include <linux/errno.h>
#include <linux/interrupt.h>
#include "arp.h"

#undef DEV_DEBUG
#ifdef DEV_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif


static  unsigned long
min(unsigned long a, unsigned long b)
{
   if (a < b) return (a);
   return (b);
}

void
dev_add_pack (struct packet_type *pt)
{
   struct packet_type *p1;
   pt->next = ptype_base;

   /* see if we need to copy it. */
   for (p1 = ptype_base; p1 != NULL; p1 = p1->next)
     {
	if (p1->type == pt->type)
	  {
	     pt->copy = 1;
	     break;
	  }
     }

   ptype_base = pt;
   
}

void
dev_remove_pack (struct packet_type *pt)
{
   struct packet_type *lpt, *pt1;
   if (pt == ptype_base)
     {
	ptype_base = pt->next;
	return;
     }

   lpt = NULL;

   for (pt1 = ptype_base; pt1->next != NULL; pt1=pt1->next)
     {
	if (pt1->next == pt )
	  {
	     cli();
	     if (!pt->copy && lpt) 
	       lpt->copy = 0;
	     pt1->next = pt->next;
	     sti();
	     return;
	  }

	if (pt1->next -> type == pt ->type)
	  {
	     lpt = pt1->next;
	  }
     }
}

struct device *
get_dev (char *name)
{
   struct device *dev;
   for (dev = dev_base; dev != NULL; dev=dev->next)
     {
	if (strcmp (dev->name, name) == 0) return (dev);
     }
   return (NULL);
}

void
dev_queue_xmit (struct sk_buff *skb, struct device *dev, int pri)
{
  struct sk_buff *skb2;
  int where=0; /* used to say if the packet should go at the
		  front or the back of the queue. */

  PRINTK (("dev_queue_xmit (skb=%X, dev=%X, pri = %d)\n", skb, dev, pri));

  if (dev == NULL)
    {
      printk ("dev.c: dev_queue_xmit: dev = NULL\n");
      return;
    }
  
  skb->dev = dev;
  if (skb->next != NULL)
    {
      /* make sure we haven't missed an interrupt. */
       dev->hard_start_xmit (NULL, dev);
       return;
    }

  if (pri < 0)
    {
      pri = -pri-1;
      where = 1;
    }

  if ( pri >= DEV_NUMBUFFS)
    {
       printk ("bad priority in dev_queue_xmit.\n");
       pri = 1;
    }

  if (dev->hard_start_xmit(skb, dev) == 0)
    {
       return;
    }

  /* put skb into a bidirectional circular linked list. */
  PRINTK (("dev_queue_xmit dev->buffs[%d]=%X\n",pri, dev->buffs[pri]));
  /* interrupts should already be cleared by hard_start_xmit. */
  cli();
  if (dev->buffs[pri] == NULL)
    {
      dev->buffs[pri]=skb;
      skb->next = skb;
      skb->prev = skb;
    }
  else
    {
      if (where)
	{
	  skb->next = (struct sk_buff *)dev->buffs[pri];
	  skb->prev = (struct sk_buff *)dev->buffs[pri]->prev;
	  skb->prev->next = skb;
	  skb->next->prev = skb;
	  dev->buffs[pri] = skb;
	}
      else
	{
	  skb2= (struct sk_buff *)dev->buffs[pri];
	  skb->next = skb2;
	  skb->prev = skb2->prev;
	  skb->next->prev = skb;
	  skb->prev->next = skb;
	}
    }
  skb->magic = DEV_QUEUE_MAGIC;
  sti();

}


/* this routine now just gets the data out of the card and returns.
   it's return values now mean.

   1 <- exit even if you have more packets.
   0 <- call me again no matter what.
  -1 <- last packet not processed, try again.

  It's changed now 
  1 <- exit I can't do any more
  0 <- feed me more. 

  */

static volatile struct sk_buff * volatile backlog = NULL;

int
dev_rint(unsigned char *buff, long len, int flags,
	 struct device * dev)
{
   struct sk_buff *skb=NULL;
   unsigned char *to;
   int amount;

   if (dev == NULL || buff == NULL || len <= 0) return (1);

   if (flags & IN_SKBUFF)
     {
       skb = (struct sk_buff *)buff;
     }
   else
     {
       skb = kmalloc (sizeof (*skb) + len, GFP_ATOMIC);
       if (skb == NULL)
	 {
	   printk ("dev_rint:dropping packet due to lack of memory.\n");
	   return (1);
	 }
       skb->lock = 0;
       skb->mem_len = sizeof (*skb) + len;
       skb->mem_addr = (struct sk_buff *)skb;
       /* first we copy the packet into a buffer, and save it for later. */

       to = (unsigned char *)(skb+1);
       while (len > 0)
	 {
	   amount = min (len, (unsigned long) dev->rmem_end -
			 (unsigned long) buff);
	   memcpy (to, buff, amount);
	   len -= amount;
	   buff += amount;
	   to += amount;
	   if ((unsigned long)buff == dev->rmem_end)
	     buff = (unsigned char *)dev->rmem_start;
	 }
     }

   skb->len = len;
   skb->dev = dev;
   skb->sk = NULL;

   /* now add it to the backlog. */
   cli();
   if (backlog == NULL)
     {
       skb->prev = skb;
       skb->next = skb;
       backlog = skb;
     }
   else
     {
       skb->prev = (struct sk_buff *)backlog->prev;
       skb->next = (struct sk_buff *)backlog;
       skb->next->prev = skb;
       skb->prev->next = skb;
     }
   sti();
   
   if (backlog != NULL)
     mark_bh(INET_BH);

  return (0);
}

void
dev_transmit(void)
{
  struct device *dev;

  for (dev = dev_base; dev != NULL; dev=dev->next)
    {
      if (!dev->tbusy)
	{
	  dev_tint (dev);
	}
    }
}

void
inet_bh(void *tmp)
{
  struct sk_buff *skb;
  struct packet_type *ptype;
  unsigned short type;
  unsigned char flag =0;
  static volatile int in_bh=0;

  cli();
  if (in_bh != 0)
    {
      sti();
      return;
    }
  in_bh=1;
  sti();

  dev_transmit();
  /* anything left to process? */
  
  cli();
  while (backlog != NULL)
     {
       skb= (struct sk_buff *)backlog;
       if (skb->next == skb)
	 {
	   backlog = NULL;
	 }
       else
	 {
	   backlog = skb->next;
	   skb->next->prev = skb->prev;
	   skb->prev->next = skb->next;
	 }
       sti();

       /* bump the pointer to the next structure. */
       skb->h.raw = (unsigned char *)(skb+1) + skb->dev->hard_header_len;
       skb->len -= skb->dev->hard_header_len;

       /* convert the type to an ethernet type. */
       type = skb->dev->type_trans ((struct sk_buff *)skb, skb->dev);

       /* if there get to be a lot of types we should changes this to
	  a bunch of linked lists like we do for ip protocols. */
       for (ptype = ptype_base; ptype != NULL; ptype=ptype->next)
	 {
	   if (ptype->type == type)
	     {
	       struct sk_buff *skb2;
	       /* copy the packet if we need to. */
	       if (ptype->copy)
		 {
		   skb2 = kmalloc (skb->mem_len, GFP_ATOMIC);
		   if (skb2 == NULL) continue;
		   memcpy (skb2, (const void *) skb, skb->mem_len);
		   skb2->mem_addr = skb2;
		   skb2->lock = 0;
		   skb2->h.raw = (void *)((unsigned long)skb2
					  + (unsigned long)skb->h.raw
					  - (unsigned long)skb);

		 }
	       else
		 {
		   skb2 = (struct sk_buff *)skb;
		   flag = 1;
		 }
	       
	       ptype->func (skb2, skb->dev, ptype);
	     }
	 }

       if (!flag)
	 {
	   PRINTK (("discarding packet type = %X\n", type));
	   kfree_skb ((struct sk_buff *)skb, FREE_READ);
	 }
       dev_transmit();
       cli();
     }
  in_bh = 0;
  sti();
}

/* This routine is called when an device interface is ready to
   transmit a packet.  Buffer points to where the packet should
   be put, and the routine returns the length of the packet.  A
   length of zero is interrpreted to mean the transmit buffers
   are empty, and the transmitter should be shut down. */

/* now the packet is passed on via the other call. */

void
dev_tint( struct device *dev)
{
  int i;
  struct sk_buff *skb;
  for (i=0; i < DEV_NUMBUFFS; i++)
    {
      while (dev->buffs[i]!=NULL)
	{
	  cli();
	  skb=(struct sk_buff *)dev->buffs[i];
	  if (skb->magic != DEV_QUEUE_MAGIC)
	    {
	      printk ("dev.c skb with bad magic-%X: squashing queue\n",
		      skb->magic);
	      cli();
	      dev->buffs[i] = NULL;
	      sti();
	      continue;
	    }

	  skb->magic = 0;

	  if (skb->next == skb)
	    {
	      dev->buffs[i] = NULL;
	    }
	  else
	    {
	      /* extra consistancy check. */
	      if (skb->next == NULL
#ifdef CONFIG_MAX_16M
		  || (unsigned long)(skb->next) > 16*1024*1024
#endif
		  )

		{
		  printk ("dev.c: *** bug bad skb->next, squashing queue \n");
		  cli();
		  dev->buffs[i] = NULL;
		}
	      else
		{
		  dev->buffs[i]= skb->next;
		  skb->prev->next = skb->next;
		  skb->next->prev = skb->prev;
		}
	    }
	  skb->next = NULL;
	  skb->prev = NULL;
	  sti();
	  /* this will send it through the process again. */
	  dev->queue_xmit (skb, dev, -i-1);
	  if (dev->tbusy)
	  	return;
	}
    }
}
