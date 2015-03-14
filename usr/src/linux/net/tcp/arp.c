/* arp.c */
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
/* $Id: arp.c,v 0.8.4.8 1993/01/23 18:00:11 bir7 Exp $ */
/* $Log: arp.c,v $
 * Revision 0.8.4.8  1993/01/23  18:00:11  bir7
 * Added ioctls as supplied by R.P. Bellis <rpb@psy.ox.ac.uk>
 *
 * Revision 0.8.4.7  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.5  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.4  1992/12/03  19:52:20  bir7
 * Added paranoid queue checking.
 *
 * Revision 0.8.4.3  1992/11/15  14:55:30  bir7
 * Put more cli/sti pairs in send_q and another sanity check
 * in arp_queue.
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.3  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added Id and Log
 *
 */

#include <linux/types.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/config.h>

#include <linux/socket.h>
#include <netinet/in.h>
#include <linux/sock_ioctl.h>
#include <linux/errno.h>
#include <asm/segment.h>
#include <asm/system.h>

#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include "arp.h"

#undef ARP_DEBUG

#ifdef  ARP_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

static volatile struct arp_table *arp_table[ARP_TABLE_SIZE] ={NULL, };
volatile struct sk_buff *arp_q=NULL;

/* this will try to retransmit everything on the queue. */
static void
send_arp_q(void)
{
   struct sk_buff *skb;
   struct sk_buff *next;

   cli();
   next = (struct sk_buff *)arp_q;
   arp_q = NULL;
   sti();
   while ((skb = next) != NULL) {
     if (skb->magic != ARP_QUEUE_MAGIC)
       {
	 printk ("arp.c skb with bad magic - %X: squashing queue\n", skb->magic);
	 return;
       }
     /* extra consistancy check. */
     if (skb->next == NULL
#ifdef CONFIG_MAX_16M
	 || (unsigned long)(skb->next) > 16*1024*1024
#endif
	 )
       {
	 printk ("dev.c: *** bug bad skb->next, squashing queue \n");
	 return;
       }

     /* first remove skb from the queue. */
     next = (struct sk_buff *)skb->next;
     if (next == skb)
       {
	 next = NULL;
       }
     else
       {
	 skb->prev->next = next;
	 next->prev = skb->prev;
       }

     skb->magic = 0;
     skb->next = NULL;
     skb->prev = NULL;

     if (!skb->dev->rebuild_header (skb+1, skb->dev))
       {
	   skb->next = NULL;
	   skb->prev = NULL;
	   skb->arp  = 1;
	   skb->dev->queue_xmit (skb, skb->dev, 0);
	}
     else
       {
	 cli();
	 skb->magic = ARP_QUEUE_MAGIC;      
	 if (arp_q == NULL)
	   {
	     skb->next = skb;
	     skb->prev = skb;
	     arp_q = skb;
	   }
	 else
	   {
	     skb->next = arp_q;
	     skb->prev = arp_q->prev;  
	     arp_q->prev->next = skb;
	     arp_q->prev = skb;
	   }
	 sti();
       }
   }
}

static  void
print_arp(struct arp *arp)
{
  int i;
  unsigned long *lptr;
  unsigned char *ptr;
  PRINTK (("arp: \n"));
  if (arp == NULL)
    {
      PRINTK (("(null)\n"));
      return;
    }
  PRINTK (("   hrd = %d\n",net16(arp->hrd)));
  PRINTK (("   pro = %d\n",net16(arp->pro)));
  PRINTK (("   hlen = %d plen = %d\n",arp->hlen, arp->plen));
  PRINTK (("   op = %d\n", net16(arp->op)));
  ptr = (unsigned char *)(arp+1);
  PRINTK (("   sender haddr = "));
  for (i = 0; i < arp->hlen; i++)
    {
      PRINTK (("0x%02X ",*ptr++));
    }
  lptr = (void *)ptr;
  PRINTK ((" send paddr = %X\n",*lptr));
  lptr ++;
  ptr = (void *)lptr;
  PRINTK (("   destination haddr = "));
  for (i = 0; i < arp->hlen; i++)
    {
      PRINTK (("0x%02X ",*ptr++));
    }
  lptr = (void *)ptr;
  PRINTK ((" destination paddr = %X\n",*lptr));
}

static  unsigned char *
arp_sourceh(struct arp *arp)
{
  unsigned char *ptr;
  ptr = (unsigned char *)(arp + 1);
  return (ptr);
}

static  unsigned char *
arp_targeth(struct arp *arp)
{
  unsigned char *ptr;
  ptr = (unsigned char *)(arp + 1);
  ptr += arp->hlen+4;
  return (ptr);
}

static  unsigned long *
arp_sourcep(struct arp *arp)
{
  unsigned long *lptr;
  unsigned char *ptr;
  ptr = (unsigned char *)(arp + 1);
  ptr += arp->hlen;
  lptr = (unsigned long *)ptr;
  return (lptr);
}


static  unsigned long *
arp_targetp(struct arp *arp)
{
  unsigned long *lptr;
  unsigned char *ptr;
  ptr = (unsigned char *)(arp + 1);
  ptr += 2*arp->hlen+4;
  lptr = (unsigned long *)ptr;
  return (lptr);
}

static  void
arp_free (void *ptr, unsigned long len)
{
  kfree_s(ptr, len);
}

static  void *
arp_malloc (unsigned long amount, int priority)
{
  return (kmalloc (amount, priority));
}

static  int
arp_response (struct arp *arp1, struct device *dev)
{
  struct arp *arp2;
  struct sk_buff *skb;
  int tmp;

  /* get some mem and initialize it for the return trip. */
  skb = arp_malloc (sizeof (*skb) + sizeof (*arp2) +
		    2*arp1->hlen + 2*arp1->plen + dev->hard_header_len,
		    GFP_ATOMIC);
  if (skb == NULL) return (1);

  skb->lock = 0;
  skb->mem_addr = skb;
  skb->mem_len = sizeof (*skb) + sizeof (*arp2) + 2*arp1->hlen + 
    2*arp1->plen + dev->hard_header_len;
  skb->len = sizeof (*arp2) + 2*arp1->hlen + 
    2*arp1->plen + dev->hard_header_len;

  tmp = dev->hard_header((unsigned char *)(skb+1), dev,
			 ETHERTYPE_ARP, *arp_sourcep(arp1),
			 *arp_targetp(arp1),skb->len);

  if (tmp < 0) return (1);

  arp2 =(struct arp *) ((unsigned char *)skb+sizeof (*skb) + tmp );
  memcpy (arp2, arp1, sizeof (*arp2));

  /* now swap the addresses. */
  *arp_sourcep(arp2) = *arp_targetp(arp1);
  memcpy(arp_sourceh(arp2), dev->dev_addr, arp1->hlen);

  *arp_targetp(arp2) = *arp_sourcep(arp1);
  memcpy(arp_targeth(arp2), arp_sourceh(arp1), arp1->hlen);

  arp2->op = NET16(ARP_REPLY);
  skb->free = 1;
  skb->arp = 1; /* so the code will know it's not waiting on an arp. */
  skb->sk = NULL;
  skb->next = NULL;
  PRINTK ((">>"));
  print_arp(arp2);
  /* send it. */
  dev->queue_xmit (skb, dev, 0);
  return (0);
}

/* This will find an entry in the arp table by looking at the ip
   address. */
static  struct arp_table *
arp_lookup (unsigned long paddr)
{
  unsigned long hash;
  struct arp_table *apt;
  PRINTK (("arp_lookup(paddr=%X)\n", paddr));
  /* we don't want to arp ourselves. */
  if (my_ip_addr(paddr)) return (NULL);
  hash = net32(paddr) & (ARP_TABLE_SIZE - 1);
  cli();
  for (apt = (struct arp_table *)arp_table[hash];
       apt != NULL;
       apt = (struct arp_table *)apt->next)
    {
      if (apt->ip == paddr)
	{
	   sti();
	   return (apt);
	}
    }
  sti();
  return (NULL);
}

void
arp_destroy(unsigned long paddr)
{
  unsigned long hash;
  struct arp_table *apt;
  struct arp_table **lapt;
  PRINTK (("arp_destroy (paddr=%X)\n",paddr));
  /* we don't want to destroy are own arp */
  if (my_ip_addr(paddr)) return;
  hash = net32(paddr) & (ARP_TABLE_SIZE - 1);

  cli(); /* can't be interrupted. */
  lapt = (struct arp_table **) &arp_table[hash];
  while ((apt = *lapt) != NULL) {
    if (apt->ip == paddr)
      {
      	*lapt = (struct arp_table *) apt->next;
      	arp_free(apt, sizeof(*apt));
      	sti();
      	return;
      }
    lapt = (struct arp_table **) &apt->next;
  }
  sti();
}

/* this routine does not check for duplicates.  It assumes the caller
   does. */
static  struct arp_table *
create_arp (unsigned long paddr, unsigned char *addr, int hlen)
{
  struct arp_table *apt;
  unsigned long hash;
  apt = arp_malloc (sizeof (*apt), GFP_ATOMIC);
  if (apt == NULL) return (NULL);

  hash = net32(paddr) & (ARP_TABLE_SIZE - 1);
  apt->ip = paddr;
  apt->hlen =hlen;
  memcpy (apt->hard, addr, hlen);
  apt->last_used=timer_seq;
  cli();
  apt->next = arp_table[hash];
  arp_table[hash] = apt;
  sti();
  return (apt);
}

int
arp_rcv(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
   struct arp *arp;
   struct arp_table *tbl;
   int ret;

   PRINTK (("<<\n"));
   arp = skb->h.arp;
   print_arp(arp);

  /* if this test doesn't pass, something fishy is going on. */
  if (arp->hlen != dev->addr_len || dev->type !=NET16( arp->hrd))
    {
       kfree_skb(skb, FREE_READ);
       return (0);
    }

  /* for now we will only deal with ip addresses. */
  if (arp->pro != NET16(ARP_IP_PROT) || arp->plen != 4)
    {
       kfree_skb (skb, FREE_READ);
       return (0);
    }

  /* now look up the ip address in the table. */
  tbl = arp_lookup (*arp_sourcep(arp));
  if (tbl != NULL)
    {
       memcpy (tbl->hard, arp+1, arp->hlen);
       tbl->hlen = arp->hlen;
       tbl->last_used = timer_seq;
    }

  if (my_ip_addr(*arp_targetp(arp)) != IS_MYADDR)
    {
       kfree_skb (skb, FREE_READ);
       return (0);
    }

  if (tbl == NULL)
    create_arp (*arp_sourcep(arp), arp_sourceh(arp), arp->hlen);

   /* now see if we can send anything. */
   send_arp_q();
     
  if (arp->op != NET16(ARP_REQUEST))
    {
       kfree_skb (skb, FREE_READ);
       return (0);
    }

  /* now we need to create a new packet. */
   ret = arp_response(arp, dev);
   kfree_skb (skb, FREE_READ);
   return (ret);
}

void
arp_snd (unsigned long paddr, struct device *dev, unsigned long saddr)
{
  struct sk_buff *skb;
  struct arp *arp;
  struct arp_table *apt;
  int tmp;
  PRINTK (("arp_snd (paddr=%X, dev=%X, saddr=%X)\n",paddr, dev, saddr));

  /* first we build a dummy arp table entry. */
  apt = create_arp (paddr, NULL, 0);
  if (apt == NULL) return;

  skb = arp_malloc (sizeof (*arp) + sizeof (*skb) + dev->hard_header_len +
		    2*dev->addr_len+8, GFP_ATOMIC);
  if (skb == NULL) return;
  
  skb->lock = 0;
  skb->sk = NULL;
  skb->mem_addr = skb;
  skb->mem_len = sizeof (*arp) + sizeof (*skb) + dev->hard_header_len +
		    2*dev->addr_len+8;
  skb->arp = 1;
  skb->dev = dev;
  skb->len = sizeof (*arp) + dev->hard_header_len + 2*dev->addr_len+8;
  skb->next = NULL;

  tmp = dev->hard_header ((unsigned char *)(skb+1), dev,
			  ETHERTYPE_ARP, 0, saddr, skb->len);
  if (tmp < 0)
    {
       arp_free (skb->mem_addr, skb->mem_len);
       return;
    }

  arp =(struct arp *) ((unsigned char *)skb+sizeof (*skb) + tmp );
  arp->hrd = net16(dev->type);
  arp->pro = NET16(ARP_IP_PROT);
  arp->hlen = dev->addr_len;
  arp->plen = 4;
  arp->op = NET16(ARP_REQUEST);
  *arp_sourcep(arp) = saddr;
  *arp_targetp(arp) = paddr;
  memcpy (arp_sourceh(arp), dev->dev_addr, dev->addr_len);
  memcpy (arp_targeth(arp), dev->broadcast, dev->addr_len);
  PRINTK((">>\n"));
  print_arp(arp);
  dev->queue_xmit (skb, dev, 0);
}

int
arp_find(unsigned char *haddr, unsigned long paddr, struct device *dev,
	   unsigned long saddr)
{
  struct arp_table *apt;
  PRINTK (("arp_find(haddr=%X, paddr=%X, dev=%X, saddr=%X)\n",
	  haddr, paddr, dev, saddr));
  if (my_ip_addr (paddr))
    {
      memcpy (haddr, dev->dev_addr, dev->addr_len);
      return (0);
    }
  apt = arp_lookup (paddr);
  if (apt != NULL)
    {
       /* make sure it's not too old. If it is too old, we will
	  just pretend we did not find it, and then arp_snd
	  will verify the address for us. */
       if (!before (apt->last_used, timer_seq+ARP_TIMEOUT) &&
	   apt->hlen != 0)
	 {
	    apt->last_used=timer_seq;
	    memcpy (haddr, apt->hard, dev->addr_len);
	    return (0);
	 }
	else
	 {
	    arp_destroy(paddr);
	 }
    }

  /* this assume haddr are at least 4 bytes.
     If this isn't true we can use a lookup
     table, one for every dev. */
  *(unsigned long *)haddr = paddr;

  /* if we didn't find an entry, we will try to 
     send an arp packet. */
  arp_snd(paddr,dev,saddr);

  return (1);
}

void
arp_add (unsigned long addr, unsigned char *haddr, struct device *dev)
{
   struct arp_table *apt;
   /* first see if the address is already in the table. */
   apt = arp_lookup (addr);
   if (apt != NULL)
     {
	apt->last_used = timer_seq;
	memcpy (apt->hard, haddr , dev->addr_len);
	return;
     }
   create_arp (addr, haddr, dev->addr_len);
}

void
arp_add_broad (unsigned long addr, struct device *dev)
{
  arp_add (addr,  dev->broadcast , dev);
}

void
arp_queue(struct sk_buff *skb)
{
  cli();
  if (skb->next != NULL)
    {
      sti();
      printk ("arp.c: arp_queue skb already on queue magic=%X. \n",
	      skb->magic);
      return;
    }
   if (arp_q == NULL)
     {
	arp_q = skb;
	skb->next = skb;
	skb->prev = skb;
     }
   else
     {
	skb->next = arp_q;
	skb->prev = arp_q->prev;
	skb->next->prev = skb;
	skb->prev->next = skb;
     }
  skb->magic = ARP_QUEUE_MAGIC;
   sti();
}

static int arpreq_check(struct arpreq *req)
{
	/* Check protocol familys */
	if (req->arp_pa.sa_family != AF_INET) return -1;

	if (req->arp_ha.sa_family != AF_UNSPEC) return -1;

	return 0;
}

int arp_ioctl_set(struct arpreq *req)
{
	struct arpreq		r;
	struct arp_table	*apt;

	memcpy_fromfs(&r, req, sizeof(r));
	if (arpreq_check(&r) != 0) return -EPFNOSUPPORT;

	apt = arp_lookup(*(unsigned long *)r.arp_pa.sa_data);
	if (apt) {
		apt->last_used = timer_seq;
		memcpy(apt->hard, r.arp_ha.sa_data , 6);
	} else {
		if (!create_arp(*(unsigned long *)r.arp_pa.sa_data,
				r.arp_ha.sa_data, 6)) {
			return -ENOMEM;
		}
	}

	return 0;
}

int arp_ioctl_get(struct arpreq *req)
{
	struct arpreq		r;
	struct arp_table	*apt;

	memcpy_fromfs(&r, req, sizeof(r));
	if (arpreq_check(&r) != 0) return -EPFNOSUPPORT;
	apt = arp_lookup(*(unsigned long *)r.arp_pa.sa_data);
	if (apt) {
		memcpy(r.arp_ha.sa_data, apt->hard, apt->hlen);
	} else {
		return -ENXIO;
	}

	/* Copy the information back */
	memcpy_tofs(req, &r, sizeof(r));
	return 0;
}

int arp_ioctl_del(struct arpreq *req)
{
	struct arpreq		r;

	memcpy_fromfs(&r, req, sizeof(r));
	if (arpreq_check(&r) != 0) return -EPFNOSUPPORT;

	arp_destroy(*(unsigned long *)r.arp_pa.sa_data);

	return 0;
}
