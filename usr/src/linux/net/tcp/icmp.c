/* Internet Control Message Protocol (ICMP) icmp.c */

/*
    Copyright (C) 1992  Bob Harris

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

    The Author of tcpip package may be reached as bir7@leland.stanford.edu or
    C/O Department of Mathematics; Stanford University; Stanford, CA 94305

    The author of this file may be reached at rth@sparta.com or Sparta, Inc.
    7926 Jones Branch Dr. Suite 900, McLean Va 22102.
*/
/* $Id: icmp.c,v 0.8.4.9 1993/01/23 18:00:11 bir7 Exp $ */
/* $Log: icmp.c,v $
 * Revision 0.8.4.9  1993/01/23  18:00:11  bir7
 * added volatile keyword to many variables.
 *
 * Revision 0.8.4.8  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.7  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.6  1992/12/12  01:50:49  bir7
 * Fixed bug in call to err routine.
 *
 * Revision 0.8.4.5  1992/12/05  21:35:53  bir7
 * fixed type mismatch.
 *
 * Revision 0.8.4.4  1992/12/03  19:52:20  bir7
 * Fixed minor pugs in icmp_reply.
 *
 * Revision 0.8.4.3  1992/11/18  15:38:03  bir7
 * Fixed some printk's.
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

/* modified by Ross Biro bir7@leland.stanford.edu to do more than just
   echo responses. */

#include <linux/types.h>
#include <linux/sched.h>
#include <linux/kernel.h>	/* kfree_s */
#include <linux/fcntl.h>
#include <linux/socket.h>
#include <netinet/in.h>
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include <linux/errno.h>
#include <linux/timer.h>
#include <asm/system.h>
#include <asm/segment.h>
#include "icmp.h"
#ifdef PRINTK
#undef PRINTK
#endif

#undef ICMP_DEBUG

#ifdef ICMP_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

#define min(a,b) ((a)<(b)?(a):(b))

/* an array of errno for error messages from dest unreach. */
struct icmp_err icmp_err_convert[]=
{
   {ENETUNREACH, 1},
   {EHOSTUNREACH, 1},
   {ENOPROTOOPT, 1},
   {ECONNREFUSED, 1},
   {EOPNOTSUPP, 0},
   {EOPNOTSUPP, 0},
   {ENETUNREACH, 1},
   {EHOSTDOWN, 1},
   {ENONET, 1},
   {ENETUNREACH, 1},
   {EHOSTUNREACH, 1},
   {EOPNOTSUPP, 0},
   {EOPNOTSUPP, 0}
};

void
print_icmph (struct icmp_header *icmph)
{
   PRINTK (("  type = %d, code = %d, checksum = %X\n", icmph->type,
	   icmph->code, icmph->checksum));
   PRINTK ((" gateway = %X\n", icmph->un.gateway));
}

/* sends an icmp message in response to a packet. */
void
icmp_reply (struct sk_buff *skb_in,  int type, int code, struct device *dev)
{
   struct sk_buff *skb;
   struct ip_header *iph;
   int offset;
   struct icmp_header *icmph;
   int len;

   PRINTK (("icmp_reply (skb_in = %X, type = %d, code = %d, dev=%X)\n",
	   skb_in, type, code, dev));

   /* get some memory for the reply. */
   len = sizeof (*skb) + 8 /* amount of header to return. */ +
         sizeof (struct icmp_header) +
	 64 /* enough for an ip header. */ +
	 dev->hard_header_len;
	   
   skb = kmalloc (len, GFP_ATOMIC);
   if (skb == NULL) return;

   skb->lock = 0;
   skb->mem_addr = skb;
   skb->mem_len = len;

   len -= sizeof (*skb);

   /* find the ip header. */
   iph = (struct ip_header *)(skb_in+1);
   iph = (struct ip_header *)((unsigned char *)iph + dev->hard_header_len);

   /* Build Layer 2-3 headers for message back to source */
   offset = ip_build_header( skb, iph->daddr, iph->saddr,
			    &dev, IPPROTO_ICMP, NULL, len );

   if (offset < 0)
     {
	skb->sk = NULL;
	kfree_skb (skb, FREE_READ);
	return;
     }

   /* Readjust length according to actual IP header size */
   skb->len = offset + sizeof (struct icmp_header) + 8;
   
   icmph = (struct icmp_header *)((unsigned char *)(skb+1) + offset);
   icmph->type = type;
   icmph->code = code;
   icmph->checksum = 0; /* we don't need to compute this. */
   icmph->un.gateway = 0; /* might as well 0 it. */
   memcpy (icmph+1, iph+1, 8);
   /* send it and free it. */
   ip_queue_xmit (NULL, dev, skb, 1);
   
}

/* deals with incoming icmp packets. */

int
icmp_rcv(struct sk_buff *skb1, struct device *dev, struct options *opt,
	unsigned long daddr, unsigned short len,
	unsigned long saddr, int redo, struct ip_protocol *protocol )
{
   int size, offset;
   struct icmp_header *icmph, *icmphr;
   struct sk_buff *skb;
   unsigned char *buff;


   /* drop broadcast packets.  */
   if ((daddr & 0xff000000) == 0 || (daddr & 0xff000000) == 0xff000000)
     {
	skb1->sk = NULL;
	kfree_skb (skb1, FREE_READ);
	return (0);
     }

   buff = skb1->h.raw;

   icmph = (struct icmp_header *)buff;

   /* Validate the packet first */
   if( icmph->checksum )
     { /* Checksums Enabled? */
	if( ip_compute_csum( (unsigned char *)icmph, len ) )
	  {
	     /* Failed checksum! */
	     PRINTK(("ICMP ECHO failed checksum!\n"));
	     skb1->sk = NULL;
	     kfree_skb (skb1, FREE_READ);
	     return (0);
	  }
     }

   print_icmph(icmph);

   /* Parse the ICMP message */
   switch( icmph->type )
     {
       case ICMP_DEST_UNREACH:
       case ICMP_SOURCE_QUENCH:
	{
	   struct ip_header *iph;
	   struct ip_protocol *ipprot;
	   unsigned char hash;
	   int err;

	   err = icmph->type << 8 | icmph->code;

	   /* we need to cause the socket to be closed and the error message
	      to be set appropriately. */
	   iph = (struct ip_header *)(icmph+1);

	   /* get the protocol(s) */
	   hash = iph->protocol & (MAX_IP_PROTOS -1 );

	   /* this can change while we are doing it. */
	   for (ipprot = (struct ip_protocol *)ip_protos[hash];
		ipprot != NULL; )
	     {
	       struct ip_protocol *nextip;
	       nextip = (struct ip_protocol *)ipprot->next;
	       /* pass it off to everyone who wants it. */
	       if (iph->protocol == ipprot->protocol && ipprot->err_handler)
		 ipprot->err_handler (err, (unsigned char *)(icmph+1),
				     iph->daddr, iph->saddr, ipprot);
	       ipprot = nextip;
	     }

	   skb1->sk = NULL;
	   kfree_skb (skb1, FREE_READ);
	   return (0);
	}

       case ICMP_REDIRECT:
	{
	   /* we need to put a new route in the routing table. */
	   struct rtable *rt; /* we will add a new route. */
	   struct ip_header *iph;

	   iph = (struct ip_header *)(icmph+1);
	   rt = kmalloc (sizeof (*rt), GFP_ATOMIC);
	   if (rt != NULL)
	     {
		rt->net = iph->daddr;
		/* assume class C network.  Technically this is incorrect,
		   but will give it a try. */
		if ((icmph->code & 1) == 0) rt->net &= 0x00ffffff;
		rt->dev = dev;
		rt->router = icmph->un.gateway;
		add_route (rt);
	     }
	   skb1->sk = NULL;
	   kfree_skb (skb1, FREE_READ);
	   return (0);
	}

       case ICMP_ECHO: 
	
	/* Allocate an sk_buff response buffer (assume 64 byte IP header) */

	size = sizeof( struct sk_buff ) + dev->hard_header_len + 64 + len;
	skb = kmalloc( size, GFP_ATOMIC );
	if (skb == NULL)
	  {
	     skb1->sk = NULL;
	     kfree_skb (skb1, FREE_READ);
	     return (0);
	  }
	skb->sk = NULL;
	skb->lock = 0;
	skb->mem_addr = skb;
	skb->mem_len = size;

	/* Build Layer 2-3 headers for message back to source */
	offset = ip_build_header( skb, daddr, saddr, &dev, IPPROTO_ICMP, opt, len );
	if (offset < 0)
	  {
	     /* Problems building header */
	     PRINTK(("Could not build IP Header for ICMP ECHO Response\n"));
	     kfree_s (skb->mem_addr, skb->mem_len);
	     skb1->sk = NULL;
	     kfree_skb (skb1, FREE_READ);
	     return( 0 ); /* just toss the received packet */
	  }

	/* Readjust length according to actual IP header size */
	skb->len = offset + len;

	/* Build ICMP_ECHO Response message */
	icmphr = (struct icmp_header *)( (char *)( skb + 1 ) + offset );
	memcpy( (char *)icmphr, (char *)icmph, len );
	icmphr->type = ICMP_ECHOREPLY;
	icmphr->code = 0;
	icmphr->checksum = 0;

	if( icmph->checksum )
	  { /* Calculate Checksum */
	     icmphr->checksum = ip_compute_csum( (void *)icmphr, len );
	  }

	/* Ship it out - free it when done */
	ip_queue_xmit( (volatile struct sock *)NULL, dev, skb, 1 );
	
	skb1->sk = NULL;
	kfree_skb (skb1, FREE_READ);
	return( 0 );

	default:
	PRINTK(("Unsupported ICMP type = x%x\n", icmph->type ));
	skb1->sk = NULL;
	kfree_skb (skb1, FREE_READ);
	return( 0 ); /* just toss the packet */
     }

   /* should be unecessary, but just in case. */
   skb1->sk = NULL;
   kfree_skb (skb1, FREE_READ);
   return( 0 ); /* just toss the packet */
}

