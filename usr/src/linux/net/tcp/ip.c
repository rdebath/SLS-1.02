/* ip.c */
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
/* $Id: ip.c,v 0.8.4.10 1993/01/23 18:00:11 bir7 Exp $ */
/* $Log: ip.c,v $
 * Revision 0.8.4.10  1993/01/23  18:00:11  bir7
 * added volatile keyword to many variables.
 *
 * Revision 0.8.4.9  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.8  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.7  1992/12/06  23:29:59  bir7
 * Changed retransmit to double rtt.
 *
 * Revision 0.8.4.6  1992/12/05  21:35:53  bir7
 * fixed checking of wrong fragmentation bit.
 *
 * Revision 0.8.4.5  1992/12/03  19:52:20  bir7
 * added paranoid queue checking
 *
 * Revision 0.8.4.4  1992/11/18  15:38:03  bir7
 * Fixed bug in copying packet and checking packet type.
 *
 * Revision 0.8.4.3  1992/11/17  14:19:47  bir7
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

#include <asm/segment.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/socket.h>
#include <netinet/in.h>
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include <linux/errno.h>
#include "arp.h"
#include "icmp.h"

unsigned long ip_addr[MAX_IP_ADDRES]={0,0,0};

#ifdef PRINTK
#undef PRINTK
#endif

#undef IP_DEBUG

#ifdef IP_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

static struct rtable *rt_base=NULL; /* used to base all the routing data. */

volatile struct ip_protocol *ip_protos[MAX_IP_PROTOS] = { NULL, };
int ip_ads = 0;

static char *in_ntoa(unsigned long addr)
{
	static char buf[100];

	sprintf(buf,"%d.%d.%d.%d",
		(addr & 0xff),
		((addr >> 8) & 0xff),
		((addr >> 16) & 0xff),
		((addr >> 24) & 0xff));
	return buf;
}

#if 0
static  struct ip_protocol *
get_protocol(unsigned char prot)
{
   unsigned char hash;
   struct ip_protocol *p;
   PRINTK (("get_protocol (%d)\n ", prot));
   hash = prot & (MAX_IP_PROTOS -1);
   for (p = ip_protos[hash] ; p != NULL; p=p->next)
     {
	PRINTK (("trying protocol %d\n", p->protocol));
	if (p->protocol == prot)
	     return (p);
     }
   return (NULL);
    
}
#endif

void
add_ip_protocol (struct ip_protocol *prot)
{
   unsigned char hash;
   struct ip_protocol *p2;
   hash = prot->protocol & (MAX_IP_PROTOS-1);
   prot ->next = ip_protos[hash];
   ip_protos[hash] = prot;
   prot->copy = 0;
   /* set the copy bit if we need to. */
   for (p2 = (struct ip_protocol *)prot->next;
	p2 != NULL;
	p2= (struct ip_protocol *)p2->next)
     {
	if (p2->protocol == prot->protocol)
	  {
	     prot->copy = 1;
	     break;
	  }
     }

}

int
delete_ip_protocol (struct ip_protocol *prot)
{
   struct ip_protocol *p;
   struct ip_protocol *lp=NULL;
   unsigned char hash;


   hash = prot->protocol & (MAX_IP_PROTOS -1);
   if (prot == ip_protos[hash])
     {
        ip_protos[hash]=(struct ip_protocol *)ip_protos[hash]->next;
	return (0);
     }

   for (p = (struct ip_protocol *)ip_protos[hash];
	p != NULL;
	p = (struct ip_protocol *) p->next)
     {
	/* we have to worry if the protocol being deleted is the
	   last one on the list, then we may need to reset someones
	   copied bit. */
	if (p->next != NULL && p->next == prot)
	  {
	     /* if we are the last one with this protocol and
		there is a previous one, reset its copy bit. */

	     if (p->copy == 0 && lp != NULL)
	       lp->copy = 0;
	     p->next = prot->next;
	     return (0);
	  }

	if (p->next != NULL && p->next->protocol == prot->protocol)
	  {
	     lp = p;
	  }
     }
   return (-1);
}

/* addr1 is the address which may or may not be broadcast etc.
   addr2 is the "real addr." */

int
ip_addr_match (unsigned long addr1, unsigned long addr2)
{
  int i;
  if (addr1 == addr2) return (IS_MYADDR);
  for (i = 0; i < 4; i++, addr1 >>= 8, addr2 >>= 8)
    {
      if ((addr1 & 0xff) != (addr2 & 0xff))
	{
	  /* the only way this could be a match is for the rest of
	     addr1 to be 0. */
	  if (addr1 != 0) 
	    {
	      return (0);
	    }
	  return (IS_BROADCAST);
	}
    }
  return (IS_MYADDR);
}

int
my_ip_addr(unsigned long addr)
{
  int i;
  int result;
  for (i = 0; i < MAX_IP_ADDRES; i++)
    {
      if (ip_addr[i] == 0) return (0);
      result = ip_addr_match (addr, ip_addr[i]);
      if (result) return result;
    }
  return (0);
}

/* these two routines will do routining. */
static  void
strict_route(struct ip_header *iph, struct options *opt)
{
}

static  void
loose_route(struct ip_header *iph, struct options *opt)
{
}

void
print_rt(struct rtable *rt)
{
#ifdef IP_DEBUG
  printk("RT: %06lx NXT=%06lx DEV=%06lx(%s) NET=%s ",
	(long) rt, (long) rt->next, (long) rt->dev,
			rt->dev->name, in_ntoa(rt->net));
  printk("ROUTER=%s\n", in_ntoa(rt->router));
#endif
}

void
print_ipprot (struct ip_protocol *ipprot)
{
   PRINTK (("handler = %X, protocol = %d, copy=%d \n",
	    ipprot->handler, ipprot->protocol, ipprot->copy));
}

/* This assumes that address are all in net order. */
static  struct device *
ip_route(struct options *opt, unsigned long daddr, unsigned long *raddr)
{
  struct rtable *rt;
  /* look through the routing table for some
     kind of match. */
  for (rt=rt_base; rt != NULL; rt=rt->next)
    {
      /* see if we found one. */
      if (ip_addr_match (rt->net, daddr))
	{
	  PRINTK (("IP: %X via %s (%X)\n", daddr, rt->dev->name, rt->router));
	  *raddr = rt->router;
	  return (rt->dev);
	}
    }
  return (NULL);
};

/* Remove all routing table entries for a device. */
void
del_devroute (struct device *dev)
{
  struct rtable *r, *x, *p;

  if ((r = rt_base) == NULL) return;	/* nothing to remove! */
  PRINTK (("IFACE DOWN: clearing routing table for dev 0x%08lx (%s)\n",
						(long) dev, dev->name));
  p = NULL;
  while(r != NULL)
    {
	PRINTK ((">> R=%06lx N=%06lx P=%06lx DEV=%06lx(%s) A=%s\n",
		(long) r, (long) r->next, (long) p, (long) r->dev,
					r->dev->name, in_ntoa(r->net)));
	if (r->dev == dev)
	  {
		PRINTK ((">>> MATCH: removing rt=%08lx\n", (long) r));
		if (p == NULL) rt_base = r->next;
		  else p->next = r->next;
		x = r->next;
		kfree_s(r, sizeof(*r));
		r = x;
	  }
	else
	  {
		p = r;
		r = r->next;
	  }
    }
}

void
add_route (struct rtable *rt)
{
  int mask;
  struct rtable *r;
  struct rtable *r1;

  print_rt(rt);

  if (rt_base == NULL)
    {
      rt->next = NULL;
      rt_base = rt;
      return;
    }

  /* what we have to do is loop though this until we have found the
     first address which has the same generality as the one in rt.  Then
     we can put rt in after it. */
  for (mask = 0xff000000; mask != 0xffffffff; mask = (mask >> 8) | mask)
    {
      if (mask & rt->net)
	{
	  mask = mask << 8;
	  break;
	}
    }
  PRINTK (("mask = %X\n",mask));
  r1=rt_base;
  for (r=rt_base; r != NULL; r=r->next)
    {
       /* see if we are getting a duplicate. */
       if (r->net == rt->net)
	 {
	    if (r == rt_base)
	      {
		 rt->next = r->next;
		 rt_base = rt;
	      }
	    else
	      {
		 rt->next = r->next;
		 r1->next = rt;
	      }
	    kfree_s (r, sizeof (*r));
	    return;
	 }

      if (!(r->net & mask))
	{
	   PRINTK (("adding before r=%X\n",r));
	   print_rt(r);
	   if (r == rt_base)
	     {
		rt->next = rt_base;
		rt_base = rt;
		return;
	     }
	   rt->next = r;
	   r1->next = rt;
	   return;
	}
      r1 = r;
    }
  PRINTK (("adding after r1=%X\n",r1));
  print_rt(r1);
  /* goes at the end. */
  rt->next = NULL;
  r1->next = rt;
}

int
ip_set_dev (struct ip_config *u_ipc)
{
  struct rtable *rt;
  struct device *dev;
  struct ip_config ipc;



/*  verify_area (VERIFY_WRITE, u_ipc, sizeof (ipc));*/
  memcpy_fromfs(&ipc, u_ipc, sizeof (ipc));
  ipc.name[MAX_IP_NAME-1] = 0;
  dev = get_dev (ipc.name);

#if 1 /* making this a 0 will let you remove an ip address from
	 the list, which is useful under SLIP.  But it may not
	 be compatible with older configs. */
  ipc.destroy = 0;
#endif

  if (dev == NULL) return (-EINVAL);
  if (ip_ads >= MAX_IP_ADDRES && !ipc.destroy && ipc.paddr != -1)
    return (-EINVAL);

  /* see if we need to add a broadcast address. */
  if (ipc.net != -1)
    {
       PRINTK (("new broadcast for %s: %08X\n", dev->name, ipc.net));
       arp_add_broad (ipc.net, dev);
       rt = kmalloc (sizeof (*rt), GFP_KERNEL);
       if (rt == NULL) return (-ENOMEM);
       rt->net = ipc.net;
       rt->dev = dev;
       rt->router = 0;
       add_route (rt);
/*     dev->net = ipc.net;*/
    }

  if (ipc.router != -1)
    {
       PRINTK (("new router for %s: %08X\n", dev->name, ipc.router));
       rt = kmalloc (sizeof (*rt),GFP_KERNEL);
       if (rt == NULL) return (-ENOMEM);
       rt->net = 0;
       rt->dev = dev;
       rt->router = ipc.router;
       add_route (rt);
    }

  if (dev->loopback)
    {
       PRINTK (("new loopback addr: %08X\n", ipc.paddr));
       rt = kmalloc (sizeof (*rt), GFP_KERNEL);
       if (rt == NULL) return (-ENOMEM);
       rt->net = ipc.paddr;
       rt->dev = dev;
       rt->router = 0;
       add_route (rt);
    }

  if (ipc.destroy)
    {
      int i;
      for (i = 0; i <MAX_IP_ADDRES; i++)
	{
	  if (ip_addr[i] == ipc.paddr)
	    {
	      break;
	    }
	}
      if (i != MAX_IP_ADDRES)
	{
	  PRINTK (("ip.c: Destroying Identity %8X, entry %d\n", ipc.paddr, i));
	  i++;
	  ip_ads--;
	  while (i < MAX_IP_ADDRES)
	    {
	      ip_addr[i-1] = ip_addr[i];
	      i++;
	    }
	  ip_addr[MAX_IP_ADDRES-1] = 0;
	}
    }

  /* FIX per FvK 92/11/15 */
  /* When "downing" an interface, this must be done with paddr = -1L. */
  if (ipc.paddr != -1L && !ipc.destroy)
    {
      if (!my_ip_addr (ipc.paddr))
	{
	  PRINTK (("new identity: %08X\n", ipc.paddr));
	  ip_addr[ip_ads++] = ipc.paddr;
	}
    }

  dev->up = ipc.up;
  if (dev->up)
    {
       if (dev->open)
	 dev->open(dev);
    }
  else
    {
       if (dev->stop)
	 dev->stop(dev);
	del_devroute(dev);		/* clear routing table for dev	*/
    }

  return (0);
}

/* this routine will check to see if we have lost a gateway. */
void
ip_route_check (unsigned long daddr)
{
}

#if 0
/* this routine puts the options at the end of an ip header. */
static  int
build_options (struct ip_header *iph, struct options *opt)
{
  unsigned char *ptr;
  /* currently we don't support any options. */
  ptr = (unsigned char *)(iph+1);
  *ptr = 0;
  return (4);
}
#endif

/* This routine builds the appropriate hardware/ip headers for
   the routine.  It assumes that if *prot != NULL then the
   protocol knows what it's doing, otherwise it uses the
   routing/arp tables to select a protocol struct. */

int
ip_build_header (struct sk_buff *skb, unsigned long saddr,
		 unsigned long daddr, struct device **dev, int type,
		 struct options *opt, int len)
{
  static struct options optmem;
  struct ip_header *iph;
  unsigned char *buff;
  static int count = 0;
  unsigned long raddr; /* for the router. */
  int tmp;
  if (saddr == 0) saddr = MY_IP_ADDR;
  PRINTK (("ip_build_header (skb=%X, saddr=%X, daddr=%X, *dev=%X,\n"
	   "                 type=%d, opt=%X, len = %d)\n",
	   skb, saddr, daddr, *dev, type, opt, len));
  buff = (unsigned char *)(skb + 1);
  /* see if we need to look up the device. */
  if (*dev == NULL)
    {
      *dev = ip_route(&optmem,daddr, &raddr);
      if (*dev == NULL)
	{
	  return (-ENETUNREACH);
	}
      opt = &optmem;
    }
  else
    {
      /* we still need the address of the first hop. */
      ip_route (&optmem, daddr, &raddr);
    }
  if (raddr == 0) raddr = daddr;
  /* now build the header. */
  /* we need to worry about routing in here.  daddr should
     really be the address of the next hop. */
  /* but raddr is . */
  if ((*dev)->hard_header)
    {
       tmp = (*dev)->hard_header(buff, *dev, ETHERTYPE_IP, raddr, saddr, len);
    }
  else
    {
       tmp = 0;
    }
  if (tmp < 0)
    {
       tmp = -tmp;
       skb->arp = 0;
    }
  else
    {
       skb->arp = 1;
    }
  buff += tmp;
  len -= tmp;
  skb->dev = *dev;
  /* now build the ip header. */
  iph = (struct ip_header *)buff;
  iph->version = 4;
  iph->tos = 0;
  iph->frag_off = 0;
  iph->ttl = 32;
  iph->daddr = daddr;
  iph->saddr = saddr;
  iph->protocol=type;
  iph->ihl = 5;
  iph->id = net16(count++);
  /* build_options (iph, opt);*/
  return (20+tmp);
}

static  int
do_options(struct ip_header *iph, struct options *opt)
{
  unsigned char *buff;
  int done = 0;
  int len=sizeof (*iph);
  int i;
  /* zero  out the options. */
  opt->record_route.route_size = 0;
  opt->loose_route.route_size = 0;
  opt->strict_route.route_size = 0;
  opt->tstamp.ptr = 0;
  opt->security = 0;
  opt->compartment = 0;
  opt->handling = 0;
  opt->stream = 0;
  opt->tcc = 0;
  return (0);
  /* advance the pointer to start at the options. */
  buff = (unsigned char *)(iph + 1);

  /*now start the processing. */
  while (!done && len < iph->ihl*4)
    {
      switch (*buff)
	{
	case IPOPT_END:
	  done=1;
	  break;

	case IPOPT_NOOP:
	  buff++;
	  len ++;
	  break;

	case IPOPT_SEC:
	  buff++;
	  if (*buff != 11)
	    return (1);
	  buff++;
	  opt->security = net16(*(unsigned short *)buff);
	  buff += 2;
	  opt->compartment = net16(*(unsigned short *)buff);
	  buff += 2;
	  opt-> handling = net16(*(unsigned short *)buff);
	  buff += 2;
	  opt->tcc = ((*buff) << 16) + net16(*(unsigned short *)(buff+1));
	  buff += 3;
	  len += 11;
	  break;

	case IPOPT_LSRR:
	  buff ++;
	  if ((*buff - 3)% 4 != 0) return (1);
	  len += *buff;
	  opt->loose_route.route_size = (*buff -3)/4;
	  buff ++;
	  if (*buff % 4 != 0) return (1);
	  opt->loose_route.pointer = *buff/4 - 1;
	  buff ++;
	  buff ++;
	  for (i = 0; i < opt->loose_route.route_size; i++)
	    {
	      opt->loose_route.route[i]=*(unsigned long *)buff;
	      buff += 4;
	    }
	  break;


	case IPOPT_SSRR:
	  buff ++;
	  if ((*buff - 3)% 4 != 0) return (1);
	  len += *buff;
	  opt->strict_route.route_size = (*buff -3)/4;
	  buff ++;
	  if (*buff % 4 != 0) return (1);
	  opt->strict_route.pointer = *buff/4 - 1;
	  buff ++;
	  buff ++;
	  for (i = 0; i < opt->strict_route.route_size; i++)
	    {
	      opt->strict_route.route[i]=*(unsigned long *)buff;
	      buff += 4;
	    }
	  break;

	case IPOPT_RR:
	  buff ++;
	  if ((*buff - 3)% 4 != 0) return (1);
	  len += *buff;
	  opt->record_route.route_size = (*buff -3)/4;
	  buff ++;
	  if (*buff % 4 != 0) return (1);
	  opt->record_route.pointer = *buff/4 - 1;
	  buff ++;
	  buff ++;
	  for (i = 0; i < opt->record_route.route_size; i++)
	    {
	      opt->record_route.route[i]=*(unsigned long *)buff;
	      buff += 4;
	    }
	  break;

	case IPOPT_SID:
	  len += 4;
	  buff +=2;
	  opt->stream = *(unsigned short *)buff;
	  buff += 2;
	  break;

	case IPOPT_TIMESTAMP:
	  buff ++;
	  len += *buff;
	  if (*buff % 4 != 0) return (1);
	  opt->tstamp.len = *buff / 4 - 1;
	  buff ++;
	  if ((*buff - 1) % 4 != 0) return (1);
	  opt->tstamp.ptr = (*buff-1)/4;
	  buff ++;
	  opt->tstamp.x.full_char = *buff;
	  buff ++;
	  for (i = 0; i < opt->tstamp.len; i++)
	    {
	      opt->tstamp.data[i] = *(unsigned long *)buff;
	      buff += 4;
	    }
	  break;

	default:
	  return (1);
	}
    }
  if (opt->record_route.route_size == 0)
    {
      if (opt->strict_route.route_size != 0)
	{
	  memcpy (&(opt->record_route), &(opt->strict_route),
		  sizeof (opt->record_route));
	}
      else if (opt->loose_route.route_size != 0)
	{
	  memcpy (&(opt->record_route), &(opt->loose_route),
		  sizeof (opt->record_route));
	}
    }

  if (opt->strict_route.route_size != 0 &&
      opt->strict_route.route_size != opt->strict_route.pointer)
    {
      strict_route (iph, opt);
      return (0);
    }

  if (opt->loose_route.route_size != 0 &&
      opt->loose_route.route_size != opt->loose_route.pointer)
    {
      loose_route (iph, opt);
      return (0);
    }

  return (0);
}


/* This routine does all the checksum computations that don't require
   anything special (like copying or special headers.) */

unsigned short
ip_compute_csum(unsigned char * buff, int len)
{
  unsigned long sum = 0;
  if (len > 3)
    {
       /* do the first multiple of 4 bytes and convert to 16 bits. */
       __asm__("\t clc\n"
	       "1:\n"
	       "\t lodsl\n"
	       "\t adcl %%eax, %%ebx\n"
	       "\t loop 1b\n"
	       "\t adcl $0, %%ebx\n"
	       "\t movl %%ebx, %%eax\n"
	       "\t shrl $16, %%eax\n"
	       "\t addw %%ax, %%bx\n"
	       "\t adcw $0, %%bx\n"
	       : "=b" (sum) , "=S" (buff)
	       : "0" (sum), "c" (len >> 2) ,"1" (buff)
	       : "ax", "cx", "si", "bx" );
    }
  if (len & 2)
    {
       __asm__("\t lodsw\n"
	       "\t addw %%ax, %%bx\n"
	       "\t adcw $0, %%bx\n"
	       : "=b" (sum), "=S" (buff)
	       : "0" (sum), "1" (buff)
	       : "bx", "ax", "si");
    }
  if (len & 1)
    {
       __asm__("\t lodsb\n"
	       "\t movb $0, %%ah\n"
	       "\t addw %%ax, %%bx\n"
	       "\t adcw $0, %%bx\n"
	       : "=b" (sum), "=S" (buff)
	       : "0" (sum), "1" (buff)
	       : "bx", "ax", "si");
    }
  sum =~sum;
  return (sum&0xffff);
}

static  int
ip_csum(struct ip_header *iph)
{
  if (iph->check == 0) return (0);
  if (ip_compute_csum((unsigned char *)iph, iph->ihl*4) == 0)  return (0);
  return (1);
}

static  void
ip_send_check(struct ip_header *iph)
{
   iph->check = 0;
   iph->check = ip_compute_csum((unsigned char *)iph, iph->ihl*4);
}

int
ip_rcv(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
  struct ip_header *iph;
  unsigned char hash;
  unsigned char flag=0;
  static struct options opt; /* since we don't use these yet, and they
				take up stack space. */
  struct ip_protocol *ipprot;

  iph=skb->h.iph;

  PRINTK (("<<\n"));
  print_iph(iph);

  if (ip_csum (iph) || do_options (iph,&opt) || iph->version != 4)
    {
       PRINTK (("ip packet thrown out. \n"));
       skb->sk = NULL;
       kfree_skb(skb, 0);
       return (0);
    }

  /* for now we will only deal with packets meant for us. */
  if (!my_ip_addr(iph->daddr))
    {
	PRINTK(("\nIP: *** datagram routing not yet implemented ***\n"));
	PRINTK(("    SRC = %s   ", in_ntoa(iph->saddr)));
	PRINTK(("    DST = %s (ignored)\n", in_ntoa(iph->daddr)));
/*      icmp_reply (skb, ICMP_DEST_UNREACH, ICMP_PROT_UNREACH, dev); */

       skb->sk = NULL;
       kfree_skb(skb, 0);
       return (0);
    }

  /* deal with fragments.  or don't for now.*/
  if ((iph->frag_off & 32) || (net16(iph->frag_off)&0x1fff))
    {	/* FIXME: this ^^^ used to be 64, as per bugfix */
	printk("\nIP: *** datagram fragmentation not yet implemented ***\n");
	printk("    SRC = %s   ", in_ntoa(iph->saddr));
	printk("    DST = %s (ignored)\n", in_ntoa(iph->daddr));
       icmp_reply (skb, ICMP_DEST_UNREACH, ICMP_PROT_UNREACH, dev);
       skb->sk = NULL;
       kfree_skb(skb, 0);
       return(0);
    }

  skb->h.raw += iph->ihl*4;

  hash = iph->protocol & (MAX_IP_PROTOS -1);
  for (ipprot = (struct ip_protocol *)ip_protos[hash];
       ipprot != NULL;
       ipprot=(struct ip_protocol *)ipprot->next)
    {
       struct sk_buff *skb2;
       if (ipprot->protocol != iph->protocol) continue;
       PRINTK (("Using protocol = %X:\n", ipprot));
       print_ipprot (ipprot);
       /* pass it off to everyone who wants it. */
       /* we should check the return values here. */
       /* see if we need to make a copy of it.  This will
	  only be set if more than one protpocol wants it. 
	  and then not for the last one. */

       if (ipprot->copy)
	 {
	    skb2 = kmalloc (skb->mem_len, GFP_ATOMIC);
	    if (skb2 == NULL) continue;
	    memcpy (skb2, skb, skb->mem_len);
	    skb2->mem_addr = skb2;
	    skb2->lock = 0;
	    skb2->h.raw = (void *)((unsigned long)skb2
				   + (unsigned long)skb->h.raw
				   - (unsigned long)skb);
	 }
       else
	 {
	    skb2 = skb;
	 }
       flag = 1;
       ipprot->handler (skb2, dev, &opt, iph->daddr,
			net16(iph->tot_len) - iph->ihl*4,
			iph->saddr, 0, ipprot);

    }
  if (!flag)
    {
       icmp_reply (skb, ICMP_DEST_UNREACH, ICMP_PROT_UNREACH, dev);
       skb->sk = NULL;
       kfree_skb (skb, 0);
    }


  return (0);
}


/* queues a packet to be sent, and starts the transmitter if
   necessary.  if free = 1 then we free the block after transmit,
   otherwise we don't. */
/* This routine also needs to put in the total length, and compute
   the checksum. */
void
ip_queue_xmit (volatile struct sock *sk, struct device *dev, 
	       struct sk_buff *skb, int free)
{
  struct ip_header *iph;
  unsigned char *ptr;
  if (sk == NULL) free = 1;

  if (dev == NULL)
    {
      printk ("ip.c: ip_queue_xmit dev = NULL\n");
      return;
    }

  skb->free = free;
  skb->dev = dev;
  skb->when = jiffies;
  PRINTK ((">>\n"));
  ptr = (unsigned char *)(skb + 1);
  ptr += dev->hard_header_len;
  iph = (struct ip_header *)ptr;
  iph->tot_len = net16(skb->len-dev->hard_header_len);
  ip_send_check (iph);
  print_iph(iph);
  skb->next = NULL;

  /* see if this is the one
     trashing our queue. */
  skb->magic = 1;

  if (!free)
    {
      skb->link3 = NULL;
      sk->packets_out++;
      cli();
      if (sk->send_tail == NULL)
	{
	  sk->send_tail = skb;
	  sk->send_head = skb;
	}
      else
	{
	  sk->send_tail->link3 = skb;
	  sk->send_tail = skb;
	}
      sti();
      sk->time_wait.len = sk->rtt*2;
      sk->timeout=TIME_WRITE;
      reset_timer ((struct timer *)&sk->time_wait);
   }
  else
    {
       skb->sk = sk;
    }
  if (dev->up)
    {
       if (sk != NULL)
	 {
	   dev->queue_xmit(skb, dev, sk->priority);
	 }
       else
	 {
	   dev->queue_xmit (skb, dev, SOPRI_NORMAL);
	 }
    }
  else
    {
       if (free) 
	 kfree_skb (skb, FREE_WRITE);
    }
}

void
ip_retransmit (volatile struct sock *sk, int all)
{
  struct sk_buff * skb;
  struct proto *prot;
  struct device *dev;

  prot = sk->prot;
  skb = sk->send_head;
  while (skb != NULL)
    {
      dev = skb->dev;
      /* rebuild_header sees if the arp is done.  If not it sends a new
	 arp, and if so it builds the header. */
      if (!skb->arp)
	{
	  if (dev->rebuild_header ((struct enet_header *)(skb+1),dev))
	    {
	       if (!all) break;
	       skb=(struct sk_buff *)skb->link3;
	       continue;
	    }
       }
      skb->arp = 1;
      skb->when = jiffies;

      if (dev->up)
	if (sk)
	  dev->queue_xmit(skb, dev, sk->priority);
	else
	  dev->queue_xmit(skb, dev, SOPRI_NORMAL );

      sk->retransmits++;
      sk->prot->retransmits ++;
      if (!all) break;

      /* this should cut it off before we send too
	 many packets. */
      if (sk->retransmits > sk->cong_window) break;
      skb=(struct sk_buff *)skb->link3;
    }
  /* double the rtt time every time we retransmit. 
     This will cause exponential back off on how
     hard we try to get through again.  Once we
     get through, the rtt will settle back down
     reasonably quickly. */

  sk->rtt *= 2;
  sk->time_wait.len = sk->rtt;
  sk->timeout = TIME_WRITE;
  reset_timer ((struct timer *)&sk->time_wait);
}

void
print_iph (struct ip_header *ip)
{
  PRINTK (("ip header:\n"));
  PRINTK (("  ihl = %d, version = %d, tos = %d, tot_len = %d\n",
	   ip->ihl, ip->version, ip->tos, net16(ip->tot_len)));
  PRINTK (("  id = %x, ttl = %d, prot = %d, check=%x\n",
	   ip->id, ip->ttl, ip->protocol, ip->check));
  PRINTK ((" frag_off=%d\n", ip->frag_off));
  PRINTK (("  saddr = %X, daddr = %X\n",ip->saddr, ip->daddr));
}
