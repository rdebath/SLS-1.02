/* raw.c - implements raw ip sockets. */
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
/* $Id: raw.c,v 0.8.4.12 1993/01/26 22:04:00 bir7 Exp $ */
/* $Log: raw.c,v $
 * Revision 0.8.4.12  1993/01/26  22:04:00  bir7
 * Added support for proc fs.
 *
 * Revision 0.8.4.11  1993/01/23  18:00:11  bir7
 * Added volatile keyword
 *
 * Revision 0.8.4.10  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.9  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.8  1992/12/12  01:50:49  bir7
 * Fixed bug in call to err routine.
 *
 * Revision 0.8.4.7  1992/12/06  11:31:47  bir7
 * added raw_err.
 *
 * Revision 0.8.4.6  1992/11/18  15:38:03  bir7
 * Works now.
 *
 *
 * Revision 0.8.4.4  1992/11/17  09:27:07  bir7
 * Fixed error in header building.
 *
 * Revision 0.8.4.3  1992/11/16  16:13:40  bir7
 * Added debuggin information.
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.3  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added Id and Log
 * */

#include <linux/types.h>
#include <linux/sched.h>
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
#include <linux/mm.h>
#include <linux/kernel.h>
#include "icmp.h"


#ifdef PRINTK
#undef PRINTK
#endif

#undef RAW_DEBUG
#ifdef RAW_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

extern struct proto raw_prot;

static  unsigned long
min(unsigned long a, unsigned long b)
{
   if (a < b) return (a);
   return (b);
}

/* raw_err gets called by the icmp module. */
void
raw_err (int err, unsigned char *header, unsigned long daddr,
	 unsigned long saddr, struct ip_protocol *protocol)
{
   volatile struct sock *sk;
   
   PRINTK (("raw_err (err=%d, header=%X, daddr=%X, saddr=%X, ip_protocl=%X)\n"));

   if (protocol == NULL) return;

   sk = protocol->data;

   if (sk == NULL) return;

   /* This is meaningless in raw sockets. */
   if (err & 0xff00 == (ICMP_SOURCE_QUENCH << 8))
     {
	if (sk->cong_window > 1)
	  sk->cong_window = sk->cong_window/2;
	return;
     }

   sk->err = icmp_err_convert[err & 0xff].errno;
   /* none of them are fatal for raw sockets. */
/*   if (icmp_err_convert[err & 0xff].fatal)
     {
	sk->prot->close(sk, 0);
     } */

   return;

}

/* this should be the easiest of all, all we do is copy it into
   a buffer. */
int
raw_rcv (struct sk_buff *skb, struct device *dev, struct options *opt,
	 unsigned long daddr, unsigned short len, unsigned long saddr,
	 int redo, struct ip_protocol *protocol)
{

  volatile struct sock *sk;

   PRINTK (("raw_rcv (skb=%X, dev=%X, opt=%X, daddr=%X,\n"
	   "         len=%d, saddr=%X, redo=%d, protocol=%X)\n",
	   skb, dev, opt, daddr, len, saddr, redo, protocol));

   if (skb == NULL) return (0);
   if (protocol == NULL)
     {
       kfree_skb (skb, FREE_READ);
       return (0);
     }
   sk = protocol->data;
   if (sk == NULL)
     {
       kfree_skb (skb, FREE_READ);
       return (0);
     }

   /* now we need to copy this into memory. */
   skb->sk = sk;
   skb->len = len;
   skb->dev = dev;
   skb->saddr = daddr;
   skb->daddr = saddr;

   if (!redo )
     {
	/* now see if we are in use. */
	cli();
	if (sk->inuse)
	  {
	     PRINTK (("raw_rcv adding to backlog. \n"));
	     if (sk->back_log == NULL)
	       {
		  sk->back_log = skb;
		  skb->next = skb;
		  skb->prev = skb;
	       }
	     else
	       {
		  skb->next = sk->back_log;
		  skb->prev = sk->back_log->prev;
		  skb->prev->next = skb;
		  skb->next->prev = skb;
	       }
	     sti();
	     return (0);
	  }
	sk->inuse = 1;
	sti();
     }

   /* charge it too the socket. */
   if (sk->rmem_alloc + skb->mem_len >= SK_RMEM_MAX)
     {
	skb->sk = NULL;
	kfree_skb (skb, FREE_READ);
	return (0);
     }
	     
   sk->rmem_alloc += skb->mem_len;

   /* now just put it onto the queue. */
   if (sk->rqueue == NULL)
     {
	sk->rqueue = skb;
	skb->next = skb;
	skb->prev = skb;
     }
   else
     {
	skb->next = sk->rqueue;
	skb->prev = sk->rqueue->prev;
	skb->prev->next = skb;
	skb->next->prev = skb;
     }
   wake_up (sk->sleep);
   release_sock (sk);
   return (0);
}

/* this will do terrible things if len + ipheader + devheader > dev->mtu */
static int
raw_sendto (volatile struct sock *sk, unsigned char *from, int len,
	    int noblock,
	    unsigned flags, struct sockaddr_in *usin, int addr_len)
{
   struct sk_buff *skb;
   struct device *dev=NULL;
   struct sockaddr_in sin;
   int tmp;

   PRINTK (("raw_sendto (sk=%X, from=%X, len=%d, noblock=%d, flags=%X,\n"
	   "            usin=%X, addr_len = %d)\n", sk, from, len, noblock,
	   flags, usin, addr_len));

   /* check the flags. */
   if (flags) return (-EINVAL);
   if (len < 0) return (-EINVAL);

   /* get and verify the address. */
   if (usin)
     {
	if (addr_len < sizeof (sin))
	  return (-EINVAL);
/*	verify_area (VERIFY_WRITE, usin, sizeof (sin));*/
	memcpy_fromfs (&sin, usin, sizeof(sin));
	if (sin.sin_family &&
	    sin.sin_family != AF_INET)
	  return (-EINVAL);
     }
   else
     {
	if (sk->state != TCP_ESTABLISHED)
	  return (-EINVAL);
	sin.sin_family = AF_INET;
	sin.sin_port = sk->protocol;
	sin.sin_addr.s_addr = sk->daddr;
     }
   if (sin.sin_port == 0) sin.sin_port = sk->protocol;

   sk->inuse = 1;
   skb = NULL;
   while (skb == NULL)
     {
       skb = sk->prot->wmalloc (sk, len+sizeof (*skb) + sk->prot->max_header,
				0, GFP_KERNEL);
       /* this shouldn't happen, but it could. */
       /* need to change this to sleep. */
       if (skb == NULL)
	 {
	   int tmp;
	   PRINTK (("raw_sendto: write buffer full?\n"));
	   if (noblock) return (-EAGAIN);
	   tmp = sk->wmem_alloc;
	   release_sock (sk);
	   cli();
	   if (tmp <= sk->wmem_alloc)
	     {
	       interruptible_sleep_on (sk->sleep);
	       if (current->signal & ~current->blocked)
		 {
		   sti();
		   return (-ERESTARTSYS);
		 }
	     }
	   sk->inuse = 1;
	   sti();
	 }
     }
   skb->lock = 0;
   skb->mem_addr = skb;
   skb->mem_len = len + sizeof (*skb) +sk->prot->max_header;
   skb->sk = sk;

   skb->free = 1; /* these two should be unecessary. */
   skb->arp = 0;

   tmp = sk->prot->build_header (skb, sk->saddr, 
				 sin.sin_addr.s_addr, &dev,
				 sk->protocol, sk->opt, skb->mem_len);
   if (tmp < 0)
     {
       PRINTK (("raw_sendto: error building ip header.\n"));
	sk->prot->wfree (sk, skb->mem_addr, skb->mem_len);
	release_sock (sk);
	return (tmp);
     }

/*   verify_area (VERIFY_WRITE, from, len);*/
   memcpy_fromfs ((unsigned char *)(skb+1)+tmp, from, len);
   skb->len = tmp + len;
   sk->prot->queue_xmit (sk, dev, skb, 1);
   release_sock (sk);
   return (len);
}

static int
raw_write (volatile struct sock *sk, unsigned char *buff, int len, int noblock,
	   unsigned flags)
{
   return (raw_sendto (sk, buff, len, noblock, flags, NULL, 0));
}

static void
raw_close (volatile struct sock *sk, int timeout)
{
   sk->inuse = 1;
   sk->state = TCP_CLOSE;
   PRINTK (("raw_close: deleting ip_protocol %d\n",
	   ((struct ip_protocol *)sk->pair)->protocol));
   if (delete_ip_protocol ((struct ip_protocol *)sk->pair) < 0)
     PRINTK (("raw_close: delete_ip_protocol failed. \n"));
   kfree_s ((void *)sk->pair, sizeof (struct ip_protocol));
   sk->pair = NULL;
   release_sock (sk);
}

static int
raw_init (volatile struct sock *sk)
{
   struct ip_protocol *p;
   p = kmalloc (sizeof (*p), GFP_KERNEL);
   if (p == NULL) return (-ENOMEM);

   p->handler = raw_rcv;
   p->protocol = sk->protocol;
   p->data = (void *)sk;
   p->err_handler = raw_err;
   add_ip_protocol (p);
   
   /* we need to remember this somewhere. */
   sk->pair = (volatile struct sock *)p;

   PRINTK (("raw init added protocol %d\n", sk->protocol));

   return (0);
}


int
raw_recvfrom (volatile struct sock *sk, unsigned char *to, int len,
	      int noblock,
	      unsigned flags, struct sockaddr_in *sin, int *addr_len)
{
	/* this should be easy, if there is something there we
	   return it, otherwise we block. */
	int copied=0;
	struct sk_buff *skb;

	PRINTK (("raw_recvfrom (sk=%X, to=%X, len=%d, noblock=%d, flags=%X,\n"
		"              sin=%X, addr_len=%X)\n", sk, to, len, noblock,
		flags, sin, addr_len));

	if (len == 0) return (0);
	if (len < 0) return (-EINVAL);

	if (sk->shutdown & RCV_SHUTDOWN) return (0);
	if (addr_len)
	  {
		  verify_area (VERIFY_WRITE, addr_len, sizeof(*addr_len));
		  put_fs_long (sizeof (*sin), addr_len);
	  }
	sk->inuse = 1;
	while (sk->rqueue == NULL)
	  {
	     if (noblock)
	       {
		  release_sock (sk);
		  if (copied) return (copied);
		  return (-EAGAIN);
	       }
	     release_sock (sk);
	     cli();
	     if (sk->rqueue == NULL)
	       {
		  interruptible_sleep_on (sk->sleep);
		  if (current->signal & ~current->blocked)
		    {
		      sti();
		      return (-ERESTARTSYS);
		    }
	       }
	     sk->inuse = 1;
	     sti();
	  }
	skb = sk->rqueue;

	if (!(flags & MSG_PEEK))
	  {
		  if (skb->next == skb )
		    {
			    sk->rqueue = NULL;
		    }
		  else
		    {
			    sk->rqueue = (struct sk_buff *)sk->rqueue ->next;
			    skb->prev->next = skb->next;
			    skb->next->prev = skb->prev;
		    }
	  }
	copied = min (len, skb->len);
	verify_area (VERIFY_WRITE, to, copied);
	memcpy_tofs (to, skb->h.raw,  copied);
	/* copy the address. */
	if (sin)
	  {
		  struct sockaddr_in addr;
		  addr.sin_family = AF_INET;
		  addr.sin_addr.s_addr = skb->daddr;
		  verify_area (VERIFY_WRITE, sin, sizeof (*sin));
		  memcpy_tofs(sin, &addr, sizeof (*sin));
	  }

	if (!(flags & MSG_PEEK))
	  {
	     kfree_skb (skb, FREE_READ);
	  }
	release_sock (sk);
	return (copied);

}

int
raw_read (volatile struct sock *sk, unsigned char *buff, int len, int noblock,
	  unsigned flags)
{
	return (raw_recvfrom (sk, buff, len, noblock, flags, NULL, NULL));
}


int udp_connect (volatile struct sock *sk, struct sockaddr_in *usin,
		 int addr_len);

int udp_select (volatile struct sock *sk, int sel_type, select_table *wait);


struct proto raw_prot =
{
  sock_wmalloc,
  sock_rmalloc,
  sock_wfree,
  sock_rfree,
  sock_rspace,
  sock_wspace,
  raw_close,
  raw_read,
  raw_write,
  raw_sendto,
  raw_recvfrom,
  ip_build_header,
  udp_connect,
  NULL,
  ip_queue_xmit,
  ip_retransmit,
  NULL,
  NULL,
  raw_rcv,
  udp_select,
  NULL,
  raw_init,
  NULL,
  128,
  0,
  {NULL,},
  "RAW"
};
