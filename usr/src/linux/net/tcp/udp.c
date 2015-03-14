/* udp.c */
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
/* $Id: udp.c,v 0.8.4.12 1993/01/26 22:04:00 bir7 Exp $ */
/* $Log: udp.c,v $
 * Revision 0.8.4.12  1993/01/26  22:04:00  bir7
 * Added support for proc fs.
 *
 * Revision 0.8.4.11  1993/01/23  18:00:11  bir7
 * added volatile keyword.
 *
 * Revision 0.8.4.10  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.9  1992/12/12  19:25:04  bir7
 * cleaned up Log messages.
 *
 * Revision 0.8.4.8  1992/12/12  01:50:49  bir7
 * Changed connect.
 *
 * Revision 0.8.4.7  1992/12/05  21:35:53  bir7
 * Added more debuggin code.
 *
 * Revision 0.8.4.6  1992/12/03  19:52:20  bir7
 * fixed problems in udp_error.
 *
 * Revision 0.8.4.5  1992/11/18  15:38:03  bir7
 * fixed minor problem in waiting for memory.
 *
 * Revision 0.8.4.3  1992/11/15  14:55:30  bir7
 * Fixed ctrl-h and added NULL checking to print_uh
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.5  1992/11/10  00:14:47  bir7
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
#include <linux/termios.h> /* for ioctl's */
#include <asm/system.h>
#include <asm/segment.h>
#include <linux/mm.h>
#include "../kern_sock.h" /* for PRINTK */
#include "udp.h"
#include "icmp.h"

#undef UDP_DEBUG

#ifdef PRINTK
#undef PRINTK
#endif

#ifdef UDP_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

#define min(a,b) ((a)<(b)?(a):(b))



static void
print_uh(struct udp_header *uh)
{
  if (uh == NULL)
    {
      PRINTK (("(NULL)\n"));
      return;
    }
	PRINTK(("source = %d, dest = %d\n", net16(uh->source), net16(uh->dest)));
	PRINTK(("len = %d, check = %d\n", net16(uh->len), net16(uh->check)));
}


int
udp_select (volatile struct sock *sk, int sel_type, select_table *wait)
{
   select_wait(sk->sleep, wait);
   switch (sel_type)
     {
       case SEL_IN:
	if (sk->rqueue != NULL) 
	  {
	     return (1);
	  }
	return (0);

       case SEL_OUT:
	if (sk->prot->wspace(sk) >= MIN_WRITE_SPACE)
	  {
	     return (1);
	  }
	return (0);
	
       case SEL_EX:
	if (sk->err) return (1); /* can this ever happen? */
	return (0);
     }
   return (0);
}

/* this routine is called by the icmp module when it gets some
   sort of error condition.  If err < 0 then the socket should
   be closed and the error returned to the user.  If err > 0
   it's just the icmp type << 8 | icmp code.  
   header points to the first 8 bytes of the tcp header.  We need
   to find the appropriate port. */

void
udp_err (int err, unsigned char *header, unsigned long daddr,
	 unsigned long saddr, struct ip_protocol *protocol)
{
   struct udp_header *th;
   volatile struct sock *sk;
   
   PRINTK (("udp_err (err=%d, header=%X, daddr=%X, saddr=%X, ip_protocl=%X)\n"));

   th = (struct udp_header *)header;
   sk = get_sock (&udp_prot, net16(th->dest), saddr, th->source, daddr);

   if (sk == NULL) return;
   if (err & 0xff00 == (ICMP_SOURCE_QUENCH << 8))
     {
	if (sk->cong_window > 1)
	  sk->cong_window = sk->cong_window/2;
	return;
     }

   sk->err = icmp_err_convert[err & 0xff].errno;
   /* it's only fatal if we have connected to them. */
   if (icmp_err_convert[err & 0xff].fatal && sk->state == TCP_ESTABLISHED)
     {
	sk->prot->close(sk, 0);
     }

   return;

}

static  unsigned short
udp_check (struct udp_header *uh, int len,
	   unsigned long saddr, unsigned long daddr)
{
   unsigned long sum;
   PRINTK (("udp_check (uh=%X, len = %d, saddr = %X, daddr = %X)\n",
	   uh, len, saddr, daddr));

   print_uh (uh);

  __asm__("\t addl %%ecx,%%ebx\n"
	  "\t adcl %%edx,%%ebx\n"
	  "\t adcl $0, %%ebx\n"
	  : "=b" (sum)
	  : "0" (daddr), "c" (saddr), "d" ((net16(len) << 16) + IPPROTO_UDP*256)
	  : "cx","bx","dx" );

  if (len > 3)
    {
      __asm__(
	      "\tclc\n"
	      "1:\n"
	      "\t lodsl\n"
	      "\t adcl %%eax, %%ebx\n"
	      "\t loop 1b\n"
	      "\t adcl $0, %%ebx\n"
	      : "=b" (sum) , "=S" (uh)
	      : "0" (sum), "c" (len/4) ,"1" (uh)
	      : "ax", "cx", "bx", "si" );
    }

  /* convert from 32 bits to 16 bits. */
  __asm__(
	  "\t movl %%ebx, %%ecx\n"
	  "\t shrl $16,%%ecx\n"
	  "\t addw %%cx, %%bx\n"
	  "\t adcw $0, %%bx\n"
	  : "=b" (sum)
	  : "0" (sum)
	  : "bx", "cx");


  /* check for an extra word. */
  if ((len & 2) != 0)
    {
      __asm__("\t lodsw\n"
	      "\t addw %%ax,%%bx\n"
	      "\t adcw $0, %%bx\n"
	      : "=b" (sum), "=S" (uh)
	      : "0" (sum) ,"1" (uh)
	      : "si", "ax", "bx");
    }

  /* now check for the extra byte. */
  if ((len & 1) != 0)
    {
      __asm__("\t lodsb\n"
	      "\t movb $0,%%ah\n"
	      "\t addw %%ax,%%bx\n"
	      "\t adcw $0, %%bx\n"
	      : "=b" (sum)
	      : "0" (sum) ,"S" (uh)
	      : "si", "ax", "bx");
    }
  /* we only want the bottom 16 bits, but we never cleared
     the top 16. */
   return ((~sum) & 0xffff);
}

static  void
udp_send_check (struct udp_header *uh, unsigned long saddr, 
		unsigned long daddr, int len, volatile struct sock *sk)
{
  uh->check = 0;
  if (sk && sk->no_check) return;
  uh->check = udp_check (uh, len, saddr, daddr);
}

static  int
udp_loopback (volatile struct sock *sk, unsigned short port,
	      unsigned char *from,
	      int len, unsigned long daddr, unsigned long saddr)
{
	struct udp_header *uh;
	struct sk_buff *skb;
	volatile struct sock *pair;
	sk->inuse = 1;

	PRINTK (("udp_loopback \n"));

	pair = get_sock (sk->prot, net16(port), saddr,
			 sk->dummy_th.source, daddr);

	if (pair == NULL) return (0);

	skb = pair->prot->rmalloc (pair,
				   sizeof (*skb) + sizeof (*uh) + len + 4,
				   0, GFP_KERNEL);

	/* if we didn't get the memory, just drop the packet. */
	if (skb == NULL) return (len);
	skb->lock = 0;
	skb->mem_addr = skb;
	skb->mem_len = sizeof (*skb) + len + sizeof (*uh) + 4;

	skb->daddr = saddr;
	skb->saddr = daddr;

	skb->len = len;
	skb->h.raw = (unsigned char *)(skb+1);

	uh = skb->h.uh;
	uh -> source = sk->dummy_th.source;
	uh -> dest = port;
	uh -> len = len + sizeof (*uh);
/*	verify_area (VERIFY_WRITE, from , len); */
	memcpy_fromfs(uh+1, from, len);
	pair->inuse = 1;
	if (pair->rqueue == NULL)
	  {
		  pair->rqueue = skb;
		  skb->next = skb;
		  skb->prev = skb;
	  }
	else
	  {
		  skb->next = pair->rqueue;
		  skb->prev = pair->rqueue->prev;
		  skb->prev->next = skb;
		  skb->next->prev = skb;
	  }
	wake_up (pair->sleep);
	release_sock (pair);
	release_sock (sk);
	return (len);

}

static int
udp_sendto (volatile struct sock *sk, unsigned char *from, int len,
	    int noblock,
	    unsigned flags, struct sockaddr_in *usin, int addr_len)
{
	/* this should be easy, we just send the packet. */
	struct sk_buff *skb;
	struct udp_header *uh;
	unsigned char *buff;
	unsigned long saddr;
	int copied=0;
	int amt;
	struct device *dev=NULL;
	struct sockaddr_in sin;

	/* check the flags. */
	if (flags) return (-EINVAL);
	if (len < 0) return (-EINVAL);
	if (len == 0) return (0);

	PRINTK (("sendto len = %d\n", len));

	/* get and verify the address. */
	if (usin)
	  {
		  if (addr_len < sizeof (sin))
		    return (-EINVAL);
/*		  verify_area (VERIFY_WRITE, usin, sizeof (sin));*/
		  memcpy_fromfs (&sin, usin, sizeof(sin));
		  if (sin.sin_family &&
		      sin.sin_family != AF_INET)
		    return (-EINVAL);
		  if (sin.sin_port == 0)
		    return (-EINVAL);
	  }
	else
	  {
		  if (sk->state != TCP_ESTABLISHED)
		    return (-EINVAL);
		  sin.sin_family = AF_INET;
		  sin.sin_port = sk->dummy_th.dest;
		  sin.sin_addr.s_addr = sk->daddr;
	  }

	/* check for a valid saddr. */
	saddr = sk->saddr;
	if ((saddr &  0xff000000) == 0)
	  {
	     saddr = MY_IP_ADDR;
	  }

	/* if it's a broadcast, make sure we get it. */
	if ((sin.sin_addr.s_addr & 0xff000000) == 0)
	  {
	     int err;
	     err = udp_loopback (sk, sin.sin_port, from, len,
				 sin.sin_addr.s_addr, saddr);
	     if (err < 0)
	       return (err);
	  }

	sk->inuse = 1;

	while (len > 0)
	  {
		  int tmp;
		  skb = sk->prot->wmalloc (sk, len + sizeof (*skb)
					       + sk->prot->max_header, 0,
					   GFP_KERNEL);
		  /* this should never happen, but it is possible. */

		  if (skb == NULL)
		    {
		       tmp = sk->wmem_alloc;
		       release_sock (sk);
		       if (copied) return (copied);
		       if (noblock) return (-EAGAIN);
		       cli();
		       if (tmp <= sk->wmem_alloc)
			 {
			   interruptible_sleep_on (sk->sleep);
			   if (current->signal & ~current->blocked)
			     {
			       sti();
			       if (copied) return (copied);
			       return (-ERESTARTSYS);
			     }
			 }
		       sk->inuse = 1;
		       sti();
		       continue;
		    }

		  skb->lock = 0;
		  skb->mem_addr = skb;
		  skb->mem_len = len + sizeof (*skb) + sk->prot->max_header;
		  skb->sk = sk;
		  skb->free = 1;
		  skb->arp = 0;

		  /* now build the ip and dev header. */
		  buff = (unsigned char *)(skb+1);
		  tmp = sk->prot->build_header (skb, saddr,
						sin.sin_addr.s_addr, &dev,
						IPPROTO_UDP, sk->opt, skb->mem_len);
		  if (tmp < 0 )
		    {
			    sk->prot->wfree (sk, skb->mem_addr, skb->mem_len);
			    release_sock (sk);
			    return (tmp);
		    }
		  buff += tmp;

		  /* we shouldn't do this, instead we should just
		     let the ip protocol fragment the packet. */
		  amt = min (len + tmp + sizeof (*uh), dev->mtu);

		  PRINTK (("amt = %d, dev = %X, dev->mtu = %d\n",
			  amt, dev, dev->mtu));

		  skb->len = amt;
		  amt -= tmp; 

		  uh = (struct udp_header *)buff;
		  uh->len = net16(amt);
		  uh->source = sk->dummy_th.source;
		  uh->dest = sin.sin_port;

		  amt -= sizeof (*uh);
		  buff += sizeof (*uh);
		  if (amt < 0)
		    {
		      printk ("udp.c: amt = %d < 0\n",amt);
		      release_sock (sk);
		      return (copied);
		    }

/*		  verify_area (VERIFY_WRITE, from, amt);*/
		  memcpy_fromfs( buff, from, amt);

		  len -= amt;
		  copied += amt;
		  from += amt;
		  udp_send_check (uh, saddr, sin.sin_addr.s_addr,
				  amt+sizeof (*uh), sk);
				  
		  sk->prot->queue_xmit (sk, dev, skb, 1);
	  }
	release_sock (sk);
	return (copied);
}

static int
udp_write (volatile struct sock *sk, unsigned char *buff, int len, int noblock,
	   unsigned flags)
{
	return (udp_sendto (sk, buff, len, noblock, flags, NULL, 0));
}


static int
udp_ioctl (volatile struct sock *sk, int cmd, unsigned long arg)
{
  switch (cmd)
    {
    default:
      return (-EINVAL);

      case TIOCOUTQ:
	{
	  unsigned long amount;
	  if (sk->state == TCP_LISTEN)
	    return (-EINVAL);
	  amount = sk->prot->wspace(sk)/2;
	  verify_area (VERIFY_WRITE, (void *)arg, sizeof (unsigned long));
	  put_fs_long (amount, (unsigned long *)arg);
	  return (0);
	}


      case TIOCINQ:
/*      case FIONREAD:*/
	{
	  struct sk_buff *skb;
	  unsigned long amount;
	  if (sk->state == TCP_LISTEN)
	    return (-EINVAL);
	  amount = 0;
	  skb = sk->rqueue;
	  if (skb != NULL)
	    {
	      /* we will only return the amount of this packet since that is all
		 that will be read. */
	      amount = skb->len;
	    }

	  verify_area (VERIFY_WRITE, (void *)arg, sizeof (unsigned long));
	  put_fs_long (amount, (unsigned long *)arg);
	  return (0);
	}
    }
}

int
udp_recvfrom (volatile struct sock *sk, unsigned char *to, int len,
	      int noblock,
	      unsigned flags, struct sockaddr_in *sin, int *addr_len)
{
	/* this should be easy, if there is something there we
	   return it, otherwise we block. */
	int copied=0;
	struct sk_buff *skb;
	if (len == 0) return (0);
	if (len < 0) return (-EINVAL);

	/* this will pick up errors that occured
	   while the program was doing something
	   else. */
	if (sk->err)
	  {
	    int err;
	    err = -sk->err;
	    sk->err = 0;
	    return (err);
	  }
	if (addr_len)
	  {
		  verify_area (VERIFY_WRITE, addr_len, sizeof(*addr_len));
		  put_fs_long (sizeof (*sin), addr_len);
	  }
	sk->inuse = 1;
	while (sk->rqueue == NULL)
	  {
	    if (sk->shutdown & RCV_SHUTDOWN)
	      {
		return (0);
	      }

	    if (noblock)
	       {
		  release_sock (sk);
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
	memcpy_tofs (to, skb->h.raw + sizeof (struct udp_header), copied);
	/* copy the address. */
	if (sin)
	  {
		  struct sockaddr_in addr;
		  addr.sin_family = AF_INET;
		  addr.sin_port = skb->h.uh->source;
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
udp_read (volatile struct sock *sk, unsigned char *buff, int len, int noblock,
	  unsigned flags)
{
	return (udp_recvfrom (sk, buff, len, noblock, flags, NULL, NULL));
}

int
udp_connect (volatile struct sock *sk, struct sockaddr_in *usin, int addr_len)
{
	struct sockaddr_in sin;
	if (addr_len < sizeof (sin)) return (-EINVAL);
/*	verify_area (VERIFY_WRITE, usin, sizeof (sin)); */
	memcpy_fromfs (&sin, usin, sizeof (sin));
	if (sin.sin_family && sin.sin_family != AF_INET)
	  return (-EAFNOSUPPORT);
	sk->daddr = sin.sin_addr.s_addr;
	sk->dummy_th.dest = sin.sin_port;
	sk->state = TCP_ESTABLISHED;
	return(0);
}

static void
udp_close(volatile struct sock *sk, int timeout)
{
	sk->inuse = 1;
	sk->state = TCP_CLOSE;
	if (sk->dead)
	  destroy_sock (sk);
	else
	  release_sock (sk);
}

int
udp_rcv(struct sk_buff *skb, struct device *dev, struct options *opt,
	unsigned long daddr, unsigned short len,
	unsigned long saddr, int redo, struct ip_protocol *protocol)
{
	/* all we need to do is get the socket, and then do a checksum. */
	struct proto *prot=&udp_prot;
	volatile struct sock *sk;
	struct udp_header *uh;

	uh = (struct udp_header *) skb->h.uh;

	if (dev->add_arp) dev->add_arp (saddr, skb, dev);

	sk = get_sock (prot, net16(uh->dest), saddr, uh->source, daddr);

	/* if we don't know about the socket, forget about it. */
	if (sk == NULL)
	  {
	    if ((daddr & 0xff000000 != 0) &&
		(daddr & 0xff000000 != 0xff000000))
	      {
		icmp_reply (skb, ICMP_DEST_UNREACH, ICMP_PORT_UNREACH, dev);
	      }
	    skb->sk = NULL;
	    kfree_skb (skb, 0);
	    return (0);
	  }


	if (!redo)
	  {
	     if (uh->check && udp_check (uh, len, saddr, daddr))
	       {
		  PRINTK (("bad udp checksum\n"));
		  skb->sk = NULL;
		  kfree_skb (skb, 0);
		  return (0);
	       }

	     skb->sk = sk;
	     skb->dev = dev;
	     skb->len = len;

	     /* these are supposed to be switched. */
	     skb->daddr = saddr;
	     skb->saddr = daddr;

	     /* Now deal with the in use. */
	     cli();
	     if (sk->inuse)
	       {
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
	     kfree_skb (skb, 0);
	     release_sock (sk);
	     return (0);
	  }
	     
	sk->rmem_alloc += skb->mem_len;

	/* At this point we should print the thing out. */
	PRINTK (("<< \n"));

	/* now add it to the data chain and wake things up. */
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

	skb->len = len - sizeof (*uh);

	if (!sk->dead)
	  wake_up (sk->sleep);

	release_sock (sk);
	return (0);
}



struct proto udp_prot =
{
  sock_wmalloc,
  sock_rmalloc,
  sock_wfree,
  sock_rfree,
  sock_rspace,
  sock_wspace,
  udp_close,
  udp_read,
  udp_write,
  udp_sendto,
  udp_recvfrom,
  ip_build_header,
  udp_connect,
  NULL,
  ip_queue_xmit,
  ip_retransmit,
  NULL,
  NULL,
  udp_rcv,
  udp_select,
  udp_ioctl,
  NULL,
  NULL,
  128,
  0,
  {NULL,},
  "UDP"
};

