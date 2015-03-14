/* sock.c */
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
/* $Id: sock.c,v 0.8.4.16 1993/01/26 22:04:00 bir7 Exp $ */
/* $Log: sock.c,v $
 * Revision 0.8.4.16  1993/01/26  22:04:00  bir7
 * Added support for proc fs.
 *
 * Revision 0.8.4.15  1993/01/23  18:00:11  bir7
 * Added volatile keyword
 *
 * Revision 0.8.4.14  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.13  1993/01/22  22:58:08  bir7
 * *** empty log message ***
 *
 * Revision 0.8.4.12  1992/12/12  19:25:04  bir7
 * Made memory leak checking more leanent.
 *
 * Revision 0.8.4.11  1992/12/12  01:50:49  bir7
 * Fixed memory leak in accept.
 *
 * Revision 0.8.4.10  1992/12/08  20:49:15  bir7
 * Added support for -EINPROGRESS
 *
 * Revision 0.8.4.9  1992/12/06  23:29:59  bir7
 * Added mss and support for half completed packets.
 *
 * Revision 0.8.4.8  1992/12/05  21:35:53  bir7
 * changed dev->init to return an int.
 *
 * Revision 0.8.4.7  1992/12/03  19:52:20  bir7
 * added paranoid queue checking
 *
 * Revision 0.8.4.6  1992/11/18  15:38:03  bir7
 * Fixed minor problem in setsockopt.
 *
 * Revision 0.8.4.5  1992/11/17  14:19:47  bir7
 *
 * Revision 0.8.4.4  1992/11/16  16:13:40  bir7
 * Fixed some error returns and undid one of the accept changes.
 *
 * Revision 0.8.4.3  1992/11/15  14:55:30  bir7
 * Added more checking for a packet being on a queue before it's
 * dropped when a socket is closed.  Added check to see if it's
 * on the arp_q also.
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

#include <linux/errno.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <netinet/in.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/string.h>
#include <linux/config.h>
#include <linux/sock_ioctl.h>
#include "../kern_sock.h"
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "udp.h"
#include "sock.h"
#include "arp.h"
#include <asm/segment.h>
#include <asm/system.h>
#include <linux/fcntl.h>
#include <linux/mm.h>
#include <linux/interrupt.h>

#undef ISOCK_DEBUG

#ifdef PRINTK
#undef PRINTK
#endif

#ifdef ISOCK_DEBUG
#define PRINTK(x) printk x
#else
#define PRINTK(x) /**/
#endif

#ifdef MEM_DEBUG
#define MPRINTK(x) printk x
#else
#define MPRINTK(x) /**/
#endif

#define min(a,b) ((a)<(b)?(a):(b))
#define swap(a,b) {unsigned long c; c=a; a=b; b=c;}

extern struct proto tcp_prot;
extern struct proto udp_prot;
extern struct proto raw_prot;
extern struct proto packet_prot;

static int ip_proto_init(void);
static int ip_proto_create(struct socket *sock, int protocol);
static int ip_proto_dup(struct socket *newsock, struct socket *oldsock);
static int ip_proto_release(struct socket *sock, struct socket *peer);
static int ip_proto_bind(struct socket *sock, struct sockaddr *umyaddr,
			   int sockaddr_len);
static int ip_proto_connect(struct socket *sock, struct sockaddr *uservaddr,
			      int sockaddr_len, int flags);
static int ip_proto_socketpair(struct socket *sock1, struct socket *sock2);
static int ip_proto_accept(struct socket *sock, struct socket *newsock, int flags);
static int ip_proto_getname(struct socket *sock, struct sockaddr *usockaddr,
			      int *usockaddr_len, int peer);
static int ip_proto_read(struct socket *sock, char *ubuf, int size,
			   int nonblock);
static int ip_proto_write(struct socket *sock, char *ubuf, int size,
			    int nonblock);
static int ip_proto_select(struct socket *sock, int which, select_table *wait);
static int ip_proto_ioctl(struct socket *sock, unsigned int cmd,
			    unsigned long arg);
static int ip_proto_listen(struct socket *sock, int backlog);

static int ip_proto_send (struct socket *sock, void *buff, int len,
			  int nonblock, unsigned flags);
static int ip_proto_recv (struct socket *sock, void *buff, int len,
			  int nonblock, unsigned flags);
static int ip_proto_sendto (struct socket *sock, void *buff, int len,
			    int nonblock, unsigned flags,
			    struct sockaddr *addr, int addr_len);
static int ip_proto_recvfrom (struct socket *sock, void *buff, int len,
			      int nonblock, unsigned flags,
			      struct sockaddr *addr, int *addr_len);

static int ip_proto_shutdown (struct socket *sock, int how);


static int ip_proto_setsockopt (struct socket *sock, int level, int optname,
				char *optval, int optlen);
static int ip_proto_getsockopt (struct socket *sock, int level, int optname,
				char *optval, int *optlen);
static int ip_proto_fcntl (struct socket *sock, unsigned int cmd,
			   unsigned long arg);

struct proto_ops inet_proto_ops = 
{
  ip_proto_init,
  ip_proto_create,
  ip_proto_dup,
  ip_proto_release,
  ip_proto_bind,
  ip_proto_connect,
  ip_proto_socketpair,
  ip_proto_accept,
  ip_proto_getname, 
  ip_proto_read,
  ip_proto_write,
  ip_proto_select,
  ip_proto_ioctl,
  ip_proto_listen,
  ip_proto_send,
  ip_proto_recv,
  ip_proto_sendto,
  ip_proto_recvfrom,
  ip_proto_shutdown,
  ip_proto_setsockopt,
  ip_proto_getsockopt,
  ip_proto_fcntl,
};

void
print_sk (volatile struct sock *sk)
{
  if (!sk) {
    printk ("  print_sk(NULL)\n");
    return;
  }
  printk ("  wmem_alloc = %d\n", sk->wmem_alloc);
  printk ("  rmem_alloc = %d\n", sk->rmem_alloc);
  printk ("  send_head = %X\n", sk->send_head);
  printk ("  state = %d\n",sk->state);
  printk ("  wback = %X, rqueue = %X\n", sk->wback, sk->rqueue);
  printk ("  wfront = %X\n", sk->wfront);
  printk ("  daddr = %X, saddr = %X\n", sk->daddr,sk->saddr);
  printk ("  num = %d", sk->num);
  printk (" next = %X\n", sk->next);
  printk ("  send_seq = %d, acked_seq = %d, copied_seq = %d\n",
	  sk->send_seq, sk->acked_seq, sk->copied_seq);
  printk ("  rcv_ack_seq = %d, window_seq = %d, fin_seq = %d\n",
	  sk->rcv_ack_seq, sk->window_seq, sk->fin_seq);
  printk ("  prot = %X\n", sk->prot);
  printk ("  pair = %X, back_log = %X\n", sk->pair,sk->back_log);
  printk ("  inuse = %d , blog = %d\n", sk->inuse, sk->blog);
  printk ("  dead = %d delay_acks=%d\n", sk->dead, sk->delay_acks);
  printk ("  retransmits = %d, timeout = %d\n", sk->retransmits, sk->timeout);
  printk ("  cong_window = %d, packets_out = %d\n", sk->cong_window,
	  sk->packets_out);
  printk ("  urg = %d shutdown=%d\n", sk->urg, sk->shutdown);
}

void
print_skb(struct sk_buff *skb)
{
  if (!skb) {
    printk ("  print_skb(NULL)\n");
    return;
  }
  printk ("  prev = %X, next = %X\n", skb->prev, skb->next);
  printk ("  sk = %X link3 = %X\n", skb->sk, skb->link3);
  printk ("  mem_addr = %X, mem_len = %d\n", skb->mem_addr, skb->mem_len);
  printk ("  used = %d free = %d\n", skb->used,skb->free);
}

/* just used to reference some pointers to keep gcc from over optimizing
   my code so that it doesn't work. */
void dummy_routine(void *dummy, ...)
{
   return;
}

void
lock_skb (struct sk_buff *skb)
{
   if (skb->lock)
     {
	printk ("*** bug more than one lock on sk_buff. \n");
     }
   skb->lock = 1;
}


void
kfree_skb (struct sk_buff *skb, int rw)
{
  if (skb == NULL)
    {
      printk ("kfree_skb: skb = NULL\n");
      return;
    }

   if (skb->lock)
     {
	skb->free = 1;
	return;
     }
   skb->magic = 0;
   if (skb->sk)
     {
	if (rw)
	  {
	     skb->sk->prot->rfree (skb->sk, skb->mem_addr, skb->mem_len);
	  }
	else
	  {
	     skb->sk->prot->wfree (skb->sk, skb->mem_addr, skb->mem_len);
	  }
     }
   else
     {
	kfree_s (skb->mem_addr, skb->mem_len);
     }
}

void
unlock_skb (struct sk_buff *skb, int rw)
{
   if (skb->lock != 1)
     {
	printk ("*** bug unlocking non-locked sk_buff. \n");
     }
   skb->lock = 0;
   if (skb->free)
     kfree_skb (skb, rw);
}

static  int
sk_inuse( struct proto *prot, int num)
{
  volatile struct sock *sk;
  for (sk = prot->sock_array[num & (SOCK_ARRAY_SIZE -1 )];
       sk != NULL; sk=sk->next)
    {
      if (sk->num == num) return (1);
    }
  return (0);
}

unsigned short
get_new_socknum(struct proto *prot, unsigned short base)
{
  static int start=0;
  /* used to cycle through the port numbers so the chances of
     a confused connection drop. */

  int i,j;
  int best=0;
  int size=32767; /* a big num. */
  volatile struct sock *sk;

  if (base == 0) base = PROT_SOCK+1+(start % 1024);
  if (base <= PROT_SOCK)
    {
      base += PROT_SOCK+(start % 1024);
    }

  /* now look through the entire array and try to find an empty
     ptr. */
  for (i=0; i < SOCK_ARRAY_SIZE; i++)
    {
      j = 0;
      sk = prot->sock_array[(i+base+1) & (SOCK_ARRAY_SIZE -1)];
      while (sk != NULL)
	{
	  sk = sk->next;
	  j++;
	}
      if (j == 0)
	{
	  start = (i+1+start )%1024;
          PRINTK (("get_new_socknum returning %d, start = %d\n",
		    i+base+1,start));
	  return (i+base+1);
	}
      if (j < size) 
	{
	  best = i;
	  size = j;
	}
    }
  /* now make sure the one we want is not in use. */
  while (sk_inuse (prot, base +best+1))
    {
      best += SOCK_ARRAY_SIZE;
    }
  PRINTK (("get_new_socknum returning %d, start = %d\n", best+base+1,start));
  return (best+base+1);
  
}

void
put_sock(unsigned short num, volatile struct sock *sk)
{
   volatile struct sock *sk1;
   volatile struct sock *sk2;
   int mask;

   PRINTK (("put_sock (num = %d, sk = %X\n", num, sk));
   sk->num = num;
   sk->next = NULL;
   num = num & (SOCK_ARRAY_SIZE -1);

   /* we can't have an interupt renter here. */
   cli();
   if (sk->prot->sock_array[num] == NULL)
     {
	sk->prot->sock_array[num] = sk;
	sti();
	return;
     }
   sti();
   for (mask = 0xff000000; mask != 0xffffffff; mask = (mask >> 8) | mask)
     {
	if (mask & sk->saddr)
	  {
	     mask = mask << 8;
	     break;
	  }
     }

   PRINTK (("mask = %X\n", mask));

   cli();
   sk1 = sk->prot->sock_array[num];
   for (sk2 = sk1; sk2 != NULL; sk2=sk2->next)
     {
	if (!(sk2->saddr & mask))
	  {
	     if (sk2 == sk1)
	       {
		  sk->next = sk->prot->sock_array[num];
		  sk->prot->sock_array[num] = sk;
		  sti();
		  return;
	       }
	     sk->next = sk2;
	     sk1->next= sk;
	     sti();
	     return;
	  }
	sk1 = sk2;
     }
   /* goes at the end. */
   sk->next = NULL;
   sk1->next = sk;
   sti();
}


static void
remove_sock(volatile struct sock *sk1)
{
  volatile struct sock *sk2;
  PRINTK (("remove_sock(sk1=%X)\n",sk1));

  if (!sk1)
    {
      printk ("sock.c: remove_sock: sk1 == NULL\n");
      return;
    }

  if (!sk1->prot)
    {
      printk ("sock.c: remove_sock: sk1->prot == NULL\n");
      return;
    }

  /* we can't have this changing out from under us. */
  cli();
  sk2 = sk1->prot->sock_array[sk1->num & (SOCK_ARRAY_SIZE -1)];
  if (sk2 == sk1)
    {
       sk1->prot->sock_array[sk1->num & (SOCK_ARRAY_SIZE -1)] = sk1->next;
       sti();
       return;
    }

  while (sk2 && sk2->next != sk1)
    sk2 = sk2->next;

  if (sk2)
    {
      sk2->next = sk1->next;
      sti();
      return;
    }
  sti();

  if (sk1->num != 0)
    PRINTK (("remove_sock: sock  not found.\n"));
}

void
destroy_sock(volatile struct sock *sk)
{

  struct sk_buff *skb;
  PRINTK (("destroying socket %X\n",sk));
  /* just to be safe. */
  sk->inuse = 1;

  /* incase it's sleeping somewhere. */
  if (!sk->dead) wake_up (sk->sleep);

  remove_sock (sk);
  /* now we can no longer get new packets. */

  delete_timer((struct timer *)&sk->time_wait);

  if (sk->send_tmp != NULL) kfree_skb (sk->send_tmp, FREE_WRITE);

  /* cleanup up the write buffer. */
  for (skb = sk->wfront; skb != NULL; )
    {
      struct sk_buff *skb2;
      skb2=(struct sk_buff *)skb->next;
      if (skb->magic != TCP_WRITE_QUEUE_MAGIC)
	{
	  printk ("sock.c:destroy_sock write queue with bad magic (%X)\n",
		  skb->magic);
	  break;
	}
      kfree_skb(skb, FREE_WRITE);
      skb=skb2;
    }

  sk->wfront = NULL;
  sk->wback = NULL;

  if (sk->rqueue != NULL)
    {
       skb = sk->rqueue;
       do {
	  struct sk_buff *skb2;
	  skb2=(struct sk_buff *)skb->next;

	  /* this will take care of closing sockets that were
	     listening and didn't accept everything. */

	  if (skb->sk != NULL && skb->sk != sk)
	    {
	       skb->sk->dead = 1;
	       skb->sk->prot->close (skb->sk, 0);
	    }
	  kfree_skb(skb, FREE_READ);
	  skb=skb2;
       } while (skb != sk->rqueue);
    }

  sk->rqueue = NULL;

  /* now we need to clean up the send head. */
  for (skb = sk->send_head; skb != NULL; )
    {
      struct sk_buff *skb2;
      /* we need to remove skb from the transmit queue, or
       maybe the arp queue */
      cli();
      /* see if it's in a transmit queue. */
      /* this can be simplified quite a bit.  Look
	 at tcp.c:tcp_ack to see how. */

      if (skb->next != NULL)
	{
	  extern volatile struct sk_buff *arp_q;
	  int i;
	   if (skb->next != skb)
	     {
		skb->next->prev = skb->prev;
		skb->prev->next = skb->next;

		if (skb == arp_q)
		  {
		    if (skb->magic != ARP_QUEUE_MAGIC)
		      {
			sti();
			printk ("sock.c: destroy_sock skb on arp queue with"
				"bas magic (%X)\n", skb->magic);
			cli();
			arp_q = NULL;
			continue;
		      }
		    arp_q = skb->next;
		  }
		else
		  {
		    for (i = 0; i < DEV_NUMBUFFS; i++)
		      {
			if (skb->dev && skb->dev->buffs[i] == skb)
			  {
			    if (skb->magic != DEV_QUEUE_MAGIC)
			      {
				sti();
				printk ("sock.c: destroy sock skb on dev queue"
					"with bad magic (%X)\n", skb->magic);
				cli();
				break;
			      }
			    skb->dev->buffs[i]= skb->next;
			    break;
			  }
		      }
		  }
	      }
	   else
	     {
	       if (skb == arp_q)
		 {
		    if (skb->magic != ARP_QUEUE_MAGIC)
		      {
			sti();
			printk ("sock.c: destroy_sock skb on arp queue with"
				"bas magic (%X)\n", skb->magic);
			cli();
		      }
		   arp_q = NULL;
		 }
	       else
		 {
		   for (i = 0; i < DEV_NUMBUFFS; i++)
		     {
		       if (skb->dev && skb->dev->buffs[i] == skb)
			 {
			    if (skb->magic != DEV_QUEUE_MAGIC)
			      {
				sti();
				printk ("sock.c: destroy sock skb on dev queue"
					"with bad magic (%X)\n", skb->magic);
				cli();
				break;
			      }
			   skb->dev->buffs[i]= NULL;
			   break;
			 }
		     }
		 }
	     }
	}
      skb->dev = NULL;
      sti();
      skb2=(struct sk_buff *)skb->link3;
      kfree_skb(skb, FREE_WRITE);
      skb=skb2;
    }

  sk->send_head = NULL;

  /* and now the backlog. */

  if (sk->back_log != NULL)
    {
       /* this should never happen. */
       printk ("cleaning back_log. \n");
       cli();
       skb = (struct sk_buff *)sk->back_log;
       do {
	  struct sk_buff *skb2;
	  skb2=(struct sk_buff *)skb->next;
	  kfree_skb(skb, FREE_READ);
	  skb=skb2;
       } while (skb != sk->back_log);
       sti();
    }

  sk->back_log = NULL;

  /* Now if it has a half accepted/ closed socket. */
  if (sk->pair)
    {
      sk->pair->dead = 1;
      sk->pair->prot->close (sk->pair, 0);
      sk->pair = NULL;
    }

  /* now if everything is gone we can free the socket structure, 
     otherwise we need to keep it around until everything is gone. */
  if (sk->rmem_alloc == 0 && sk->wmem_alloc == 0)
    {
       kfree_s ((void *)sk,sizeof (*sk));
    }
  else
    {
       /* this should never happen. */
       /* actually it can if an ack has just been sent. */
       PRINTK (("possible memory leak in socket = %X\n", sk));
       sk->destroy = 1;
       sk->ack_backlog = 0;
       sk->inuse = 0;
       sk->time_wait.len = SOCK_DESTROY_TIME;
       sk->timeout = TIME_DESTROY;
       reset_timer ((struct timer *)&sk->time_wait);
    }
  PRINTK (("leaving destroy_sock\n"));
}


static int
ip_proto_fcntl (struct socket *sock, unsigned int cmd, unsigned long arg)
{
   volatile struct sock *sk;

   sk=sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

   switch (cmd)
     {
       case F_SETOWN:

       /* this is a little restrictive, but it's the only way to make
	  sure that you can't send a sigurg to another process. */
       if (!suser() && current->pgrp != -arg && current->pid != arg)
	 return (-EPERM);

       sk->proc = arg;
       return (0);


       
       sk->proc = arg;
       return (0);

       case F_GETOWN:
	return (sk->proc);

       default:
	return (-EINVAL);
     }
}

static int
ip_proto_setsockopt(struct socket *sock, int level, int optname,
		    char *optval, int optlen)
{
    volatile struct sock *sk;
    int val;
    /* This should really pass things on to the other levels. */
    if (level != SOL_SOCKET) return (-EOPNOTSUPP);
    sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }
    if (optval == NULL) return (-EINVAL);

/*    verify_area (VERIFY_WRITE, optval, sizeof (int));*/
    val = get_fs_long ((unsigned long *)optval);
    switch (optname)
      {
	case SO_TYPE:
	case SO_ERROR:
	default:
	  return (-ENOPROTOOPT);

	case SO_DEBUG: /* not implemented. */
	case SO_DONTROUTE:
	case SO_BROADCAST:
	case SO_SNDBUF:
	case SO_RCVBUF:
	  return (0);

	case SO_REUSEADDR:
	  if (val)
	    sk->reuse = 1;
	  else 
	    sk->reuse = 0;
	  return (0);

	case SO_KEEPALIVE:
	  if (val)
	    sk->keepopen = 1;
	  else
	    sk->keepopen = 0;
	  return (0);

	 case SO_OOBINLINE:
	  if (val)
	    sk->urginline = 1;
	  else
	    sk->urginline = 0;
	  return (0);

	 case SO_NO_CHECK:
	  if (val)
	    sk->no_check = 1;
	  else
	    sk->no_check = 0;
	  return (0);

	 case SO_PRIORITY:
	  if (val >= 0 && val < DEV_NUMBUFFS)
	    {
	       sk->priority = val;
	    }
	  else
	    {
	       return (-EINVAL);
	    }
	  return (0);

      }
}

static int
ip_proto_getsockopt(struct socket *sock, int level, int optname,
		    char *optval, int *optlen)
{
    volatile struct sock *sk;
    int val;
    /* This should really pass things on to the other levels. */
    if (level != SOL_SOCKET) return (-EOPNOTSUPP);
    sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

    switch (optname)
      {
	default:
	  return (-ENOPROTOOPT);

	case SO_DEBUG: /* not implemented. */
	case SO_DONTROUTE:
	case SO_BROADCAST:
	case SO_SNDBUF:
	case SO_RCVBUF:
	  val = 0;
	  break;

	case SO_REUSEADDR:
	  val = sk->reuse;
	  break;

	case SO_KEEPALIVE:
	  val = sk->keepopen;
	  break;

	case SO_TYPE:
	  if (sk->prot == &tcp_prot)
	    val = SOCK_STREAM;
	  else
	    val = SOCK_DGRAM;
	  break;

	case SO_ERROR:
	  val = sk->err;
	  sk->err = 0;
	  break;

	 case SO_OOBINLINE:
	  val = sk->urginline;
	  break;

	 case SO_NO_CHECK:
	  val = sk->no_check;
	  break;

	 case SO_PRIORITY:
	  val = sk->priority;
	  break;
      }
    verify_area (VERIFY_WRITE, optlen, sizeof (int));
    put_fs_long (sizeof(int),(unsigned long *) optlen);

    verify_area(VERIFY_WRITE, optval, sizeof (int));
    put_fs_long (val, (unsigned long *)optval);
    return (0);
}

static int
ip_proto_listen(struct socket *sock, int backlog)
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  /* we might as well re use these. */ 
  sk->max_ack_backlog = backlog;
  sk->ack_backlog = 0;
  sk->state = TCP_LISTEN;
  return (0);
}

/* Hardware should be inited here. */
static int ip_proto_init(void)
{
  int i;
  struct device *dev, *dev2;
  struct ip_protocol *p;
  seq_offset = CURRENT_TIME*250;
  /* add all the protocols. */
  for (i = 0; i < SOCK_ARRAY_SIZE; i++)
    {
       tcp_prot.sock_array[i] = NULL;
       udp_prot.sock_array[i] = NULL;
       raw_prot.sock_array[i] = NULL;
    }

  for (p = ip_protocol_base; p != NULL;)
    {
       struct ip_protocol *tmp;
       /* add all the protocols. */
       tmp = (struct ip_protocol *) p->next;
       add_ip_protocol (p);
       p = tmp;
    }

  /* add the devices */
  /* if the call to dev->init fails, the dev is removed
     from the chain disconnecting the device until the
     next reboot. */

  dev2 = NULL;
  for (dev = dev_base; dev != NULL; dev=dev->next)
    {
       if (dev->init && dev->init(dev))
	 {
	   if (dev2 == NULL)
	     dev_base = dev->next;
	   else
	     dev2->next = dev->next;
	 }
       else
	 {
	   dev2 = dev;
	 }
    }
  bh_base[INET_BH].routine = inet_bh;
  timer_table[NET_TIMER].fn = net_timer;
  return (0);
}

static int
ip_proto_create (struct socket *sock, int protocol)
{
  volatile struct sock *sk;
  struct proto *prot;
  int err;

  sk = kmalloc (sizeof (*sk), GFP_KERNEL);
  if (sk == NULL)
    return (-ENOMEM);
  sk->num = 0;


  switch (sock->type)
    {
    case SOCK_STREAM:
    case SOCK_SEQPACKET:
       if (protocol && protocol != IPPROTO_TCP)
	 {
	    kfree_s ((void *)sk, sizeof (*sk));
	    return (-EPROTONOSUPPORT);
	 }
       sk->no_check = TCP_NO_CHECK;
       prot = &tcp_prot;
       break;

    case SOCK_DGRAM:
       if (protocol && protocol != IPPROTO_UDP)
	 {
	    kfree_s ((void *)sk, sizeof (*sk));
	    return (-EPROTONOSUPPORT);
	 }
       sk->no_check = UDP_NO_CHECK;
       prot=&udp_prot;
       break;
      
     case SOCK_RAW:
       if (!suser())
	 {
	    kfree_s ((void *)sk, sizeof (*sk));
	    return (-EPERM);
	 }

       if (!protocol)
	 {
	    kfree_s ((void *)sk, sizeof (*sk));
	    return (-EPROTONOSUPPORT);
	 }
       prot = &raw_prot;
       sk->reuse = 1;
       sk->no_check = 0; /* doesn't matter no checksum is preformed
			    anyway. */
       sk->num = protocol;
       break;

    case SOCK_PACKET:
       if (!suser())
	 {
	    kfree_s ((void *)sk, sizeof (*sk));
	    return (-EPERM);
	 }

       if (!protocol)
	 {
	    kfree_s ((void *)sk, sizeof (*sk));
	    return (-EPROTONOSUPPORT);
	 }
       prot = &packet_prot;
       sk->reuse = 1;
       sk->no_check = 0; /* doesn't matter no checksum is preformed
			    anyway. */
       sk->num = protocol;
       break;

      
    default:
       kfree_s ((void *)sk, sizeof (*sk));
       return (-ESOCKTNOSUPPORT);

    }
  sk->protocol = protocol;
  sk->wmem_alloc = 0;
  sk->rmem_alloc = 0;
  sk->pair = NULL;
  sk->opt = NULL;
  sk->send_seq = 0;
  sk->acked_seq = 0;
  sk->copied_seq = 0;
  sk->fin_seq = 0;
  sk->proc = 0;
  sk->rtt = TCP_WRITE_TIME;
  sk->packets_out = 0;
  sk->cong_window = 1; /* start with only sending one packet at a time. */
  sk->exp_growth = 1;  /* if set cong_window grow exponentially every time
			  we get an ack. */
  sk->urginline = 0;
  sk->intr = 0;
  sk->linger = 0;
  sk->destroy = 0;
  sk->reuse = 0;
  sk->priority = 1;
  sk->shutdown = 0;
  sk->urg = 0;
  sk->keepopen = 0;
  sk->done = 0;
  sk->ack_backlog = 0;
  sk->window = 0;
  sk->bytes_rcv = 0;
  sk->state = TCP_CLOSE;
  sk->dead = 0;
  sk->ack_timed = 0;
  sk->send_tmp = NULL;
  sk->mss = 0; /* we will try not to send any packets smaller
		   than this. */

  /* this is how many unacked bytes we will accept for
     this socket.  */

  sk->max_unacked = 2048; /* needs to be at most 2 full packets. */

  /* how many packets we should send before forcing an ack. 
     if this is set to zero it is the same as sk->delay_acks = 0 */

  sk->max_ack_backlog = 0;
  sk->inuse = 0;
  sk->delay_acks = 0;
  sk->wback = NULL;
  sk->wfront = NULL;
  sk->rqueue = NULL;
  sk->mtu = 576;
  sk->prot = prot;
  sk->sleep = sock->wait;
  sk->daddr = 0;
  sk->saddr = MY_IP_ADDR;
  sk->err = 0;
  sk->next = NULL;
  sk->pair = NULL;
  sk->send_tail = NULL;
  sk->send_head = NULL;
  sk->time_wait.len = TCP_CONNECT_TIME;
  sk->time_wait.when = 0;
  sk->time_wait.sk = sk;
  sk->time_wait.next = NULL;
  sk->timeout = 0;
  sk->back_log = NULL;
  sk->blog = 0;
  sock->data =(void *) sk;
  sk->dummy_th.doff = sizeof (sk->dummy_th)/4;
  sk->dummy_th.res1=0;
  sk->dummy_th.res2=0;
  sk->dummy_th.urg_ptr = 0;
  sk->dummy_th.fin = 0;
  sk->dummy_th.syn = 0;
  sk->dummy_th.rst = 0;
  sk->dummy_th.psh = 0;
  sk->dummy_th.ack = 0;
  sk->dummy_th.urg = 0;
  sk->dummy_th.dest = 0;

  if (sk->num)
    {
      /* it assumes that any protocol which allows
	 the user to assign a number at socket
	 creation time automatically
	 shares. */
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  if (sk->prot->init)
    {
       err = sk->prot->init(sk);
       if (err != 0)
	 {
	    destroy_sock (sk);
	    return (err);
	 }
    }
  return (0);
}

static int
ip_proto_dup (struct socket *newsock, struct socket *oldsock)
{
  return (ip_proto_create (newsock,
			   ((volatile struct sock *)(oldsock->data))->protocol));
}

/* the peer socket should always be NULL. */
static int
ip_proto_release(struct socket *sock, struct socket *peer)
{
  volatile struct sock *sk;
  sk = sock->data;
  if (sk == NULL) return (0);
  PRINTK (("ip_proto_release (sock = %X, peer = %X)\n", sock, peer));
  wake_up (sk->sleep);
  /* start closing the connection.  This may take a while. */
  /* if linger is set, we don't return until the close is
     complete.  Other wise we return immediately.  The
     actually closing is done the same either way. */
  if (sk->linger == 0)
    {
       sk->prot->close(sk,0);
       sk->dead = 1;
    }
  else
    {
      PRINTK (("sk->linger set.\n"));
       sk->prot->close(sk, 0);
       cli();
       while (sk->state != TCP_CLOSE)
	 {
	    interruptible_sleep_on (sk->sleep);
	    if (current->signal & ~current->blocked)
	      {
		 sti();
		 return (-ERESTARTSYS);
	      }
	 }
       sti();
       sk->dead = 1;
    }

  sk->inuse = 1;
  /* this will destroy it. */
  release_sock (sk);
  sock->data = NULL;
  PRINTK (("ip_proto_release returning\n"));
  return (0);
}


/* this needs to be changed to dissallow
   the rebinding of sockets.   What error
   should it return? */

static int
ip_proto_bind (struct socket *sock, struct sockaddr *uaddr,
	       int addr_len)
{
  struct sockaddr_in addr;
  volatile struct sock *sk, *sk2;
  unsigned short snum;

  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }
  /* check this error. */
  if (sk->state != TCP_CLOSE) return (-EIO);
  if (sk->num != 0) return (-EINVAL);

/*  verify_area (VERIFY_WRITE, uaddr, addr_len);*/
  memcpy_fromfs (&addr, uaddr, min (sizeof (addr), addr_len));

#if 0
  if (addr.sin_family && addr.sin_family != AF_INET)
    {
      /* this is really a bug in BSD which we need to emulate because
	 ftp expects it. */
      return (-EINVAL);
    }
#endif

  snum = net16(addr.sin_port);
  PRINTK (("bind sk =%X to port = %d\n", sk, snum));
  sk = sock->data;

  /* we can't just leave the socket bound wherever it is, it might be bound
     to a priveledged port. However, since there seems to be a bug here,
     we will leave it if the port is not priveledged(sp?) */

  if (snum == 0)
    {
       if ( sk->num > PROT_SOCK) return (0);
       snum = get_new_socknum (sk->prot, 0);
    }

  if (snum <= PROT_SOCK && !suser())
    return (-EACCES);

  if (my_ip_addr(addr.sin_addr.s_addr) || addr.sin_addr.s_addr == 0)
    sk->saddr = addr.sin_addr.s_addr;

  PRINTK (("sock_array[%d] = %X:\n", snum & (SOCK_ARRAY_SIZE -1),
	  sk->prot->sock_array[snum & (SOCK_ARRAY_SIZE -1)]));

  /* make sure we are allowed to bind here. */
  for (sk2 = sk->prot->sock_array[snum & (SOCK_ARRAY_SIZE -1)];
       sk2 != NULL;
       sk2 = sk2->next)
    {
       if (sk2->num != snum) continue;
       if (sk2->saddr != sk->saddr) continue;
       if (!sk->reuse) return (-EADDRINUSE);
       if (!sk2->reuse) return (-EADDRINUSE);
    }
  remove_sock (sk);
  put_sock(snum, sk);
  sk->dummy_th.source = net16(sk->num);
  sk->daddr = 0;
  sk->dummy_th.dest = 0;
  return (0);
}

static int
ip_proto_connect (struct socket *sock, struct sockaddr * uaddr,
		  int addr_len, int flags)
{
  volatile struct sock *sk;
  int err;
  sock->conn = NULL;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  if (sk->state == TCP_ESTABLISHED)
    {
      sock->state = SS_CONNECTED;
      return (-EISCONN);
    }

  if (sock->state == SS_CONNECTING)
    {
      if (sk->state == TCP_SYN_SENT || sk->state == TCP_SYN_RECV)
	return (-EINPROGRESS);

      if (sk->err) return (-sk->err);

      sock->state = SS_CONNECTED;
      return (-EISCONN);
    }

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  if (sk->prot->connect == NULL)
    return (-EOPNOTSUPP);

  err = sk->prot->connect (sk, (struct sockaddr_in *)uaddr, addr_len);
  if (err < 0) return (err);

  sock->state = SS_CONNECTING;
  if (sk->state != TCP_ESTABLISHED && (flags & O_NONBLOCK))
    return (-EINPROGRESS);

  cli(); /* avoid the race condition */

  while (sk->state == TCP_SYN_SENT || sk->state == TCP_SYN_RECV)
    {
      interruptible_sleep_on (sk->sleep);
      if (current->signal & ~current->blocked)
	{
	   sti();
	   return (-ERESTARTSYS);
	}
    }
  sti();
  sock->state = SS_CONNECTED;

  if (sk->state != TCP_ESTABLISHED && sk->err)
    {
      sock->state = SS_UNCONNECTED;
      return (-sk->err);
    }
  return (0);
}

static int
ip_proto_socketpair (struct socket *sock1, struct socket *sock2)
{
  return (-EOPNOTSUPP);
}

static int
ip_proto_accept (struct socket *sock, struct socket *newsock, int flags)
{
  volatile struct sock *sk1, *sk2;
  sk1= sock->data;
   if (sk1 == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  /* we've been passed an extra socket. We need to free it up because
   the tcp module creates it's own when it accepts one. */

  if (newsock->data)
    kfree_s (newsock->data, sizeof (struct sock));

  newsock->data = NULL;

  
if (sk1->prot->accept == NULL) return (-EOPNOTSUPP);

  /* restore the state if we have been interrupted, and
     then returned. */
  if (sk1->pair != NULL )
    {
      sk2 = sk1->pair;
      sk1->pair = NULL;
    }
  else
    {
      sk2 = sk1->prot->accept (sk1,flags);
      if (sk2 == NULL)
	{
	  if (sk1->err <= 0)
	    printk ("Warning sock.c:sk1->err <= 0.  Returning non-error.\n");
	  return (-sk1->err);
	}
    }
  newsock->data = (void *)sk2;
  sk2->sleep = (void *)newsock->wait;
  newsock->conn = NULL;
  if (flags & O_NONBLOCK)
    return (0);

  cli(); /* avoid the race. */
  while (sk2->state == TCP_SYN_RECV)
    {
      interruptible_sleep_on (sk2->sleep);
      if (current->signal & ~current->blocked)
	{
	   sti();
	   sk1->pair = sk2;
	   sk2->sleep = NULL;
	   newsock->data = NULL;
	   return (-ERESTARTSYS);
	}
    }
  sti();

  if (sk2->state != TCP_ESTABLISHED && sk2->err > 0)
    {
      int err;
      err = -sk2->err;
      destroy_sock (sk2);
      newsock->data = NULL;
      return (err);
    }
  newsock->state = SS_CONNECTED;
  return (0);
}

static int
ip_proto_getname(struct socket *sock, struct sockaddr *uaddr,
		 int *uaddr_len, int peer)
{
  struct sockaddr_in sin;
  volatile struct sock *sk;
  int len;
  len = get_fs_long(uaddr_len);
  /* check this error. */
  if (len < sizeof (sin)) return (-EINVAL);
  sin.sin_family=AF_INET;
  sk = sock->data;
  if (sk == NULL)
    {
       printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
       return (0);
    }
  if (peer)
    {
      if (!tcp_connected(sk->state))
	return (-ENOTCONN);
      sin.sin_port = sk->dummy_th.dest;
      sin.sin_addr.s_addr = sk->daddr;
      }
  else
    {
      sin.sin_port = sk->dummy_th.source;
      if (sk->saddr == 0)
	sin.sin_addr.s_addr = MY_IP_ADDR;
      else
	sin.sin_addr.s_addr = sk->saddr;
    }
  len = sizeof (sin);
  verify_area (VERIFY_WRITE, uaddr, len);
  memcpy_tofs(uaddr, &sin, sizeof (sin));
  verify_area(VERIFY_WRITE, uaddr_len, sizeof (len));
  put_fs_long (len, uaddr_len);
  return (0);
}

static int
ip_proto_read (struct socket *sock, char *ubuf, int size, int noblock)
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  return (sk->prot->read (sk, ubuf, size, noblock,0));
}

static int
ip_proto_recv (struct socket *sock, void *ubuf, int size, int noblock,
	       unsigned flags)
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  return (sk->prot->read (sk, ubuf, size, noblock, flags));
}

static int
ip_proto_write (struct socket *sock, char *ubuf, int size, int noblock)
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }
  if (sk->shutdown & SEND_SHUTDOWN)
    {
      send_sig (SIGPIPE, current, 1);
      return (-EPIPE);
    }

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  return (sk->prot->write (sk, ubuf, size, noblock, 0));
}


static int
ip_proto_send (struct socket *sock, void *ubuf, int size, int noblock, 
	       unsigned flags)
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }
  if (sk->shutdown & SEND_SHUTDOWN)
    {
      send_sig (SIGPIPE, current, 1);
      return (-EPIPE);
    }

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  return (sk->prot->write (sk, ubuf, size, noblock, flags));
}


static int
ip_proto_sendto (struct socket *sock, void *ubuf, int size, int noblock, 
		 unsigned flags, struct sockaddr *sin, int addr_len )
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }
  if (sk->shutdown & SEND_SHUTDOWN)
    {
      send_sig (SIGPIPE, current, 1);
      return (-EPIPE);
    }

  if (sk->prot->sendto == NULL) return (-EOPNOTSUPP);

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  return (sk->prot->sendto (sk, ubuf, size, noblock, flags, 
			    (struct sockaddr_in *)sin, addr_len));
}

static int
ip_proto_recvfrom (struct socket *sock, void *ubuf, int size, int noblock, 
		   unsigned flags, struct sockaddr *sin, int *addr_len )
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  if (sk->prot->recvfrom == NULL) return (-EOPNOTSUPP);

  /* we may need to bind the socket. */
  if (sk->num == 0)
    {
      sk->num = get_new_socknum (sk->prot, 0);
      if (sk->num == 0) return (-EAGAIN);
      put_sock (sk->num, sk);
      sk->dummy_th.source = net16(sk->num);
    }

  return (sk->prot->recvfrom (sk, ubuf, size, noblock, flags,
			      (struct sockaddr_in*)sin, addr_len));
}

static int
ip_proto_shutdown (struct socket *sock, int how)
{
	volatile struct sock *sk;
	/* this should really check to make sure the socket is
	   a tcp socket. */
	how++; /* maps 0->1 has the advantage of making bit 1 rcvs and
		       1->2 bit 2 snds.
		       2->3 */
	if (how & ~SHUTDOWN_MASK) return (-EINVAL);
	sk = sock->data;
	if (sk == NULL)
	  {
	     printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	     return (0);
	  }
	if (sock->state == SS_CONNECTING && sk->state == TCP_ESTABLISHED)
	  sock->state = SS_CONNECTED;

	if (!tcp_connected(sk->state)) return (-ENOTCONN);
	sk->shutdown |= how;
	if (sk->prot->shutdown)
	  sk->prot->shutdown (sk, how);
	return (0);
}

static int
ip_proto_select (struct socket *sock, int sel_type, select_table *wait )
{
  volatile struct sock *sk;
  sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  if (sk->prot->select == NULL)
    {
       PRINTK (("select on non-selectable socket. \n"));
       return (0);
    }
  return (sk->prot->select(sk, sel_type, wait));
}

/* these should be distributed to the different protocol routines. */
static int
ip_proto_ioctl (struct socket *sock, unsigned int cmd, 
		unsigned long arg)
{
   volatile struct sock *sk;
   sk = sock->data;
   if (sk == NULL)
     {
	printk ("Warning: sock->data = NULL: %d\n" ,__LINE__);
	return (0);
     }

  PRINTK (("in ip_proto_ioctl\n"));
  switch (cmd)
    {

      case IP_SET_DEV:
       if (!suser())
	 return (-EPERM);
       return (ip_set_dev((struct ip_config *)arg));

     case SIOCSARP:
       if (!suser())
         return (-EPERM);
       return (arp_ioctl_set((struct arpreq *)arg));

     case SIOCGARP:
       return (arp_ioctl_get((struct arpreq *)arg));

     case SIOCDARP:
       if (!suser())
         return (-EPERM);
       return (arp_ioctl_del((struct arpreq *)arg));

     case FIOSETOWN:
     case SIOCSPGRP:
        {
	 long user;
	 user = get_fs_long ((void *) arg);
	 sk->proc = user;
	 return (0);
       }

     case FIOGETOWN:
     case SIOCGPGRP:
       {
	 verify_area (VERIFY_WRITE, (void *)arg, sizeof (long));
	 put_fs_long (sk->proc, (void *)arg);
	 return (0);
       }

    default:
       if (!sk->prot->ioctl)
	 return (-EINVAL);
       return (sk->prot->ioctl (sk, cmd, arg));
    }
}

void *
sock_wmalloc(volatile struct sock *sk, unsigned long size, int force,
	     int priority)
{
  if (sk)
    {
       if (sk->wmem_alloc + size < SK_WMEM_MAX || force)
	 {
	   cli();
	   sk->wmem_alloc+= size;
	   sti();
	   return (kmalloc (size, priority));
	 }
       MPRINTK (("sock_wmalloc(%X,%d,%d,%d) returning NULL\n",
		sk, size, force, priority));
       return (NULL);
    }
  return (kmalloc(size, priority));
}

void *
sock_rmalloc(volatile struct sock *sk, unsigned long size, int force,
	     int priority)
{
   if (sk )
     {
	if (sk->rmem_alloc + size < SK_RMEM_MAX || force)
	  {
	    cli();
	    sk->rmem_alloc+= size;
	    sti();
	    return (kmalloc (size, priority));
	  }
	MPRINTK (("sock_rmalloc(%X,%d,%d,%d) returning NULL\n",
		 sk,size,force, priority));
	return (NULL);
      }
   return (kmalloc (size, priority));
}


unsigned long
sock_rspace (volatile struct sock *sk)
{
   int amt;
   if (sk != NULL)
     {
	if (sk->rmem_alloc >= SK_RMEM_MAX-2*MIN_WINDOW) return (0);
	amt = min ((SK_RMEM_MAX-sk->rmem_alloc)/2-MIN_WINDOW, MAX_WINDOW);
	if (amt < 0) return (0);
	return (amt);
     }
   return (0);
}

unsigned long
sock_wspace (volatile struct sock *sk)
{
  if (sk != NULL)
    {
       if (sk->shutdown & SEND_SHUTDOWN) return (0);
       if (sk->wmem_alloc >= SK_WMEM_MAX) return (0);
       return (SK_WMEM_MAX-sk->wmem_alloc );
    }
  return (0);
}


void
sock_wfree (volatile struct sock *sk, void *mem, unsigned long size)
{
   MPRINTK (("sock_wfree (sk=%X, mem=%X, size=%d)\n",sk, mem, size));
   kfree_s (mem, size);
   if (sk)
     {
	sk->wmem_alloc -= size;
	/* in case it might be waiting for more memory. */
	if (!sk->dead) wake_up(sk->sleep);
	if (sk->destroy && sk->wmem_alloc == 0 && sk->rmem_alloc == 0)
	  {
	     MPRINTK (("recovered lost memory, destroying sock = %X\n",sk));
	     delete_timer ((struct timer *)&sk->time_wait);
	     kfree_s ((void *)sk, sizeof (*sk));
	  }
	return;
     }
}

void
sock_rfree (volatile struct sock *sk, void *mem, unsigned long size)
{
   MPRINTK (("sock_rfree (sk=%X, mem=%X, size=%d)\n",sk, mem, size));
   kfree_s (mem, size);
   if (sk)
     {
	sk->rmem_alloc -= size;
	if (sk->destroy && sk->wmem_alloc == 0 && sk->rmem_alloc == 0)
	  {
	     delete_timer ((struct timer *)&sk->time_wait);
	     kfree_s ((void *)sk, sizeof (*sk));
	  }
     }
}


/* This routine must find a socket given a tcp header.  Everyhting
   is assumed to be in net order. */

volatile struct sock *get_sock (struct proto *prot, unsigned short num,
				unsigned long raddr,
				unsigned short rnum, unsigned long laddr)
{
  volatile struct sock *s;
  PRINTK (("get_sock (prot=%X, num=%d, raddr=%X, rnum=%d, laddr=%X)\n",
	  prot, num, raddr, rnum, laddr));

  /* SOCK_ARRAY_SIZE must be a power of two.  This will work better
     than a prime unless 3 or more sockets end up using the same
     array entry.  This should not be a problem because most
     well known sockets don't overlap that much, and for
     the other ones, we can just be careful about picking our
     socket number when we choose an arbitrary one. */

  for (s=prot->sock_array[num&(SOCK_ARRAY_SIZE-1)]; s != NULL; s=s->next)
    {
      if (s->num == num)
	{
	  /* we need to see if this is the socket that we want. */
	  if (!ip_addr_match (s->daddr, raddr))
	    continue;
	  if (s->dummy_th.dest != rnum && s->dummy_th.dest != 0)
	    continue;
	  if (!ip_addr_match (s->saddr, laddr))
	    continue;
	  return (s);
	}
    }
  return (NULL);
}

void release_sock (volatile struct sock *sk)
{
  if (!sk)
    {
      printk ("sock.c: release_sock sk == NULL\n");
      return;
    }

  if (!sk->prot)
    {
      printk ("sock.c: release_sock sk->prot == NULL\n");
      return;
    }

  if (sk->blog) return;
  /* see if we have any packets built up. */

  cli();
  sk->inuse = 1;
  while (sk->back_log != NULL)
    {
      struct sk_buff *skb;
      sk->blog = 1;
      skb = (struct sk_buff *)sk->back_log;
      PRINTK (("release_sock: skb = %X:\n",skb));
      if (skb->next != skb)
	{
	  sk->back_log = skb->next;
	  skb->prev->next = skb->next;
	  skb->next->prev = skb->prev;
	}
      else
	{
	  sk->back_log = NULL;
	}
      sti();
      PRINTK (("sk->back_log = %X\n",sk->back_log));
      if (sk->prot->rcv)
	sk->prot->rcv(skb, skb->dev, sk->opt,
		      skb->saddr, skb->len, skb->daddr, 1,
		      /* only used for/by raw sockets. */
		      (struct ip_protocol *)sk->pair); 
      cli();
    }
  sk->blog = 0;
  sk->inuse = 0;
  sti();
  if (sk->dead && sk->state == TCP_CLOSE)
    {
        /* should be about 2 rtt's */
       sk->time_wait.len = min (sk->rtt * 2, TCP_DONE_TIME);
       sk->timeout = TIME_DONE;
       reset_timer ((struct timer *)&sk->time_wait);
    }
}
