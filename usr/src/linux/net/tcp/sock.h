/* sock.h */
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
/* $Id: sock.h,v 0.8.4.7 1993/01/26 22:04:00 bir7 Exp $ */
/* $Log: sock.h,v $
 * Revision 0.8.4.7  1993/01/26  22:04:00  bir7
 * Added support for proc fs.
 *
 * Revision 0.8.4.6  1993/01/23  18:00:11  bir7
 * Added volatile keyword
 *
 * Revision 0.8.4.5  1992/12/12  01:50:49  bir7
 * Fixed support for half duplex connections.
 *
 * Revision 0.8.4.4  1992/12/06  23:29:59  bir7
 * Added mss and support for half completed packets.
 *
 * Revision 0.8.4.3  1992/12/03  19:54:12  bir7
 * Added paranoid queue checking.
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.4  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added Id and Log
 *
 */
#ifndef _TCP_SOCK_H
#define _TCP_SOCK_H

#define SOCK_ARRAY_SIZE 64

/* This structure really needs to be cleaned up.  Most of it is
   for tcp, and not used by any of the other protocols. */

struct sock 
{
  struct options *opt;
  volatile unsigned long wmem_alloc;
  volatile unsigned long rmem_alloc;
  unsigned long send_seq;
  unsigned long acked_seq;
  unsigned long copied_seq;
  unsigned long rcv_ack_seq;
  unsigned long window_seq;
  unsigned long fin_seq;
  /* not all are volatile, but some are, so we might as
     well say they all are. */
  volatile unsigned long  inuse:1, dead:1, urginline:1,
                intr:1, blog:1, done:1, reuse:1, keepopen:1, linger:1,
                delay_acks:1, timeout:3, destroy:1, ack_timed:1, no_check:1,
                exp_growth:1;
  int proc;
  volatile struct sock *next;
  volatile struct sock *pair;
  struct sk_buff *send_tail;
  struct sk_buff *send_head;
  volatile struct sk_buff * volatile back_log;
  struct sk_buff *send_tmp;
  long retransmits;
  struct sk_buff *wback, *wfront, *rqueue;
  struct proto *prot;
  struct wait_queue **sleep;
  unsigned long daddr;
  unsigned long saddr;
  unsigned short max_unacked;
  unsigned short window;
  unsigned short bytes_rcv;
  unsigned short mtu;
  unsigned short num;
  volatile unsigned short cong_window;
  volatile unsigned short packets_out;
  volatile unsigned short urg;
  volatile unsigned short shutdown;
  unsigned short mss;
  volatile short rtt;
  volatile short err;
  unsigned char protocol;
  volatile unsigned char state;
  volatile unsigned char ack_backlog;
  unsigned char max_ack_backlog;
  unsigned char priority;
  struct tcp_header dummy_th; /* I may be able to get rid of this. */
  volatile struct timer time_wait;
};

struct proto 
{
  void *(*wmalloc)(volatile struct sock *sk, unsigned long size, int force,
		   int priority);
  void *(*rmalloc)(volatile struct sock *sk, unsigned long size, int force,
		   int priority);
  void (*wfree)(volatile struct sock *sk, void *mem, unsigned long size);
  void (*rfree)(volatile struct sock *sk, void *mem, unsigned long size);
  unsigned long (*rspace)(volatile struct sock *sk);
  unsigned long (*wspace)(volatile struct sock *sk);
  void (*close)(volatile struct sock *sk, int timeout);
  int (*read)(volatile struct sock *sk, unsigned char *to, int len,
	      int nonblock, unsigned flags);
  int (*write)(volatile struct sock *sk, unsigned char *to, int len,
	       int nonblock, unsigned flags);
  int (*sendto) (volatile struct sock *sk, unsigned char *from, int len,
		 int noblock, unsigned flags, struct sockaddr_in *usin,
		 int addr_len);
  int (*recvfrom) (volatile struct sock *sk, unsigned char *from, int len,
		   int noblock, unsigned flags, struct sockaddr_in *usin,
		   int *addr_len);
  int (*build_header) (struct sk_buff *skb, unsigned long saddr,
		       unsigned long daddr, struct device **dev, int type,
		       struct options *opt, int len);
  int (*connect) (volatile struct sock *sk, struct sockaddr_in *usin,
		  int addr_len);
  volatile struct sock *(*accept) (volatile struct sock *sk, int flags);
  void  (*queue_xmit) (volatile struct sock *sk, struct device *dev, 
		       struct sk_buff *skb, int free);
  void (*retransmit) (volatile struct sock *sk, int all);
  void (*write_wakeup) (volatile struct sock *sk);
  void (*read_wakeup) (volatile struct sock *sk);
  int (*rcv)(struct sk_buff *buff, struct device *dev, struct options *opt,
	     unsigned long daddr, unsigned short len,
	     unsigned long saddr, int redo, struct ip_protocol *protocol);
  int (*select)(volatile struct sock *sk, int which, select_table *wait);
  int (*ioctl) (volatile struct sock *sk, int cmd, unsigned long arg);
  int (*init) (volatile struct sock *sk);
  void (*shutdown) (volatile struct sock *sk, int how);
  unsigned short max_header;
  unsigned long retransmits;
  volatile struct sock *sock_array[SOCK_ARRAY_SIZE];
  char name[80];
};

#define TIME_WRITE 1
#define TIME_CLOSE 2
#define TIME_KEEPOPEN 3
#define TIME_DESTROY 4
#define TIME_DONE 5 /* used to absorb those last few packets. */
#define SOCK_DESTROY_TIME 1000 /* about 10 seconds. */

/* used with free skb */
#define FREE_READ 1
#define FREE_WRITE 0

struct sk_buff
{
  volatile struct sk_buff *next;
  volatile struct sk_buff *prev;
  volatile struct sk_buff *link3;
  volatile struct sock *sk;
  volatile unsigned long when; /* used to compute rtt's. */
  struct device *dev;
  void *mem_addr;
  union
    {
       struct tcp_header *th;
       struct enet_header *eth;
       struct ip_header *iph;
       struct udp_header *uh;
       struct arp *arp;
       unsigned char *raw;
       unsigned long seq;
    } h;
  unsigned long mem_len;
  unsigned long len;
  unsigned long saddr;
  unsigned long daddr;
  int magic;
  volatile unsigned long acked:1,used:1,free:1,arp:1, urg_used:1, lock:1;
};

#define PROT_SOCK 1024
#define SK_WMEM_MAX 8192
#define SK_RMEM_MAX 32767
#define SHUTDOWN_MASK 3
#define RCV_SHUTDOWN 1
#define SEND_SHUTDOWN 2

void destroy_sock (volatile struct sock *sk);
unsigned short get_new_socknum (struct proto *, unsigned short);
void put_sock (unsigned short, volatile struct sock *); 
void release_sock (volatile struct sock *sk);
volatile struct sock *get_sock(struct proto *, unsigned short, unsigned long,
			       unsigned short, unsigned long);
void print_sk (volatile struct sock *);
void print_skb (struct sk_buff *);
void *sock_wmalloc(volatile struct sock *sk, unsigned long size, int force,
		   int priority);
void *sock_rmalloc(volatile struct sock *sk, unsigned long size, int force,
		   int priority);
void sock_wfree(volatile struct sock *sk, void *mem, unsigned long size);
void sock_rfree(volatile struct sock *sk, void *mem, unsigned long size);
unsigned long sock_rspace(volatile struct sock *sk);
unsigned long sock_wspace(volatile struct sock *sk);
void kfree_skb (struct sk_buff *skb, int rw);
void lock_skb (struct sk_buff *skb);
void unlock_skb (struct sk_buff *skb, int rw);

void dummy_routine(void *, ... );

#endif
