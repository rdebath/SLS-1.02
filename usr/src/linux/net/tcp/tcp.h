/* tcp.h */
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
/* $Id: tcp.h,v 0.8.4.7 1993/01/22 22:58:08 bir7 Exp $ */
/* $Log: tcp.h,v $
 * Revision 0.8.4.7  1993/01/22  22:58:08  bir7
 * Check in for merge with previous .99 pl 4.
 *
 * Revision 0.8.4.6  1992/12/12  19:25:04  bir7
 * Fixed anti-memory Leak in shutdown.
 *
 * Revision 0.8.4.5  1992/12/12  01:50:49  bir7
 * Fixed several bugs including half-duplex connections.
 *
 * Revision 0.8.4.4  1992/12/08  20:49:15  bir7
 * Fixed minor bugs and checked out MSS.
 *
 * Revision 0.8.4.3  1992/12/06  23:29:59  bir7
 * Added support for mss and half completed packets.  Also added
 * support for shrinking windows.
 *
 * Revision 0.8.4.2  1992/12/03  19:54:12  bir7
 * Added paranoid queue checking.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.2  1992/11/10  00:14:47  bir7
 * Changed malloc to kmalloc and added Id and Log
 *
 */

#ifndef _TCP_TCP_H
#define _TCP_TCP_H

struct tcp_header
{
  unsigned short source;
  unsigned short dest;
  unsigned long seq;
  unsigned long ack_seq;
  unsigned short res1:4, doff:4, fin:1, syn:1, rst:1, psh:1,
                 ack:1, urg:1,res2:2;
  unsigned short window;
  unsigned short check;
  unsigned short urg_ptr;
};

enum {
  TCP_ESTABLISHED=1,
  TCP_SYN_SENT,
  TCP_SYN_RECV,
#if 0
  TCP_CLOSING, /* not a valid state, just a seperator so we can use
		  < tcp_closing or > tcp_closing for checks. */
#endif
  TCP_FIN_WAIT1,
  TCP_FIN_WAIT2,
  TCP_TIME_WAIT,
  TCP_CLOSE,
  TCP_CLOSE_WAIT,
  TCP_LAST_ACK,
  TCP_LISTEN
};

#define MAX_SYN_SIZE 44 + sizeof (struct sk_buff) + MAX_HEADER
#define MAX_FIN_SIZE 40 + sizeof (struct sk_buff) + MAX_HEADER
#define MAX_ACK_SIZE 40 + sizeof (struct sk_buff) + MAX_HEADER
#define MAX_RESET_SIZE 40 + sizeof (struct sk_buff) + MAX_HEADER
#define MAX_WINDOW  12000
#define MIN_WINDOW   2048
#define MAX_ACK_BACKLOG 2
#define MIN_WRITE_SPACE 2048
#define TCP_WINDOW_DIFF 2048

#define TCP_RETR1      10       /* this is howmany retries it does
				   before it tries to figure out
				   if the gateway is down. */

#define TCP_RETR2      25	/* this should take at least
				   90 minutes to time out. */


#define TCP_TIMEOUT_LEN 720000 /* should be about 2 hrs. */
#define TCP_TIMEWAIT_LEN 6000 /* How long to wait to sucessfully 
				 close the socket, about 60 seconds. */
#define TCP_ACK_TIME 35 /* time to delay before sending an ack. */
#define TCP_DONE_TIME 250 /* maximum time to wait before actually destroying
			     a socket. */
#define TCP_WRITE_TIME 100 /* initial time to wait for an ack,
			      after last transmit. */
#define TCP_CONNECT_TIME 200 /* time to retransmit first syn. */
#define TCP_SYN_RETRIES 30 /* number of times to retry openning a connection.
			      */
#define TCP_PROBEWAIT_LEN 250 /* time to wait between probes when I've got
				 something to write and there is no window. */

#define TCP_NO_CHECK 0 /* turn to one if you want the default to be no
			  checksum . */

void print_th (struct tcp_header *);
#define HEADER_SIZE 64 /* Maximum header size we need to deal with. */

#define TCP_WRITE_QUEUE_MAGIC 0xa5f23477

 /* this next routines deal with comparing 32 bit unsigned ints and
    worry about wrap around. The general strategy is to do a normal
    compare so long as neither of the numbers is within 4k of wrapping.
    Otherwise we must check for the wrap. */

 static inline int
 before (unsigned long seq1, unsigned long seq2)
 {
   /* this inequality is strict. */
   if (seq1 == seq2) return (0);
   if (seq1 < seq2) 
     {
       if ((unsigned long)seq2-(unsigned long)seq1 < 32767UL) 
	 {
	   return (1);
	 }
       else
	 {
	   return (0);
	 }
     }
   /* now we know seq1 > seq2.  So all we need to do is check to see
      if seq1 has wrapped. */
   if (seq2 < 4096UL && seq1 > (0xffffffUL - 4096UL))
     {
       return (1);
     }
   return (0);

 }

 static inline int
 after (unsigned long seq1, unsigned long seq2)
 {
   return (before (seq2, seq1));
 }

 /* is s2<=s1<=s3 ? */
 static inline int
 between (unsigned long seq1, unsigned long seq2, unsigned long seq3)
 {
   return (after (seq1+1, seq2) && before (seq1, seq3+1));
 }

static inline const int
tcp_connected (const int state)
{
  return (state == TCP_ESTABLISHED || state == TCP_CLOSE_WAIT ||
 	  state == TCP_FIN_WAIT1   || state == TCP_FIN_WAIT2);
}

#endif
