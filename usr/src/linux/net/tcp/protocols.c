/* protocols.c */

/* these headers are overkill, but until I clean up the socket header
   files, this is the best way. */

/* $Id: protocols.c,v 0.8.4.3 1992/11/15 14:55:30 bir7 Exp $ */
/* $Log: protocols.c,v $
 * Revision 0.8.4.3  1992/11/15  14:55:30  bir7
 * Remove ctrl-h so diff no longer thinks it's a binary file.
 *
 * Revision 0.8.4.2  1992/11/10  10:38:48  bir7
 * Change free_s to kfree_s and accidently changed free_skb to kfree_skb.
 *
 * Revision 0.8.4.1  1992/11/10  00:17:18  bir7
 * version change only.
 *
 * Revision 0.8.3.2  1992/11/10  00:14:47  bir7
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
#include "icmp.h"

int udp_rcv(struct sk_buff *skb, struct device *dev, struct options *opt,
	    unsigned long daddr, unsigned short len,
	    unsigned long saddr, int redo, struct ip_protocol *protocol);

void udp_err  (int err, unsigned char *header, unsigned long daddr,
	       unsigned long saddr, struct ip_protocol *protocol);


int tcp_rcv(struct sk_buff *skb, struct device *dev, struct options *opt,
	    unsigned long daddr, unsigned short len,
	    unsigned long saddr, int redo, struct ip_protocol *protocol);

void tcp_err  (int err, unsigned char *header, unsigned long daddr,
	       unsigned long saddr, struct ip_protocol *protocol);

int icmp_rcv(struct sk_buff *skb, struct device *dev, struct options *opt,
	     unsigned long daddr, unsigned short len,
	     unsigned long saddr, int redo, struct ip_protocol *protocol);


static struct ip_protocol tcp_protocol =
{
   tcp_rcv,
   tcp_err,
   NULL,
   IPPROTO_TCP,
   0, /* copy */
   NULL
};

static struct ip_protocol udp_protocol =
{
   udp_rcv,
   udp_err,
   &tcp_protocol,
   IPPROTO_UDP,
   0, /* copy */
   NULL
};

static struct ip_protocol icmp_protocol =
{
   icmp_rcv,
   NULL,
   &udp_protocol,
   IPPROTO_ICMP,
   0, /* copy */
   NULL
};

struct ip_protocol *ip_protocol_base = &icmp_protocol;
