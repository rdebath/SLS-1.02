/* Space.c */

/* Holds initial configuration information for devices. */
/* $Id: Space.c,v 0.8.4.7 1993/01/22 23:21:38 bir7 Exp $ */
/* $Log: Space.c,v $
 * Revision 0.8.4.7  1993/01/22  23:21:38  bir7
 * Merged with 99 pl4
 *
 * Revision 0.8.4.6  1993/01/22  22:58:08  bir7
 * *** empty log message ***
 *
 * Revision 0.8.4.5  1992/12/12  19:25:04  bir7
 * Cleaned up Log messages.
 *
 * Revision 0.8.4.4  1992/12/05  21:35:53  bir7
 * Updated dev->init type.
 *
 * Revision 0.8.4.3  1992/11/15  14:55:30  bir7
 * Removed ctrl-h so diff no longer thinks it's a binary file.
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

#include "dev.h"
#include <linux/stddef.h>

extern int wd8003_init(struct device *);
extern int loopback_init(struct device *dev);

static struct device wd8003_dev =
{
  "eth0",
  0xd2000,   /* recv memory end. */
  0xd0600,   /* recv memory start. */
  0xd2000,  /* memory end. */
  0xd0000,  /* memory start. */
  0x280,    /* base i/o address. */
  5,	    /* irq */
  0,0,0,0,0, /* flags */
  NULL, /* next device */
  wd8003_init,
  /* wd8003_init should set up the rest. */
  0,  /* trans start. */
  {NULL}, /* buffs */
  NULL, /* backlog */
  NULL, /* open */
  NULL, /* stop */
  NULL, /* hard_start_xmit */
  NULL, /* hard_header */
  NULL, /* add arp */
  NULL, /* queue xmit */
  NULL, /* rebuild header */
  NULL, /* type_trans */
  NULL, /* send_packet */
  NULL, /* private */
  0,    /* type. */
  0,    /* hard_header_len */
  0,    /* mtu */
  {0,}, /* broadcast address */
  {0,}, /* device address */
  0     /* addr len */
};

static struct device loopback_dev =
{
  "loopback",
  -1,       /* recv memory end. */
  0x0,      /* recv memory start. */
  -1,       /* memory end. */
  0,        /* memory start. */
  0,        /* base i/o address. */
  0,	    /* irq */
  0,0,1,0,0, /* flags */
  &wd8003_dev, /* next device */
  loopback_init,
  /* loopback_init should set up the rest. */
  0,  /* trans start. */
  {NULL}, /* buffs */
  NULL, /* backlog */
  NULL, /* open */
  NULL, /* stop */
  NULL, /* hard_start_xmit */
  NULL, /* hard_header */
  NULL, /* add arp */
  NULL, /* queue xmit */
  NULL, /* rebuild header */
  NULL, /* type_trans */
  NULL, /* send_packet */
  NULL, /* private */
  0,    /* type. */
  0,    /* hard_header_len */
  0,    /* mtu */
  {0,}, /* broadcast address */
  {0,}, /* device address */
  0     /* addr len */
};

struct device *dev_base = &loopback_dev;
