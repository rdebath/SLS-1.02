/* plip.c: A parallel port "network" driver for linux. */
/*
    Written 1993 by Donald Becker.  This is unreleased software.
    
    This is parallel port packet pusher.  It's actually more general
    than the "IP" in its name suggests -- but 'plip' is just such a
    great name!

    The Author may be reached as becker@super.org or
    C/O Supercomputing Research Ctr., 17100 Science Dr., Bowie MD 20715
*/

static char *version =
    "plip.c:v0.04 Mar 19 1993 Donald Becker (becker@super.org)\n";

#include <linux/config.h>

/*
  Sources:
	Ideas and protocols came from Russ Nelson's (nelson@crynwr.com)
	"parallel.asm" parallel port packet driver.
  The "Crynwr" parallel port standard specifies the following protocol:
   send header nibble '8'
   count-low octet
   count-high octet
   ... data octets
   checksum octet
Each octet is sent as <wait for rx. '1'> <send 0x10+(octet&0x0F)>
			<wait for rx. '0'> <send 0x00+((octet>>4)&0x0F)>
The cable used is a de facto standard parallel null cable -- sold as
a "LapLink" cable by various places.  You'll need a 10-conductor cable to
make one yourself.  The wiring is:
    INIT	16 - 16		SLCTIN	17 - 17
    GROUND	25 - 25
    D0->ERROR	2 - 15		15 - 2
    D1->SLCT	3 - 13		13 - 3
    D2->PAPOUT	4 - 12		12 - 4
    D3->ACK	5 - 10		10 - 5
    D4->BUSY	6 - 11		11 - 6
  Do not connect the other pins.  They are
    D5,D6,D7 are 7,8,9
    STROBE is 1, FEED is 14
    extra grounds are 18,19,20,21,22,23,24
*/

#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/string.h>
#include <linux/ptrace.h>
#include <asm/system.h>
#include <asm/io.h>
#include <netinet/in.h>
#include <errno.h>

#include "dev.h"
#include "eth.h"
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include "arp.h"

/* use 0 for production, 1 for verification, >2 for debug */
#ifndef PLIP_DEBUG
#define PLIP_DEBUG 9
#endif
static unsigned int plip_debug = PLIP_DEBUG;

/* The map from IRQ number (as passed to the interrupt handler) to
   'struct device'. */
extern struct device *irq2dev_map[16];

#define PAR_DATA	0
#define PAR_STATUS	1
#define PAR_CONTROL	2
/* Common network statistics -- these will be in *.h someday. */
struct netstats {
    int tx_packets;
    int rx_packets;
    int tx_errors;
    int rx_errors;
    int missed_packets;
    int soft_tx_errors;
    int soft_rx_errors;
    int soft_trx_err_bits;
};
static struct netstats *localstats;

/* Index to functions, as function prototypes. */
extern int plip_probe(int ioaddr, struct device *dev);
/* Put in the device structure. */
static int plip_open(struct device *dev);
static int plip_close(struct device *dev);
static int plip_tx_packet(struct sk_buff *skb, struct device *dev);

/* Routines used internally. */
/* Dispatch from interrupts. */
static void plip_interrupt(int reg_ptr);
static int plip_write(struct device *dev, unsigned char *buf, int length);

int
plip_init(struct device *dev)
{
    int i;

    /* Alpha testers must have the version number to report bugs. */
    if (plip_debug > 1) {
	static int version_shown = 0;
	if (! version_shown)
	    printk(version), version_shown++;
    }

    /* We don't actually probe for anything here, although we might
       someday check to see if there's bi-directional port at
       dev->base_addr. */

    /* Initialize the device structure. */
    dev->private = kmalloc(sizeof(struct netstats), GFP_KERNEL);
    memset(dev->private, 0, sizeof(struct netstats));
    localstats = (struct netstats*) dev->private;

    for (i = 0; i < DEV_NUMBUFFS; i++)
	dev->buffs[i] = NULL;
    dev->hard_header = eth_hard_header;
    dev->add_arp = eth_add_arp;
    dev->queue_xmit = dev_queue_xmit;
    dev->rebuild_header = eth_rebuild_header;
    dev->type_trans = eth_type_trans;

    dev->open = &plip_open;
    dev->stop = &plip_close;
    dev->hard_start_xmit = &plip_tx_packet;

    /* These are ethernet specific. */
    dev->type = ETHER_TYPE;
    dev->hard_header_len = sizeof (struct enet_header);
    dev->mtu = 1500; /* eth_mtu */
    dev->addr_len = ETHER_ADDR_LEN;
    for (i = 0; i < dev->addr_len; i++) {
	dev->broadcast[i]=0xff;
	dev->dev_addr[i] = i;	/* The physical address is 0:1:2:3:4:5! */
    }
    printk("%s: using parallel port at %#3x, IRQ %d.\n", dev->name,
	   dev->base_addr, dev->irq);

    return 0;
}

/* Open/initialize the board.  This is called (in the current kernel)
   sometime after booting when the 'config <dev->name>' program is
   run.

   This routine gets exclusive access to the parallel port by allocating
   its IRQ line.
   */
static int
plip_open(struct device *dev)
{
    if (dev->irq == 0)
	dev->irq = 7;
    if (request_irq(dev->irq , &plip_interrupt) != 0) {
	if (plip_debug > 2)
	    printk("%s: couldn't get the IRQ.\n", dev->name);
	return EAGAIN;
    }

    irq2dev_map[dev->irq] = dev;
    outb(0x10, dev->base_addr + PAR_CONTROL);		/* Enable the rx interrupt. */
    dev->tbusy = 0;		/* Transmit busy...  */
    dev->interrupt = 0;
    dev->start = 1;
    return 0;
}

/* The inverse routine to plip_open(). */
static int
plip_close(struct device *dev)
{
    dev->start = 0;
    free_irq(dev->irq);
    irq2dev_map[dev->irq] = NULL;
    outb(0x00, dev->base_addr);		/* Release the interrupt. */
    return 0;
}

static int
plip_tx_packet(struct sk_buff *skb, struct device *dev)
{
    int ret_val;
    /* If some higher layer thinks we've missed an tx-done interrupt
       we are passed NULL. Caution: dev_tint() handles the cli()/sti()
       itself. */
    if (skb == NULL) {
	dev_tint(dev);
	return 0;
    }

    /* Pretend we are an ethernet and fill in the header.  This could use
       a simplified routine someday. */
    if (!skb->arp  &&  dev->rebuild_header(skb+1, dev)) {
	skb->dev = dev;
	arp_queue (skb);
	return 0;
    }

    dev->trans_start = jiffies;
    ret_val = plip_write(dev, (void*)(skb+1), skb->len);
    if (skb->free)
	kfree_skb (skb, FREE_WRITE);
    dev->tbusy = 0;
    mark_bh (INET_BH);
    return ret_val;
}


static inline int get_byte(struct device *dev)
{
    unsigned char val;
    unsigned char low_nibble;
    int boguscount = 1500;
    do {
	val = inb(dev->base_addr + PAR_STATUS);
    } while ( ! (val & 0x80) && --boguscount > 0);
    low_nibble = (val >> 3) & 0x0f;
    if (plip_debug > 8)
	printk("%1x", low_nibble);
    outb(0x10, dev->base_addr + PAR_DATA);
    do {
	val = inb(dev->base_addr + PAR_STATUS);
    } while ((val & 0x80)  &&  --boguscount > 0);
    if (plip_debug > 8)
	printk("%1x %s", low_nibble,
	       boguscount <= 0 ? "timeout":"");
    outb(0x00, dev->base_addr + PAR_DATA);
    return low_nibble | ((val << 1) & 0xf0);
}

/* The typical workload of the driver:
   Handle the parallel port interrupts. */
static void
plip_interrupt(int reg_ptr)
{
    int irq = -(((struct pt_regs *)reg_ptr)->orig_eax+2);
    struct device *dev = irq2dev_map[irq];
    int boguscount = 1500;
    unsigned length;
    int sksize;
    struct sk_buff *skb;

    if (dev == NULL) {
	printk ("plip_interrupt(): irq %d for unknown device.\n", irq);
	return;
    }
    dev->interrupt = 1;
    outb(0x00, dev->base_addr + PAR_CONTROL);		/* Disable the rx interrupt. */
    sti(); /* Allow other interrupts. */
    
    if (plip_debug >= 4)
	printk("%s: interrupt.\n", dev->name);
    
    localstats = (struct netstats*) dev->private;
    
    /* Receive the packet here. */
    if (inb(dev->base_addr + PAR_STATUS) != 0xc7) {
	localstats->rx_errors++;			/* No interrupt! */
	if (plip_debug > 4)
	    printk("%s: No interrupt (status=%#02x)!\n",
		   dev->name, inb(dev->base_addr + PAR_STATUS));
	return;
    }
    outb(1, dev->base_addr + PAR_DATA);		/* Ack: 'Ready' */
    length = get_byte(dev);
    length |= (get_byte(dev) << 8);
    if (length > dev->mtu) {
	printk("%s: Bogus packet size %d, dropping it.\n", dev->name, length);
	return;
    }
    boguscount = length << 5;
    sksize = sizeof(struct sk_buff) + length;
    skb = kmalloc(sksize, GFP_ATOMIC);
    if (skb == NULL) {
	if (plip_debug)
	    printk("%s: Couldn't allocate a sk_buff of size %d.\n",
		   dev->name, sksize);
	localstats->rx_errors++;
	return;
    }
    skb->lock = 0;
    skb->mem_len = sksize;
    skb->mem_addr = skb;
    {
	/* 'skb+1' points to the start of sk_buff data area. */
	unsigned char *buf = (void*) (skb+1);
	int checksum = 0;

	while (length--) {
	    unsigned char new_byte = get_byte(dev);
	    checksum += new_byte, *buf++ = new_byte;
	}
	if (checksum != get_byte(dev))
	    localstats->soft_rx_errors++;
	else if(dev_rint((void *)skb, length, IN_SKBUFF, dev)) {
	    printk("%s: receive buffers full.\n", dev->name);
	    localstats->rx_errors++;
	    return;
	}
    }
    /* Wait for the remote end to reset. */
    while (inb(dev->base_addr + PAR_STATUS) != 0x87)
	if (boguscount-- <= 0 )
	    break;
    outb(0x00, dev->base_addr + PAR_DATA);
    outb(0x10, dev->base_addr + PAR_CONTROL);		/* Enable the rx interrupt. */
    localstats->rx_packets++;
    return;
}

    
static inline int send_byte(struct device *dev, unsigned char val)
{
    int boguscount = 1500;
    if (plip_debug > 8)
	printk("send(%02x) ", val);
    outb(0x10 | val, dev->base_addr);
    while(inb(dev->base_addr+PAR_STATUS) & 0x80)
	if (--boguscount <= 0) break;
    outb(val >> 4, dev->base_addr);
    while((inb(dev->base_addr+PAR_STATUS) & 0x80) == 0)
	if (--boguscount <= 0) break;
    if (plip_debug > 4 && boguscount <= 0)
	printk("timeout");
}
static int
plip_write(struct device *dev, unsigned char *buf, int length)
{
    int timeout = 1000;			/* Approx 1 ms. */
    char checksum = 0;
    int i;
    if (plip_debug > 5)
	printk("%s: plip_write(%d) %02x %02x %02x %02x %02x...",
	       dev->name, length, buf[0], buf[1], buf[2], buf[3], buf[4]);
    if (length > dev->mtu) {
	printk("%s: packet too big, %d.\n", dev->name, length);
	return 1;
    }
    /* This starts the packet protocol by triggering a remote IRQ. */
    outb(0x00, dev->base_addr + PAR_CONTROL);	/* Disable my rx interrupt. */
    outb(0x08, dev->base_addr + PAR_DATA); 	/* Trigger remote rx interrupt. */
    while((inb(dev->base_addr+PAR_STATUS) & 0x08) == 0 )
	if (--timeout < 0) {
	    outb(0x00, dev->base_addr);
	    localstats->tx_errors++;
	    if (plip_debug > 3)
		printk("%s: Connect failed during send_packet() (length=%d).\n",
		       dev->name, length);
	    /* We failed to send the packet.  To emulate the ethernet we
	       should pretent the send worked fine, but we don't right now. */
	    return 1;			/* Failed to send the packet! */
	}
    send_byte(dev, length); send_byte(dev, length >> 8);
    for (i = 0; i < length; i++)
	checksum += buf[i], send_byte(dev, buf[i]);
    send_byte(dev, checksum);
    outb(0x00, dev->base_addr);
    outb(0x10, dev->base_addr + PAR_CONTROL);	/* Enable the rx interrupt. */
    localstats->tx_packets++;
    if (plip_debug > 5)
	printk("plip_write(%d) done.\n", length);
    return 0;
}

/*
 * Local variables:
 *  compile-command: "gcc -DKERNEL -Wall -O6 -fomit-frame-pointer -I/usr/src/linux/net/tcp -c plip.c"
 *  version-control: t
 *  kept-new-versions: 5
 * End:
 */
