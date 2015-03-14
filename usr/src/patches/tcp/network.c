/* network.c: A sample network driver core for linux. */
/*
    Written 1993 by Donald Becker.
    
    The Author may be reached as becker@super.org or
    C/O Supercomputing Research Ctr., 17100 Science Dr., Bowie MD 20715
*/

static char *version =
    "network.c:v0.01 Donald Becker (becker@super.org)\n";

/* Always include 'config.h' first in case the user wants to turn on
   or override something. */
#include <linux/config.h>

/*
  Sources:
	List your sources of programming information to document that
	the driver is your own creation, and give due credit to others
	that contributed to the work.  Remember that GNU project code
	cannot use proprietary or trade secret information.  Interface
	definitions are generally considered non-copyrightable to the
	extent that the same names and structures must be used to be
	compatible. */

#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>
#include <linux/ptrace.h>
#include <asm/system.h>
#include <asm/io.h>
#include <netinet/in.h>

#include "dev.h"
#include "eth.h"
#include "timer.h"
#include "ip.h"
#include "tcp.h"
#include "sock.h"
#include "arp.h"

/* use 0 for production, 1 for verification, >2 for debug */
#ifndef NET_DEBUG
#define NET_DEBUG 2
#endif
static unsigned int net_debug = NET_DEBUG;

/* The map from IRQ number (as passed to the interrupt handler) to
   'struct device'. */
extern struct device *irq2dev_map[16];

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
/* Put in the device structure. */
static int net_open(struct device *dev);
static void net_send_packet(struct sk_buff *skb, struct device *dev);
/* Dispatch from interrupts. */
static void net_interrupt(int reg_ptr);
static void net_tx_intr(struct device *dev);
static void net_rx_intr(struct device *dev);
/* Routines used internally. */
static void chipset_init(struct device *dev, int startp);
static void trigger_send(struct device *dev, unsigned int length,
				int start_page);
extern int netcard_probe(int ioaddr, struct device *dev);


/* Open/initialize the board.  This is called (in the current kernel)
   sometime after booting when the 'config <dev->name>' program is
   run.

   This routine should set everything up anew at each open, even
   registers that "should" only need to be set once at boot, so that
   there is non-reboot way to recover if something goes wrong.
   */
static int
net_open(struct device *dev)
{
  if ( ! net_status.exists) {
      printk("%s: Opening a non-existent physical device\n",
	     dev ? dev->name : "(null)");
      return ENXIO;		/* Anything non-zero will do. */
  }

  chipset_init(dev, 1);

  /* If the IRQ line can be software selected find a free line to use. */
  if (dev->irq < 2) {
      int irq_list[] = { 11, 10, 5, 3, 4, 7, 9, 0};
      int *irq = irq_list;
      for (; *irq; irq++) {
	  if (request_irq(dev->irq = *irq, &net_interrupt) == 0)
	      break;
      }
      if (*irq == 0) {
	  printk (" unable to get an IRQ.\n");
	  return EAGAIN;
      }
      card_set_irq(dev, *irq);
  }

  irq2dev_map[irq] = dev;
  dev->tbusy = 0;		/* Transmit busy...  */
  dev->interrupt = 0;
  dev->start = 1;
  return 0;
}

/* The inverse routine to net_open(). */
static int
net_close(struct device *dev)
{
    dev->start = 0;
    card_flush_tx(dev);
    card_disable_reciever(dev);
    irq2dev_map[dev->irq] = NULL;
    /* If not IRQ jumpered, free up the interrupt line. */
    card_set_irq(0);
    free_irq(dev->irq);
    return 0;
}

static int
net_start_xmit(struct sk_buff *skb, struct device *dev)
{

    if (dev->tbusy) {	/* Do timeouts, to avoid hangs. */
	int tickssofar = jiffies - dev->trans_start;
	if (tickssofar < 5)
	    return 1;
	printk("%s: transmit timed out, %s?\n", dev->name,
	       tx_done(dev) ? "IRQ conflict" : "network cable problem");
	/* Try to restart the adaptor. */
	chipset_init(dev, 1);
    }

    /* If some higher layer thinks we've missed an tx-done interrupt
       we are passed NULL. Caution: dev_tint() handles the cli()/sti()
       itself. */
    if (skb == NULL) {
	dev_tint(dev);
	return 0;
    }

    /* For ethernet, fill in the header. */
    if (!skb->arp  &&  dev->rebuild_header(skb+1, dev)) {
	skb->dev = dev;
	arp_queue (skb);
	return 0;
    }

    dev->trans_start = jiffies;
    cli();
    net_send_packet(skb, dev);
    sti();
    if (skb->free)
	kfree_skb (skb, FREE_WRITE);
    return 0;
}

/* The typical workload of the driver:
   Handle the network interface interrupts. */
static void
net_interrupt(int reg_ptr)
{
    int irq = -(((struct pt_regs *)reg_ptr)->orig_eax+2);
    struct device *dev = irq2dev_map[irq];
    int interrupts, boguscount = 0;

    if (dev == NULL) {
	printk ("net_interrupt(): irq %d for unknown device.\n", irq);
	return;
    }
    dev->interrupt = 1;
    sti(); /* Allow other interrupts. */

    if (net_debug >= 4)
	printk("%s: interrupt.\n", dev->name);

    localstats = (struct netstats*) dev->private;

    do {
	interrupts = chip_read_status(dev);
	if (interrupts & OVERRUN_INTR) {
	    localstats->missed_packets++;
	    net_rx_overrun(dev);
	} else if (interrupts & RX_INTR) {
	    /* Got a good (?) packet. */
	    net_receive(dev);
	    localstats->rx_packets++;
	}
	if (interrupts & TX_INTR) {
	    /* Push the next to-transmit packet through. */
	    net_tx_intr(dev);
	} else if (interrupts & COUNTERS_INTR) {
	    /* Increment the appropriate 'localstats' field. */
	}
    } while (++boguscount < 20) ;

    if (interrupts && net_debug) {
      printk("%s: unknown interrupt %#2x\n", dev->name, interrupts);
    }
    return;
}

/* We have finished a transmit: check for errors and then mark that
   the bottom half has some work to do. */
static void
net_tx_intr(struct device *dev)
{
    int status = card_get_status(dev);
    if (status & TX_OK)
	tx_packets++;
    else {
	tx_errors++;
	card_reset(dev);
    }
    dev->tbusy = 0;
    mark_bh (INET_BH);
}

/* We have a good packet(s), get it/them out of the buffers. */
static void
net_receive(struct device *dev)
{
    int boguscount = 0;

    do {
	int size = chip_get_rx_size(dev);
	int sksize;
	struct sk_buff *skb;
      
	if (size == 0)		/* Read all the frames? */
	    break;			/* Done for now */
      
	if ((size < 32  ||  size > dev->mtu) && net_debug)
	    printk("%s: Bogus packet size %d.\n", dev->name, size);
      
	sksize = sizeof(struct sk_buff) + size;
	skb = kmalloc(sksize, GFP_ATOMIC);
	if (skb != NULL) {
	    skb->lock = 0;
	    skb->mem_len = sksize;
	    skb->mem_addr = skb;
	    /* 'skb+1' points to the start of sk_buff data area. */
	    card_get_packet(dev, size, (void *)(skb+1));
	    if(dev_rint((void *)skb, size, IN_SKBUFF, dev)) {
		printk("%s: receive buffers full.\n", dev->name);
		break;
	    }
	} else if (net_debug) {
	    printk("%s: Couldn't allocate a sk_buff of size %d.\n",
		   dev->name, sksize);
	    break;
	}
	rx_packets++;
    } while (++boguscount < 10);

    /* If any worth-while packets have been received, dev_rint()
       has done a mark_bh(INET_BH) for us and will work on them
       when we get to the bottom-half routine. */
    return;
}

/* We have a receiver overrun: do whatever this chipset needs
 to recover. */
static void
net_rx_overrun(struct device *dev)
{
    int reset_start_time = jiffies;
    if (net_debug)
	printk("%s: Receiver overrun.\n", dev->name);
    return;
}

int
netdev_init(struct device *dev)
{
    int i, found = 0;
    int *port, ports[] = {0x300, 0x320, 0x340, 0x280, 0x2C0, 0x200, 0x240, 0};

    /* Alpha testers must have the version number to report bugs. */
    if (net_debug > 1)
	printk(version);

    /*  Probe for an adaptor at a likely set of locations. */
    if (dev->base_addr > 0x100) {
	if (card_probe_addr(dev, ioaddr) == 0) {
	    printk("%s: probe failed at %#3x.\n", dev->name, ioaddr);
	    return ENODEV;
	}
    } else {
	for (port = &ports[0]; *port; port++)
	    if (card_probe_addr(dev, *port))
		break;
	if (*port == 0) {
	    dev->open = NULL;
	    printk("No network device found.\n");
	    return ENODEV;
	}
    }

    /* Initialize the device structure. */
    dev->private = kmalloc(sizeof(struct netstats), GFP_KERNEL);
    memset(dev->private, 0, sizeof(struct netstats));
    for (i = 0; i < DEV_NUMBUFFS; i++)
	dev->buffs[i] = NULL;
    dev->hard_header = eth_hard_header;
    dev->add_arp = eth_add_arp;
    dev->queue_xmit = dev_queue_xmit;
    dev->rebuild_header = eth_rebuild_header;
    dev->type_trans = eth_type_trans;

    dev->open = &net_open;
    dev->stop = &net_close;
    dev->hard_start_xmit = &net_start_xmit;

    /* These are ethernet specific. */
    dev->type = ETHER_TYPE;
    dev->hard_header_len = sizeof (struct enet_header);
    dev->mtu = 1500; /* eth_mtu */
    dev->addr_len = ETHER_ADDR_LEN;
    for (i = 0; i < dev->addr_len; i++) {
	dev->broadcast[i]=0xff;
    }
    card_get_station_address(dev->dev_addr);

#ifdef jumpered_interrupts
  /* If this board has jumpered interrupts, snarf the interrupt vector
     now.  There is no point in waiting since no other device can use
     the interrupt, and this marks the 'irqaction' as busy. */

  if (dev->irq == -1)
      ;			/* Do nothing: a user-level program will set it. */
  else if (dev->irq < 2) {	/* "Auto-IRQ" */
      autoirq_setup(0);
      /* Trigger an interrupt here. */
      dev->irq = autoirq_report(0);
      if (net_debug >= 2)
	  printk(" autoirq is %d", dev->irq);
  } else if (dev->irq == 2)
      /* Fixup for users that don't know that IRQ 2 is really IRQ 9,
	 or don't know which one to set. */
      dev->irq = 9;

  {    int irqval = request_irq(dev->irq, &net_interrupt);
       if (irqval) {
	   printk ("%s: unable to get IRQ %d (irqval=%d).\n", dev->name,
		   dev->irq, irqval);
	   return 0;
       }
   }
#endif  /* jumpered interrupt */
    printk("%s: %s found at %#3x, IRQ %d.\n", dev->name,
	   "network card", dev->base_addr, dev->irq);

    return 0;
}

/* Check IOADDR for a network adaptor of this type, and return
   a non-zero value iff one exists. */
static int
card_probe_addr(struct device *dev, int ioaddr)
{
  if (inb_p(ioaddr) == 0xFF) {	/* Trivial check. */
      return 0;
  }

  /* Check for the card here.  You should minimize outb()s, and
     restore the previous value if possible. */
  if (/*probe result */1 == 0)
      return 0;

  return dev->base_addr = ioaddr;
}

/*
 * Local variables:
 *  compile-command: "gcc -DKERNEL -Wall -O6 -fomit-frame-pointer -I/usr/src/linux/net/tcp -c network.c"
 *  version-control: t
 *  kept-new-versions: 5
 * End:
 */
