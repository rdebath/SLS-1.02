/* $Id: aha1542.c,v 1.1 1992/07/24 06:27:38 root Exp root $
 *  linux/kernel/aha1542.c
 *
 *  Copyright (C) 1992  Tommy Thorn
 *
 *  Modified by Eric Youngdale
 *        Use request_irq and request_dma to help prevent unexpected conflicts
 *        Set up on-board DMA controller, such that we do not have to
 *        have the bios enabled to use the aha1542.
 */

#include <linux/kernel.h>
#include <linux/head.h>
#include <linux/types.h>
#include <linux/string.h>

#include <linux/sched.h>
#include <asm/dma.h>

#include <asm/system.h>
#include <asm/io.h>
#include "../blk.h"
#include "scsi.h"
#include "hosts.h"

#include "aha1542.h"

#ifdef DEBUG
#define DEB(x) x
#else
#define DEB(x)
#endif
/*
static const char RCSid[] = "$Header: /usr/src/linux/kernel/blk_drv/scsi/RCS/aha1542.c,v 1.1 1992/07/24 06:27:38 root Exp root $";
*/

/* The adaptec can be configured for quite a number of addresses, but
I generally do not want the card poking around at random.  We allow
two addresses - this allows people to use the Adaptec with a Midi
card, which also used 0x330 */

static unsigned int bases[]={0x330, 0x334};

static unsigned int base;
static unsigned char dma_chan;
static unsigned char irq_level;

static int aha_disable = 0;

/* The DMA-Controller.  We need to fool with this because we want to 
   be able to use the aha1542 without having to have the bios enabled */
#define DMA_MODE_REG	0xd6
#define DMA_MASK_REG	0xd4
#define	CASCADE		0xc0

static struct mailbox mb[2*AHA1542_MAILBOXES];
static struct ccb ccb[AHA1542_MAILBOXES];

static Scsi_Cmnd * SCint[AHA1542_MAILBOXES] = {NULL, };

/* This will effectively start both of them at the first mailbox */
static int aha1542_last_mbi_used  = (2*AHA1542_MAILBOXES - 1);
static int aha1542_last_mbo_used  = (AHA1542_MAILBOXES - 1);


static long WAITnexttimeout = 3000000;

static int aha1542_host = 0;
static void setup_mailboxes(void);

#define aha1542_intr_reset()  outb(IRST, CONTROL)

#define WAIT(port, mask, allof, noneof)					\
 { register WAITbits;							\
   register WAITtimeout = WAITnexttimeout;				\
   while (1) {								\
     WAITbits = inb(port) & (mask);					\
     if ((WAITbits & (allof)) == (allof) && ((WAITbits & (noneof)) == 0)) \
       break;                                                         	\
     if (--WAITtimeout == 0) goto fail;					\
   }									\
 }

static void aha1542_stat(void)
{
/*    int s = inb(STATUS), i = inb(INTRFLAGS);
  printk("status=%x intrflags=%x\n", s, i, WAITnexttimeout-WAITtimeout); */
}

static int aha1542_out(unchar *cmdp, int len)
{
    while (len--)
      {
	  WAIT(STATUS, CDF, 0, CDF);
	  outb(*cmdp++, DATA);
      }
    return 0;
  fail:
    printk("aha1542_out failed(%d): ", len+1); aha1542_stat();
    return 1;
}

static int aha1542_in(unchar *cmdp, int len)
{
    while (len--)
      {
	  WAIT(STATUS, DF, DF, 0);
	  *cmdp++ = inb(DATA);
      }
    return 0;
  fail:
    printk("aha1542_in failed(%d): ", len+1); aha1542_stat();
    return 1;
}
int makecode(unsigned hosterr, unsigned scsierr)
{
    switch (hosterr) {
      case 0x0:
      case 0xa: /* Linked command complete without error and linked normally */
      case 0xb: /* Linked command complete without error, interrupt generated */
	hosterr = 0;
	break;

      case 0x11: /* Selection time out-The initiator selection or target
		    reselection was not complete within the SCSI Time out period */
	hosterr = DID_TIME_OUT;
	break;

      case 0x12: /* Data overrun/underrun-The target attempted to transfer more data
		    thean was allocated by the Data Length field or the sum of the
		    Scatter / Gather Data Length fields. */

      case 0x13: /* Unexpected bus free-The target dropped the SCSI BSY at an unexpected time. */

      case 0x15: /* MBO command was not 00, 01 or 02-The first byte of the CB was
		    invalid. This usually indicates a software failure. */

      case 0x16: /* Invalid CCB Operation Code-The first byte of the CCB was invalid.
		    This usually indicates a software failure. */

      case 0x17: /* Linked CCB does not have the same LUN-A subsequent CCB of a set
		    of linked CCB's does not specify the same logical unit number as
		    the first. */
      case 0x18: /* Invalid Target Direction received from Host-The direction of a
		    Target Mode CCB was invalid. */

      case 0x19: /* Duplicate CCB Received in Target Mode-More than once CCB was
		    received to service data transfer between the same target LUN
		    and initiator SCSI ID in the same direction. */

      case 0x1a: /* Invalid CCB or Segment List Parameter-A segment list with a zero
		    length segment or invalid segment list boundaries was received.
		    A CCB parameter was invalid. */
	hosterr = DID_ERROR; /* Couldn't find any better */
	break;

      case 0x14: /* Target bus phase sequence failure-An invalid bus phase or bus
		    phase sequence was requested by the target. The host adapter
		    will generate a SCSI Reset Condition, notifying the host with
		    a SCRD interrupt */
	hosterr = DID_RESET;
	break;
      default:
	printk("makecode: unknown hoststatus %x\n", hosterr);
	break;
    }
    return scsierr|(hosterr << 16);
}

int aha1542_test_port(int bse)
{
    volatile int debug = 0;
    
    base = bse;
    /* Reset the adapter. I ought to make a hard reset, but it's not really nessesary */
    
    /*  DEB(printk("aha1542_test_port called \n")); */
    
    outb(SRST|IRST/*|SCRST*/, CONTROL);
    
    debug = 1;
    /* Expect INIT and IDLE, any of the others are bad */
    WAIT(STATUS, STATMASK, INIT|IDLE, STST|DIAGF|INVDCMD|DF|CDF);
    
    debug = 2;
    /* Shouldn't have generated any interrupts during reset */
    if (inb(INTRFLAGS)&INTRMASK) goto fail;
    setup_mailboxes();
    
    debug = 3;
    /* Test the basic ECHO command */
    outb(CMD_ECHO, DATA);
    
    debug = 4;
    /* Wait for CDF=0. If any of the others are set, it's bad */
    WAIT(STATUS, STATMASK, 0, STST|DIAGF|INVDCMD|DF|CDF);
    
    debug = 5;
    /* The meaning of life */
    outb(42, DATA);
    
    debug = 6;
    /* Expect only DF, that is, data ready */
    WAIT(STATUS, STATMASK, DF, STST|DIAGF|CDF|INVDCMD);
    
    debug = 7;
    /* Is the answer correct? */
    if (inb(DATA) != 42) goto fail;
    
    debug = 8;
    /* Reading port should reset DF */
    if (inb(STATUS) & DF) goto fail;
    
    debug = 9;
    /* When HACC, command is completed, and we're though testing */
    WAIT(INTRFLAGS, HACC, HACC, 0);
    /* now initialize adapter */
    
    debug = 10;
    /* Clear interrupts */
    outb(IRST, CONTROL);
    
    debug = 11;
    
    return debug;				/* 1 = ok */
  fail:
    return 0;					/* 0 = not ok */
}

/* What's this little function for? */
const char *aha1542_info(void)
{
    static char buffer[] = "Adaptec 1542";
    return buffer;
}

/* A "high" level interrupt handler */
void aha1542_intr_handle(int foo)
{
    void (*my_done)(Scsi_Cmnd *) = NULL;
    int errstatus, mbi, mbo, mbistatus;
    int number_serviced;
    Scsi_Cmnd * SCtmp;

#ifdef DEBUG
    {
    int flag = inb(INTRFLAGS);
    printk("aha1542_intr_handle: ");
    if (!(flag&ANYINTR)) printk("no interrupt?");
    if (flag&MBIF) printk("MBIF ");
    if (flag&MBOA) printk("MBOF ");
    if (flag&HACC) printk("HACC ");
    if (flag&SCRD) printk("SCRD ");
    printk("status %02x\n", inb(STATUS));
  };
#endif
    number_serviced = 0;

    while(1==1){
      aha1542_intr_reset();

      cli();
      mbi = aha1542_last_mbi_used + 1;
      if (mbi >= 2*AHA1542_MAILBOXES) mbi = AHA1542_MAILBOXES;
      
      do{
	if(mb[mbi].status != 0) break;
	mbi++;
	if (mbi >= 2*AHA1542_MAILBOXES) mbi = AHA1542_MAILBOXES;
      } while (mbi != aha1542_last_mbi_used);
      
      if(mb[mbi].status == 0){
	sti();
	/* Hmm, no mail.  Must have read it the last time around */
	if (number_serviced) return;
	printk("aha1542.c: interrupt received, but no mail.\n");
	return;
      };

      mbo = (scsi2int(mb[mbi].ccbptr) - ((unsigned int) &ccb[0])) / sizeof(struct ccb);
      mbistatus = mb[mbi].status;
      mb[mbi].status = 0;
      aha1542_last_mbi_used = mbi;
      sti();
      
#ifdef DEBUG
      {
	if (ccb[mbo].tarstat|ccb[mbo].hastat)
	  printk("aha1542_command: returning %x (status %d)\n", 
		 ccb[mbo].tarstat + ((int) ccb[mbo].hastat << 16), mb[mbi].status);
      };
#endif

      if(mbistatus == 3) continue; /* Aborted command not found */

#ifdef DEBUG
      printk("...done %d %d\n",mbo, mbi);
#endif
      
      SCtmp = SCint[mbo];

      if (!SCtmp || !SCtmp->scsi_done) {
	printk("aha1542_intr_handle: Unexpected interrupt\n");
	return;
      }
      
      my_done = SCtmp->scsi_done;
      if (SCtmp->host_scribble) scsi_free(SCtmp->host_scribble, 512);
      
      /* Fetch the sense data, and tuck it away, in the required slot.  The
	 Adaptec automatically fetches it, and there is no guarantee that
	 we will still have it in the cdb when we come back */
      if (ccb[mbo].tarstat == 2)
	memcpy(SCtmp->sense_buffer, &ccb[mbo].cdb[ccb[mbo].cdblen], 
	       sizeof(SCtmp->sense_buffer));
      
      
      /* is there mail :-) */
      
      /* more error checking left out here */
      if (mbistatus != 1)
	/* This is surely wrong, but I don't know what's right */
	errstatus = makecode(ccb[mbo].hastat, ccb[mbo].tarstat);
      else
	errstatus = 0;
      
#ifdef DEBUG
      if(errstatus) printk("(aha1542 error:%x %x %x) ",errstatus, 
			   ccb[mbo].hastat, ccb[mbo].tarstat);
#endif

      if (ccb[mbo].tarstat == 2) {
#ifdef DEBUG
	int i;
#endif
	DEB(printk("aha1542_intr_handle: sense:"));
#ifdef DEBUG
	for (i = 0; i < 12; i++)
	  printk("%02x ", ccb[mbo].cdb[ccb[mbo].cdblen+i]);
	printk("\n");
#endif
	/*
	  DEB(printk("aha1542_intr_handle: buf:"));
	  for (i = 0; i < bufflen; i++)
	  printk("%02x ", ((unchar *)buff)[i]);
	  printk("\n");
	  */
      }
      DEB(if (errstatus) printk("aha1542_intr_handle: returning %6x\n", errstatus));
      SCtmp->result = errstatus;
      SCint[mbo] = NULL;  /* This effectively frees up the mailbox slot, as
			     far as queuecommand is concerned */
      my_done(SCtmp);
      number_serviced++;
    };
}

int aha1542_queuecommand(Scsi_Cmnd * SCpnt, void (*done)(Scsi_Cmnd *))
{
    unchar ahacmd = CMD_START_SCSI;
    unchar direction;
    unchar *cmd = (unchar *) SCpnt->cmnd;
    unchar target = SCpnt->target;
    unchar lun = SCpnt->lun;
    void *buff = SCpnt->request_buffer;
    int bufflen = SCpnt->request_bufflen;
    int mbo;

    DEB(int i);

    DEB(if (target > 1) {
      SCpnt->result = DID_TIME_OUT << 16;
      done(SCpnt); return 0;});
    
    if(*cmd == REQUEST_SENSE){
#ifndef DEBUG
      if (bufflen != 16) {
	printk("Wrong buffer length supplied for request sense (%d)\n",bufflen);
	panic("aha1542.c");
      };
#endif
      SCpnt->result = 0;
      done(SCpnt); 
      return 0;
    };

#ifdef DEBUG
    if (*cmd == READ_10 || *cmd == WRITE_10)
      i = xscsi2int(cmd+2);
    else if (*cmd == READ_6 || *cmd == WRITE_6)
      i = scsi2int(cmd+2);
    else
      i = -1;
    if (done)
      printk("aha1542_queuecommand: dev %d cmd %02x pos %d len %d ", target, *cmd, i, bufflen);
    else
      printk("aha1542_command: dev %d cmd %02x pos %d len %d ", target, *cmd, i, bufflen);
    aha1542_stat();
    printk("aha1542_queuecommand: dumping scsi cmd:");
    for (i = 0; i < (COMMAND_SIZE(*cmd)); i++) printk("%02x ", cmd[i]);
    printk("\n");
    if (*cmd == WRITE_10 || *cmd == WRITE_6)
      return 0; /* we are still testing, so *don't* write */
#endif
/* Use the outgoing mailboxes in a round-robin fashion, because this
   is how the host adapter will scan for them */

    cli();
    mbo = aha1542_last_mbo_used + 1;
    if (mbo >= AHA1542_MAILBOXES) mbo = 0;

    do{
      if(mb[mbo].status == 0 && SCint[mbo] == NULL)
	break;
      mbo++;
      if (mbo >= AHA1542_MAILBOXES) mbo = 0;
    } while (mbo != aha1542_last_mbo_used);

    if(mb[mbo].status || SCint[mbo])
      panic("Unable to find empty mailbox for aha1542.\n");

    SCint[mbo] = SCpnt;  /* This will effectively prevent someone else from
			    screwing with this cdb. */

    aha1542_last_mbo_used = mbo;    
    sti();

#ifdef DEBUG
    printk("Sending command (%d %x)...",mbo, done);
#endif

    memset(&ccb[mbo], 0, sizeof(struct ccb));

    ccb[mbo].cdblen = COMMAND_SIZE(*cmd);     /* SCSI Command Descriptor Block Length */

    direction = 0;
    if (*cmd == READ_10 || *cmd == READ_6)
	direction = 8;
    else if (*cmd == WRITE_10 || *cmd == WRITE_6)
	direction = 16;

    memcpy(ccb[mbo].cdb, cmd, ccb[mbo].cdblen);

    if (SCpnt->use_sg) {
      struct scatterlist * sgpnt;
      struct chain * cptr;
#ifdef DEBUG
      unsigned char * ptr;
#endif
      int i;
      ccb[mbo].op = 2;	      /* SCSI Initiator Command  w/scatter-gather*/
      SCpnt->host_scribble = scsi_malloc(512);
      sgpnt = (struct scatterlist *) SCpnt->request_buffer;
      cptr = (struct chain *) SCpnt->host_scribble; 
      if (cptr == NULL) panic("aha1542.c: unable to allocate DMA memory\n");
      for(i=0; i<SCpnt->use_sg; i++) {
	if(sgpnt[i].length == 0 || SCpnt->use_sg > 16 || 
	   (((int)sgpnt[i].address) & 1) || (sgpnt[i].length & 1)){
	  unsigned char * ptr;
	  printk("Bad segment list supplied to aha1542.c (%d, %d)\n",SCpnt->use_sg,i);
	  for(i=0;i<SCpnt->use_sg++;i++){
	    printk("%d: %x %x %d\n",i,sgpnt[i].address, sgpnt[i].alt_address,
		   sgpnt[i].length);
	  };
	  printk("cptr %x: ",cptr);
	  ptr = (unsigned char *) &cptr[i];
	  for(i=0;i<18;i++) printk("%02x ", ptr[i]);
	  panic("Foooooooood fight!");
	};
	any2scsi(cptr[i].dataptr, sgpnt[i].address);
	any2scsi(cptr[i].datalen, sgpnt[i].length);
      };
      any2scsi(ccb[mbo].datalen, SCpnt->use_sg * sizeof(struct chain));
      any2scsi(ccb[mbo].dataptr, cptr);
#ifdef DEBUG
      printk("cptr %x: ",cptr);
      ptr = (unsigned char *) cptr;
      for(i=0;i<18;i++) printk("%02x ", ptr[i]);
#endif
    } else {
      ccb[mbo].op = 0;	      /* SCSI Initiator Command */
      SCpnt->host_scribble = NULL;
      any2scsi(ccb[mbo].datalen, bufflen);
      any2scsi(ccb[mbo].dataptr, buff);
    };
    ccb[mbo].idlun = (target&7)<<5 | direction | (lun & 7); /*SCSI Target Id*/
    ccb[mbo].rsalen = 12;
    ccb[mbo].linkptr[0] = ccb[mbo].linkptr[1] = ccb[mbo].linkptr[2] = 0;
    ccb[mbo].commlinkid = 0;

#ifdef DEBUGd
    { int i;
    printk("aha1542_command: sending.. ");
    for (i = 0; i < sizeof(ccb[mbo])-10; i++)
      printk("%02x ", ((unchar *)&ccb[mbo])[i]);
    };
#endif
    
    if (done) {
	DEB(printk("aha1542_queuecommand: now waiting for interrupt "); aha1542_stat());
	SCpnt->scsi_done = done;
	mb[mbo].status = 1;
	aha1542_out(&ahacmd, 1);		/* start scsi command */
	DEB(aha1542_stat());
    }
    else
      printk("aha1542_queuecommand: done can't be NULL\n");
    
    return 0;
}

static volatile int internal_done_flag = 0;
static volatile int internal_done_errcode = 0;
static void internal_done(Scsi_Cmnd * SCpnt)
{
    internal_done_errcode = SCpnt->result;
    ++internal_done_flag;
}

int aha1542_command(Scsi_Cmnd * SCpnt)
{
    DEB(printk("aha1542_command: ..calling aha1542_queuecommand\n"));

    aha1542_queuecommand(SCpnt, internal_done);

    while (!internal_done_flag);
    internal_done_flag = 0;
    return internal_done_errcode;
}

/* Initialize mailboxes */
static void setup_mailboxes(void)
{
    int i;
    static unchar cmd[5] = {CMD_MBINIT, AHA1542_MAILBOXES};

    for(i=0; i<AHA1542_MAILBOXES; i++){
      mb[i].status = mb[AHA1542_MAILBOXES+i].status = 0;
      any2scsi(mb[i].ccbptr, &ccb[i]);
    };
    aha1542_intr_reset();     /* reset interrupts, so they don't block */	
    any2scsi((cmd+2), mb);
    aha1542_out(cmd, 5);
    WAIT(INTRFLAGS, INTRMASK, HACC, 0);
    while (0) {
      fail:
	printk("aha1542_detect: failed setting up mailboxes\n");
    }
    aha1542_intr_reset();
}

static int aha1542_getconfig(int hostnum)
{
  static unchar inquiry_cmd[] = {CMD_RETCONF };
  static unchar inquiry_result[3];
  int i;
  i = inb(STATUS);
  if (i & DF) {
    i = inb(DATA);
  };
  aha1542_out(inquiry_cmd, 1);
  aha1542_in(inquiry_result, 3);
  WAIT(INTRFLAGS, INTRMASK, HACC, 0);
  while (0) {
  fail:
    printk("aha1542_detect: query board settings\n");
  }
  aha1542_intr_reset();
  switch(inquiry_result[0]){
  case 0x80:
    dma_chan = 7;
    break;
  case 0x40:
    dma_chan = 6;
    break;
  case 0x20:
    dma_chan = 5;
    break;
  case 1:
    printk("DMA priority 0 not available for Adaptec driver\n");
    return -1;
  default:
    printk("Unable to determine Adaptec DMA priority.  Disabling board\n");
    return -1;
  };
  switch(inquiry_result[1]){
  case 0x40:
    irq_level = 15;
    break;
  case 0x20:
    irq_level = 14;
    break;
  case 0x8:
    irq_level = 12;
    break;
  case 0x4:
    irq_level = 11;
    break;
  case 0x2:
    irq_level = 10;
    break;
  case 0x1:
    irq_level = 9;
    break;
  default:
    printk("Unable to determine Adaptec IRQ level.  Disabling board\n");
    return -1;
  };
  return 0;
}

/* Query the board to find out if it is a 1542 or a 1740, or whatever. */
static void aha1542_query(int hostnum)
{
  static unchar inquiry_cmd[] = {CMD_INQUIRY };
  static unchar inquiry_result[4];
  int i;
  i = inb(STATUS);
  if (i & DF) {
    i = inb(DATA);
  };
  aha1542_out(inquiry_cmd, 1);
  aha1542_in(inquiry_result, 4);
  WAIT(INTRFLAGS, INTRMASK, HACC, 0);
  while (0) {
  fail:
    printk("aha1542_detect: query card type\n");
  }
  aha1542_intr_reset();

/* For an AHA1740 series board, we ignore the board since there is a
   hardware bug which can lead to wrong blocks being returned if the board
   is operating in the 1542 emulation mode.  Since there is an extended mode
   driver, we simply ignore the board and let the 1740 driver pick it up.
*/

  if (inquiry_result[0] == 0x43) {
    printk("aha1542.c: Emulation mode not supported for AHA 174N hardware.\n");
    aha_disable = 1;
  };
}

/* return non-zero on detection */
int aha1542_detect(int hostnum)
{
    int i;
    int indx;

    DEB(printk("aha1542_detect: \n"));
    
    indx = 0;
    while(indx < sizeof(bases)/sizeof(bases[0])){
      i = aha1542_test_port(bases[indx]);
      if (i) break;
      indx++;
    }
    if (indx == sizeof(bases)/sizeof(bases[0])) return 0;
 
    /* Set the Bus on/off-times as not to ruin floppy performance */
    {
	static unchar oncmd[] = {CMD_BUSON_TIME, 7};
	static unchar offcmd[] = {CMD_BUSOFF_TIME, 5};

	aha1542_intr_reset();
	aha1542_out(oncmd, 2);
	WAIT(INTRFLAGS, INTRMASK, HACC, 0);
	aha1542_intr_reset();
	aha1542_out(offcmd, 2);
	WAIT(INTRFLAGS, INTRMASK, HACC, 0);
	while (0) {
	  fail:
	    printk("aha1542_detect: setting bus on/off-time failed\n");
	}
	aha1542_intr_reset();
    }
    aha1542_query(hostnum);

    if (aha_disable) return 0;

    if (aha1542_getconfig(hostnum) == -1) return 0;
    
    printk("Configuring Adaptec at IO:%x, IRQ %d, DMA priority %d\n",base,
	   irq_level, dma_chan);

    DEB(aha1542_stat());
    setup_mailboxes();

    DEB(aha1542_stat());

    DEB(printk("aha1542_detect: enable interrupt channel %d\n", irq_level));

    if (request_irq(irq_level,aha1542_intr_handle)) {
      printk("Unable to allocate IRQ for adaptec controller.\n");
      return 0;
    };

    if(request_dma(dma_chan)){
      printk("Unable to allocate DMA channel for Adaptec.\n");
      free_irq(irq_level);
      return 0;
    };

    if(dma_chan >= 5){
      outb(((dma_chan - 4)|CASCADE),DMA_MODE_REG);
      outb((dma_chan-4),DMA_MASK_REG);
    };

#if 0
    DEB(printk(" *** READ CAPACITY ***\n"));

    {
	unchar buf[8];
	static unchar cmd[] = {	READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	int i;
	
	for (i = 0; i < sizeof(buf); ++i) buf[i] = 0x87;
	for (i = 0; i < 2; ++i)
	  if (!aha1542_command(i, cmd, buf, sizeof(buf))) {
	      printk("aha_detect: LU %d sector_size %d device_size %d\n",
		     i, xscsi2int(buf+4), xscsi2int(buf));
	  }
    }

    DEB(printk(" *** NOW RUNNING MY OWN TEST *** \n"));

    for (i = 0; i < 4; ++i)
      {
	  unsigned char cmd[10];
	  static buffer[512];
	  
	  cmd[0] = READ_10;
	  cmd[1] = 0;
	  xany2scsi(cmd+2, i);
	  cmd[6] = 0;
	  cmd[7] = 0;
	  cmd[8] = 1;
	  cmd[9] = 0;
	  aha1542_command(0, cmd, buffer, 512);
      }
#endif
    aha1542_host = hostnum;
    return 1;
}

/* The abort command does not leave the device in a clean state where
   it is available to be used again.  Until this gets worked out, we will
   leave it commented out.  */

int aha1542_abort(Scsi_Cmnd * SCpnt, int i)
{
#if 0
    unchar ahacmd = CMD_START_SCSI;
    int mbo;
#endif
    DEB(printk("aha1542_abort\n"));
#if 0
    cli();
    for(mbo = 0; mbo < AHA1542_MAILBOXES; mbo++)
      if (SCpnt == SCint[mbo]){
	mb[mbo].status = 2;  /* Abort command */
	aha1542_out(&ahacmd, 1);		/* start scsi command */
	sti();
	break;
      };
#endif
    return 0;
}

int aha1542_reset(void)
{
    DEB(printk("aha1542_reset called\n"));
    return 0;
}

int aha1542_biosparam(int size, int dev, int* info){
  info[0] = 64;
  info[1] = 32;
  info[2] = size >> 11;
/*  if (info[2] >= 1024) info[2] = 1024; */
  return 0;
}
