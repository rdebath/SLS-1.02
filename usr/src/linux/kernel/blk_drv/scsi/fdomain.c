/* fdomain.c -- Future Domain TMC-1660/TMC-1680 driver
 * Created: Sun May  3 18:53:19 1992 by faith
 * Revised: Thu Feb 18 21:02:12 1993 by faith@cs.unc.edu
 * Author: Rickard E. Faith, faith@cs.unc.edu
 * Copyright 1992, 1993 Rickard E. Faith
 *
 * $Log$

 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.

 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.

 **************************************************************************


 DESCRIPTION:

 This is the Linux low-level SCSI driver for Future Domain TMC-1660/1680
 and TMC-1670 SCSI host adapters.


 REFERENCES USED:

 "TMC-1800 SCSI Chip Specification (FDC-1800T)", Future Domain Corporation,
 1990.

 "LXT SCSI Products: Specifications and OEM Technical Manual (Revision
 B/September 1991)", Maxtor Corporation, 1991.

 "7213S product Manual (Revision P3)", Maxtor Corporation, 1992.

 Private communications, Drew Eckhardt (drew@cs.colorado.edu) and Eric
 Youngdale (eric@tantalus.nrl.navy.mil), 1992.


 NOTES ON REFERENCES:

 The Maxtor manuals were free.  Maxtor telephone technical support is
 great!

 The Future Domain manual is $25.  It documents the chip, not the TMC-16x0
 boards, so some information I had to guess at.  Future Domain sells DOS
 BIOS source for $250 and the UN*X driver source was $750, but these
 require a non-disclosure agreement, so even if I could afford them, they
 would *not* have been useful for writing this publically distributable
 driver.  Future Domain technical support has provided some information on
 the phone, and this has been somewhat helpful.


 ALPHA TESTERS:

 Todd Carrico (todd@wutc.wustl.edu), Dan Poirier (poirier@cs.unc.edu ), Ken
 Corey (kenc@sol.acs.unt.edu), C. de Bruin (bruin@dutiba.tudelft.nl),
 Sakari Aaltonen (sakaria@vipunen.hit.fi), John Rice
 (rice@xanth.cs.odu.edu), and Brad Yearwood (brad@optilink.com).


 NOTES ON USER DEFINABLE OPTIONS:

 DEBUG: This turns on the printing of various debug informaiton.

 ENABLE_PARITY: This turns on SCSI parity checking.  With the current
 driver, all attached devices must support SCSI parity.  If none of your
 devices support parity, then you can probably get the driver to work by
 turning this option off.  I have no way of testing this, however.

 QUEUE: Enable "command queueing."  This is supported by the higher level
 SCSI code, and allows the kernel to continue to schedule tasks while the
 SCSI request is pending.  If this option is turned off, then everything
 will "freeze" during SCSI requests, and system performance will become
 unbearable.  Later, this will allow multiple outstanding SCSI requests.  I
 have received reports that if this option is turned off, the driver will
 no longer function correctly.  I have not had time to track down this bug,
 since I hope to make the driver work for everyone with QUEUE on.

 FIFO_COUNT: The host adapter has an 8K cache.  When this many 512 byte
 blocks are filled by the SCSI device, an interrupt will be raised.
 Therefore, this could be as low as 0, or as high as 16.  Note, however,
 that values which are too high or too low seem to prevent any interrupts
 from occuring, and thereby lock up the machine.  I have found that 2 is a
 good number, but throughput may be increased by changing this value to
 values which are close to 2.  Please let me know if you try any different
 values.

 DO_DETECT: This activates some old scan code which was needed before the
 high level drivers got fixed.  If you are having toruble with the driver,
 turning this on should not hurt, and might help.  Please let me know if
 this is the case, since this code will be removed from future drivers.

 RESELECTION: DO *NOT* USE THIS OPTION!  This turns on SCSI device
 disconnect and reselection, which does not work at this time.  When I get
 this working, it will support multiple outstanding SCSI commands.

 **************************************************************************/

#include <linux/sched.h>
#include <asm/io.h>
#include "../blk.h"
#include "scsi.h"
#include "hosts.h"
#include "fdomain.h"
#include <asm/system.h>
#include <linux/errno.h>

#define VERSION          "3.5"	/* Change with each revision */

/* START OF USER DEFINABLE OPTIONS */

#define DEBUG            1	/* Enable debugging output */
#define ENABLE_PARITY    1	/* Enable SCSI Parity */
#define QUEUE            1	/* Enable command queueing */
#define FIFO_COUNT       2	/* Number of 512 byte blocks before INTR */
#define DO_DETECT        0	/* Do device detection here (see scsi.c) */
#define RESELECTION      0	/* Support RESELECTION PHASE (NOT stable) */

/* END OF USER DEFINABLE OPTIONS */

#if DEBUG
#define EVERY_ACCESS     0	/* Write a line on every scsi access */
#define ERRORS_ONLY      1	/* Only write a line if there is an error */
#define DEBUG_DETECT     0	/* Debug fdomain_16x0_detect() */
#define DEBUG_MESSAGES   0	/* Debug MESSAGE IN PHASE */
#define DEBUG_ABORT      1	/* Debug abort() routine */
#else
#define EVERY_ACCESS     0	/* LEAVE THESE ALONE--CHANGE THE ONES ABOVE */
#define ERRORS_ONLY      0
#define DEBUG_DETECT     0
#define DEBUG_MESSAGES   0
#define DEBUG_ABORT      0
#endif

/* Errors are reported on the line, so we don't need to report them again */
#if EVERY_ACCESS
#undef ERRORS_ONLY
#define ERRORS_ONLY      0
#endif

#if ENABLE_PARITY
#define PARITY_MASK      0x08
#else
#define PARITY_MASK      0x00
#endif

static int               port_base = 0;
static void              *bios_base = NULL;
static int               interrupt_level = 0;

static int               Data_Mode_Cntl_port;
static int               FIFO_Data_Count_port;
static int               Interrupt_Cntl_port;
static int               Interrupt_Mask_port;
static int               Read_FIFO_port;
static int               Read_SCSI_Data_port;
static int               SCSI_Cntl_port;
static int               SCSI_Data_NoACK_port;
static int               SCSI_Status_port;
static int               TMC_Cntl_port;
static int               TMC_Status_port;
static int               Write_FIFO_port;
static int               Write_SCSI_Data_port;

static int               this_host = 0;
static int               can_queue = QUEUE;

static volatile int      in_command = 0;
static Scsi_Cmnd         *current_SC = NULL;

enum { non_queueing   = 0x01,
       in_arbitration = 0x02,
       in_selection   = 0x04,
       in_other       = 0x08,
       disconnect     = 0x10,
       aborted        = 0x20,
       sent_ident     = 0x40,
     };

extern void              fdomain_16x0_intr( int unused );

enum in_port_type { Read_SCSI_Data = 0, SCSI_Status = 1, TMC_Status = 2,
			  LSB_ID_Code = 5, MSB_ID_Code = 6, Read_Loopback = 7,
		          SCSI_Data_NoACK = 8, Interrupt_Mask = 9,
		          Option_Select = 10, Read_FIFO = 12,
		          FIFO_Data_Count = 14 };

enum out_port_type { Write_SCSI_Data = 0, SCSI_Cntl = 1, Interrupt_Cntl = 2,
			   Data_Mode_Cntl = 3, TMC_Cntl = 4, Write_Loopback = 7,
			   Write_FIFO = 12 };

static void *addresses[] = {
   (void *)0xc8000,
   (void *)0xca000,
   (void *)0xce000,
   (void *)0xde000 };
#define ADDRESS_COUNT (sizeof( addresses ) / sizeof( unsigned ))
		       
static unsigned short ports[] = { 0x140, 0x150, 0x160, 0x170 };
#define PORT_COUNT (sizeof( ports ) / sizeof( unsigned short ))

static unsigned short ints[] = { 3, 5, 10, 11, 12, 14, 15, 0 };

/*

  READ THIS BEFORE YOU ADD A SIGNATURE!

  READING THIS SHORT NOTE CAN SAVE YOU LOTS OF TIME!

  READ EVERY WORD, ESPECIALLY THE WORD *NOT*

  This driver works *ONLY* for Future Domain cards using the TMC-1800 chip.
  This includes models TMC-1660, 1670, and 1680 *ONLY*.

  The following BIOS signatures have been tried with this driver.  These
  signatures are for boards which do *NOT* work with this driver (but the
  first one should work with the Seagate driver):

      FUTURE DOMAIN COPR. (C) 1986-1989 V6.0A7/28/90
      FUTURE DOMAIN CORP. (C) 1986-1990 V6.0209/18/90
      FUTURE DOMAIN CORP. (C) 1986-1990 V7.009/18/90

  */

struct signature {
   char *signature;
   int  sig_offset;
   int  sig_length;
} signatures[] = {
   /*          1         2         3         4         5         6 */
   /* 123456789012345678901234567890123456789012345678901234567890 */
   { "FUTURE DOMAIN CORP. (C) 1986-1990 1800-V2.07/28/89", 5, 50 },
   { "FUTURE DOMAIN CORP. (C) 1992 V3.00.004/02/92", 5, 44 },
   /* READ NOTICE ABOVE *BEFORE* YOU WASTE YOUR TIME ADDING A SIGANTURE */
};

#define SIGNATURE_COUNT (sizeof( signatures ) / sizeof( struct signature ))


/* These functions are based on include/asm/io.h */

inline static unsigned short inw( unsigned short port )
{
   unsigned short _v;
   
   __asm__ volatile ("inw %1,%0"
		     :"=a" (_v):"d" ((unsigned short) port));
   return _v;
}

inline static void outw( unsigned short value, unsigned short port )
{
   __asm__ volatile ("outw %0,%1"
		     ::"a" ((unsigned short) value),
		     "d" ((unsigned short) port));
}


/* These defines are copied from kernel/blk_drv/hd.c */

#define insw( buf, count, port ) \
      __asm__ volatile \
      ( "cld;rep;insw"::"d" (port),"D" (buf),"c" (count):"cx","di" )

#define outsw( buf, count, port ) \
      __asm__ volatile \
      ("cld;rep;outsw"::"d" (port),"S" (buf),"c" (count):"cx","si")


static void do_pause( unsigned amount )	/* Pause for amount*10 milliseconds */
{
   unsigned long the_time = jiffies + amount; /* 0.01 seconds per jiffy */

   while (jiffies < the_time);
}

inline static void fdomain_make_bus_idle( void )
{
   outb( 0, SCSI_Cntl_port );
   outb( 0, Data_Mode_Cntl_port );
   outb( 1 | PARITY_MASK, TMC_Cntl_port );
}

static int fdomain_is_valid_port( int port )
{
   int options;

#if DEBUG_DETECT 
   printk( " (%x%x),",
	  inb( port + MSB_ID_Code ), inb( port + LSB_ID_Code ) );
#endif

   /* The MCA ID is a unique id for each MCA compatible board.  We
      are using ISA boards, but Future Domain provides the MCA ID
      anyway.  We can use this ID to ensure that this is a Future
      Domain TMC-1660/TMC-1680.
    */

   if (inb( port + LSB_ID_Code ) != 0xe9) { /* test for 0x6127 id */
      if (inb( port + LSB_ID_Code ) != 0x27) return 0;
      if (inb( port + MSB_ID_Code ) != 0x61) return 0;
   } else {			            /* test for 0xe960 id */
      if (inb( port + MSB_ID_Code ) != 0x60) return 0;
   }

   /* We have a valid MCA ID for a TMC-1660/TMC-1680 Future Domain board.
      Now, check to be sure the bios_base matches these ports.
      If someone was unlucky enough to have purchased more than one
      Future Domain board, then they will have to modify this code, as
      we only detect one board here.  [The one with the lowest bios_base.]
    */

   options = inb( port + Option_Select );

#if DEBUG_DETECT
   printk( " Options = %x,", options );
#endif

   if (addresses[ (options & 0xc0) >> 6 ] != bios_base) return 0;
   interrupt_level = ints[ (options & 0x0e) >> 1 ];

   return 1;
}

static int fdomain_test_loopback( void )
{
   int i;
   int result;

   for (i = 0; i < 255; i++) {
      outb( i, port_base + Write_Loopback );
      result = inb( port_base + Read_Loopback );
      if (i != result) return 1;
   }
   return 0;
}

int fdomain_16x0_detect( int hostnum )
{
   int              i, j;
   int              flag;
   struct sigaction sa;
   int              retcode;
#if DO_DETECT
   const int        buflen = 255;
   Scsi_Cmnd        SCinit;
   unsigned char    do_inquiry[] =       { INQUIRY, 0, 0, 0, buflen, 0 };
   unsigned char    do_request_sense[] = { REQUEST_SENSE, 0, 0, 0, buflen, 0 };
   unsigned char    do_read_capacity[] = { READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
   unsigned char    buf[buflen];
#endif

#if DEBUG_DETECT
   printk( "SCSI: fdomain_16x0_detect()," );
#endif

   for (i = 0; !bios_base && i < ADDRESS_COUNT; i++) {
#if DEBUG_DETECT
      printk( " %x(%x),", (unsigned)addresses[i], (unsigned)bios_base );
#endif
      for (j = 0; !bios_base && j < SIGNATURE_COUNT; j++) {
	 if (!memcmp( ((char *)addresses[i] + signatures[j].sig_offset),
		    signatures[j].signature, signatures[j].sig_length )) {
	    bios_base = addresses[i];
	 }
      }
   }

   if (!bios_base) {
#if DEBUG_DETECT
      printk( " FAILED: NO BIOS\n" );
#endif
      return 0;
   }

   /* The TMC-1660/TMC-1680 has a RAM area just after the BIOS ROM.
      Assuming the ROM is enabled (otherwise we wouldn't have been
      able to read the ROM signature :-), then the ROM sets up the
      RAM area with some magic numbers, such as a list of port
      base addresses and a list of the disk "geometry" reported to
      DOS (this geometry has nothing to do with physical geometry).
    */

   port_base = *((char *)bios_base + 0x1fcc)
	 + (*((char *)bios_base + 0x1fcd) << 8);
   
#if DEBUG_DETECT
   printk( " %x,", port_base );
#endif

   for (flag = 0, i = 0; !flag && i < PORT_COUNT; i++) {
      if (port_base == ports[i]) ++flag;
   }

   if (flag) flag = fdomain_is_valid_port( port_base );

   if (!flag) {			/* Cannot get port base from BIOS RAM */
      
      /* This is a bad sign.  It usually means that someone patched the
	 BIOS signature list (the signatures variable) to contain a BIOS
	 signature for a board *OTHER THAN* the TMC-1660/TMC-1680.
       */
      
#if DEBUG_DETECT
      printk( " RAM FAILED, " );
#endif

      /* Anyway, the alternative to finding the address in the RAM is
	 to just search through every possible port address for one
	 that is attached to the Future Domain card.  Don't panic,
	 though, about reading all these random port addresses--there
	 are rumors that the Future Domain BIOS does something very
	 similar.
       */

      for (flag = 0, i = 0; !flag && i < PORT_COUNT; i++) {
	 port_base = ports[i];
#if DEBUG_DETECT
	 printk( " %x,", port_base );
#endif
	 flag = fdomain_is_valid_port( port_base );
      }
   }

   if (!flag) {
#if DEBUG_DETECT
      printk( " FAILED: NO PORT\n" );
#endif
      return 0;		/* Cannot find valid set of ports */
   }

#if DEBUG_DETECT
   printk( "\n" );
   printk( "SCSI: bios_base = %x, port_base = %x, interrupt_level = %d\n",
	  (unsigned)bios_base, port_base, interrupt_level );
#endif

   if (interrupt_level) {
      printk( "Future Domain: BIOS at %x; port base at %x; using IRQ %d\n",
	     (unsigned)bios_base, port_base, interrupt_level );
   } else {
      printk( "Future Domain: BIOS at %x; port base at %x; *NO* IRQ\n",
	     (unsigned)bios_base, port_base );
   }
   
   Data_Mode_Cntl_port  = port_base + Data_Mode_Cntl;
   FIFO_Data_Count_port = port_base + FIFO_Data_Count;
   Interrupt_Cntl_port  = port_base + Interrupt_Cntl;
   Interrupt_Mask_port  = port_base + Interrupt_Mask;
   Read_FIFO_port       = port_base + Read_FIFO;
   Read_SCSI_Data_port  = port_base + Read_SCSI_Data;
   SCSI_Cntl_port       = port_base + SCSI_Cntl;
   SCSI_Data_NoACK_port = port_base + SCSI_Data_NoACK;
   SCSI_Status_port     = port_base + SCSI_Status;
   TMC_Cntl_port        = port_base + TMC_Cntl;
   TMC_Status_port      = port_base + TMC_Status;
   Write_FIFO_port      = port_base + Write_FIFO;
   Write_SCSI_Data_port = port_base + Write_SCSI_Data;

    fdomain_16x0_reset();

   if (fdomain_test_loopback()) {
#if DEBUG_DETECT
      printk( "SCSI: LOOPBACK TEST FAILED, FAILING DETECT!\n" );
#endif
      return 0;
   }

#if DO_DETECT

   /* These routines are here because of the way the SCSI bus behaves after
      a reset.  This appropriate behavior was not handled correctly by the
      higher level SCSI routines when I first wrote this driver.  Now,
      however, correct scan routines are part of scsi.c and these routines
      are no longer needed.  However, this code is still good for
      debugging.
    */

   SCinit.request_buffer = SCinit.buffer = buf;
   SCinit.request_bufflen = SCinit.bufflen = sizeof(buf)-1;
   SCinit.use_sg = 0;
   SCinit.lun = 0;

   printk( "Future Domain detection routine scanning for devices:\n" );
   for (i = 0; i < 8; i++) {
      SCinit.target = i;
      if (i == 6) continue;	/* The host adapter is at SCSI ID 6 */
      memcpy(SCinit.cmnd, do_request_sense, sizeof(do_request_sense));
      retcode = fdomain_16x0_command(&SCinit);
      if (!retcode) {
	 memcpy(SCinit.cmnd, do_inquiry, sizeof(do_inquiry));
	 retcode = fdomain_16x0_command(&SCinit);
	 if (!retcode) {
	    printk( "     SCSI ID %d: ", i );
	    for (j = 8; j < (buf[4] < 32 ? buf[4] : 32); j++)
		  printk( "%c", buf[j] >= 20 ? buf[j] : ' ' );
	    memcpy(SCinit.cmnd, do_read_capacity, sizeof(do_read_capacity));
	    retcode = fdomain_16x0_command(&SCinit);
	    if (!retcode) {
	       unsigned long blocks, size, capacity;
	       
	       blocks = (buf[0] << 24) | (buf[1] << 16)
		     | (buf[2] << 8) | buf[3];
	       size = (buf[4] << 24) | (buf[5] << 16) | (buf[6] << 8) | buf[7];
	       capacity = +( +(blocks / 1024L) * +(size * 10L)) / 1024L;

	       printk( "%lu MB (%lu byte blocks)",
		      ((capacity + 5L) / 10L), size );
	    } else {
	       memcpy(SCinit.cmnd, do_request_sense, sizeof(do_request_sense));
	       retcode = fdomain_16x0_command(&SCinit);
	    }
	    printk ("\n" );
	 } else {
	    memcpy(SCinit.cmnd, do_request_sense, sizeof(do_request_sense));
	    retcode = fdomain_16x0_command(&SCinit);
	 }
      }
   }
#endif

   this_host      = hostnum;
   
   if (!QUEUE || !interrupt_level) {
      printk( "Future Domain: *NO* interrupt level selected!\n" );
      printk( "               COMMAND QUEUEING DISABLED!\n" );
      can_queue = scsi_hosts[this_host].can_queue = 0;
      scsi_hosts[this_host].sg_tablesize = SG_NONE;
   } else {
      /* Register the IRQ with the kernel */

      sa.sa_handler  = fdomain_16x0_intr;
      sa.sa_flags    = SA_INTERRUPT;
      sa.sa_mask     = 0;
      sa.sa_restorer = NULL;
      
      retcode = irqaction( interrupt_level, &sa );

      if (retcode < 0) {
	 if (retcode == -EINVAL) {
	    printk( "Future Domain: IRQ %d is bad!\n", interrupt_level );
	    printk( "               This shouldn't happen: REPORT TO RIK!\n" );
	 } else if (retcode == -EBUSY) {
	    printk( "Future Domain: IRQ %d is already in use!\n",
		   interrupt_level );
	    printk( "               Please use another IRQ for the FD card!\n" );
	 } else {
	    printk( "Future Domain: Error getting IRQ %d\n", interrupt_level );
	    printk( "               This shouldn't happen: REPORT TO RIK!\n" );
	 }
	 printk( "               COMMAND QUEUEING DISABLED!\n" );
      
	 can_queue = scsi_hosts[this_host].can_queue = 0;
	 scsi_hosts[this_host].sg_tablesize = SG_NONE;
      } else {
	 printk( "Future Domain: IRQ %d requested from kernel\n",
		interrupt_level );
      }
   }
   
   return 1;
}

const char *fdomain_16x0_info(void)
{
   static char buffer[] =
	 "Future Domain TMC-16x0 SCSI driver version "
	       VERSION
		     "\n";
   return buffer;
}

static int fdomain_arbitrate( void )
{
   int           status = 0;
   unsigned long timeout;

#if EVERY_ACCESS
   printk( "SCSI: fdomain_arbitrate()\n" );
#endif
   
   outb( 0x00, SCSI_Cntl_port );              /* Disable data drivers */
   outb( 0x40, port_base + SCSI_Data_NoACK ); /* Set our id bit */
   outb( 0x04 | PARITY_MASK, TMC_Cntl_port ); /* Start arbitration */

   timeout = jiffies + 50;	              /* 500 mS */
   while (jiffies < timeout) {
      status = inb( TMC_Status_port );        /* Read adapter status */
      if (status & 0x02) return 0;            /* Arbitration complete */
   }

   /* Make bus idle */
   fdomain_make_bus_idle();

#if EVERY_ACCESS
   printk( "Arbitration failed, status = %x\n", status );
#endif
#if ERRORS_ONLY
   printk( "SCSI (Future Domain): Arbitration failed, status = %x",
	  status );
#endif
   return 1;
}

static int fdomain_select( int target )
{
   int           status;
   unsigned long timeout;

   /* Send our address OR'd with target address */
   outb( 0x40 | (1 << target), SCSI_Data_NoACK_port );

   if (RESELECTION && can_queue)
	 outb( 0x8a, SCSI_Cntl_port ); /* Bus Enable + Attention + Select */
   else
	 outb( 0x82, SCSI_Cntl_port ); /* Bus Enable + Select */

   /* Stop arbitration (also set FIFO for output and enable parity) */
   outb( 0xc0 | PARITY_MASK, TMC_Cntl_port ); 

   timeout = jiffies + 25;	        /* 250mS */
   while (jiffies < timeout) {
      status = inb( SCSI_Status_port ); /* Read adapter status */
      if (status & 1) {		        /* Busy asserted */
	 /* Enable SCSI Bus (on error, should make bus idle with 0) */
	 outb( 0x80, SCSI_Cntl_port );
	 return 0;
      }
   }
   /* Make bus idle */
   fdomain_make_bus_idle();
#if EVERY_ACCESS
   if (!target) printk( "Selection failed\n" );
#endif
#if ERRORS_ONLY
   if (!target) printk( "SCSI (Future Domain): Selection failed" );
#endif
   return 1;
}

void my_done( int error )
{
   if (in_command) {
      in_command = 0;
      outb( 0x00, Interrupt_Cntl_port );
      fdomain_make_bus_idle();
      current_SC->result = error;
      if (current_SC->scsi_done) current_SC->scsi_done( current_SC );
      else panic( "SCSI (Future Domain): current_SC->scsi_done() == NULL" );
   } else {
      panic( "SCSI (Future Domain): my_done() called outside of command\n" );
   }
}

void fdomain_16x0_intr( int unused )
{
   int      status;
   int      done = 0;
   unsigned data_count;

   sti();
   
   outb( 0x00, Interrupt_Cntl_port );

   /* We usually have one spurious interrupt after each command.  Ignore it. */
   if (!in_command || !current_SC) {	/* Spurious interrupt */
      return;
   }

   if (current_SC->SCp.phase & aborted) {
#if EVERY_ACCESS
      if (current_SC->SCp.phase & (in_other || disconnect))
	    printk( "aborted (%s) = %d, ",
		   current_SC->SCp.phase & in_other
		   ? "in_other" : "disconnect",
		   current_SC->result );
      else
	    printk( "aborted = %d, ",
		   current_SC->result );
#endif
      /* Force retry for timeouts after selection complete */
      if (current_SC->SCp.phase & (in_other || disconnect)) {
	 fdomain_16x0_reset();
	 my_done( DID_RESET << 16 );
      } else {
	 my_done( current_SC->result << 16 );
      }
      return;
   }

#if RESELECTION
   if (current_SC->SCp.phase & disconnect) {
      printk( " RECON %x ", inb( SCSI_Data_NoACK_port ) );
      current_SC->SCp.phase = in_other;
      outb( 0x90 | FIFO_COUNT, Interrupt_Cntl_port );
      outb( 0x84, SCSI_Cntl_port );
      while ( (status = inb( SCSI_Status_port )) & 0x20 ) {
	 printk( "s = %x, ", status );
      }
      outb( 0x80, SCSI_Cntl_port );
   } else
#endif
	 if (current_SC->SCp.phase & in_arbitration) {
      status = inb( TMC_Status_port );        /* Read adapter status */
      if (!(status & 0x02)) {
#if EVERY_ACCESS
	 printk( " AFAIL " );
#endif
	 my_done( DID_BUS_BUSY << 16 );
	 return;
      }
      current_SC->SCp.phase = in_selection;

      outb( 0x40 | FIFO_COUNT, Interrupt_Cntl_port );

      outb( 0x40 | (1 << current_SC->target), SCSI_Data_NoACK_port );

#if RESELECTION
      outb( 0x8a, SCSI_Cntl_port ); /* Bus Enable + Attention + Select */
#else
      outb( 0x82, SCSI_Cntl_port ); /* Bus Enable + Select */
#endif

      /* Stop arbitration (also set FIFO for output and enable parity) */
      outb( 0xd0 | PARITY_MASK, TMC_Cntl_port );

      return;
   } else if (current_SC->SCp.phase & in_selection) {
      status = inb( SCSI_Status_port );
      if (!(status & 0x01)) {
	 /* Try again, for slow devices */
	 if (fdomain_select( current_SC->target )) {
#if EVERY_ACCESS
	    printk( " SFAIL " );
#endif
	    my_done( DID_NO_CONNECT << 16 );
	    return;
	 } else {
#if EVERY_ACCESS
	 printk( " AltSel " );
#endif
	 /* Stop arbitration (also set FIFO for output and enable parity) */
	 outb( 0xd0 | PARITY_MASK, TMC_Cntl_port );
       }
      }
      current_SC->SCp.phase = in_other;
      outb( 0x90 | FIFO_COUNT, Interrupt_Cntl_port );
#if RESELECTION
      outb( 0x88, SCSI_Cntl_port );
#else
      outb( 0x80, SCSI_Cntl_port );
#endif
      return;
   }

   /* current_SC->SCp.phase == in_other: this is the body of the routine */

   switch (current_SC->cmnd[0]) {
   case 0x04: case 0x07: case 0x0a: case 0x15: case 0x2a:
   case 0x2e: case 0x3b: case 0xea: case 0x3f:
      while ( (data_count = 0x2000 - inw( FIFO_Data_Count_port )) > 512 ) {
#if EVERY_ACCESS
	 printk( "DC=%d, ", data_count );
#endif
	 if (data_count > current_SC->SCp.this_residual)
	       data_count = current_SC->SCp.this_residual;
	 if (data_count > 0) {
#if EVERY_ACCESS
	    printk( "%d OUT, ", data_count );
#endif
	    if (data_count == 1) {
	       outb( *current_SC->SCp.ptr++, Write_FIFO_port );
	       --current_SC->SCp.this_residual;
	    } else {
	       data_count >>= 1;
	       outsw( current_SC->SCp.ptr, data_count, Write_FIFO_port );
	       current_SC->SCp.ptr += 2 * data_count;
	       current_SC->SCp.this_residual -= 2 * data_count;
	    }
	 }
	 if (!current_SC->SCp.this_residual) {
	    if (current_SC->SCp.buffers_residual) {
	       --current_SC->SCp.buffers_residual;
	       ++current_SC->SCp.buffer;
	       current_SC->SCp.ptr = current_SC->SCp.buffer->address;
	       current_SC->SCp.this_residual = current_SC->SCp.buffer->length;
	    } else
		  break;
	 }
      }
      break;
   default:
      if (!current_SC->SCp.have_data_in) {
	 outb( 0x90 | PARITY_MASK, TMC_Cntl_port );
	 ++current_SC->SCp.have_data_in;
      } else {
	 while ((data_count = inw( FIFO_Data_Count_port )) != 0) {
#if EVERY_ACCESS
	    printk( "DC=%d, ", data_count );
#endif
	    if (data_count > current_SC->SCp.this_residual)
		  data_count = current_SC->SCp.this_residual;
	    if (data_count) {
#if EVERY_ACCESS
	       printk( "%d IN, ", data_count );
#endif
	       if (data_count == 1) {
		  *current_SC->SCp.ptr++ = inb( Read_FIFO_port );
		  --current_SC->SCp.this_residual;
	       } else {
		  data_count >>= 1; /* Number of words */
		  insw( current_SC->SCp.ptr, data_count, Read_FIFO_port );
		  current_SC->SCp.ptr += 2 * data_count;
		  current_SC->SCp.this_residual -= 2 * data_count;
	       }
	    }
	    if (!current_SC->SCp.this_residual && current_SC->SCp.buffers_residual) {
	       --current_SC->SCp.buffers_residual;
	       ++current_SC->SCp.buffer;
	       current_SC->SCp.ptr = current_SC->SCp.buffer->address;
	       current_SC->SCp.this_residual = current_SC->SCp.buffer->length;
	    }
	 }
      }
      break;
   }

   status = inb( SCSI_Status_port );

   if (status & 0x10) {	/* REQ */
      
      switch (status & 0x0e) {
      case 0x08:		/* COMMAND OUT */
#if 0
	 if (!current_SC->SCp.sent_command) {
	    int i;
	    
	    ++current_SC->SCp.sent_command;
	    
	    for (i = 0; i < COMMAND_SIZE( current_SC->cmnd[0] ); i++) {
	       outb( current_SC->cmnd[i], Write_SCSI_Data_port );
#if EVERY_ACCESS
	       printk( "CMD = %x,", current_SC->cmnd[i] );
#endif
	    }
	 }
#else
	 outb( current_SC->cmnd[current_SC->SCp.sent_command++],
	       Write_SCSI_Data_port );
#if EVERY_ACCESS
	 printk( "CMD = %x,",
		 current_SC->cmnd[ current_SC->SCp.sent_command - 1] );
#endif
	    
#endif
	 break;
      case 0x0c:		/* STATUS IN */
	 current_SC->SCp.Status = inb( Read_SCSI_Data_port );
#if EVERY_ACCESS
	 printk( "Status = %x, ", current_SC->SCp.Status );
#endif
#if ERRORS_ONLY
	 if (current_SC->SCp.Status && current_SC->SCp.Status != 2) {
	    printk( "SCSI (Future Domain): target = %d, command = %x, Status = %x\n",
		   current_SC->target, current_SC->cmnd[0], current_SC->SCp.Status );
	 }
#endif
	 break;
      case 0x0a:		/* MESSAGE OUT */
#if RESELECTION
	 if (!(current_SC->SCp.phase & sent_ident)) {
#if EVERY_ACCESS
	    printk( " IDENT " );
#endif
	    outb( 0x80, SCSI_Cntl_port );
	    outb( IDENTIFY( 1, 0 ), Write_SCSI_Data_port );
	    current_SC->SCp.phase |= sent_ident;
	 } else
#else
	       outb( MESSAGE_REJECT, Write_SCSI_Data_port ); /* Reject */
#endif
	 break;
      case 0x0e:		/* MESSAGE IN */
	 current_SC->SCp.Message = inb( Read_SCSI_Data_port );
#if EVERY_ACCESS
	 printk( "Message = %x, ", current_SC->SCp.Message );
#endif
	 if (!current_SC->SCp.Message) ++done;
#if RESELECTION
	 if (current_SC->SCp.Message == DISCONNECT) {
	    printk( " DISCON " );
	    current_SC->SCp.phase = disconnect;
	 }
#endif
#if DEBUG_MESSAGES || EVERY_ACCESS
	 if (current_SC->SCp.Message) {
	    printk( "SCSI (Future Domain): Message = %x\n",
		   current_SC->SCp.Message );
	 }
#endif
	 break;
      }
   }

   if (done) {
#if EVERY_ACCESS
      printk( " ** IN DONE ** " );
#endif

      if (current_SC->SCp.have_data_in) {
	 while ((data_count = inw( FIFO_Data_Count_port )) != 0) {
	    if (data_count > current_SC->SCp.this_residual)
		  data_count = current_SC->SCp.this_residual;

	    if (data_count) {
#if EVERY_ACCESS
	       printk( "%d IN, ", data_count );
#endif
	       if (data_count == 1) {
		  *current_SC->SCp.ptr++ = inb( Read_FIFO_port );
		  --current_SC->SCp.this_residual;
	       } else {
		  data_count >>= 1; /* Number of words */
		  insw( current_SC->SCp.ptr, data_count, Read_FIFO_port );
		  current_SC->SCp.this_residual -= 2 * data_count;
	       }
	    }

	    if (!current_SC->SCp.this_residual
		&& current_SC->SCp.buffers_residual) {
	       
	       --current_SC->SCp.buffers_residual;
	       ++current_SC->SCp.buffer;
	       current_SC->SCp.ptr = current_SC->SCp.buffer->address;
	       current_SC->SCp.this_residual = current_SC->SCp.buffer->length;
	    }
	 }
      }
#if EVERY_ACCESS
      printk( "AFTER DATA GET\n" );
#endif
      
#if ERRORS_ONLY
      if (current_SC->cmnd[0] == REQUEST_SENSE && !current_SC->SCp.Status) {
	 if ((unsigned char)(*((char *)current_SC->request_buffer + 2)) & 0x0f) {
	    unsigned char key;
	    unsigned char code;

	    key = (unsigned char)(*((char *)current_SC->request_buffer + 2)) & 0x0f;
	    code = (unsigned char)(*((char *)current_SC->request_buffer + 12));

	    if (!(key == UNIT_ATTENTION && (code == 0x29 || !code))
		&& !(key == ILLEGAL_REQUEST && (code == 0x25 || !code)))
		
		printk( "SCSI REQUEST SENSE: Sense Key = %x, Sense Code = %x\n",
		       key, code );
	 }
      }
#endif
#if EVERY_ACCESS
      printk( "BEFORE MY_DONE. . ." );
#endif
      my_done( (current_SC->SCp.Status & 0xff) | ((current_SC->SCp.Message & 0xff) << 8) | (DID_OK << 16) );
#if EVERY_ACCESS
      printk( "RETURNING.\n" );
#endif
      
   } else {
      if (current_SC->SCp.phase & disconnect) {
	 outb( 0xd0 | FIFO_COUNT, Interrupt_Cntl_port );
	 outb( 0x00, SCSI_Cntl_port );
      } else {
	 outb( 0x90 | FIFO_COUNT, Interrupt_Cntl_port );
      }
   }

   return;
}

int fdomain_16x0_queue( Scsi_Cmnd * SCpnt, void (*done)(Scsi_Cmnd *))
{
   if (in_command) {
      panic( "SCSI (Future Domain): fdomain_16x0_queue() NOT REENTRANT!\n" );
   }
#if EVERY_ACCESS
   printk( "queue: target = %d cmnd = 0x%02x pieces = %d size = %u\n",
	   SCpnt->target,
	   *(unsigned char *)SCpnt->cmnd,
	   SCpnt->use_sg,
	   SCpnt->request_bufflen );
#endif

   fdomain_make_bus_idle();

   current_SC            = SCpnt; /* Save this for the done function */
   current_SC->scsi_done = done;

   /* Initialize static data */

   if (current_SC->use_sg) {
      current_SC->SCp.buffer =
	    (struct scatterlist *)current_SC->request_buffer;
      current_SC->SCp.ptr              = current_SC->SCp.buffer->address;
      current_SC->SCp.this_residual    = current_SC->SCp.buffer->length;
      current_SC->SCp.buffers_residual = current_SC->use_sg - 1;
   } else {
      current_SC->SCp.ptr              = current_SC->request_buffer;
      current_SC->SCp.this_residual    = current_SC->request_bufflen;
      current_SC->SCp.buffer           = NULL;
      current_SC->SCp.buffers_residual = 0;
   }
	 
   
   current_SC->SCp.Status              = 0;
   current_SC->SCp.Message             = 0;
   current_SC->SCp.have_data_in        = 0;
   current_SC->SCp.sent_command        = 0;
   current_SC->SCp.phase               = in_arbitration;

   /* Start arbitration */
   outb( 0x00, Interrupt_Cntl_port );
   outb( 0x00, SCSI_Cntl_port );              /* Disable data drivers */
   outb( 0x40, SCSI_Data_NoACK_port );        /* Set our id bit */
   ++in_command;
   outb( 0x20, Interrupt_Cntl_port );
   outb( 0x14 | PARITY_MASK, TMC_Cntl_port ); /* Start arbitration */

   return 0;
}

int fdomain_16x0_command( Scsi_Cmnd *SCpnt )
{
   const char     *cmd_pt = SCpnt->cmnd;
   const char     *the_command = SCpnt->cmnd;
   unsigned char  *out_buf_pt = SCpnt->request_buffer;
   unsigned char  *in_buf_pt = SCpnt->request_buffer;
   unsigned char  target = SCpnt->target;
   void           *buff = SCpnt->request_buffer;
   int            bufflen = SCpnt->request_bufflen;
   int            Status = 0;
   int            Message = 0;
   int            status;
   int            done = 0;
   unsigned long  timeout;
   unsigned       data_sent = 0;
   unsigned       data_count;
   int            have_data_in = 0;

   current_SC = SCpnt;

#if EVERY_ACCESS
   printk( "fdomain_command(%d, %x): ", target, (unsigned char)*the_command );
#endif

   if (fdomain_arbitrate()) {
#if ERRORS_ONLY
      printk( ", target = %d, command = %x\n",
	     target, (unsigned char)*the_command );
#endif
      return DID_TIME_OUT << 16;
   }

   if (fdomain_select( target )) {
#if ERRORS_ONLY
      if (!target) printk( ", target = %d, command = %x\n",
			 target, (unsigned char)*the_command );
#endif
      return DID_NO_CONNECT << 16;
   }

   timeout = jiffies + 500;	/* 5000 mS -- For Maxtor after a RST */
   current_SC->SCp.phase = non_queueing;

   switch ((unsigned char)*the_command) {
   case 0x04: case 0x07: case 0x0a: case 0x15: case 0x2a:
   case 0x2e: case 0x3b: case 0xea: case 0x3f:
      data_count = 0x2000 - inw( FIFO_Data_Count_port );
      if (bufflen - data_sent < data_count)
	    data_count = bufflen - data_sent;
      if (data_count == 1) {
	 outb( *out_buf_pt++, Write_FIFO_port );
	 ++data_sent;
      } else {
	 data_count >>= 1;
	 outsw( out_buf_pt, data_count, Write_FIFO_port );
	 out_buf_pt += 2 * data_count;
	 data_sent += 2 * data_count;
      }
      break;
   default:
      outb( 0x80 | PARITY_MASK, TMC_Cntl_port );
      ++have_data_in;
      break;
   }
   
   while (((status = inb( SCSI_Status_port )) & 1)
	  && !done && !(current_SC->SCp.phase & aborted)
	  && jiffies < timeout) {
      
      if (status & 0x10) {	/* REQ */

	 switch (status & 0x0e) {
	 case 0x00:		/* DATA OUT */
	    data_count = 0x2000 - inw( FIFO_Data_Count_port );
	    if (bufflen - data_sent < data_count)
		  data_count = bufflen - data_sent;
	    if (data_count == 1) {
	       outb( *out_buf_pt++, Write_FIFO_port );
	       ++data_sent;
	    } else {
	       data_count >>= 1;
	       outsw( out_buf_pt, data_count, Write_FIFO_port );
	       out_buf_pt += 2 * data_count;
	       data_sent += 2 * data_count;
	    }
	    break;
	 case 0x04:		/* DATA IN */
	    if (!have_data_in) {
	       outb( 0x80 | PARITY_MASK, TMC_Cntl_port );
	       ++have_data_in;
	    }
	    data_count = inw( FIFO_Data_Count_port );
	    if (data_count == 1) {
	       *in_buf_pt++ = inb( Read_FIFO_port );
	    } else {
	       data_count >>= 1; /* Number of words */
	       insw( in_buf_pt, data_count, Read_FIFO_port );
	       in_buf_pt += 2 * data_count;
	    }
	    break;
	 case 0x08:		/* COMMAND OUT */
	    outb( *cmd_pt++, Write_SCSI_Data_port );
#if EVERY_ACCESS
	    printk( "%x,", (unsigned char)cmd_pt[-1] );
#endif
	    break;
	 case 0x0c:		/* STATUS IN */
	    Status = inb( Read_SCSI_Data_port );
#if EVERY_ACCESS
	    printk( "Status = %x, ", Status );
#endif
#if ERRORS_ONLY
	    if (Status) {
	       printk( "SCSI (Future Domain): target = %d, command = %x, Status = %x\n",
		      target, (unsigned char)*the_command, Status );
	    }
#endif
	    break;
	 case 0x0a:		/* MESSAGE OUT */
	    outb( 0x07, Write_SCSI_Data_port ); /* Reject */
	    break;
	 case 0x0e:		/* MESSAGE IN */
	    Message = inb( Read_SCSI_Data_port );
#if EVERY_ACCESS
	    printk( "Message = %x, ", Message );
#endif
	    if (!Message) ++done;
	    if (Message == DISCONNECT) printk( "DISCONNECT\n" );
	    break;
	 }
      }
   }

   if (jiffies >= timeout) {
#if EVERY_ACCESS
      printk( "Time out, status = %x\n", status );
#endif
#if ERRORS_ONLY
      printk( "SCSI (Future Domain): "
	     "Time out, status = %x (target = %d, command = %x)\n",
	     status, target, (unsigned char)*the_command );
#endif
      fdomain_make_bus_idle();
      return DID_BUS_BUSY << 16;
   }

   if (current_SC->SCp.phase & aborted) {
#if EVERY_ACCESS
      printk( "Aborted\n" );
#endif
#if ERRORS_ONLY
      printk( "SCSI (Future Domain): Aborted (command = %x)\n",
	     (unsigned char)*the_command );
#endif
      fdomain_16x0_reset();
      return DID_RESET << 16;
   }
   
   if (have_data_in) {
      while ((data_count = inw( FIFO_Data_Count_port )) != 0) {
	 if (data_count == 1) {
	    *in_buf_pt++ = inb( Read_FIFO_port );
	 } else {
	    data_count >>= 1; /* Number of words */
	    insw( in_buf_pt, data_count, Read_FIFO_port );
	    in_buf_pt += 2 * data_count;
	 }
      }
   }

   fdomain_make_bus_idle();

#if EVERY_ACCESS
   printk( "Retcode = %x\n",
	  (Status & 0xff) | ((Message & 0xff) << 8) | (DID_OK << 16) );
#endif
#if ERRORS_ONLY
   if (*the_command == REQUEST_SENSE && !Status) {
      if ((unsigned char)(*((char *)buff + 2)) & 0x0f) {
	 printk( "SCSI REQUEST SENSE: Sense Key = %x, Sense Code = %x\n",
		(unsigned char)(*((char *)buff + 2)) & 0x0f,
		(unsigned char)(*((char *)buff + 12)) );
      }
   }
#endif

   return (Status & 0xff) | ((Message & 0xff) << 8) | (DID_OK << 16);
}

int fdomain_16x0_abort( Scsi_Cmnd *SCpnt, int code )
{

#if EVERY_ACCESS || ERRORS_ONLY || DEBUG_ABORT
   printk( "SCSI (Future Domain): Abort " );
#endif

#if DEBUG_ABORT
   printk( "Phase = %d, target = %d cmnd = 0x%02x pieces = %d size = %u\n",
    current_SC->SCp.phase,
    current_SC->target,
    *(unsigned char *)current_SC->cmnd,
    current_SC->use_sg,
    current_SC->request_bufflen );
   printk( "IMR = 0x%02x%02x\n", inb( 0x0a1 ), inb( 0x21 ) );
   outb( 0x0a, 0xa0 );
   printk( "IRR = 0x%02x", inb( 0xa0 ) );
   outb( 0x0a, 0x20 );
   printk( "%02x\n", inb( 0x20 ) );
   outb( 0x0b, 0xa0 );
   printk( "ISR = 0x%02x", inb( 0xa0 ) );
   outb( 0x0b, 0x20 );
   printk( "%02x\n", inb( 0x20 ) );
   printk( "SCSI Status    = %x\n", inb( SCSI_Status_port ) );
   printk( "TMC Status     = %x\n", inb( TMC_Status_port ) );
   printk( "Interrupt Mask = %x\n", inb( Interrupt_Mask_port ) );
#else
   cli();
   if (!in_command) {
#if EVERY_ACCESS || ERRORS_ONLY
      printk( " (not in command)\n" );
#endif
      sti();
      return 0;
   } else {
#if EVERY_ACCESS || ERRORS_ONLY
      printk( " code = %d\n", code );
#endif
   }

   current_SC->SCp.phase |= aborted;

   current_SC->result = code ? code : DID_ABORT;

   sti();
   
   /* Aborts are not done well. . . */
   my_done( code << 16 );
#endif
   return 0;
}

int fdomain_16x0_reset( void )
{
#if ERRORS_ONLY
   printk( "Future Domain: SCSI Bus Reset\n" );
#endif
   outb( 1, SCSI_Cntl_port );
   do_pause( 2 );
   outb( 0, SCSI_Cntl_port );
   do_pause( 115 );
   outb( 0, Data_Mode_Cntl_port );
   outb( PARITY_MASK, TMC_Cntl_port );
   return 0;
}

int fdomain_16x0_biosparam( int size, int dev, int *info )
{
   int    drive;
   struct drive_info {
      unsigned short cylinders;
      unsigned char  heads;
      unsigned char  sectors;
   } *i;

   /* NOTES:
      The RAM area starts at 0x1f00 from the bios_base address.
      The drive parameter table seems to start at 0x1f30.
      The first byte's purpose is not known.
      Next is the cylinder, head, and sector information.
      The last 4 bytes appear to be the drive's size in sectors.
      The other bytes in the drive parameter table are unknown.
      If anyone figures them out, please send me mail, and I will
      update these notes.

      Tape drives do not get placed in this table.

      There is another table at 0x1fea:
      If the byte is 0x01, then the SCSI ID is not in use.
      If the byte is 0x18 or 0x48, then the SCSI ID is in use,
      although tapes don't seem to be in this table.  I haven't
      seen any other numbers (in a limited sample).

      0x1f2d is a drive count (i.e., not including tapes)

      The table at 0x1fcc are I/O ports addresses for the various
      operations.  I calculate these by hand in this driver code.
    */

   drive = MINOR(dev) / 16;
   i = (struct drive_info *)( (char *)bios_base + 0x1f31 + drive * 25 );
   info[0] = i->heads;
   info[1] = i->sectors;
   info[2] = i->cylinders;
   return 0;
}
