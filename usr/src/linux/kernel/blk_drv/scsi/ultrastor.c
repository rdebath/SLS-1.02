/*
 *	ultrastor.c	Copyright (C) 1992 David B. Gentzel
 *	Low-level SCSI driver for UltraStor 14F
 *	by David B. Gentzel, Whitfield Software Services, Carnegie, PA
 *	    (gentzel@nova.enet.dec.com)
 *  scatter/gather added by Scott Taylor (n217cg@tamuts.tamu.edu)
 *	Thanks to UltraStor for providing the necessary documentation
 */

/*
 * TODO:
 *	1. Cleanup error handling & reporting.
 *	2. Find out why scatter/gather is limited to 16 requests per command.
 *	3. Add multiple outstanding requests.
 *	4. See if we can make good use of having more than one command per lun.
 *	5. Test/improve/fix abort & reset functions.
 *	6. Look at command linking (mscp.command_link and
 *	   mscp.command_link_id).
 */

/*
 * NOTES:
 *    The UltraStor 14F is one of a family of intelligent, high performance
 *    SCSI-2 host adapters.  They all support command queueing and
 *    scatter/gather I/O.  Some of them can also emulate the standard
 *    WD1003 interface for use with OS's which don't support SCSI.
 *    Here is the scoop on the various models:
 *	14F - ISA first-party DMA HA with floppy support and WD1003 emulation.
 *	14N - ISA HA with floppy support.  I think that this is a non-DMA
 *	      HA.  Nothing further known.
 *	24F - EISA Bus Master HA with floppy support and WD1003 emulation.
 *	34F - VL-Bus Bus Master HA with floppy support (no WD1003 emulation).
 *
 *    The 14F is supported by this driver.  An effort has been made to support
 *    the 34F.  It should work, but is untested.  The 24F does not work at
 *    present.
 *
 *    Places flagged with a triple question-mark are things which are either
 *    unfinished, questionable, or wrong.
 */

#include <linux/stddef.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/kernel.h>

#include <asm/io.h>
#include <asm/system.h>
#include <asm/dma.h>

#define ULTRASTOR_PRIVATE	/* Get the private stuff from ultrastor.h */
#include "../blk.h"
#include "scsi.h"
#include "hosts.h"
#include "ultrastor.h"

#define ULTRASTOR_DEBUG 0

#define VERSION "1.1 alpha"

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr)[0])
#define BYTE(num, n) ((unsigned char)((unsigned int)(num) >> ((n) * 8)))

/* Simply using "unsigned long" in these structures won't work as it causes
   alignment.  Perhaps the "aligned" attribute may be used in GCC 2.0 to get
   around this, but for now I use this hack. */
typedef struct {
    unsigned char bytes[4];
} Longword;

/* Used to fetch the configuration info from the config i/o registers.  We
   then store (in a friendlier format) in config. */
struct config_1 {
    unsigned char bios_segment: 3;
    unsigned char removable_disks_as_fixed: 1;
    unsigned char interrupt: 2;
    unsigned char dma_channel: 2;
};
struct config_2 {
    unsigned char ha_scsi_id: 3;
    unsigned char mapping_mode: 2;
    unsigned char bios_drive_number: 1;
    unsigned char tfr_port: 2;
};

/* Used to store configuration info read from config i/o registers.  Most of
   this is not used yet, but might as well save it. */
struct config {
    const void *bios_segment;
    unsigned short port_address;
    unsigned char interrupt: 4;
    unsigned char dma_channel: 3;
    unsigned char bios_drive_number: 1;
    unsigned char heads;
    unsigned char sectors;
    unsigned char ha_scsi_id: 3;
    unsigned char subversion: 4;
};

/* MailBox SCSI Command Packet.  Basic command structure for communicating
   with controller. */
struct mscp {
    unsigned char opcode: 3;		/* type of command */
    unsigned char xdir: 2;		/* data transfer direction */
    unsigned char dcn: 1;		/* disable disconnect */
    unsigned char ca: 1;		/* use cache (if available) */
    unsigned char sg: 1;		/* scatter/gather operation */
    unsigned char target_id: 3;		/* target SCSI id */
    unsigned char ch_no: 2;		/* SCSI channel (always 0 for 14f) */
    unsigned char lun: 3;		/* logical unit number */
    Longword transfer_data;		/* transfer data pointer */
    Longword transfer_data_length;	/* length in bytes */
    Longword command_link;		/* for linking command chains */
    unsigned char scsi_command_link_id;	/* identifies command in chain */
    unsigned char number_of_sg_list;	/* (if sg is set) 8 bytes per list */
    unsigned char length_of_sense_byte;
    unsigned char length_of_scsi_cdbs;	/* 6, 10, or 12 */
    unsigned char scsi_cdbs[12];	/* SCSI commands */
    unsigned char adapter_status;	/* non-zero indicates HA error */
    unsigned char target_status;	/* non-zero indicates target error */
    Longword sense_data;
};

/* The 14F uses an array of unaligned 4-byte ints for its scatter/gather list. */
typedef struct {
	Longword address;
	Longword num_bytes;
} ultrastor_sg_list;

/* This is our semaphore for mscp block availability */
int mscp_free = TRUE;

/* Allowed BIOS base addresses for 14f (NULL indicates reserved) */
static const void *const bios_segment_table_14f[8] = {
    NULL,	     (void *)0xC4000, (void *)0xC8000, (void *)0xCC000,
    (void *)0xD0000, (void *)0xD4000, (void *)0xD8000, (void *)0xDC000,
};

/* Allowed IRQs for 14f */
static const unsigned char interrupt_table_14f[4] = { 15, 14, 11, 10 };

/* Allowed DMA channels for 14f (0 indicates reserved) */
static const unsigned char dma_channel_table_14f[4] = { 5, 6, 7, 0 };

/* Head/sector mappings allowed by 14f */
static const struct {
    unsigned char heads;
    unsigned char sectors;
} mapping_table_14f[4] = { { 16, 63 }, { 64, 32 }, { 64, 63 }, { 0, 0 } };

/* Subversions of the 14F */
static const char *const subversion_names[] = { "14F", "34F" };

/* Config info */
static struct config config;

/* Our index in the host adapter array maintained by higher-level driver */
static int host_number;

/* PORT_ADDRESS is first port address used for i/o of messages. */
#ifdef PORT_OVERRIDE
# define PORT_ADDRESS PORT_OVERRIDE
#else
# define PORT_ADDRESS (config.port_address)
#endif

static volatile int aborted = 0;

#ifndef PORT_OVERRIDE
/* ??? A probe of address 0x310 screws up NE2000 cards */
static const unsigned short ultrastor_ports_14f[] = {
    0x330, 0x340, /*0x310,*/ 0x230, 0x240, 0x210, 0x130, 0x140,
};
#endif

static void ultrastor_interrupt(int cpl);
static inline void build_sg_list(Scsi_Cmnd *SCpnt);

static void (*ultrastor_done)(Scsi_Cmnd *) = 0;
static Scsi_Cmnd *SCint = NULL;

int ultrastor_detect(int hostnum)
{
    size_t i;
    unsigned char in_byte, version_byte = 0;
    struct config_1 config_1;
    struct config_2 config_2;

#if (ULTRASTOR_DEBUG & UD_DETECT)
    printk("US14F: detect: called\n");
#endif

#ifndef PORT_OVERRIDE
    PORT_ADDRESS = 0;
    for (i = 0; i < ARRAY_SIZE(ultrastor_ports_14f); i++) {
	PORT_ADDRESS = ultrastor_ports_14f[i];
#endif

#if (ULTRASTOR_DEBUG & UD_DETECT)
	printk("US14F: detect: testing port address %03X\n", PORT_ADDRESS);
#endif

	in_byte = inb(PRODUCT_ID(PORT_ADDRESS + 0));
	if (in_byte != US14F_PRODUCT_ID_0) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
# ifdef PORT_OVERRIDE
	    printk("US14F: detect: wrong product ID 0 - %02X\n", in_byte);
# else
	    printk("US14F: detect: no adapter at port %03X\n", PORT_ADDRESS);
# endif
#endif
#ifdef PORT_OVERRIDE
	    return FALSE;
#else
	    continue;
#endif
	}
	in_byte = inb(PRODUCT_ID(PORT_ADDRESS + 1));
	/* Only upper nibble is significant for Product ID 1 */
	if ((in_byte & 0xF0) != US14F_PRODUCT_ID_1) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
# ifdef PORT_OVERRIDE
	    printk("US14F: detect: wrong product ID 1 - %02X\n", in_byte);
# else
	    printk("US14F: detect: no adapter at port %03X\n", PORT_ADDRESS);
# endif
#endif
#ifdef PORT_OVERRIDE
	    return FALSE;
#else
	    continue;
#endif
	}
	version_byte = in_byte;
#ifndef PORT_OVERRIDE
	break;
    }
    if (i == ARRAY_SIZE(ultrastor_ports_14f)) {
# if (ULTRASTOR_DEBUG & UD_DETECT)
	printk("US14F: detect: no port address found!\n");
# endif
	return FALSE;
    }
#endif

#if (ULTRASTOR_DEBUG & UD_DETECT)
    printk("US14F: detect: adapter found at port address %03X\n",
	   PORT_ADDRESS);
#endif

    /* All above tests passed, must be the right thing.  Get some useful
       info. */
    *(char *)&config_1 = inb(CONFIG(PORT_ADDRESS + 0));
    *(char *)&config_2 = inb(CONFIG(PORT_ADDRESS + 1));
    config.bios_segment = bios_segment_table_14f[config_1.bios_segment];
    config.interrupt = interrupt_table_14f[config_1.interrupt];
    config.ha_scsi_id = config_2.ha_scsi_id;
    config.heads = mapping_table_14f[config_2.mapping_mode].heads;
    config.sectors = mapping_table_14f[config_2.mapping_mode].sectors;
    config.bios_drive_number = config_2.bios_drive_number;
    config.subversion = (version_byte & 0x0F);
    if (config.subversion == U34F)
	config.dma_channel = 0;
    else
	config.dma_channel = dma_channel_table_14f[config_1.dma_channel];

    if (!config.bios_segment) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
	printk("US14F: detect: not detected.\n");
#endif
	return FALSE;
    }

    /* Final consistancy check, verify previous info. */
    if (config.subversion != U34F)
	if (!config.dma_channel || !(config_2.tfr_port & 0x2)) {
#if (ULTRASTOR_DEBUG & UD_DETECT)
	    printk("US14F: detect: consistancy check failed\n");
#endif
	    return FALSE;
	}

    /* If we were TRULY paranoid, we could issue a host adapter inquiry
       command here and verify the data returned.  But frankly, I'm
       exhausted! */

    /* Finally!  Now I'm satisfied... */
#if (ULTRASTOR_DEBUG & UD_DETECT)
    printk("US14F: detect: detect succeeded\n"
	   "  Port address: %03X\n"
	   "  BIOS segment: %05X\n"
	   "  Interrupt: %u\n"
	   "  DMA channel: %u\n"
	   "  H/A SCSI ID: %u\n"
	   "  Subversion: %u\n",
	   PORT_ADDRESS, config.bios_segment, config.interrupt,
	   config.dma_channel, config.ha_scsi_id, config.subversion);
#endif
    host_number = hostnum;
    scsi_hosts[hostnum].this_id = config.ha_scsi_id;
    scsi_hosts[hostnum].unchecked_isa_dma = (config.subversion != U34F);

    if (request_irq(config.interrupt, ultrastor_interrupt)) {
	printk("Unable to allocate IRQ%u for UltraStor controller.\n",
	       config.interrupt);
	return FALSE;
    }
    if (config.dma_channel && request_dma(config.dma_channel)) {
	printk("Unable to allocate DMA channel %u for UltraStor controller.\n",
	       config.dma_channel);
	free_irq(config.interrupt);
	return FALSE;
    }
	scsi_hosts[hostnum].sg_tablesize = ULTRASTOR_14F_MAX_SG;
	printk("UltraStor: scatter/gather enabled.  Using %d SG lists.\n", ULTRASTOR_14F_MAX_SG);

    return TRUE;
}

const char *ultrastor_info(void)
{
    static char buf[64];

    (void)sprintf(buf, "UltraStor %s SCSI @ Port %03X BIOS %05X IRQ%u DMA%u\n",
		  ((config.subversion < ARRAY_SIZE(subversion_names))
		   ? subversion_names[config.subversion] : "14F?"),
		  PORT_ADDRESS, (int)config.bios_segment, config.interrupt,
		  config.dma_channel);
    return buf;
}

static struct mscp mscp = {
    OP_SCSI, DTD_SCSI, 0, 1, 0		/* This stuff doesn't change */
};

static inline void build_sg_list(Scsi_Cmnd *SCpnt)
{
	ultrastor_sg_list *sglist;
	struct scatterlist *sl;
	long transfer_length = 0;
	int i;

	sl = (struct scatterlist *) SCpnt->request_buffer;
	SCpnt->host_scribble = scsi_malloc(512);
	if (SCpnt->host_scribble == NULL)
		/* Not sure what to do here; just panic for now */
		panic("US14F: Can't allocate DMA buffer for scatter-gather list!\n");
	/* Save ourselves some casts; can eliminate when we don't have to look at it anymore! */
	sglist = (ultrastor_sg_list *) SCpnt->host_scribble;
	for (i = 0; i < SCpnt->use_sg; i++) {
		sglist[i].address = *(Longword *)&(sl[i].address);
		sglist[i].num_bytes = *(Longword *)&(sl[i].length);
		transfer_length += sl[i].length;
	}
	mscp.number_of_sg_list = (char) SCpnt->use_sg;
	mscp.transfer_data = *(Longword *)&sglist;
	/* ??? May not be necessary.  Docs are unclear as to whether transfer length field is */
	/* ignored or whether it should be set to the total number of bytes of the transfer.  */
	mscp.transfer_data_length = *(Longword *)&transfer_length;
}

int ultrastor_queuecommand(Scsi_Cmnd *SCpnt, void (*done)(Scsi_Cmnd *))
{
    unsigned char in_byte;

#if (ULTRASTOR_DEBUG & UD_COMMAND)
    printk("US14F: queuecommand: called\n");
#endif

	/* We want to be sure that a command queued while another command   */
	/* is running doesn't overwrite the mscp block until the running    */
	/* command is finished.  mscp_free is set in the interrupt handler. */
	/* I'm not sure if the upper level driver will send another command */
	/* with a command pending; this is just insurance.                  */
	while (1) {
		cli();
		if (mscp_free) {
			mscp_free = FALSE;
			sti();
			break;
		}
		sti();
	}
    mscp.opcode = OP_SCSI;
    mscp.xdir = DTD_SCSI;
    mscp.dcn = FALSE;
    mscp.ca = TRUE;
    mscp.target_id = SCpnt->target;
    mscp.ch_no = 0;
    mscp.lun = SCpnt->lun;
	if (SCpnt->use_sg) {
		/* Set scatter/gather flag in SCSI command packet */
		mscp.sg = TRUE;
		build_sg_list(SCpnt);
	}
	else {
		/* Unset scatter/gather flag in SCSI command packet */
		mscp.sg = FALSE;
		mscp.transfer_data = *(Longword *)&SCpnt->request_buffer;
		mscp.transfer_data_length = *(Longword *)&SCpnt->request_bufflen;
		SCpnt->host_scribble = NULL;
	}
    memset(&mscp.command_link, 0, sizeof(mscp.command_link));	/*???*/
    mscp.scsi_command_link_id = 0;	/*???*/
    mscp.length_of_sense_byte = 0;	/*???*/
    mscp.length_of_scsi_cdbs = COMMAND_SIZE(*(unsigned char *)SCpnt->cmnd);
    memcpy(mscp.scsi_cdbs, SCpnt->cmnd, mscp.length_of_scsi_cdbs);
    mscp.adapter_status = 0;
    mscp.target_status = 0;
    memset(&mscp.sense_data, 0, sizeof(mscp.sense_data));	/*???*/

    /* Find free OGM slot (OGMINT bit is 0) */
    do
	in_byte = inb_p(LCL_DOORBELL_INTR(PORT_ADDRESS));
    while (!aborted && (in_byte & 1));
    if (aborted) {
#if (ULTRASTOR_DEBUG & (UD_COMMAND | UD_ABORT))
	printk("US14F: queuecommand: aborted\n");
#endif
	/* ??? is this right? */
	return (aborted << 16);
    }

    /* Store pointer in OGM address bytes */
    outb_p(BYTE(&mscp, 0), OGM_DATA_PTR(PORT_ADDRESS + 0));
    outb_p(BYTE(&mscp, 1), OGM_DATA_PTR(PORT_ADDRESS + 1));
    outb_p(BYTE(&mscp, 2), OGM_DATA_PTR(PORT_ADDRESS + 2));
    outb_p(BYTE(&mscp, 3), OGM_DATA_PTR(PORT_ADDRESS + 3));

    /* Issue OGM interrupt */
    outb_p(0x1, LCL_DOORBELL_INTR(PORT_ADDRESS));

    ultrastor_done = done;
    SCint = SCpnt;

#if (ULTRASTOR_DEBUG & UD_COMMAND)
    printk("US14F: queuecommand: returning\n");
#endif

    return 0;
}

int ultrastor_abort(Scsi_Cmnd *SCpnt, int code)
{
#if (ULTRASTOR_DEBUG & UD_ABORT)
    printk("US14F: abort: called\n");
#endif

    aborted = (code ? code : DID_ABORT);

	/* Free DMA buffer used for scatter/gather list */
	if (SCpnt->host_scribble)
		scsi_free(SCpnt->host_scribble, 512);

	/* Free up mscp block for next command */
	mscp_free = TRUE;

#if (ULTRASTOR_DEBUG & UD_ABORT)
    printk("US14F: abort: returning\n");
#endif

    return 0;
}

int ultrastor_reset(void)
{
#if 0
    unsigned char in_byte;
#endif

#if (ULTRASTOR_DEBUG & UD_RESET)
    printk("US14F: reset: called\n");
#endif

	/* ??? SCSI bus reset causes problems on some systems. */
#if 0
    /* Issue SCSI BUS reset */
    outb_p(0x20, LCL_DOORBELL_INTR(PORT_ADDRESS));

    /* Wait for completion... */
    do
	in_byte = inb_p(LCL_DOORBELL_INTR(PORT_ADDRESS));
    while (in_byte & 0x20);

    aborted = DID_RESET;
#endif

#if (ULTRASTOR_DEBUG & UD_RESET)
    printk("US14F: reset: returning\n");
#endif
    return 0;
}

int ultrastor_biosparam(int size, int dev, int *info)
{
    unsigned int s = config.heads * config.sectors;

    info[0] = config.heads;
    info[1] = config.sectors;
    info[2] = (size + (s - 1)) / s;
/*    if (info[2] > 1024)
	info[2] = 1024; */
    return 0;
}

static void ultrastor_interrupt(int cpl)
{
#if (ULTRASTOR_DEBUG & UD_INTERRUPT)
    printk("US14F: interrupt: called: status = %08X\n",
	   (mscp.adapter_status << 16) | mscp.target_status);
#endif

    if (ultrastor_done == 0)
	panic("US14F: interrupt: unexpected interrupt");
    else {
	void (*done)(Scsi_Cmnd *);
	Scsi_Cmnd *SCtmp;

	/* Save ultrastor_done locally and zero before calling.  This is needed
	   as once we call done, we may get another command queued before this
	   interrupt service routine can return. */
	done = ultrastor_done;
	ultrastor_done = 0;
	SCtmp = SCint;

	/* Clean ICM slot (set ICMINT bit to 0) */
	outb_p(0x1, SYS_DOORBELL_INTR(PORT_ADDRESS));

	/* Let the higher levels know that we're done */
	/* ??? status is wrong here... */
	SCtmp->result = (mscp.adapter_status << 16) | mscp.target_status;

	/* Free temp space used for scatter-gather list */
	if (SCtmp->host_scribble)
		scsi_free(SCtmp->host_scribble, 512);

	/* Free up mscp block for next command */
	mscp_free = TRUE;

	done(SCtmp);
    }

#if (ULTRASTOR_DEBUG & UD_INTERRUPT)
    printk("US14F: interrupt: returning\n");
#endif
}
