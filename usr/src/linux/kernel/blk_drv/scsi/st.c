/*
  SCSI Tape Driver for Linux

  Version 0.02 for Linux 0.98.4 and Eric Youngdale's new scsi driver

  History:
  Rewritten from Dwayne Forsyth's SCSI tape driver by Kai Makisara.

  Features:
  - support for different block sizes and internal buffering
  - *nix-style ioctl with codes from mtio.h from the QIC-02 driver by
    Hennus Bergman (command MTSETBLK added)
  - character device
  - rewind and non-rewind devices
  - capability to handle several tape drives simultaneously
  - one buffer if one drive, two buffers if more than one drive (limits the
    number of simultaneously open drives to two)
  - write behind
  - seek and tell (Tandberg compatible and SCSI-2)

  Devices:
  Autorewind devices have minor numbers equal to the tape numbers (0 > ).
  Nonrewind device has the minor number equal to tape number + 128.

  Problems:
  The end of media detection may not work correctly because of the buffering.
  If you want to do multiple tape backups relying on end of tape detection,
  you should disable write behind and in addition to that check that the
  tapes are readable.

  Kai Makisara, Nov 9, 1992  email makisara@vtinsx.ins.vtt.fi or
                                    Kai.Makisara@vtt.fi
  Last changes Jan 3, 1993.
*/

#include <linux/fs.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/mtio.h>
#include <linux/ioctl.h>
#include <linux/fcntl.h>
#include <asm/segment.h>
#include <asm/system.h>

#define MAJOR_NR 9
#include "../blk.h"
#include "scsi.h"
#include "st.h"

#define MAX_RETRIES 5
#define NO_TAPE  NOT_READY

/* Uncomment the following if you want the rewind, etc. commands return
   before command completion. */
/* #define ST_NOWAIT */

/* Uncomment the following if you want the tape to be positioned correctly
   within file after close (the tape is positioned correctly with respect
   to the filemarks even wihout ST_IN_FILE_POS defined */
/* #define ST_IN_FILE_POS */

/* #define DEBUG */

#define ST_TIMEOUT 6000
#define ST_LONG_TIMEOUT 200000

/* Number of ST_BLOCK_SIZE blocks in the buffers */
#define ST_BUFFER_BLOCKS 64
/* Write-behind can be disabled by setting ST_WRITE_THRESHOLD_BLOCKS equal to or
   larger than ST_BUFFER_BLOCKS */
#define ST_WRITE_THRESHOLD_BLOCKS 60
#define ST_BLOCK_SIZE 512
#define ST_BUFFER_SIZE (ST_BUFFER_BLOCKS * ST_BLOCK_SIZE)
#define ST_WRITE_THRESHOLD (ST_WRITE_THRESHOLD_BLOCKS * ST_BLOCK_SIZE)

static int st_nbr_buffers;
static ST_buffer *st_buffers[2];

static Scsi_Tape * scsi_tapes;
int NR_ST=0;
int MAX_ST=0;

static int st_int_ioctl(struct inode * inode,struct file * file,
	     unsigned int cmd_in, unsigned long arg);




/* Wakeup from interrupt */
static void st_sleep_done (Scsi_Cmnd * SCpnt)
{
  int st_nbr;

  if ((st_nbr = SCpnt->request.dev) < NR_ST && st_nbr >= 0) {
    if (scsi_tapes[st_nbr].buffer->writing &&
	(SCpnt->sense_buffer[0] & 0x70) == 0x70 &&
	(SCpnt->sense_buffer[2] & 0x40))
      scsi_tapes[st_nbr].buffer->last_result = 0x7fffffff;
    else
      scsi_tapes[st_nbr].buffer->last_result = SCpnt->result;
    if (scsi_tapes[st_nbr].buffer->writing)
      SCpnt->request.dev = -1;
    else
      SCpnt->request.dev = 0xffff;
    if (scsi_tapes[st_nbr].buffer->writing <= 0)
      wake_up( &scsi_tapes[st_nbr].waiting );
  }
#ifdef DEBUG
  else
    printk("st?: Illegal interrupt device %x\n", st_nbr);
#endif
}



#ifdef DEBUG
/* Print sense information */
static void decode_sns(int dev, char *sense_buffer)
{
   static char *snstext[] = {
   "None","Recovered Error","Not Ready","Medium Error","Hardware Error",
   "Illegal Request","Unit Attention","Data Protect","Blank Check",
   "Key=E","Key=F","Filemark","End-Of-Medium","Incorrect Block Length",
   "14","15"};

   if (sense_buffer[0]!=0) {
      if ((sense_buffer[0] & 0x70) == 0x70) {
         if (sense_buffer[2] & 0x80) printk( "FMK ");
         if (sense_buffer[2] & 0x40) printk( "EOM ");
         if (sense_buffer[2] & 0x20) printk( "ILI ");
        printk( "st%d: sense key %s\n", dev, snstext[sense_buffer[2] & 0x0f]);
      } else {
      if (sense_buffer[0] < 15)
	printk("st%d: old sense key %s\n", dev, snstext[sense_buffer[0] & 0x0f]);
      else
	printk("st%d: sns = %2x %2x\n", dev, sense_buffer[0], sense_buffer[2]);
      }
   }
   return;
}
#endif


/* Convert the result to success code */
static int st_chk_result(int dev, int result, char *sense)
{
  if (!result)
    return 0;
#ifdef DEBUG
  printk("st%d: Error: %x\n", dev, result);
  decode_sns(dev, sense);
#endif
  if ((sense[0] & 0x70) == 0x70 &&
       ((sense[2] & 0x80) /* || ((sense[2] & 0x0f) == 8) */ ))
    return 0;
  return (-EIO);
}


#if ST_WRITE_THRESHOLD_BLOCKS < ST_BUFFER_BLOCKS
/* Handle the write-behind checking */
static void write_behind_check(int dev)
{
  cli();
  if (scsi_tapes[dev].buffer->last_result < 0) {
    scsi_tapes[dev].buffer->writing = (- scsi_tapes[dev].buffer->writing);
    sleep_on( &scsi_tapes[dev].waiting );
    scsi_tapes[dev].buffer->writing = (- scsi_tapes[dev].buffer->writing);
  }
  sti();

  if (scsi_tapes[dev].buffer->writing < scsi_tapes[dev].buffer->buffer_bytes)
    memcpy(scsi_tapes[dev].buffer->b_data,
	   scsi_tapes[dev].buffer->b_data + scsi_tapes[dev].buffer->writing,
	   scsi_tapes[dev].buffer->buffer_bytes -
	   scsi_tapes[dev].buffer->writing);
  scsi_tapes[dev].buffer->buffer_bytes -= scsi_tapes[dev].buffer->writing;
  scsi_tapes[dev].buffer->writing = 0;

  return;
}
#endif


/* Flush the write buffer */
static int flush_write_buffer(int dev)
{
  int offset, transfer, blks;
  int result;
  unsigned char cmd[10];
  Scsi_Cmnd *SCpnt;

#if ST_WRITE_THRESHOLD_BLOCKS < ST_BUFFER_BLOCKS
  if (scsi_tapes[dev].buffer->writing) {
    write_behind_check(dev);
    if (scsi_tapes[dev].buffer->last_result) {
#ifdef DEBUG
      printk("st%d: Async write error %x.\n", dev,
	     scsi_tapes[dev].buffer->last_result);
#endif
      return (-EIO);
    }
  }
#endif

  result = 0;
  if (scsi_tapes[dev].dirty==1) {
    SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);

    offset = scsi_tapes[dev].buffer->buffer_bytes;
    transfer = ((offset + scsi_tapes[dev].block_size - 1) /
		scsi_tapes[dev].block_size) * scsi_tapes[dev].block_size;
#ifdef DEBUG
    printk("st%d: Flushing %d bytes.\n", dev, transfer);
#endif
    memset(scsi_tapes[dev].buffer->b_data + offset, 0, transfer - offset);

    SCpnt->sense_buffer[0] = 0;
    memset(cmd, 0, 10);
    cmd[0] = WRITE_6;
    cmd[1] = 1;
    blks = transfer / scsi_tapes[dev].block_size;
    cmd[2] = blks >> 16;
    cmd[3] = blks >> 8;
    cmd[4] = blks;
    SCpnt->request.dev = dev;
    scsi_do_cmd (SCpnt,
		 (void *) cmd, scsi_tapes[dev].buffer->b_data, transfer,
		 st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

    if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

    if (SCpnt->result != 0) {
      printk("st%d: Error on flush:\n", dev);
#ifdef DEBUG
      st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);
#endif
      result = (-EIO);
    }
    else {
      scsi_tapes[dev].dirty = 0;
      scsi_tapes[dev].buffer->buffer_bytes = 0;
    }
    SCpnt->request.dev = -1;  /* Mark as not busy */
  }
  return result;
}


/* Flush the tape buffer. The tape will be positioned correctly unless
   seek_next is true. */
static int flush_buffer(struct inode * inode, struct file * filp,
			int seek_next)
{
  int dev;
  int backspace, result;

  dev = inode->i_rdev & 127;

  if (scsi_tapes[dev].rw == 2)  /* Writing */
    return flush_write_buffer(dev);

  backspace = (scsi_tapes[dev].buffer->buffer_bytes +
    scsi_tapes[dev].buffer->read_pointer) / scsi_tapes[dev].block_size -
      (scsi_tapes[dev].buffer->read_pointer + scsi_tapes[dev].block_size - 1) /
	scsi_tapes[dev].block_size;
  scsi_tapes[dev].buffer->buffer_bytes = 0;
  scsi_tapes[dev].buffer->read_pointer = 0;
  result = 0;
  if (!seek_next && backspace > 0) {
    result = st_int_ioctl(inode, filp, MTBSR, backspace);
    if (!result) {
      scsi_tapes[dev].eof = 0;
      scsi_tapes[dev].eof_hit = 0;
    }
  }
  return result;

}


/* Open the device */
static int scsi_tape_open(struct inode * inode, struct file * filp)
{
    int dev;
    unsigned short flags;
    int i;
    unsigned char cmd[10];
    Scsi_Cmnd * SCpnt;

    dev = inode->i_rdev & 127;
    if (dev >= NR_ST)
      return (-ENODEV);
    if (scsi_tapes[dev].in_use) {
      printk("st%d: Device already in use.\n", dev);
      return (-EBUSY);
    }

    /* Allocate buffer for this user */
    for (i=0; i < st_nbr_buffers; i++)
      if (!st_buffers[i]->in_use)
	break;
    if (i >= st_nbr_buffers) {
      printk("st%d: No free buffers.\n", dev);
      return (-EBUSY);
    }
    st_buffers[i]->in_use = 1;
    st_buffers[i]->writing = 0;
    scsi_tapes[dev].buffer = st_buffers[i];
    scsi_tapes[dev].in_use = 1;

    flags = filp->f_flags;
    scsi_tapes[dev].write_prot = ((flags & O_ACCMODE) == O_RDONLY);

    scsi_tapes[dev].dirty = 0;
    scsi_tapes[dev].rw = 0;
    scsi_tapes[dev].eof = 0;
    scsi_tapes[dev].eof_hit = 0;

    SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);
    if (!SCpnt) {
      printk("st%d: Tape request not allocated", dev);
      return (-EBUSY);
    }

    SCpnt->sense_buffer[0]=0;
    memset ((void *) &cmd[0], 0, 10);
    cmd[0] = TEST_UNIT_READY;
    SCpnt->request.dev = dev;
    scsi_do_cmd(SCpnt,
                (void *) cmd, (void *) scsi_tapes[dev].buffer->b_data,
                ST_BLOCK_SIZE, st_sleep_done, ST_LONG_TIMEOUT, MAX_RETRIES);

    if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

    if ((SCpnt->sense_buffer[0] & 0x70) == 0x70 &&
	(SCpnt->sense_buffer[2] & 0x0f) == UNIT_ATTENTION) { /* New media? */
#ifdef DEBUG
      decode_sns(dev, SCpnt->sense_buffer);
#endif
      SCpnt->sense_buffer[0]=0;
      memset ((void *) &cmd[0], 0, 10);
      cmd[0] = TEST_UNIT_READY;
      SCpnt->request.dev = dev;
      scsi_do_cmd(SCpnt,
		  (void *) cmd, (void *) scsi_tapes[dev].buffer->b_data,
		  ST_BLOCK_SIZE, st_sleep_done, ST_LONG_TIMEOUT, MAX_RETRIES);

      if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );
    }

    if (SCpnt->result != 0) {
#ifdef DEBUG
      decode_sns(dev, SCpnt->sense_buffer);
#endif
      if ((SCpnt->sense_buffer[0] & 0x70) == 0x70 &&
	  (SCpnt->sense_buffer[2] & 0x0f) == NO_TAPE)
	printk("st%d: No tape.\n", dev);
      else
	printk("st%d: Error %x.\n", dev, SCpnt->result);
      scsi_tapes[dev].buffer->in_use = 0;
      scsi_tapes[dev].in_use = 0;
      SCpnt->request.dev = -1;  /* Mark as not busy */
      return (-EIO);
    }

    SCpnt->sense_buffer[0]=0;
    memset ((void *) &cmd[0], 0, 10);
    cmd[0] = READ_BLOCK_LIMITS;
    SCpnt->request.dev = dev;
    scsi_do_cmd(SCpnt,
                (void *) cmd, (void *) scsi_tapes[dev].buffer->b_data,
                ST_BLOCK_SIZE, st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

    if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

    if (!SCpnt->result && !SCpnt->sense_buffer[0]) {
      scsi_tapes[dev].max_block = (scsi_tapes[dev].buffer->b_data[1] << 16) |
	(scsi_tapes[dev].buffer->b_data[2] << 8) | scsi_tapes[dev].buffer->b_data[3];
      scsi_tapes[dev].min_block = (scsi_tapes[dev].buffer->b_data[4] << 8) |
	scsi_tapes[dev].buffer->b_data[5];
#ifdef DEBUG
      printk("st%d: Block limits %d - %d bytes.\n", dev, scsi_tapes[dev].min_block,
	     scsi_tapes[dev].max_block);
#endif
    }
    else {
      scsi_tapes[dev].min_block = scsi_tapes[dev].max_block = (-1);
#ifdef DEBUG
      printk("st%d: Can't read block limits.\n", dev);
#endif
    }

    SCpnt->sense_buffer[0]=0;
    memset ((void *) &cmd[0], 0, 10);
    cmd[0] = MODE_SENSE;
    cmd[4] = 12;
    SCpnt->request.dev = dev;
    scsi_do_cmd(SCpnt,
                (void *) cmd, (void *) scsi_tapes[dev].buffer->b_data,
                ST_BLOCK_SIZE, st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

    if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

    i = st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);
    if (i) {
#ifdef DEBUG
      printk("st%d: No Mode Sense.\n", dev);
#endif
      scsi_tapes[dev].buffer->b_data[2] =
      scsi_tapes[dev].buffer->b_data[3] = 0;
    }
    SCpnt->request.dev = -1;  /* Mark as not busy */

#ifdef DEBUG
    printk("st%d: Mode sense. Length %d, medium %x, WBS %x, BLL %d\n", dev,
	   scsi_tapes[dev].buffer->b_data[0], scsi_tapes[dev].buffer->b_data[1],
	   scsi_tapes[dev].buffer->b_data[2], scsi_tapes[dev].buffer->b_data[3]);
#endif

    if (scsi_tapes[dev].buffer->b_data[3] >= 8) {
      scsi_tapes[dev].block_size = scsi_tapes[dev].buffer->b_data[9] * 65536 +
	scsi_tapes[dev].buffer->b_data[10] * 256 + scsi_tapes[dev].buffer->b_data[11];
#ifdef DEBUG
      printk("st%d: Density %x, tape length: %x, blocksize: %d\n", dev,
	     scsi_tapes[dev].buffer->b_data[4], scsi_tapes[dev].buffer->b_data[5] *
	     65536 + scsi_tapes[dev].buffer->b_data[6] * 256 +
	     scsi_tapes[dev].buffer->b_data[7], scsi_tapes[dev].buffer->b_data[9] *
	     65536 + scsi_tapes[dev].buffer->b_data[10] * 256 +
	     scsi_tapes[dev].buffer->b_data[11]);
#endif
      if (scsi_tapes[dev].block_size > ST_BUFFER_SIZE) {
	printk("st%d: Blocksize %d too large for buffer.\n", dev,
	       scsi_tapes[dev].block_size);
	scsi_tapes[dev].buffer->in_use = 0;
	scsi_tapes[dev].in_use = 0;
	return (-EIO);
      }

      if (scsi_tapes[dev].block_size == 0) {
	printk("st%d: Fixing block size to 512 bytes.\n", dev);
	if (st_int_ioctl(inode, filp, MTSETBLK, ST_BLOCK_SIZE)) {
	  printk("st%d: Can't set fixed block size.\n", dev);
	  scsi_tapes[dev].buffer->in_use = 0;
	  scsi_tapes[dev].in_use = 0;
	  return (-EIO);
	}
	scsi_tapes[dev].block_size = ST_BLOCK_SIZE;
      }
    }
    else
      scsi_tapes[dev].block_size = ST_BLOCK_SIZE;

    scsi_tapes[dev].buffer->buffer_blocks =
      ST_BUFFER_SIZE / scsi_tapes[dev].block_size;
    scsi_tapes[dev].buffer->buffer_size =
      scsi_tapes[dev].buffer->buffer_blocks * scsi_tapes[dev].block_size;
    scsi_tapes[dev].buffer->buffer_bytes = scsi_tapes[dev].buffer->read_pointer = 0;

#ifdef DEBUG
    printk("st%d: Block size: %d, buffer size: %d (%d blocks).\n", dev,
	   scsi_tapes[dev].block_size, scsi_tapes[dev].buffer->buffer_size,
	   scsi_tapes[dev].buffer->buffer_blocks);
#endif

    if (scsi_tapes[dev].buffer->b_data[2] & 0x80) {
      scsi_tapes[dev].write_prot = 1;
#ifdef DEBUG
      printk( "st%d: Write protected\n", dev);
#endif
    }

    return 0;
}


/* Close the device*/
static void scsi_tape_close(struct inode * inode, struct file * filp)
{
    int dev;
    int result;
    int rewind;
    static unsigned char cmd[10];
    Scsi_Cmnd * SCpnt;
   
    dev = inode->i_rdev;
    rewind = (dev & 0x80) == 0;
    dev = dev & 127;

    if ( scsi_tapes[dev].rw == 2) {

      result = flush_write_buffer(dev);

#ifdef DEBUG
      printk("st%d: File length %d bytes.\n", dev, filp->f_pos);
#endif

      if (!result) {
	SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);

	SCpnt->sense_buffer[0] = 0;
	memset(cmd, 0, 10);
	cmd[0] = WRITE_FILEMARKS;
	cmd[4] = 1;
	SCpnt->request.dev = dev;
	scsi_do_cmd( SCpnt,
		    (void *) cmd, (void *) scsi_tapes[dev].buffer->b_data,
		    ST_BLOCK_SIZE, st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

	if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

	if (SCpnt->result) {
	  printk("st%d: Error on write filemark:\n", dev);
#ifdef DEBUG
	  st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);
#endif
	}
	SCpnt->request.dev = -1;  /* Mark as not busy */
      }

#ifdef DEBUG
      printk("st%d: Buffer flushed, EOF written\n", dev);
#endif
    }
    else if (!rewind) {
      if ((scsi_tapes[dev].eof == 1) && !scsi_tapes[dev].eof_hit)
	st_int_ioctl(inode, filp, MTBSF, 1); /* Back over the EOF hit */
#ifdef ST_IN_FILE_POS
      flush_buffer(inode, filp, 0);
#endif
    }

    if (rewind)
      st_int_ioctl(inode, filp, MTREW, 1);

    scsi_tapes[dev].buffer->in_use = 0;
    scsi_tapes[dev].in_use = 0;

    return;
}


/* Write command */
int st_write(struct inode * inode, struct file * filp, char * buf, int count)
{
    int dev;
    int total, do_count, blks, retval;
    static unsigned char cmd[10];
    char *b_point;
    Scsi_Cmnd * SCpnt;

    dev = inode->i_rdev & 127;
#ifdef DEBUG
    if (!scsi_tapes[dev].in_use) {
      printk("st%d: Incorrect device.\n", dev);
      return (-EIO);
    }
#endif

    if (scsi_tapes[dev].write_prot)
      return (-EACCES);

    if (scsi_tapes[dev].rw == 1) {
      retval = flush_buffer(inode, filp, 0);
      if (retval)
	return retval;
      scsi_tapes[dev].rw = 2;
    }

#if ST_WRITE_THRESHOLD_BLOCKS < ST_BUFFER_BLOCKS
    if (scsi_tapes[dev].buffer->writing) {
      write_behind_check(dev);
      if (scsi_tapes[dev].buffer->last_result) {
#ifdef DEBUG
	printk("st%d: Async write error %x.\n", dev,
	       scsi_tapes[dev].buffer->last_result);
#endif
	/*if (scsi_tapes[dev].buffer->last_result = 0x7fffffff)
	  retval = (-ENOSPC);
	else */
	  retval = (-EIO);
	return retval;
      }
    }
#endif

    SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);

    total = count;

    memset(cmd, 0, 10);
    cmd[0] = WRITE_6;
    cmd[1] = 1;

    scsi_tapes[dev].rw = 2;

    b_point = buf;
    while((scsi_tapes[dev].buffer->buffer_bytes + count) >=
	  scsi_tapes[dev].buffer->buffer_size) {
      do_count = scsi_tapes[dev].buffer->buffer_size -
	scsi_tapes[dev].buffer->buffer_bytes;
      memcpy_fromfs(scsi_tapes[dev].buffer->b_data +
		    scsi_tapes[dev].buffer->buffer_bytes,b_point,do_count);

      blks = scsi_tapes[dev].buffer->buffer_blocks;
      cmd[2] = blks >> 16;
      cmd[3] = blks >> 8;
      cmd[4] = blks;
      SCpnt->sense_buffer[0] = 0;
      SCpnt->request.dev = dev;
      scsi_do_cmd (SCpnt,
		   (void *) cmd, scsi_tapes[dev].buffer->b_data,
		   scsi_tapes[dev].buffer->buffer_size,
		   st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

      if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

      if (SCpnt->result || SCpnt->sense_buffer[0] != 0) {
#ifdef DEBUG
	printk("st%d: Error on write:\n", dev);
	st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);
#endif
	if ((SCpnt->sense_buffer[0] & 0x70) == 0x70 &&
	    (SCpnt->sense_buffer[2] & 0x40))
	  retval = (-ENOSPC); /* EOM */
	else
	  retval = (-EIO);
	SCpnt->request.dev = -1;  /* Mark as not busy */
	if (count < total)
	  return total - count;
	else
	  return retval;
      }
      filp->f_pos += do_count;
      b_point += do_count;
      count -= do_count;
      scsi_tapes[dev].buffer->buffer_bytes = 0;
      scsi_tapes[dev].dirty = 0;
    }
    if (count != 0) {
      scsi_tapes[dev].dirty = 1;
      memcpy_fromfs(scsi_tapes[dev].buffer->b_data +
		    scsi_tapes[dev].buffer->buffer_bytes,b_point,count);
      filp->f_pos += count;
      scsi_tapes[dev].buffer->buffer_bytes += count;
      count = 0;
    }

    do_count = st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);
    if (do_count) {
      SCpnt->request.dev = -1;
      return do_count;
    }

#if ST_WRITE_THRESHOLD_BLOCKS < ST_BUFFER_BLOCKS
    if (scsi_tapes[dev].buffer->buffer_bytes >= ST_WRITE_THRESHOLD) {
      /* Schedule an asynchronous write */
      scsi_tapes[dev].buffer->writing = (scsi_tapes[dev].buffer->buffer_bytes /
	scsi_tapes[dev].block_size) * scsi_tapes[dev].block_size;
      scsi_tapes[dev].dirty = 0;

      blks = scsi_tapes[dev].buffer->writing / scsi_tapes[dev].block_size;
      cmd[2] = blks >> 16;
      cmd[3] = blks >> 8;
      cmd[4] = blks;
      SCpnt->result = scsi_tapes[dev].buffer->last_result = -1;
      SCpnt->sense_buffer[0] = 0;
      SCpnt->request.dev = dev;
      scsi_do_cmd (SCpnt,
		   (void *) cmd, scsi_tapes[dev].buffer->b_data,
		   scsi_tapes[dev].buffer->writing,
		   st_sleep_done, ST_TIMEOUT, MAX_RETRIES);
    }
    else
#endif
      SCpnt->request.dev = -1;  /* Mark as not busy */

    return( total);
}   


/* Read command */
int st_read(struct inode * inode, struct file * filp, char * buf, int count)
{
    int dev;
    int total;
    int transfer, blks;
    static unsigned char cmd[10];
    Scsi_Cmnd * SCpnt;

    dev = inode->i_rdev & 127;
#ifdef DEBUG
    if (!scsi_tapes[dev].in_use) {
      printk("st%d: Incorrect device.\n", dev);
      return (-EIO);
    }
#endif

    if (scsi_tapes[dev].rw == 2) {
      transfer = flush_buffer(inode, filp, 0);
      if (transfer)
	return transfer;
      scsi_tapes[dev].rw = 1;
    }

#ifdef DEBUG
    if (scsi_tapes[dev].eof)
      printk("st%d: EOF flag up. Bytes %d\n", dev,
	     scsi_tapes[dev].buffer->buffer_bytes);
#endif
    if ((scsi_tapes[dev].buffer->buffer_bytes == 0) &&
	scsi_tapes[dev].eof == 2)  /* EOM or Blank Check */
      return (-EIO);

    scsi_tapes[dev].rw = 1;

    SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);

    for (total = 0; total < count; ) {

      if (scsi_tapes[dev].buffer->buffer_bytes == 0 &&
	  scsi_tapes[dev].eof == 0) {

	memset(cmd, 0, 10);
	cmd[0] = READ_6;
	cmd[1] = 1;
	blks = scsi_tapes[dev].buffer->buffer_blocks;
	cmd[2] = blks >> 16;
	cmd[3] = blks >> 8;
	cmd[4] = blks;

	SCpnt->sense_buffer[0] = 0;
	SCpnt->request.dev = dev;
	scsi_do_cmd (SCpnt,
		     (void *) cmd, scsi_tapes[dev].buffer->b_data,
		     scsi_tapes[dev].buffer->buffer_size,
		     st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

	if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

	scsi_tapes[dev].buffer->read_pointer = 0;
	scsi_tapes[dev].eof_hit = 0;

	if (SCpnt->result != 0 || SCpnt->sense_buffer[0] != 0) {
#ifdef DEBUG
	  printk("st%d: Sense: %2x %2x %2x %2x %2x %2x %2x %2x\n", dev,
		 SCpnt->sense_buffer[0], SCpnt->sense_buffer[1],
		 SCpnt->sense_buffer[2], SCpnt->sense_buffer[3],
		 SCpnt->sense_buffer[4], SCpnt->sense_buffer[5],
		 SCpnt->sense_buffer[6], SCpnt->sense_buffer[7]);
#endif
	  if ((SCpnt->sense_buffer[0] & 0x70) == 0x70) { /* extended sense */

	    if ((SCpnt->sense_buffer[2] & 0xe0) != 0) { /* EOF, EOM, or ILI */
	      transfer = (SCpnt->sense_buffer[3] << 24) |
		(SCpnt->sense_buffer[4] << 16) |
		  (SCpnt->sense_buffer[5] << 8) | SCpnt->sense_buffer[6];

	      if (SCpnt->sense_buffer[2] & 0x20) {
		printk("st%d: Incorrect block size.\n", dev);
		SCpnt->request.dev = -1;  /* Mark as not busy */
		return (-EIO);
	      }
	      else if (SCpnt->sense_buffer[2] & 0x40) {
		scsi_tapes[dev].eof = 2; /* What should be done at EOM ? */
		scsi_tapes[dev].buffer->buffer_bytes =
		  (scsi_tapes[dev].buffer->buffer_blocks - transfer) *
		    scsi_tapes[dev].block_size;
#ifdef DEBUG
		printk("st%d: EOM detected (%d blocks read).\n", dev,
		       scsi_tapes[dev].buffer->buffer_blocks - transfer);
#endif
	      }
	      else if (SCpnt->sense_buffer[2] & 0x80) {
		scsi_tapes[dev].eof = 1;
		scsi_tapes[dev].buffer->buffer_bytes =
		  (scsi_tapes[dev].buffer->buffer_blocks - transfer) *
		    scsi_tapes[dev].block_size;
#ifdef DEBUG
		printk("st%d: EOF detected (%d blocks read, transferred %d bytes).\n",
		       dev, scsi_tapes[dev].buffer->buffer_blocks - transfer, total);
#endif
	      } /* end of EOF, EOM, ILI test */
	    }
	    else { /* nonzero sense key */
#ifdef DEBUG
	      printk("st%d: Tape error. Sense key %x\n", dev,
		     SCpnt->sense_buffer[2] & 0x0f);
	      decode_sns(dev, SCpnt->sense_buffer);
#endif
	      SCpnt->request.dev = -1;
	      if (total)
		return total;
	      else
		return -EIO;
	    }
	  }
	  else {
	    transfer = st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);
	    SCpnt->request.dev = -1;  /* Mark as not busy */
	    return transfer;
	  }
	}
	else
	  scsi_tapes[dev].buffer->buffer_bytes =
	    scsi_tapes[dev].buffer->buffer_size;
      } /* if (scsi_tapes[dev].buffer->buffer_bytes == 0 &&
	   scsi_tapes[dev].eof == 0) */

      if (scsi_tapes[dev].buffer->buffer_bytes > 0) {
#ifdef DEBUG
	if (scsi_tapes[dev].eof)
	  printk("st%d: EOF up. Left %d, needed %d.\n", dev,
		 scsi_tapes[dev].buffer->buffer_bytes, count - total);
#endif
	transfer = scsi_tapes[dev].buffer->buffer_bytes < count - total ?
	  scsi_tapes[dev].buffer->buffer_bytes : count - total;
	memcpy_tofs(buf, scsi_tapes[dev].buffer->b_data +
		    scsi_tapes[dev].buffer->read_pointer,transfer);
	filp->f_pos += transfer;
	buf += transfer;
	total += transfer;
	scsi_tapes[dev].buffer->buffer_bytes -= transfer;
	scsi_tapes[dev].buffer->read_pointer += transfer;
      }
      else if (scsi_tapes[dev].eof) {
	scsi_tapes[dev].eof_hit = 1;
	SCpnt->request.dev = -1;  /* Mark as not busy */
	if (total == 0 && scsi_tapes[dev].eof == 1)
	  scsi_tapes[dev].eof = 0;
	if (total == 0 && scsi_tapes[dev].eof == 2)
	  return (-EIO);
	return total;
      }

    } /* for (total = 0; total < count; ) */

    SCpnt->request.dev = -1;  /* Mark as not busy */

    return total;
}


/* Internal ioctl function */
static int st_int_ioctl(struct inode * inode,struct file * file,
	     unsigned int cmd_in, unsigned long arg)
{
   int dev = inode->i_rdev;
   int timeout = ST_LONG_TIMEOUT;
   long ltmp;
   int ioctl_result;
   unsigned char cmd[10];
   Scsi_Cmnd * SCpnt;

   dev = dev & 127;

   memset(cmd, 0, 10);
   switch (cmd_in) {
     case MTFSF:
     case MTFSFM:
       cmd[0] = SPACE;
       cmd[1] = 0x01; /* Space FileMarks */
       cmd[2] = (arg >> 16);
       cmd[3] = (arg >> 8);
       cmd[4] = arg;
#ifdef DEBUG
       printk("st%d: Spacing tape forward %d files.\n", dev,
	      cmd[2] * 65536 + cmd[3] * 256 + cmd[4]);
#endif
       break; 
     case MTBSF:
     case MTBSFM:
       cmd[0] = SPACE;
       cmd[1] = 0x01; /* Space FileMarks */
       ltmp = (-arg);
       cmd[2] = (ltmp >> 16);
       cmd[3] = (ltmp >> 8);
       cmd[4] = ltmp;
#ifdef DEBUG
       if (cmd[2] & 0x80)
	 ltmp = 0xff000000;
       ltmp = ltmp | (cmd[2] << 16) | (cmd[3] << 8) | cmd[4];
       printk("st%d: Spacing tape backward %d files.\n", dev, (-ltmp));
#endif
       break; 
      case MTFSR:
       cmd[0] = SPACE;
       cmd[1] = 0x00; /* Space Blocks */
       cmd[2] = (arg >> 16);
       cmd[3] = (arg >> 8);
       cmd[4] = arg;
#ifdef DEBUG
       printk("st%d: Spacing tape forward %d blocks.\n", dev,
	      cmd[2] * 65536 + cmd[3] * 256 + cmd[4]);
#endif
       break; 
     case MTBSR:
       cmd[0] = SPACE;
       cmd[1] = 0x00; /* Space Blocks */
       ltmp = (-arg);
       cmd[2] = (ltmp >> 16);
       cmd[3] = (ltmp >> 8);
       cmd[4] = ltmp;
#ifdef DEBUG
       if (cmd[2] & 0x80)
	 ltmp = 0xff000000;
       ltmp = ltmp | (cmd[2] << 16) | (cmd[3] << 8) | cmd[4];
       printk("st%d: Spacing tape backward %d blocks.\n", dev, (-ltmp));
#endif
       break; 
     case MTWEOF:
       if (scsi_tapes[dev].write_prot)
	 return (-EACCES);
       cmd[0] = WRITE_FILEMARKS;
       cmd[2] = (arg >> 16);
       cmd[3] = (arg >> 8);
       cmd[4] = arg;
       timeout = ST_TIMEOUT;
#ifdef DEBUG
       printk("st%d: Writing %d filemarks.\n", dev,
	      cmd[2] * 65536 + cmd[3] * 256 + cmd[4]);
#endif
       break; 
     case MTREW:
       cmd[0] = REZERO_UNIT;
#ifdef ST_NOWAIT
       cmd[1] = 1;  /* Don't wait for completion */
       timeout = ST_TIMEOUT;
#endif
#ifdef DEBUG
       printk("st%d: Rewinding tape.\n", dev);
#endif
       break; 
     case MTOFFL:
       cmd[0] = START_STOP;
#ifdef ST_NOWAIT
       cmd[1] = 1;  /* Don't wait for completion */
       timeout = ST_TIMEOUT;
#endif
#ifdef DEBUG
       printk("st%d: Unloading tape.\n", dev);
#endif
       break; 
     case MTNOP:
#ifdef DEBUG
       printk("st%d: No op on tape.\n", dev);
#endif
       return 0;  /* Should do something ? */
       break;
     case MTRETEN:
       cmd[0] = START_STOP;
#ifdef ST_NOWAIT
       cmd[1] = 1;  /* Don't wait for completion */
       timeout = ST_TIMEOUT;
#endif
       cmd[4] = 3;
#ifdef DEBUG
       printk("st%d: Retensioning tape.\n", dev);
#endif
       break; 
     case MTEOM:
       cmd[0] = SPACE;
       cmd[1] = 3;
#ifdef DEBUG
       printk("st%d: Spacing to end of recorded medium.\n", dev);
#endif
       break; 
     case MTERASE:
       if (scsi_tapes[dev].write_prot)
	 return (-EACCES);
       cmd[0] = ERASE;
       cmd[1] = 1;  /* To the end of tape */
#ifdef DEBUG
       printk("st%d: Erasing tape.\n", dev);
#endif
       break;
     case MTSEEK:
       if (scsi_tapes[dev].device->scsi_level < SCSI_2) {
	 cmd[0] = QFA_SEEK_BLOCK;
	 cmd[2] = (arg >> 16);
	 cmd[3] = (arg >> 8);
	 cmd[4] = arg;
	 cmd[5] = 0;
       }
       else {
	 cmd[0] = SEEK_10;
	 cmd[1] = 4;
	 cmd[3] = (arg >> 24);
	 cmd[4] = (arg >> 16);
	 cmd[5] = (arg >> 8);
	 cmd[6] = arg;
       }
#ifdef ST_NOWAIT
       cmd[1] |= 1;  /* Don't wait for completion */
       timeout = ST_TIMEOUT;
#endif
#ifdef DEBUG
       printk("st%d: Seeking tape to block %d.\n", dev, arg);
#endif
       break;
     case MTSETBLK:  /* Set block length */
     case MTSETDENSITY: /* Set tape density */
       if (scsi_tapes[dev].dirty || scsi_tapes[dev].buffer->buffer_bytes != 0)
	 return (-EIO);   /* Not allowed if data in buffer */
       if (cmd_in == MTSETBLK &&
	   (arg < scsi_tapes[dev].min_block || arg > scsi_tapes[dev].max_block ||
	    arg > ST_BUFFER_SIZE)) {
	 printk("st%d: Illegal block size.\n", dev);
	 return (-EINVAL);
       }
       cmd[0] = MODE_SELECT;
       cmd[4] = 12;

       memset(scsi_tapes[dev].buffer->b_data, 0, 12);
       scsi_tapes[dev].buffer->b_data[2] = 0x10;  /* buffered mode */
       scsi_tapes[dev].buffer->b_data[3] = 8;     /* block descriptor length */
       if (cmd_in == MTSETBLK)
	 ltmp = arg;
       else {
	 scsi_tapes[dev].buffer->b_data[4] = arg;
	 ltmp = scsi_tapes[dev].block_size;
       }
       scsi_tapes[dev].buffer->b_data[9] = (ltmp >> 16);
       scsi_tapes[dev].buffer->b_data[10] = (ltmp >> 8);
       scsi_tapes[dev].buffer->b_data[11] = ltmp;
       timeout = ST_TIMEOUT;
#ifdef DEBUG
       if (cmd_in == MTSETBLK)
	 printk("st%d: Setting block size to %d bytes.\n", dev,
		scsi_tapes[dev].buffer->b_data[9] * 65536 +
		scsi_tapes[dev].buffer->b_data[10] * 256 +
		scsi_tapes[dev].buffer->b_data[11]);
       else
	 printk("st%d: Setting density code to %x.\n", dev,
		scsi_tapes[dev].buffer->b_data[4]);
#endif
       break;
     default:
       printk("st%d: Unknown st_ioctl command %x.\n", dev, cmd_in);
       return (-ENOSYS);
     }

   SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);
   SCpnt->sense_buffer[0] = 0;
   SCpnt->request.dev = dev;
   scsi_do_cmd(SCpnt,
	       (void *) cmd, (void *) scsi_tapes[dev].buffer->b_data, ST_BLOCK_SIZE,
	       st_sleep_done, timeout, MAX_RETRIES);

   if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );

   SCpnt->request.dev = -1;  /* Mark as not busy */

   ioctl_result = st_chk_result(dev, SCpnt->result, SCpnt->sense_buffer);

   if (!ioctl_result) {
     if (cmd_in == MTBSFM)
       ioctl_result = st_int_ioctl(inode, file, MTFSF, 1);
     else if (cmd_in == MTFSFM)
       ioctl_result = st_int_ioctl(inode, file, MTBSF, 1);
     else if (cmd_in == MTSETBLK) {
       scsi_tapes[dev].block_size = arg;
       scsi_tapes[dev].buffer->buffer_blocks =
	 ST_BUFFER_SIZE / scsi_tapes[dev].block_size;
       scsi_tapes[dev].buffer->buffer_size =
	 scsi_tapes[dev].buffer->buffer_blocks * scsi_tapes[dev].block_size;
       scsi_tapes[dev].buffer->buffer_bytes =
	 scsi_tapes[dev].buffer->read_pointer = 0;
     }
     if (cmd_in == MTEOM || cmd_in == MTWEOF) {
       scsi_tapes[dev].eof = 2;
       scsi_tapes[dev].eof_hit = 0;
     }
     else if (cmd_in != MTSETBLK && cmd_in != MTNOP) {
       scsi_tapes[dev].eof = 0;
       scsi_tapes[dev].eof_hit = 0;
     }
   }

   return ioctl_result ;
}



/* The ioctl command */
static int st_ioctl(struct inode * inode,struct file * file,
	     unsigned int cmd_in, unsigned long arg)
{
   int dev = inode->i_rdev;
   int i, cmd, result;
   struct mtop mtc;
   struct mtpos mt_pos;
   unsigned char scmd[10];
   Scsi_Cmnd *SCpnt;

   dev = dev & 127;
#ifdef DEBUG
   if (!scsi_tapes[dev].in_use) {
     printk("st%d: Incorrect device.\n", dev);
     return (-EIO);
   }
#endif

   cmd = cmd_in & IOCCMD_MASK;
   if (cmd == (MTIOCTOP & IOCCMD_MASK)) {

     if (((cmd_in & IOCSIZE_MASK) >> IOCSIZE_SHIFT) != sizeof(mtc))
       return (-EINVAL);

     i = verify_area(VERIFY_WRITE, (void *)arg, sizeof(mtc));
     if (i)
        return i;

     memcpy_fromfs((char *) &mtc, (char *)arg, sizeof(struct mtop));

     i = flush_buffer(inode, file, mtc.mt_op == MTSEEK ||
		      mtc.mt_op == MTREW || mtc.mt_op == MTOFFL ||
		      mtc.mt_op == MTRETEN || mtc.mt_op == MTEOM);
     if (i < 0)
       return i;

     return st_int_ioctl(inode, file, mtc.mt_op, mtc.mt_count);
   }
   else if (cmd == (MTIOCGET & IOCCMD_MASK)) {

     if (((cmd_in & IOCSIZE_MASK) >> IOCSIZE_SHIFT) != sizeof(struct mtget))
       return (-EINVAL);
     i = verify_area(VERIFY_WRITE, (void *)arg, sizeof(struct mtget));
     if (i)
       return i;

     memcpy_tofs((char *)arg, (char *)scsi_tapes[dev].buffer->mt_status,
		 sizeof(struct mtget));
     return 0;
   }
   else if (cmd == (MTIOCPOS & IOCCMD_MASK)) {
#ifdef DEBUG
     printk("st%d: get tape position.\n", dev);
#endif
     if (((cmd_in & IOCSIZE_MASK) >> IOCSIZE_SHIFT) != sizeof(struct mtpos))
       return (-EINVAL);

     i = flush_buffer(inode, file, 0);
     if (i < 0)
       return i;

     i = verify_area(VERIFY_WRITE, (void *)arg, sizeof(struct mtpos));
     if (i)
       return i;

     SCpnt = allocate_device(NULL, scsi_tapes[dev].device->index, 1);

     SCpnt->sense_buffer[0]=0;
     memset (scmd, 0, 10);
     if (scsi_tapes[dev].device->scsi_level < SCSI_2) {
       scmd[0] = QFA_REQUEST_BLOCK;
       scmd[4] = 3;
     }
     else {
       scmd[0] = READ_POSITION;
       scmd[1] = 1;
     }
     SCpnt->request.dev = dev;
     SCpnt->sense_buffer[0] = 0;
     scsi_do_cmd(SCpnt,
		 (void *) scmd, (void *) scsi_tapes[dev].buffer->b_data,
		 ST_BLOCK_SIZE, st_sleep_done, ST_TIMEOUT, MAX_RETRIES);

     if (SCpnt->request.dev == dev) sleep_on( &scsi_tapes[dev].waiting );
     
     if (SCpnt->result || SCpnt->sense_buffer[0]) {
       mt_pos.mt_blkno = (-1);
#ifdef DEBUG
       printk("st%d: Can't read tape position.\n", dev);
#endif
       result = (-EIO);
     }
     else {
       result = 0;
       if (scsi_tapes[dev].device->scsi_level < SCSI_2)
	 mt_pos.mt_blkno = (scsi_tapes[dev].buffer->b_data[0] << 16) 
	   + (scsi_tapes[dev].buffer->b_data[1] << 8) 
	     + scsi_tapes[dev].buffer->b_data[2];
       else
	 mt_pos.mt_blkno = (scsi_tapes[dev].buffer->b_data[4] << 24)
	   + (scsi_tapes[dev].buffer->b_data[5] << 16) 
	     + (scsi_tapes[dev].buffer->b_data[6] << 8) 
	       + scsi_tapes[dev].buffer->b_data[7];

     }

     SCpnt->request.dev = -1;  /* Mark as not busy */

     memcpy_tofs((char *)arg, (char *) (&mt_pos), sizeof(struct mtpos));
     return result;
   }
   else
     return scsi_ioctl(scsi_tapes[dev].device, cmd_in, (void *) arg);
}



static struct file_operations st_fops = {
   NULL,            /* lseek - default */
   st_read,         /* read - general block-dev read */
   st_write,        /* write - general block-dev write */
   NULL,            /* readdir - bad */
   NULL,            /* select */
   st_ioctl,        /* ioctl */
   NULL,            /* mmap */
   scsi_tape_open,  /* open */
   scsi_tape_close  /* release */
};

void st_attach(Scsi_Device * SDp){
  scsi_tapes[NR_ST++].device = SDp;
  if(NR_ST > MAX_ST) panic ("scsi_devices corrupt (st)");
};

unsigned long st_init1(unsigned long mem_start, unsigned long mem_end){
  scsi_tapes = (Scsi_Tape *) mem_start;
  mem_start += MAX_ST * sizeof(Scsi_Tape);
  return mem_start;
};

/* Driver initialization */
unsigned long st_init(unsigned long mem_start, unsigned long mem_end)
{
  int i;

  if (register_chrdev(MAJOR_NR,"st",&st_fops)) {
    printk("Unable to get major %d for SCSI tapes\n",MAJOR_NR);
    return mem_start;
  }
  if (NR_ST == 0) return mem_start;

#ifdef DEBUG
  printk("st: Init tape.\n");
#endif

  for (i=0; i < NR_ST; ++i) {
    scsi_tapes[i].capacity = 0xfffff;
    scsi_tapes[i].dirty = 0;
    scsi_tapes[i].rw = 0;
    scsi_tapes[i].eof = 0;
    scsi_tapes[i].waiting = NULL;
    scsi_tapes[i].in_use = 0;
  }


  /* Allocate the buffers */
  if (NR_ST == 1)
    st_nbr_buffers = 1;
  else
    st_nbr_buffers = 2;
  for (i=0; i < st_nbr_buffers; i++) {
    st_buffers[i] = (ST_buffer *) mem_start;
#ifdef DEBUG
    printk("st: Buffer address: %x\n", st_buffers[i]);
#endif
    mem_start += sizeof(ST_buffer) - 1 + ST_BUFFER_BLOCKS * ST_BLOCK_SIZE;
    st_buffers[i]->mt_status = (struct mtget *) mem_start;
    mem_start += sizeof(struct mtget);
    st_buffers[i]->in_use = 0;
    st_buffers[i]->writing = 0;

    /* "generic" status */
    memset((void *) st_buffers[i]->mt_status, 0, sizeof(struct mtget));
    st_buffers[i]->mt_status->mt_type = MT_ISSCSI1;
  }

  return mem_start;
}
