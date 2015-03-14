
#ifndef _ST_H
	#define _ST_H
/*
	$Header: /usr/src/linux/kernel/blk_drv/scsi/RCS/st.h,v 1.1 1992/04/24 18:01:50 root Exp root $
*/

#ifndef _SCSI_H
#include "scsi.h"
#endif

typedef struct {
  int in_use;
  struct mtget * mt_status;
  int buffer_size;
  int buffer_blocks;
  int buffer_bytes;
  int read_pointer;
  int writing;
  int last_result;
  unsigned char b_data[1];
} ST_buffer;

typedef struct {
  unsigned capacity;
  struct wait_queue * waiting;
  Scsi_Device* device;
  unsigned dirty:1;
  unsigned rw:2;
  unsigned eof:2;
  unsigned write_prot:1;
  unsigned in_use:1;
  unsigned eof_hit:1;
  ST_buffer * buffer;
  int block_size;
  int min_block;
  int max_block;
  Scsi_Cmnd SCpnt;
} Scsi_Tape;


/* Positioning SCSI-commands for Tandberg, etc. drives */
#define	QFA_REQUEST_BLOCK	0x02
#define	QFA_SEEK_BLOCK		0x0c

#endif

