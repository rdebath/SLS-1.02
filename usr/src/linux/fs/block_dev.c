/*
 *  linux/fs/block_dev.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/locks.h>
#include <asm/segment.h>
#include <asm/system.h>

extern int *blk_size[];

int block_write(struct inode * inode, struct file * filp, char * buf, int count)
{
	int block = filp->f_pos >> BLOCK_SIZE_BITS;
	int offset = filp->f_pos & (BLOCK_SIZE-1);
	int chars;
	int written = 0;
	int size;
	unsigned int dev;
	struct buffer_head * bh;
	register char * p;

	dev = inode->i_rdev;
	if (blk_size[MAJOR(dev)])
		size = blk_size[MAJOR(dev)][MINOR(dev)];
	else
		size = 0x7fffffff;
	while (count>0) {
		if (block >= size)
			return written;
		chars = BLOCK_SIZE - offset;
		if (chars > count)
			chars=count;
		if (chars == BLOCK_SIZE)
			bh = getblk(dev, block, BLOCK_SIZE);
		else
			bh = breada(dev,block,block+1,block+2,-1);
		block++;
		if (!bh)
			return written?written:-EIO;
		p = offset + bh->b_data;
		offset = 0;
		filp->f_pos += chars;
		written += chars;
		count -= chars;
		memcpy_fromfs(p,buf,chars);
		p += chars;
		buf += chars;
		bh->b_uptodate = 1;
		bh->b_dirt = 1;
		brelse(bh);
	}
	return written;
}

#define NBUF 16

int block_read(struct inode * inode, struct file * filp, char * buf, int count)
{
	unsigned int block;
	unsigned int offset;
	int blocks, left;
	int bhrequest, uptodate;
	struct buffer_head ** bhb, ** bhe;
	struct buffer_head * buflist[NBUF];
	struct buffer_head * bhreq[NBUF];
	unsigned int chars;
	unsigned int size;
	unsigned int dev;
	int read;

	dev = inode->i_rdev;
	offset = filp->f_pos;
	if (blk_size[MAJOR(dev)])
		size = blk_size[MAJOR(dev)][MINOR(dev)] << BLOCK_SIZE_BITS;
	else
		size = 0x7fffffff;

	if (offset > size)
		left = 0;
	else
		left = size - offset;
	if (left > count)
		left = count;
	if (left <= 0)
		return 0;
	read = 0;
	block = offset >> BLOCK_SIZE_BITS;
	offset &= BLOCK_SIZE-1;
	size >>= BLOCK_SIZE_BITS;
	blocks = (left + offset + BLOCK_SIZE - 1) >> BLOCK_SIZE_BITS;
	bhb = bhe = buflist;
	if (filp->f_reada) {
		blocks += read_ahead[MAJOR(dev)] / (BLOCK_SIZE >> 9);
		if (block + blocks > size)
			blocks = size - block;
	}

	/* We do this in a two stage process.  We first try and request
	   as many blocks as we can, then we wait for the first one to
	   complete, and then we try and wrap up as many as are actually
	   done.  This routine is rather generic, in that it can be used
	   in a filesystem by substituting the appropriate function in
	   for getblk.

	   This routine is optimized to make maximum use of the various
	   buffers and caches. */

	do {
	        bhrequest = 0;
	        uptodate = 1;
		while (blocks) {
			--blocks;
			*bhb = getblk(dev, block++, BLOCK_SIZE);
			if (*bhb && !(*bhb)->b_uptodate) {
			        uptodate = 0;
			        bhreq[bhrequest++] = *bhb;
			}

			if (++bhb == &buflist[NBUF])
				bhb = buflist;

			/* If the block we have on hand is uptodate, go ahead
			   and complete processing. */
			if (uptodate)
				break;
			if (bhb == bhe)
				break;
		}

		/* Now request them all */
		if (bhrequest)
			ll_rw_block(READ, bhrequest, bhreq);

		do { /* Finish off all I/O that has actually completed */
			if (*bhe) {
				wait_on_buffer(*bhe);
				if (!(*bhe)->b_uptodate) {	/* read error? */
					left = 0;
					break;
				}
			}			
			if (left < BLOCK_SIZE - offset)
				chars = left;
			else
				chars = BLOCK_SIZE - offset;
			filp->f_pos += chars;
			left -= chars;
			read += chars;
			if (*bhe) {
				memcpy_tofs(buf,offset+(*bhe)->b_data,chars);
				brelse(*bhe);
				buf += chars;
			} else {
				while (chars-->0)
					put_fs_byte(0,buf++);
			}
			offset = 0;
			if (++bhe == &buflist[NBUF])
				bhe = buflist;
		} while (left > 0 && bhe != bhb && (!*bhe || !(*bhe)->b_lock));
	} while (left > 0);

/* Release the read-ahead blocks */
	while (bhe != bhb) {
		brelse(*bhe);
		if (++bhe == &buflist[NBUF])
			bhe = buflist;
	};
	if (!read)
		return -EIO;
	filp->f_reada = 1;
	return read;
}
