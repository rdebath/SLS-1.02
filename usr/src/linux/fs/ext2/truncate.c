/*
 *  linux/fs/ext2/truncate.c
 *
 *  Copyright (C) 1992, 1993  Remy Card (card@masi.ibp.fr)
 *
 *  from
 *
 *  linux/fs/minix/truncate.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linux/sched.h>
#include <linux/ext2_fs.h>
#include <linux/tty.h>
#include <linux/stat.h>
#include <linux/fcntl.h>
#include <linux/errno.h>

/*
 * Truncate has the most races in the whole filesystem: coding it is
 * a pain in the a**. Especially as I don't do any locking...
 *
 * The code may look a bit weird, but that's just because I've tried to
 * handle things like file-size changes in a somewhat graceful manner.
 * Anyway, truncating a file at the same time somebody else writes to it
 * is likely to result in pretty weird behaviour...
 *
 * The new code handles normal truncates (size = 0) as well as the more
 * general case (size = XXX). I hope.
 */

static int trunc_direct (struct inode * inode)
{
	int i, tmp;
	unsigned long * p;
	struct buffer_head * bh;
	int retry = 0;
	int blocks = inode->i_sb->s_blocksize / 512;
#define DIRECT_BLOCK ((inode->i_size + inode->i_sb->s_blocksize - 1) / \
			inode->i_sb->s_blocksize)
	int direct_block = DIRECT_BLOCK;

repeat:
	for (i = direct_block ; i < EXT2_NDIR_BLOCKS ; i++) {
		p = inode->u.ext2_i.i_data + i;
		tmp = *p;
		if (!tmp)
			continue;
		bh = get_hash_table (inode->i_dev, tmp, inode->i_sb->s_blocksize);
		if (i < direct_block) {
			brelse (bh);
			goto repeat;
		}
		if ((bh && bh->b_count != 1) || tmp != *p) {
			retry = 1;
			brelse (bh);
			continue;
		}
		*p = 0;
		inode->i_blocks -= blocks;
		inode->i_dirt = 1;
		brelse (bh);
		ext2_free_block (inode->i_sb, tmp);
	}
	return retry;
}

static int trunc_indirect (struct inode * inode, int offset, unsigned long * p)
{
	int i, tmp;
	struct buffer_head * bh;
	struct buffer_head * ind_bh;
	unsigned long * ind;
	int retry = 0;
	int addr_per_block = EXT2_ADDR_PER_BLOCK(inode->i_sb);
	int blocks = inode->i_sb->s_blocksize / 512;
#define INDIRECT_BLOCK ((int)DIRECT_BLOCK - offset)
	int indirect_block = INDIRECT_BLOCK;

	tmp = *p;
	if (!tmp)
		return 0;
	ind_bh = bread (inode->i_dev, tmp, inode->i_sb->s_blocksize);
	if (tmp != *p) {
		brelse (ind_bh);
		return 1;
	}
	if (!ind_bh) {
		*p = 0;
		return 0;
	}
repeat:
	for (i = indirect_block ; i < addr_per_block ; i++) {
		if (i < 0)
			i = 0;
		if (i < indirect_block)
			goto repeat;
		ind = i + (unsigned long *) ind_bh->b_data;
		tmp = *ind;
		if (!tmp)
			continue;
		bh = get_hash_table (inode->i_dev, tmp,
				     inode->i_sb->s_blocksize);
		if (i < indirect_block) {
			brelse (bh);
			goto repeat;
		}
		if ((bh && bh->b_count != 1) || tmp != *ind) {
			retry = 1;
			brelse (bh);
			continue;
		}
		*ind = 0;
		ind_bh->b_dirt = 1;
		brelse (bh);
		ext2_free_block (inode->i_sb, tmp);
		inode->i_blocks -= blocks;
		inode->i_dirt = 1;
	}
	ind = (unsigned long *) ind_bh->b_data;
	for (i = 0; i < addr_per_block; i++)
		if (*(ind++))
			break;
	if (i >= addr_per_block)
		if (ind_bh->b_count != 1)
			retry = 1;
		else {
			tmp = *p;
			*p = 0;
			inode->i_blocks -= blocks;
			inode->i_dirt = 1;
			ext2_free_block (inode->i_sb, tmp);
		}
	brelse (ind_bh);
	return retry;
}
		
static int trunc_dindirect (struct inode * inode, int offset,
			    unsigned long * p)
{
	int i, tmp;
	struct buffer_head * dind_bh;
	unsigned long * dind;
	int retry = 0;
	int addr_per_block = EXT2_ADDR_PER_BLOCK(inode->i_sb);
	int blocks = inode->i_sb->s_blocksize / 512;
#define DINDIRECT_BLOCK (((int)DIRECT_BLOCK - offset) / addr_per_block)
	int dindirect_block = DINDIRECT_BLOCK;

	tmp = *p;
	if (!tmp)
		return 0;
	dind_bh = bread (inode->i_dev, tmp, inode->i_sb->s_blocksize);
	if (tmp != *p) {
		brelse (dind_bh);
		return 1;
	}
	if (!dind_bh) {
		*p = 0;
		return 0;
	}
repeat:
	for (i = dindirect_block ; i < addr_per_block ; i ++) {
		if (i < 0)
			i = 0;
		if (i < dindirect_block)
			goto repeat;
		dind = i + (unsigned long *) dind_bh->b_data;
		tmp = *dind;
		if (!tmp)
			continue;
		retry |= trunc_indirect (inode, offset + (i * addr_per_block),
					  dind);
		dind_bh->b_dirt = 1;
	}
	dind = (unsigned long *) dind_bh->b_data;
	for (i = 0; i < addr_per_block; i++)
		if (*(dind++))
			break;
	if (i >= addr_per_block)
		if (dind_bh->b_count != 1)
			retry = 1;
		else {
			tmp = *p;
			*p = 0;
			inode->i_blocks -= blocks;
			inode->i_dirt = 1;
			ext2_free_block (inode->i_sb, tmp);
		}
	brelse (dind_bh);
	return retry;
}

static int trunc_tindirect (struct inode * inode)
{
	int i, tmp;
	struct buffer_head * tind_bh;
	unsigned long * tind, * p;
	int retry = 0;
	int addr_per_block = EXT2_ADDR_PER_BLOCK(inode->i_sb);
	int blocks = inode->i_sb->s_blocksize / 512;
#define TINDIRECT_BLOCK (((int)DIRECT_BLOCK - (addr_per_block * addr_per_block + \
			  addr_per_block + EXT2_NDIR_BLOCKS)) / \
			  (addr_per_block * addr_per_block))
	int tindirect_block = TINDIRECT_BLOCK;

	p = inode->u.ext2_i.i_data + EXT2_TIND_BLOCK;
	if (!(tmp = *p))
		return 0;
	tind_bh = bread (inode->i_dev, tmp, inode->i_sb->s_blocksize);
	if (tmp != *p) {
		brelse (tind_bh);
		return 1;
	}
	if (!tind_bh) {
		*p = 0;
		return 0;
	}
repeat:
	for (i = tindirect_block ; i < addr_per_block ; i ++) {
		if (i < 0)
			i = 0;
		if (i < tindirect_block)
			goto repeat;
		tind = i + (unsigned long *) tind_bh->b_data;
		retry |= trunc_dindirect(inode, EXT2_NDIR_BLOCKS +
/*			addr_per_block + addr_per_block * addr_per_block +
			(i * (addr_per_block * addr_per_block)), tind); */
			addr_per_block + (i + 1) * addr_per_block * addr_per_block,
			tind);
		tind_bh->b_dirt = 1;
	}
	tind = (unsigned long *) tind_bh->b_data;
	for (i = 0; i < addr_per_block; i++)
		if (*(tind++))
			break;
	if (i >= addr_per_block)
		if (tind_bh->b_count != 1)
			retry = 1;
		else {
			tmp = *p;
			*p = 0;
			inode->i_blocks -= blocks;
			inode->i_dirt = 1;
			ext2_free_block (inode->i_sb, tmp);
		}
	brelse (tind_bh);
	return retry;
}
		
void ext2_truncate (struct inode * inode)
{
	int retry;

	if (!(S_ISREG(inode->i_mode) || S_ISDIR(inode->i_mode) ||
	    S_ISLNK(inode->i_mode)))
		return;
	while (1) {
		retry = trunc_direct(inode);
		retry |= trunc_indirect (inode, EXT2_IND_BLOCK,
			(unsigned long *) &inode->u.ext2_i.i_data[EXT2_IND_BLOCK]);
		retry |= trunc_dindirect (inode, EXT2_IND_BLOCK +
			EXT2_ADDR_PER_BLOCK(inode->i_sb),
			(unsigned long *) &inode->u.ext2_i.i_data[EXT2_DIND_BLOCK]);
		retry |= trunc_tindirect (inode);
		if (!retry)
			break;
		current->counter = 0;
		schedule ();
	}
	inode->i_mtime = inode->i_ctime = CURRENT_TIME;
	inode->i_dirt = 1;
}

/*
 * Called when a inode is released. Note that this is different
 * from ext2_open: open gets called at every open, but release
 * gets called only when /all/ the files are closed.
 */
void ext2_release (struct inode * inode, struct file * filp)
{
	printk ("ext2_release not implemented\n");
}
