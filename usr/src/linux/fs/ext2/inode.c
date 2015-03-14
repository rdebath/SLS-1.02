/*
 *  linux/fs/ext2/inode.c
 *
 *  Copyright (C) 1992, 1993  Remy Card (card@masi.ibp.fr)
 *
 *  from
 *
 *  linux/fs/minix/inode.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Goal-directed block allocation by Stephen Tweedie (sct@dcs.ed.ac.uk), 1993
 */

#include <linux/sched.h>
#include <linux/ext2_fs.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/locks.h>
#include <linux/errno.h>

#include <asm/system.h>
#include <asm/segment.h>

void ext2_put_inode (struct inode * inode)
{
	if (inode->i_nlink || inode->i_ino == EXT2_ACL_INO)
		return;
	inode->i_size = 0;
	if (inode->i_blocks)
		ext2_truncate (inode);
	ext2_free_inode (inode);
}

void ext2_put_super (struct super_block * sb)
{
	struct ext2_super_block * es;
	int i;

	lock_super (sb);
	es = (struct ext2_super_block *) sb->u.ext2_sb.s_sbh->b_data;
	es->s_valid = sb->u.ext2_sb.s_was_mounted_valid;
	sb->u.ext2_sb.s_sbh->b_dirt = 1;
#ifndef DONT_USE_DCACHE
	ext2_dcache_invalidate (sb->s_dev);
#endif
	sb->s_dev = 0;
	for (i = 0; i < EXT2_MAX_GROUP_DESC; i++)
		if (sb->u.ext2_sb.s_group_desc[i])
			brelse (sb->u.ext2_sb.s_group_desc[i]);
	for (i = 0; i < EXT2_MAX_GROUP_LOADED; i++)
		if (sb->u.ext2_sb.s_inode_bitmap[i])
			brelse (sb->u.ext2_sb.s_inode_bitmap[i]);
	for (i = 0; i < EXT2_MAX_GROUP_LOADED; i++)
		if (sb->u.ext2_sb.s_block_bitmap[i])
			brelse (sb->u.ext2_sb.s_block_bitmap[i]);
	unlock_super (sb);
	return;
}

static struct super_operations ext2_sops = { 
	ext2_read_inode,
	NULL,
	ext2_write_inode,
	ext2_put_inode,
	ext2_put_super,
	ext2_write_super,
	ext2_statfs
};

#ifdef EXT2FS_PRE_02B_COMPAT

static int convert_pre_02b_fs (struct super_block * sb,
			       struct buffer_head * bh)
{
	struct ext2_super_block * es;
	struct ext2_old_group_desc old_group_desc [BLOCK_SIZE / sizeof (struct ext2_old_group_desc)];
	struct ext2_group_desc * gdp;
	struct buffer_head * bh2;
	int groups_count;
	int i;

	es = (struct ext2_super_block *) bh->b_data;
	bh2 = bread (sb->s_dev, 2, BLOCK_SIZE);
	if (!bh2) {
		printk ("Cannot read descriptor blocks while converting !\n");
		return 0;
	}
	memcpy (old_group_desc, bh2->b_data, BLOCK_SIZE);
	groups_count = (sb->u.ext2_sb.s_blocks_count - 
			sb->u.ext2_sb.s_first_data_block +
			(EXT2_BLOCK_SIZE(sb) * 8) - 1) /
				(EXT2_BLOCK_SIZE(sb) * 8);
	memset (bh2->b_data, 0, BLOCK_SIZE);
	gdp = (struct ext2_group_desc *) bh2->b_data;
	for (i = 0; i < groups_count; i++) {
		gdp[i].bg_block_bitmap = old_group_desc[i].bg_block_bitmap;
		gdp[i].bg_inode_bitmap = old_group_desc[i].bg_inode_bitmap;
		gdp[i].bg_inode_table = old_group_desc[i].bg_inode_table;
		gdp[i].bg_free_blocks_count = old_group_desc[i].bg_free_blocks_count;
		gdp[i].bg_free_inodes_count = old_group_desc[i].bg_free_inodes_count;
	}
	bh2->b_dirt = 1;
	brelse (bh2);
	es->s_magic = EXT2_SUPER_MAGIC;
	bh->b_dirt = 1;
	sb->s_magic = EXT2_SUPER_MAGIC;
	return 1;
}

#endif

struct super_block * ext2_read_super (struct super_block * s, void * data,
				      int silent)
{
	struct buffer_head * bh;
	struct ext2_super_block * es;
	int dev = s->s_dev;
	int bh_count;
	int i, j;
#ifdef EXT2FS_PRE_02B_COMPAT
	int fs_converted = 0;
#endif

	lock_super (s);
	if (!(bh = bread (dev, 1, BLOCK_SIZE))) {
		s->s_dev = 0;
		unlock_super (s);
		printk ("EXT2-fs: unable to read superblock\n");
		return NULL;
	}
	es = (struct ext2_super_block *) bh->b_data;
	s->s_magic = es->s_magic;
	s->s_blocksize = EXT2_MIN_BLOCK_SIZE << es->s_log_block_size;
	s->u.ext2_sb.s_inodes_count = es->s_inodes_count;
	s->u.ext2_sb.s_blocks_count = es->s_blocks_count;
	s->u.ext2_sb.s_r_blocks_count = es->s_r_blocks_count;
	s->u.ext2_sb.s_first_data_block = es->s_first_data_block;
	s->u.ext2_sb.s_log_block_size = es->s_log_block_size;
	s->u.ext2_sb.s_log_frag_size = es->s_log_frag_size;
	s->u.ext2_sb.s_frag_size = EXT2_MIN_FRAG_SIZE <<
				     es->s_log_frag_size;
	if (s->u.ext2_sb.s_frag_size)
		s->u.ext2_sb.s_frags_per_block = s->s_blocksize /
					   s->u.ext2_sb.s_frag_size;
	else
		s->s_magic = 0;
	s->u.ext2_sb.s_blocks_per_group = es->s_blocks_per_group;
	s->u.ext2_sb.s_frags_per_group = es->s_frags_per_group;
	s->u.ext2_sb.s_inodes_per_group = es->s_inodes_per_group;
	s->u.ext2_sb.s_inodes_per_block = s->s_blocksize /
					    sizeof (struct ext2_inode);
	s->u.ext2_sb.s_desc_per_block = s->s_blocksize /
					  sizeof (struct ext2_group_desc);
	s->u.ext2_sb.s_sbh = bh;
	s->u.ext2_sb.s_was_mounted_valid = es->s_valid;
	s->u.ext2_sb.s_rename_lock = 0;
	s->u.ext2_sb.s_rename_wait = NULL;
#ifdef EXT2FS_PRE_02B_COMPAT
	if (s->s_magic == EXT2_OLD_SUPER_MAGIC) {
		if (es->s_blocks_count > 262144) {
			 /* fs > 256 MB can't be converted */ 
			s->s_dev = 0;
			unlock_super (s);
			brelse (bh);
			printk ("EXT2-fs: trying to mount a pre-0.2b file"
				"system which cannot be converted\n");
			return NULL;
		}
		printk ("EXT2-fs: mounting a pre 0.2b file system, "
			"will try to convert the structure\n");
		if (s->s_flags & MS_RDONLY == 0) {
			s->s_dev = 0;
			unlock_super (s);
			brelse (bh);
			printk ("EXT2-fs: cannot convert a read-only fs\n");
			return NULL;
		}
		if (!convert_pre_02b_fs (s, bh)) {
			s->s_dev = 0;
			unlock_super (s);
			brelse (bh);
			printk ("EXT2-fs: conversion failed !!!\n");
			return NULL;
		}
		printk ("EXT2-fs: conversion succeeded !!!\n");
		fs_converted = 1;
	}
#endif
	if (s->s_magic != EXT2_SUPER_MAGIC) {
		s->s_dev = 0;
		unlock_super (s);
		brelse (bh);
		if (!silent)
			printk("VFS: Can't find an ext2fs filesystem on dev 0x%04x.\n",
				   dev);
		return NULL;
	}
	if (s->s_blocksize != s->u.ext2_sb.s_frag_size) {
		s->s_dev = 0;
		unlock_super (s);
		brelse (bh);
		printk ("EXT2-fs: fragsize != blocksize (not supported yet)\n");
		return NULL;
	}
	if (!es->s_valid)
		printk ("EXT2-fs warning: mounting unchecked file system, "
			"running e2fsck is recommended\n");
	s->u.ext2_sb.s_groups_count = (s->u.ext2_sb.s_blocks_count -
				       s->u.ext2_sb.s_first_data_block +
				       EXT2_BLOCKS_PER_GROUP(s) - 1) /
				       EXT2_BLOCKS_PER_GROUP(s);
	for (i = 0; i < EXT2_MAX_GROUP_DESC; i++)
		s->u.ext2_sb.s_group_desc[i] = NULL;
	bh_count = (s->u.ext2_sb.s_groups_count +
		   EXT2_DESC_PER_BLOCK(s) - 1) /
		   EXT2_DESC_PER_BLOCK(s);
	for (i = 0; i < bh_count; i++) {
		s->u.ext2_sb.s_group_desc[i] = bread (dev, i + 2, s->s_blocksize);
		if (!s->u.ext2_sb.s_group_desc[i]) {
			s->s_dev = 0;
			unlock_super (s);
			for (j = 0; j < i; j++)
				brelse (s->u.ext2_sb.s_group_desc[i]);
			brelse (bh);
			printk ("ext2_read_super: unable to read group descriptors\n");
			return NULL;
		}
	}
	for (i = 0; i < EXT2_MAX_GROUP_LOADED; i++) {
		s->u.ext2_sb.s_inode_bitmap_number[i] = 0;
		s->u.ext2_sb.s_inode_bitmap[i] = NULL;
		s->u.ext2_sb.s_block_bitmap_number[i] = 0;
		s->u.ext2_sb.s_block_bitmap[i] = NULL;
	}
	s->u.ext2_sb.s_loaded_inode_bitmaps = 0;
	s->u.ext2_sb.s_loaded_block_bitmaps = 0;
	unlock_super (s);
	/* set up enough so that it can read an inode */
	s->s_dev = dev;
	s->s_op = &ext2_sops;
	if (!(s->s_mounted = iget(s, EXT2_ROOT_INO))) {
		s->s_dev = 0;
		for (i = 0; i < EXT2_MAX_GROUP_DESC; i++)
			if (s->u.ext2_sb.s_group_desc[i])
				brelse (s->u.ext2_sb.s_group_desc[i]);
		brelse (bh);
		printk ("EXT2-fs: get root inode failed\n");
		return NULL;
	}
	if ((s->s_flags & MS_RDONLY) == 0) {
		es->s_valid = 0;
		es->s_mtime = CURRENT_TIME;
		bh->b_dirt = 1;
		s->s_dirt = 1;
	}
#ifdef EXT2FS_PRE_02B_COMPAT
	if (fs_converted) {
		for (i = 0; i < bh_count; i++)
			s->u.ext2_sb.s_group_desc[i]->b_dirt = 1;
		s->s_dirt = 1;
	}
#endif
	printk ("[EXT II FS %s, bs=%d, fs=%d, gc=%d, bpg=%d, ipg=%d]\n",
		EXT2FS_VERSION, s->s_blocksize, s->u.ext2_sb.s_frag_size,
		s->u.ext2_sb.s_groups_count,
		EXT2_BLOCKS_PER_GROUP(s), EXT2_INODES_PER_GROUP(s));
	return s;
}

/*
 * In the second extended file system, it is not necessary to
 * write the super block since we use a mapping of the
 * disk super block in a buffer.
 *
 * However, this function is still used to set the fs valid
 * flags to 0.  We need to set this flag to 0 since the fs
 * may have been checked while mounted and e2fsck may have
 * set s_valid to 1 after some corrections.
 *
 * Note that this function also writes backups of the super block and
 * of the group descriptors in each group.
 */
void ext2_write_super (struct super_block * sb)
{
	struct ext2_super_block * es;
	struct buffer_head * bh;
	unsigned long block;
	unsigned long bh_count;
	int i, j;

	if ((sb->s_flags & MS_RDONLY) == 0) {
		es = (struct ext2_super_block *) sb->u.ext2_sb.s_sbh->b_data;
#ifdef EXT2FS_DEBUG
		printk ("ext2_write_super: setting valid to 0\n");
#endif
		es->s_valid = 0;
		es->s_wtime = CURRENT_TIME;
		sb->u.ext2_sb.s_sbh->b_dirt = 1;
		bh_count = (sb->u.ext2_sb.s_groups_count +
			    EXT2_DESC_PER_BLOCK(sb) - 1) /
			    EXT2_DESC_PER_BLOCK(sb);
		for (i = 0; i < sb->u.ext2_sb.s_groups_count; i++) {
			block = sb->u.ext2_sb.s_first_data_block +
				i * sb->u.ext2_sb.s_blocks_per_group;
			if (!(bh = bread (sb->s_dev, block, BLOCK_SIZE)))
				printk ("ext2_write_super: Unable to read backup super block for group %d\n", i);
			else {
#ifdef EXT2FS_DEBUG
				printk ("ext2_write_super: writing super block backup in group %d at block %d\n", i, block);
#endif	
				memcpy (bh->b_data, es, BLOCK_SIZE);
				bh ->b_dirt = 1;
				brelse (bh);
			}
			for (j = 0; j < bh_count; j++) {
				block ++;
#ifdef EXT2FS_DEBUG
				printk ("ext2_write_super: writing descriptors (block %d) backup in group %d at block %d\n", j, i, block);
#endif	
				if (!(bh = bread (sb->s_dev, block, sb->s_blocksize)))
					printk ("ext2_write_super: Unable to read backup descriptor for group %d\n", i);
				else {
					memcpy (bh->b_data, sb->u.ext2_sb.s_group_desc[j]->b_data, sb->s_blocksize);
					bh ->b_dirt = 1;
					brelse (bh);
				}
			}
		}
	}
	sb->s_dirt = 0;
}

void ext2_statfs (struct super_block * sb, struct statfs * buf)
{
	long tmp;

	put_fs_long (EXT2_SUPER_MAGIC, &buf->f_type);
	put_fs_long (sb->s_blocksize, &buf->f_bsize);
	put_fs_long (sb->u.ext2_sb.s_blocks_count << sb->u.ext2_sb.s_log_block_size,
		&buf->f_blocks);
	tmp = ext2_count_free_blocks (sb);
	put_fs_long (tmp, &buf->f_bfree);
	if (tmp >= sb->u.ext2_sb.s_r_blocks_count)
		put_fs_long (tmp - sb->u.ext2_sb.s_r_blocks_count,
			     &buf->f_bavail);
	else
		put_fs_long (0, &buf->f_bavail);
	put_fs_long (sb->u.ext2_sb.s_inodes_count, &buf->f_files);
	put_fs_long (ext2_count_free_inodes(sb), &buf->f_ffree);
	put_fs_long (EXT2_NAME_LEN, &buf->f_namelen);
	/* Don't know what value to put in buf->f_fsid */
}

#define inode_bmap(inode, nr) ((inode)->u.ext2_i.i_data[(nr)])

static int block_bmap (struct buffer_head * bh, int nr)
{
	int tmp;

	if (!bh)
		return 0;
	tmp = ((unsigned long *) bh->b_data)[nr];
	brelse (bh);
	return tmp;
}

int ext2_bmap (struct inode * inode, int block)
{
	int i;
	int addr_per_block = EXT2_ADDR_PER_BLOCK(inode->i_sb);

	if (block < 0) {
		printk("ext2_bmap: block < 0");
		return 0;
	}
	if (block >= EXT2_NDIR_BLOCKS + addr_per_block +
		     addr_per_block * addr_per_block +
		     addr_per_block * addr_per_block * addr_per_block) {
		printk ("ext2_bmap: block > big");
		return 0;
	}
	if (block < EXT2_NDIR_BLOCKS)
		return inode_bmap (inode, block);
	block -= EXT2_NDIR_BLOCKS;
	if (block < addr_per_block) {
		i = inode_bmap (inode, EXT2_IND_BLOCK);
		if (!i)
			return 0;
		return block_bmap (bread (inode->i_dev, i,
					  inode->i_sb->s_blocksize), block);
	}
	block -= addr_per_block;
	if (block < addr_per_block * addr_per_block) {
		i = inode_bmap (inode, EXT2_DIND_BLOCK);
		if (!i)
			return 0;
		i = block_bmap (bread (inode->i_dev, i,
				       inode->i_sb->s_blocksize),
				block / addr_per_block);
		if (!i)
			return 0;
		return block_bmap (bread (inode->i_dev, i,
					  inode->i_sb->s_blocksize),
				   block & (addr_per_block - 1));
	}
	block -= addr_per_block * addr_per_block;
	i = inode_bmap (inode, EXT2_TIND_BLOCK);
	if (!i)
		return 0;
	i = block_bmap (bread (inode->i_dev, i, inode->i_sb->s_blocksize),
			block / (addr_per_block * addr_per_block));
	if (!i)
		return 0;
	i = block_bmap (bread (inode->i_dev, i, inode->i_sb->s_blocksize),
			(block / addr_per_block) & (addr_per_block - 1));
	if (!i)
		return 0;
	return block_bmap (bread (inode->i_dev, i, inode->i_sb->s_blocksize),
			   block & (addr_per_block - 1));
}

static struct buffer_head * inode_getblk (struct inode * inode, int nr,
					  int create, int new_block, int *err)
{
	int tmp, goal = 0;
	unsigned long * p;
	struct buffer_head * result;
	int blocks = inode->i_sb->s_blocksize / 512;

	p = inode->u.ext2_i.i_data + nr;
repeat:
	tmp = *p;
	if (tmp) {
		result = getblk (inode->i_dev, tmp, inode->i_sb->s_blocksize);
		if (tmp == *p)
			return result;
		brelse (result);
		goto repeat;
	}
	if (!create || new_block >= 
	    (current->rlim[RLIMIT_FSIZE].rlim_cur >>
	     EXT2_BLOCK_SIZE_BITS(inode->i_sb))) {
		*err = -EFBIG;
		return NULL;
	}
	if (inode->u.ext2_i.i_next_alloc_block == new_block)
		goal = inode->u.ext2_i.i_next_alloc_goal;
#ifdef EXT2FS_DEBUG
	printk ("ext2 inode_getblk: hint = %d,", goal);
#endif
	if (!goal) {
		for (tmp = nr-1; tmp>=0; tmp--) {
			if (inode->u.ext2_i.i_data[tmp]) {
				goal = inode->u.ext2_i.i_data[tmp];
				break;
			}
		}
		if (!goal)
			goal = (inode->u.ext2_i.i_block_group * 
				EXT2_BLOCKS_PER_GROUP(inode->i_sb))
				+ inode->i_sb->u.ext2_sb.s_first_data_block;
	}

#ifdef EXT2FS_DEBUG
	printk (" goal = %d.\n", goal);
#endif
	tmp = ext2_new_block (inode->i_sb, goal);
	if (!tmp)
		return NULL;
	result = getblk (inode->i_dev, tmp, inode->i_sb->s_blocksize);
	if (*p) {
		ext2_free_block (inode->i_sb, tmp);
		brelse (result);
		goto repeat;
	}
	*p = tmp;
	inode->u.ext2_i.i_next_alloc_block = new_block;
	inode->u.ext2_i.i_next_alloc_goal = tmp;
	inode->i_ctime = CURRENT_TIME;
	inode->i_blocks += blocks;
	inode->i_dirt = 1;
	return result;
}

static struct buffer_head * block_getblk (struct inode * inode,
					  struct buffer_head * bh, int nr,
					  int create, int blocksize, 
					  int new_block, int *err)
{
	int tmp, goal = 0;
	unsigned long * p;
	struct buffer_head * result;
	int blocks = inode->i_sb->s_blocksize / 512;

	if (!bh)
		return NULL;
	if (!bh->b_uptodate) {
		ll_rw_block (READ, 1, &bh);
		wait_on_buffer (bh);
		if (!bh->b_uptodate) {
			brelse (bh);
			return NULL;
		}
	}
	p = nr + (unsigned long *) bh->b_data;
repeat:
	tmp = *p;
	if (tmp) {
		result = getblk (bh->b_dev, tmp, blocksize);
		if (tmp == *p) {
			brelse (bh);
			return result;
		}
		brelse (result);
		goto repeat;
	}
	if (!create || new_block >= 
	    (current->rlim[RLIMIT_FSIZE].rlim_cur >> 
	     EXT2_BLOCK_SIZE_BITS(inode->i_sb))) {
		brelse (bh);
		*err = -EFBIG;
		return NULL;
	}
	if (inode->u.ext2_i.i_next_alloc_block == new_block)
		goal = inode->u.ext2_i.i_next_alloc_goal;
	if (!goal) {
		for (tmp = nr-1; tmp>=0; tmp--) {
			if (((unsigned long *) bh->b_data)[tmp]) {
				goal = ((unsigned long *)bh->b_data)[tmp];
				break;
			}
		}
		if (!goal)
			goal = bh->b_blocknr+1;
	}
	tmp = ext2_new_block (inode->i_sb, goal);
	if (!tmp) {
#ifdef EXT2FS_DEBUG
		printk ("inode_getblk: ext2_new_block returned 0\n");
#endif
		brelse (bh);
		return NULL;
	}
	result = getblk (bh->b_dev, tmp, blocksize);
	if (*p) {
		ext2_free_block (inode->i_sb, tmp);
		brelse (result);
		goto repeat;
	}
	*p = tmp;
	bh->b_dirt = 1;
	inode->i_ctime = CURRENT_TIME;
	inode->i_blocks += blocks;
	inode->i_dirt = 1;
	inode->u.ext2_i.i_next_alloc_block = new_block;
	inode->u.ext2_i.i_next_alloc_goal = tmp;
	brelse (bh);
	return result;
}

struct buffer_head * ext2_getblk (struct inode * inode, int block,
				  int create, int *err)
{
	struct buffer_head * bh;
	int b;
	unsigned long addr_per_block = EXT2_ADDR_PER_BLOCK(inode->i_sb);

	*err = -EIO;
	if (block < 0) {
		printk ("ext2_getblk: block < 0\n");
		return NULL;
	}
	if (block > EXT2_NDIR_BLOCKS + addr_per_block  +
		    addr_per_block * addr_per_block +
		    addr_per_block * addr_per_block * addr_per_block) {
		printk ("ext2_getblk: block > big\n");
		return NULL;
	}
	/* If this is a sequential block allocation, set the next_alloc_block
	   to this block now so that all the indblock and data block
	   allocations use the same goal zone */
#ifdef EXT2FS_DEBUG
	printk ("ext2_getblk: block %d, next %d, goal %d.\n", block, 
		inode->u.ext2_i.i_next_alloc_block,
		inode->u.ext2_i.i_next_alloc_goal);
#endif
	if (block == inode->u.ext2_i.i_next_alloc_block + 1) {
		inode->u.ext2_i.i_next_alloc_block++;
		inode->u.ext2_i.i_next_alloc_goal++;
	}

	*err = -ENOSPC;
	b = block;
	if (block < EXT2_NDIR_BLOCKS)
		return inode_getblk (inode, block, create, b, err);
	block -= EXT2_NDIR_BLOCKS;
	if (block < addr_per_block) {
		bh = inode_getblk (inode, EXT2_IND_BLOCK, create, b, err);
		return block_getblk (inode, bh, block, create,
				     inode->i_sb->s_blocksize, b, err);
	}
	block -= addr_per_block;
	if (block < addr_per_block * addr_per_block) {
		bh = inode_getblk (inode, EXT2_DIND_BLOCK, create, b, err);
		bh = block_getblk (inode, bh, block / addr_per_block, create,
				   inode->i_sb->s_blocksize, b, err);
		return block_getblk (inode, bh, block & (addr_per_block - 1),
				     create, inode->i_sb->s_blocksize, b, err);
	}
	block -= addr_per_block * addr_per_block;
	bh = inode_getblk (inode, EXT2_TIND_BLOCK, create, b, err);
	bh = block_getblk (inode, bh, block/(addr_per_block * addr_per_block),
			   create, inode->i_sb->s_blocksize, b, err);
	bh = block_getblk (inode, bh, (block/addr_per_block) & (addr_per_block - 1),
			   create, inode->i_sb->s_blocksize, b, err);
	return block_getblk (inode, bh, block & (addr_per_block - 1), create,
			     inode->i_sb->s_blocksize, b, err);
}

struct buffer_head * ext2_bread (struct inode * inode, int block, 
				 int create, int *err)
{
	struct buffer_head * bh;

	bh = ext2_getblk (inode, block, create, err);
	if (!bh || bh->b_uptodate)
		return bh;
	ll_rw_block (READ, 1, &bh);
	wait_on_buffer (bh);
	if (bh->b_uptodate)
		return bh;
	brelse (bh);
	*err = -EIO;
	return NULL;
}

void ext2_read_inode (struct inode * inode)
{
	struct buffer_head * bh;
	struct ext2_inode * raw_inode;
	unsigned long block_group;
	unsigned long group_desc;
	unsigned long desc;
	unsigned long block;
	struct ext2_group_desc * gdp;

	if ((inode->i_ino != EXT2_ROOT_INO && inode->i_ino != EXT2_ACL_INO &&
	     inode->i_ino < EXT2_FIRST_INO) ||
	    inode->i_ino > inode->i_sb->u.ext2_sb.s_inodes_count) {
		printk ("ext2_read_inode: bad inode number of dev %0x04: %d\n",
			inode->i_dev, inode->i_ino);
		return;
	}
	block_group = (inode->i_ino - 1) / EXT2_INODES_PER_GROUP(inode->i_sb);
	if (block_group >= inode->i_sb->u.ext2_sb.s_groups_count)
		panic ("ext2_read_inode: group >= groups count");
	group_desc = block_group / EXT2_DESC_PER_BLOCK(inode->i_sb);
	desc = block_group % EXT2_DESC_PER_BLOCK(inode->i_sb);
	bh = inode->i_sb->u.ext2_sb.s_group_desc[group_desc];
	if (!bh)
		panic ("ext2_read_inode: Descriptor not loaded");
	gdp = (struct ext2_group_desc *) bh->b_data;
	block = gdp[desc].bg_inode_table +
		(((inode->i_ino - 1) % EXT2_INODES_PER_GROUP(inode->i_sb))
		 / EXT2_INODES_PER_BLOCK(inode->i_sb));
	if (!(bh = bread (inode->i_dev, block, inode->i_sb->s_blocksize)))
		panic ("ext2_read_inode: unable to read i-node block");
	raw_inode = ((struct ext2_inode *) bh->b_data) +
		(inode->i_ino - 1) % EXT2_INODES_PER_BLOCK(inode->i_sb);
	inode->i_mode = raw_inode->i_mode;
	inode->i_uid = raw_inode->i_uid;
	inode->i_gid = raw_inode->i_gid;
	inode->i_nlink = raw_inode->i_links_count;
	inode->i_size = raw_inode->i_size;
	inode->i_atime = raw_inode->i_atime;
	inode->i_ctime = raw_inode->i_ctime;
	inode->i_mtime = raw_inode->i_mtime;
	inode->u.ext2_i.i_dtime = raw_inode->i_dtime;
	inode->i_blksize = inode->i_sb->s_blocksize;
	inode->i_blocks = raw_inode->i_blocks;
	inode->u.ext2_i.i_flags = raw_inode->i_flags;
	inode->u.ext2_i.i_faddr = raw_inode->i_faddr;
	inode->u.ext2_i.i_frag = raw_inode->i_frag;
	inode->u.ext2_i.i_fsize = raw_inode->i_fsize;
	inode->u.ext2_i.i_file_acl = raw_inode->i_file_acl;
	inode->u.ext2_i.i_dir_acl = raw_inode->i_dir_acl;
	inode->u.ext2_i.i_version = raw_inode->i_version;
	inode->u.ext2_i.i_block_group = block_group;
	inode->u.ext2_i.i_next_alloc_block = 0;
	inode->u.ext2_i.i_next_alloc_goal = 0;
	if (S_ISCHR(inode->i_mode) || S_ISBLK(inode->i_mode))
		inode->i_rdev = raw_inode->i_block[0];
	else for (block = 0; block < EXT2_N_BLOCKS; block++)
		inode->u.ext2_i.i_data[block] = raw_inode->i_block[block];
	brelse (bh);
	inode->i_op = NULL;
	if (inode->i_ino == EXT2_ACL_INO)
		/* Nothing to do */ ;
	else if (S_ISREG(inode->i_mode))
		inode->i_op = &ext2_file_inode_operations;
	else if (S_ISDIR(inode->i_mode))
		inode->i_op = &ext2_dir_inode_operations;
	else if (S_ISLNK(inode->i_mode))
		inode->i_op = &ext2_symlink_inode_operations;
	else if (S_ISCHR(inode->i_mode))
		inode->i_op = &chrdev_inode_operations;
	else if (S_ISBLK(inode->i_mode))
		inode->i_op = &blkdev_inode_operations;
	else if (S_ISFIFO(inode->i_mode))
		init_fifo(inode);
}

void ext2_write_inode (struct inode * inode)
{
	struct buffer_head * bh;
	struct ext2_inode * raw_inode;
	unsigned long block_group;
	unsigned long group_desc;
	unsigned long desc;
	unsigned long block;
	struct ext2_group_desc * gdp;

	if ((inode->i_ino != EXT2_ROOT_INO && inode->i_ino < EXT2_FIRST_INO) ||
	    inode->i_ino > inode->i_sb->u.ext2_sb.s_inodes_count) {
		printk ("ext2_write_inode: bad inode number of dev %0x04: %d\n",
			inode->i_dev, inode->i_ino);
		return;
	}
	block_group = (inode->i_ino - 1) / EXT2_INODES_PER_GROUP(inode->i_sb);
	if (block_group >= inode->i_sb->u.ext2_sb.s_groups_count)
		panic ("ext2_write_inode: group >= groups count");
	group_desc = block_group / EXT2_DESC_PER_BLOCK(inode->i_sb);
	desc = block_group % EXT2_DESC_PER_BLOCK(inode->i_sb);
	bh = inode->i_sb->u.ext2_sb.s_group_desc[group_desc];
	if (!bh)
		panic ("ext2_write_inode: Descriptor not loaded");
	gdp = (struct ext2_group_desc *) bh->b_data;
	block = gdp[desc].bg_inode_table +
		(((inode->i_ino - 1) % EXT2_INODES_PER_GROUP(inode->i_sb))
		 / EXT2_INODES_PER_BLOCK(inode->i_sb));
	if (!(bh = bread (inode->i_dev, block, inode->i_sb->s_blocksize)))
		panic ("ext2_write_inode: unable to read i-node block");
	raw_inode = ((struct ext2_inode *)bh->b_data) +
		(inode->i_ino - 1) % EXT2_INODES_PER_BLOCK(inode->i_sb);
	raw_inode->i_mode = inode->i_mode;
	raw_inode->i_uid = inode->i_uid;
	raw_inode->i_gid = inode->i_gid;
	raw_inode->i_links_count = inode->i_nlink;
	raw_inode->i_size = inode->i_size;
	raw_inode->i_atime = inode->i_atime;
	raw_inode->i_ctime = inode->i_ctime;
	raw_inode->i_mtime = inode->i_mtime;
	raw_inode->i_blocks = inode->i_blocks;
	raw_inode->i_dtime = inode->u.ext2_i.i_dtime;
	raw_inode->i_flags = inode->u.ext2_i.i_flags;
	raw_inode->i_faddr = inode->u.ext2_i.i_faddr;
	raw_inode->i_frag = inode->u.ext2_i.i_frag;
	raw_inode->i_fsize = inode->u.ext2_i.i_fsize;
	raw_inode->i_file_acl = inode->u.ext2_i.i_file_acl;
	raw_inode->i_dir_acl = inode->u.ext2_i.i_dir_acl;
	raw_inode->i_version = inode->u.ext2_i.i_version;
	if (S_ISCHR(inode->i_mode) || S_ISBLK(inode->i_mode))
		raw_inode->i_block[0] = inode->i_rdev;
	else for (block = 0; block < EXT2_N_BLOCKS; block++)
		raw_inode->i_block[block] = inode->u.ext2_i.i_data[block];
	bh->b_dirt = 1;
	inode->i_dirt = 0;
	brelse (bh);
}
