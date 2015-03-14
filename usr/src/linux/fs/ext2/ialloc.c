
/*
 *  linux/fs/ext2/ialloc.c
 *
 *  Copyright (C) 1992, 1993  Remy Card (card@masi.ibp.fr)
 *
 *  BSD ufs-inspired inode and directory allocation by 
 *  Stephen Tweedie (sct@dcs.ed.ac.uk), 1993
 */

/* ialloc.c contains the inodes allocation and deallocation routines */

/*

   The free inodes are managed by bitmaps.  A file system contains several
   blocks groups.  Each group contains 1 bitmap block for blocks, 1 bitmap
   block for inodes, N blocks for the inode table and data blocks.

   The file system contains group descriptors which are located after the
   super block.  Each descriptor contains the number of the bitmap block and
   the free blocks count in the block.  The descriptors are loaded in memory
   when a file system is mounted (see ext2_read_super).

*/

#include <linux/sched.h>
#include <linux/ext2_fs.h>
#include <linux/stat.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/locks.h>

#include <asm/bitops.h>

static inline int find_first_zero_bit(unsigned * addr, unsigned size)
{
	int res;
	if (!size)
		return 0;
	__asm__("
		cld
		movl $-1,%%eax
		repe; scasl
		je 1f
		subl $4,%%edi
		movl (%%edi),%%eax
		notl %%eax
		bsfl %%eax,%%edx
		jmp 2f
1:		xorl %%edx,%%edx
2:		subl %%ebx,%%edi
		shll $3,%%edi
		addl %%edi,%%edx"
		: "=d" (res)
		:"c" ((size + 31) >> 5), "D" (addr), "b" (addr)
		: "ax", "bx", "cx", "di");
	return res;
}

static void read_inode_bitmap (struct super_block * sb,
			       unsigned long block_group,
			       unsigned int bitmap_nr)
{
	unsigned long group_desc;
	unsigned long desc;
	struct ext2_group_desc * gdp;
	struct buffer_head * bh;

	group_desc = block_group / EXT2_DESC_PER_BLOCK(sb);
	desc = block_group % EXT2_DESC_PER_BLOCK(sb);
	if (!sb->u.ext2_sb.s_group_desc[group_desc]) {
		printk ("block_group = %d,group_desc = %d,desc = %d\n",
			 block_group, group_desc, desc);
		panic ("read_inode_bitmap: Group descriptor not loaded");
	}
	gdp = (struct ext2_group_desc *) sb->u.ext2_sb.s_group_desc[group_desc]->b_data;
	bh = bread (sb->s_dev, gdp[desc].bg_inode_bitmap, sb->s_blocksize);
	if (!bh) {
		printk ("block_group = %d,group_desc = %d,desc = %d,inode_bitmap = %d\n",
			block_group, group_desc, desc, gdp[desc].bg_inode_bitmap);
		panic ("read_inode_bitmap: Cannot read inode bitmap");
	}
	sb->u.ext2_sb.s_inode_bitmap_number[bitmap_nr] = block_group;
	sb->u.ext2_sb.s_inode_bitmap[bitmap_nr] = bh;
}

/*
 * load_inode_bitmap loads the inode bitmap for a blocks group
 *
 * It maintains a cache for the last bitmaps loaded.  This cache is managed
 * with a LRU algorithm.
 *
 * Notes:
 * 1/ There is one cache per mounted file system.
 * 2/ If the file system contains less than EXT2_MAX_GROUP_LOADED groups,
 *    this function reads the bitmap without maintaining a LRU cache.
 */
static int load_inode_bitmap (struct super_block * sb,
			      unsigned int block_group)
{
	int i, j;
	unsigned long inode_bitmap_number;
	struct buffer_head * inode_bitmap;

	if (block_group >= sb->u.ext2_sb.s_groups_count) {
		printk ("block_group = %d, groups_count = %d\n",
			block_group, sb->u.ext2_sb.s_groups_count);
		panic ("load_inode_bitmap: block_group >= groups_count");
	}
	if (sb->u.ext2_sb.s_loaded_inode_bitmaps > 0 &&
	    sb->u.ext2_sb.s_inode_bitmap_number[0] == block_group)
		return 0;
	if (sb->u.ext2_sb.s_groups_count <= EXT2_MAX_GROUP_LOADED) {
		if (sb->u.ext2_sb.s_inode_bitmap[block_group]) {
			if (sb->u.ext2_sb.s_inode_bitmap_number[block_group] != block_group)
				panic ("load_inode_bitmap: block_group != inode_bitmap_number");
			else
				return block_group;
		} else {
			read_inode_bitmap (sb, block_group, block_group);
			return block_group;
		}
	}

	for (i = 0; i < sb->u.ext2_sb.s_loaded_inode_bitmaps &&
		    sb->u.ext2_sb.s_inode_bitmap_number[i] != block_group;
	     i++)
		;
	if (i < sb->u.ext2_sb.s_loaded_inode_bitmaps &&
  	    sb->u.ext2_sb.s_inode_bitmap_number[i] == block_group) {
		inode_bitmap_number = sb->u.ext2_sb.s_inode_bitmap_number[i];
		inode_bitmap = sb->u.ext2_sb.s_inode_bitmap[i];
		for (j = i; j > 0; j--) {
			sb->u.ext2_sb.s_inode_bitmap_number[j] =
				sb->u.ext2_sb.s_inode_bitmap_number[j - 1];
			sb->u.ext2_sb.s_inode_bitmap[j] =
				sb->u.ext2_sb.s_inode_bitmap[j - 1];
		}
		sb->u.ext2_sb.s_inode_bitmap_number[0] = inode_bitmap_number;
		sb->u.ext2_sb.s_inode_bitmap[0] = inode_bitmap;
	} else {
		if (sb->u.ext2_sb.s_loaded_inode_bitmaps < EXT2_MAX_GROUP_LOADED)
			sb->u.ext2_sb.s_loaded_inode_bitmaps++;
		else
			brelse (sb->u.ext2_sb.s_inode_bitmap[EXT2_MAX_GROUP_LOADED - 1]);
		for (j = sb->u.ext2_sb.s_loaded_inode_bitmaps - 1; j > 0; j--) {
			sb->u.ext2_sb.s_inode_bitmap_number[j] =
				sb->u.ext2_sb.s_inode_bitmap_number[j - 1];
			sb->u.ext2_sb.s_inode_bitmap[j] =
				sb->u.ext2_sb.s_inode_bitmap[j - 1];
		}
		read_inode_bitmap (sb, block_group, 0);
	}
	return 0;
}

/*
 * This function sets the deletion time for the inode
 *
 * This may be used one day by an 'undelete' program
 */
static void set_inode_dtime (struct inode * inode,
			     struct ext2_group_desc *gdp, unsigned long desc)
{
	unsigned long inode_block;
	struct buffer_head * bh;
	struct ext2_inode * raw_inode;

	inode_block = gdp[desc].bg_inode_table + (((inode->i_ino - 1) %
			EXT2_INODES_PER_GROUP(inode->i_sb)) /
			EXT2_INODES_PER_BLOCK(inode->i_sb));
	bh = bread (inode->i_sb->s_dev, inode_block, inode->i_sb->s_blocksize);
	if (!bh) {
		printk ("inode=%d, inode_block=%d\n", inode->i_ino, inode_block);
		panic ("set_inode_dtime: Cannot load inode table block");
	}
	raw_inode = ((struct ext2_inode *) bh->b_data) +
			(((inode->i_ino - 1) %
			EXT2_INODES_PER_GROUP(inode->i_sb)) %
			EXT2_INODES_PER_BLOCK(inode->i_sb));
	raw_inode->i_dtime = CURRENT_TIME;
	bh->b_dirt = 1;
	brelse (bh);
}

void ext2_free_inode (struct inode * inode)
{
	struct super_block * sb;
	struct buffer_head * bh;
	struct buffer_head * bh2;
	unsigned long block_group;
	unsigned long bit;
	unsigned long group_desc;
	unsigned long desc;
	int bitmap_nr;
	struct ext2_group_desc * gdp;
	struct ext2_super_block * es;

	if (!inode)
		return;
	if (!inode->i_dev) {
		printk ("ext2_free_inode: inode has no device\n");
		return;
	}
	if (inode->i_count > 1) {
		printk ("ext2_free_inode: inode has count=%d\n",
			inode->i_count);
		return;
	}
	if (inode->i_nlink) {
		printk ("ext2_free_inode: inode has nlink=%d\n",
			inode->i_nlink);
		return;
	}
	if (!inode->i_sb) {
		printk("ext2_free_inode: inode on nonexistent device\n");
		return;
	}
#ifdef EXT2FS_DEBUG
	printk ("ext2_free_inode: freeing inode %d\n", inode->i_ino);
#endif
	sb = inode->i_sb;
	lock_super (sb);
	if (inode->i_ino < 1 || inode->i_ino > sb->u.ext2_sb.s_inodes_count) {
		printk("free_inode: inode 0 or nonexistent inode\n");
		unlock_super (sb);
		return;
	}
	es = (struct ext2_super_block *) sb->u.ext2_sb.s_sbh->b_data;
	block_group = (inode->i_ino - 1) / EXT2_INODES_PER_GROUP(sb);
	bit = (inode->i_ino - 1) % EXT2_INODES_PER_GROUP(sb);
	bitmap_nr = load_inode_bitmap (sb, block_group);
	bh = sb->u.ext2_sb.s_inode_bitmap[bitmap_nr];
	if (!bh) {
		printk ("block_group = %d\n", block_group);
		panic ("ext2_free_inode: Unable to load bitmap");
	}
	if (clear_bit (bit, bh->b_data))
		printk ("ext2_free_inode (%04x:%d): bit already cleared\n",
			sb->s_dev, inode->i_ino);
	else {
		group_desc = block_group / EXT2_DESC_PER_BLOCK(sb);
		desc = block_group % EXT2_DESC_PER_BLOCK(sb);
		bh2 = sb->u.ext2_sb.s_group_desc[group_desc];
		if (!bh2) {
			printk ("group_desc = %d\n", group_desc);
			panic ("ext2_free_inode: Group descriptor not loaded");
		}
		gdp = (struct ext2_group_desc *) bh2->b_data;
		gdp[desc].bg_free_inodes_count ++;
		if (S_ISDIR(inode->i_mode))
			gdp[desc].bg_used_dirs_count --;
		bh2->b_dirt = 1;
		set_inode_dtime (inode, gdp, desc);
	}
	bh->b_dirt = 1;
	es->s_free_inodes_count ++;
	sb->u.ext2_sb.s_sbh->b_dirt = 1;
	sb->s_dirt = 1;
	unlock_super (sb);
	clear_inode (inode);
}

/*
 * This function increments the inode version number
 *
 * This may be used one day by the NFS server
 */
static void inc_inode_version (struct inode * inode,
			       struct ext2_group_desc *gdp,
			       int mode)
{
	unsigned long inode_block;
	struct buffer_head * bh;
	struct ext2_inode * raw_inode;

	inode_block = gdp->bg_inode_table + (((inode->i_ino - 1) %
			EXT2_INODES_PER_GROUP(inode->i_sb)) /
			EXT2_INODES_PER_BLOCK(inode->i_sb));
	bh = bread (inode->i_sb->s_dev, inode_block, inode->i_sb->s_blocksize);
	if (!bh) {
		printk ("inode=%d, inode_block=%d\n",
			inode->i_ino, inode_block);
		printk ("inc_inode_version: Cannot load inode table block");
		inode->u.ext2_i.i_version = 1;
		return;
	}
	raw_inode = ((struct ext2_inode *) bh->b_data) +
			(((inode->i_ino - 1) %
			EXT2_INODES_PER_GROUP(inode->i_sb)) %
			EXT2_INODES_PER_BLOCK(inode->i_sb));
	raw_inode->i_version++;
	inode->u.ext2_i.i_version = raw_inode->i_version;
	bh->b_dirt = 1;
	brelse (bh);
}

static struct ext2_group_desc * get_group_desc(struct super_block * sb,
					       int group)
{
	struct ext2_group_desc * gdp;
	if (group >= sb->u.ext2_sb.s_groups_count || group < 0 )
		panic ("ext2: get_group_desc: Invalid group\n");
	if (!sb->u.ext2_sb.s_group_desc[group / EXT2_DESC_PER_BLOCK(sb)])
		panic ("ext2: get_group_desc: Descriptor not loaded");
	gdp = (struct ext2_group_desc *)
		sb->u.ext2_sb.s_group_desc[group / EXT2_DESC_PER_BLOCK(sb)]
		->b_data;
	return gdp + (group % EXT2_DESC_PER_BLOCK(sb));
}
	
/*
 * There are two policies for allocating an inode.  If the new inode is
 * a directory, then a forward search is made for a block group with both
 * free space and a low directory-to-inode ratio; if that fails, then of
 * the groups with above-average free space, that group with the fewest
 * directories already is chosen.
 *
 * For other inodes, search forward from the parent directory\'s block
 * group to find a free inode.
 */
struct inode * ext2_new_inode (const struct inode * dir, int mode)
{
	struct super_block * sb;
	struct buffer_head * bh;
	int i, j, avefreei;
	struct inode * inode;
	int bitmap_nr;
	struct ext2_group_desc * gdp, * tmp;
	struct ext2_super_block * es;

	if (!dir || !(inode = get_empty_inode ()))
		return NULL;
	sb = dir->i_sb;
	inode->i_sb = sb;
	inode->i_flags = sb->s_flags;
	lock_super (sb);
	es = (struct ext2_super_block *) sb->u.ext2_sb.s_sbh->b_data;
repeat:
	gdp = NULL; i=0;
	
	if (S_ISDIR(mode)) {
		avefreei = es->s_free_inodes_count /
			sb->u.ext2_sb.s_groups_count;
/* I am not yet convinced that this next bit is necessary.
		i = dir->u.ext2_i.i_block_group;
		for (j = 0; j < sb->u.ext2_sb.s_groups_count; j++) {
			tmp = get_group_desc(sb, i);
			if ((tmp->bg_used_dirs_count << 8) < 
			    tmp->bg_free_inodes_count) {
				gdp = tmp;
				break;
			}
			else
			i = ++i % sb->u.ext2_sb.s_groups_count;
		}
*/
		if (!gdp) {
			for (j = 0; j < sb->u.ext2_sb.s_groups_count; j++) {
				tmp = get_group_desc(sb, j);
				if (tmp->bg_free_inodes_count >= avefreei) {
					if (!gdp || 
					    (tmp->bg_free_inodes_count >
					     gdp->bg_free_inodes_count)) {
						i = j;
						gdp = tmp;
					}
				}
			}
		}
	}
	else 
	{ /* Try to place the inode in it\'s parent directory */
		i = dir->u.ext2_i.i_block_group;
		tmp = get_group_desc(sb, i);
		if (tmp->bg_free_inodes_count)
			gdp = tmp;
		else
		{ /* Use a quadratic hash to find a group with a free inode */
			for (j = 1; j < sb->u.ext2_sb.s_groups_count; j <<= 1) {
				i += j;
				if (i >= sb->u.ext2_sb.s_groups_count)
					i -= sb->u.ext2_sb.s_groups_count;
				tmp = get_group_desc(sb, i);
				if (tmp->bg_free_inodes_count) {
					gdp = tmp;
					break;
				}
			}
		}
		if (!gdp) {
			/* That failed: try linear search for a free inode */
			i = dir->u.ext2_i.i_block_group + 2;
			for (j = 2; j < sb->u.ext2_sb.s_groups_count; j++) {
				if (++i >= sb->u.ext2_sb.s_groups_count)
					i = 0;
				tmp = get_group_desc(sb,i);
				if (tmp->bg_free_inodes_count) {
					gdp = tmp;
					break;
				}
			}
		}
	}
	
	if (!gdp) {
		unlock_super (sb);
		return NULL;
	}
	bitmap_nr = load_inode_bitmap (sb, i);
	bh = sb->u.ext2_sb.s_inode_bitmap[bitmap_nr];
	if (!bh) {
		printk ("block_group = %d\n", i);
		panic ("ext2_new_inode: Unable to load group inode bitmap");
	}
	if ((j = find_first_zero_bit ((unsigned long *) bh->b_data,
				      EXT2_INODES_PER_GROUP(sb))) <
	    EXT2_INODES_PER_GROUP(sb)) {
		if (set_bit (j, bh->b_data)) {
			printk ("ext2_new_inode: bit already set\n");
			goto repeat;
		}
		bh->b_dirt = 1;
	} else
		goto repeat;
	j += i * EXT2_INODES_PER_GROUP(sb) + 1;
	if (j > sb->u.ext2_sb.s_inodes_count) {
		printk ("block_group = %d,inode=%d\n", i, j);
		printk ("ext2_new_inode: inode > inodes count");
		return NULL;
	}
	gdp->bg_free_inodes_count --;
	if (S_ISDIR(mode))
		gdp->bg_used_dirs_count ++;
	sb->u.ext2_sb.s_group_desc[i / EXT2_DESC_PER_BLOCK(sb)]->b_dirt = 1;
	es->s_free_inodes_count --;
	sb->u.ext2_sb.s_sbh->b_dirt = 1;
	sb->s_dirt = 1;
	inode->i_sb = sb;
	inode->i_count = 1;
	inode->i_nlink = 1;
	inode->i_dev = sb->s_dev;
	inode->i_uid = current->euid;
	inode->i_gid = (dir->i_mode & S_ISGID) ? dir->i_gid : current->egid;
	inode->i_dirt = 1;
	inode->i_ino = j;
	inode->i_blksize = sb->s_blocksize;
	inode->i_blocks = 0;
	inode->i_mtime = inode->i_atime = inode->i_ctime = CURRENT_TIME;
	inode->u.ext2_i.i_flags = 0;
	inode->u.ext2_i.i_faddr = 0;
	inode->u.ext2_i.i_frag = 0;
	inode->u.ext2_i.i_fsize = 0;
	inode->u.ext2_i.i_file_acl = 0;
	inode->u.ext2_i.i_dir_acl = 0;
	inode->u.ext2_i.i_dtime = 0;
	inode->u.ext2_i.i_block_group = i;
	inode->i_op = NULL;
	inc_inode_version (inode, gdp, mode);
#ifdef EXT2FS_DEBUG
	printk ("ext2_new_inode : allocating inode %d\n", inode->i_ino);
#endif
	unlock_super (sb);
	return inode;
}

unsigned long ext2_count_free_inodes (struct super_block *sb)
{
	struct ext2_super_block * es;
#ifdef EXT2FS_DEBUG
	unsigned long desc_count, bitmap_count, x;
	unsigned long group_desc;
	unsigned long desc;
	int bitmap_nr;
	struct ext2_group_desc * gdp;
	int i;

	lock_super (sb);
	es = (struct ext2_super_block *) sb->u.ext2_sb.s_sbh->b_data;
	desc_count = 0;
	bitmap_count = 0;
	group_desc = 0;
	desc = 0;
	gdp = NULL;
	for (i = 0; i < sb->u.ext2_sb.s_groups_count; i++) {
		if (!gdp) {
			if (!sb->u.ext2_sb.s_group_desc[group_desc]) {
				printk ("ext2_count_free_inodes: Descriptor not loaded\n");
				break;
			}
			gdp = (struct ext2_group_desc *) sb->u.ext2_sb.s_group_desc[group_desc]->b_data;
		}
		desc_count += gdp[desc].bg_free_inodes_count;
		bitmap_nr = load_inode_bitmap (sb, i);
		if (sb->u.ext2_sb.s_inode_bitmap[bitmap_nr])
			x = ext2_count_free (sb->u.ext2_sb.s_inode_bitmap[bitmap_nr],
					       EXT2_INODES_PER_GROUP(sb) / 8);
		else {
			x = 0;
			printk ("Cannot load inode bitmap for group %d (bitmap = %d)\n",
				i, bitmap_nr);
		}
		printk ("group %d: stored = %d, counted = %d\n",
			i, gdp[desc].bg_free_inodes_count, x);
		bitmap_count += x;
		desc ++;
		if (desc == EXT2_DESC_PER_BLOCK(sb)) {
			group_desc ++;
			desc = 0;
			gdp = NULL;
		}
	}
	printk("ext2_count_free_inodes: stored = %d, computed = %d, %d\n",
		es->s_free_inodes_count, desc_count, bitmap_count);
	unlock_super (sb);
	return desc_count;
#else
	es = (struct ext2_super_block *) sb->u.ext2_sb.s_sbh->b_data;
	return es->s_free_inodes_count;
#endif
}
