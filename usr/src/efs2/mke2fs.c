/*
 * mke2fs.c - make a linux second extended file-system.
 *
 * Copyright (C) 1992, 1993 Remy Card (card@masi.ibp.fr)
 *
 * Copyright (C) 1991 Linus Torvalds. This file may be redistributed as per
 * the Linux copyright.
 */

/*
 * 24.11.91  -	time began. Used the fsck sources to get started.
 *
 * 25.11.91  -	corrected some bugs. Added support for ".badblocks"
 *		The algorithm for ".badblocks" is a bit weird, but
 *		it should work. Oh, well.
 *
 * 25.01.92  -  Added the -l option for getting the list of bad blocks
 *              out of a named file. (Dave Rivers, rivers@ponds.uucp)
 *
 * 28.02.92  -	added %-information when using -c.
 *
 * 28.06.92  -	modifications by Remy Card (card@masi.ibp.fr)
 *		Used the mkfs sources to create mkefs.c
 *
 * 14.07.92  -	cleaned up the code
 *
 * 19.07.92  -	now use the same bit op than mkfs instead of an internal linked
 *		list of marked numbers
 *
 * 26.07.92  -	mkefs is now able to produce triple indirection for the
 *		badblocks file
 *
 * 11.08.92  -	changes by Wayne Davison (davison@borland.com)
 *		- Unified some of the buffers to make it use less memory at
 *		  runtime.
 *		- Changed some ints to unsigned longs when referring to block
 *		  numbers (there's still more to "fix" in this area).
 *		- Fixed an off-by-one error in the alarm_intr() routine.
 *		- Removed a redundant lseek call in the block test code.
 *
 * 16.08.92  -  added the -i argument to specify the ratio of inodes
 *		use getopt() to get arguments
 *
 * 19.08.92  -	converted mkefs to mke2fs to create a new extended file system
 *
 * 27.08.92  -	added creation of a lost+found directory used during checks
 *
 * 30.08.92  -  added the -b and -f options which allow the user to specify
 *		blocks and fragments sizes.  mke2fs should now be able to
 *		create file systems with bigger block sizes.
 *
 * 20.12.92  -  lots of change to make mkefs understand the new (final ?)
 *		structure of the new extended file system (with blocks
 *		groups and bitmaps)
 *
 * 16.01.93  -	added -g blocks_per_group
 *		replaced some 'block_size * 8' by GROUPSBERBLOCK
 *		corrected the i_blocks field of / and /lost+found
 *
 * 24.01.93  -	removed the -g option which seems incompatible with
 *		super block/descriptors recovery (if the blocks count per
 *		group is contained in the super block, how can e2fsck access
 *		backups in other groups if the super block is corrupted ?)
 *		add backups of the super block and of the group descriptors
 *
 * 01.03.93  -	added the new used_dirs_count field computation
 *
 * 13.03.93  -	added again -g blocks_per_group
 *		made -t an alias to -c for compatibility with e2fsck
 *		made mke2fs use much less memory
 *		display the bad blocks list in verbose mode
 *		
 * Usage:  mke2fs [-c] [-i bytes-per-inode]
 *		  [-b block-size] [-f fragment-size]
 *		  [-g blocks_per_group] [-v]
 *		  [-m reserved_blocks_ratio]
 *		  device [size-in-blocks]
 *         mke2fs [-l filename ] [-i bytes-per-inode]
 *		  [-b block-size] [-f fragment-size]
 *		  [-g blocks_per_group] [-v]
 *		  [-m reserved_blocks_ratio]
 *		  device [size-in-blocks]
 *
 *	-b to specify the size of a block
 *	-c for readablility checking (SLOW!)
 *	-f to specify the size of a fragment
 *	-g to specify the number of blocks per group
 *	-i to specify the number of inodes
 *      -l for getting a list of bad blocks from a file.
 *	-m to specify the number of blocks reserved for the super user
 *	-t for readablility checking (SLOW!)
 *	-v for verbose mode
 *
 * The device may be a block device or a image of one, but this isn't
 * enforced (but it's not much fun on a character device :-). 
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>
#include <termios.h>
#include <time.h>
#include <sys/stat.h>
#include <getopt.h>

#include <linux/fs.h>
#include <linux/ext2_fs.h>

#ifndef __GNUC__
#error "needs gcc for the bitop-__asm__'s"
#endif

#ifndef __linux__
#define volatile
#endif

#define TEST_BUFFER_BLOCKS	 16

static char blkbuf[EXT2_MAX_BLOCK_SIZE * TEST_BUFFER_BLOCKS];

#undef EXT2_INODES_PER_BLOCK
#undef EXT2_ADDR_PER_BLOCK
#undef EXT2_DESC_PER_BLOCK
#define EXT2_INODES_PER_BLOCK	(block_size / sizeof (struct ext2_inode))
#define EXT2_ADDR_PER_BLOCK (block_size / sizeof (unsigned long))
#define EXT2_DESC_PER_BLOCK (block_size / sizeof (struct ext2_group_desc))

#define UPPER(size, n)		((size + ((n) - 1)) / (n))
#define INODE_SIZE		(sizeof (struct ext2_inode))
#define INODE_BLOCKS		UPPER (INODES, EXT2_INODES_PER_BLOCK)
#define INODE_BUFFER_SIZE	(INODE_BLOCKS * block_size)

#define BITS_PER_BLOCK		(block_size << 3)

static char * program_name = "mke2fs";
static char * device_name = NULL;
static int inode_ratio = 4096;
static int reserved_ratio = 5;
static int block_size = 1024;
static int frag_size = 1024;
static int frags_per_block;
static int blocks_per_group = 8192;
static int dev = -1;
static long blocks = 0;
static int check = 0;
static int badblocks = 0;
static int verbose = 0;

#define ROOT_INO_STRING		"\002\000\000\000"
#define LPF_INO_STRING		"\013\000\000\000"

#define ROOT_RECLEN_STRING	"\014\000"
#define LPF_RECLEN_STRING	"\024\000"

#define DOT_NAMELEN_STRING	"\001\000"
#define DOTDOT_NAMELEN_STRING	"\002\000"
#define LPF_NAMELEN_STRING	"\012\000"

static char root_block [EXT2_MIN_BLOCK_SIZE] =
ROOT_INO_STRING ROOT_RECLEN_STRING DOT_NAMELEN_STRING    ".\0\0\0"
ROOT_INO_STRING ROOT_RECLEN_STRING DOTDOT_NAMELEN_STRING "..\0\0"
LPF_INO_STRING  LPF_RECLEN_STRING   LPF_NAMELEN_STRING    "lost+found\0\0";

static char * inode_buffer = NULL;

#define Inode	(((struct ext2_inode *) inode_buffer) - 1)

static char super_block_buffer[EXT2_MIN_BLOCK_SIZE];

static struct ext2_super_block * Super = (struct ext2_super_block *) super_block_buffer;

#define INODES		(Super->s_inodes_count)
#define BLOCKS		(Super->s_blocks_count)
#define RBLOCKS		(Super->s_r_blocks_count)
#define FREEBLOCKS	(Super->s_free_blocks_count)
#define FREEINODES	(Super->s_free_inodes_count)
#define FIRSTBLOCK	(Super->s_first_data_block)
#define BLOCKSIZE	(Super->s_log_block_size)
#define FRAGSIZE        (Super->s_log_frag_size)
#define BLOCKSPERGROUP	(Super->s_blocks_per_group)
#define FRAGSPERGROUP	(Super->s_frags_per_group)
#define INODESPERGROUP	(Super->s_inodes_per_group)
#define MTIME		(Super->s_mtime)
#define WTIME		(Super->s_wtime)
#define MAGIC		(Super->s_magic)
#define VALID		(Super->s_valid)

#define NORM_FIRSTBLOCK	1

static char * inode_map = NULL;
static char * block_map  = NULL;
static char * bad_map  = NULL;

static unsigned long group_desc_count;
static unsigned long group_desc_size;
static unsigned long desc_blocks;
static struct ext2_group_desc * group_desc = NULL;
static int inode_blocks_per_group;

#if 1

#define bitop(name,op) \
static inline int name (char * addr,unsigned int nr) \
{ \
	int __res; \
	__asm__ __volatile__("bt" op " %1,%2; adcl $0,%0" \
			     :"=g" (__res) \
			     :"r" (nr),"m" (*(addr)),"0" (0)); \
	return __res; \
}

bitop(testbit, "")
bitop(setbit, "s")
bitop(clrbit, "r")

#else

static int testbit (char *addr, unsigned int nr)
{
	unsigned int char_addr, bit_addr;

	char_addr = nr / 8;
	bit_addr = nr % 8;
	return addr[char_addr] & (1 << bit_addr);
}

static void clrbit (char *addr, unsigned int nr)
{
	unsigned int char_addr, bit_addr;

	char_addr = nr / 8;
	bit_addr = nr % 8;
	addr[char_addr] &= ~(1 << bit_addr);
}

static void setbit (char *addr, unsigned int nr)
{
	unsigned int char_addr, bit_addr;

	char_addr = nr / 8;
	bit_addr = nr % 8;
	addr[char_addr] |= (1 << bit_addr);
}

#endif

#define inode_in_use(x)	(testbit (inode_map, (x) - 1))
#define block_in_use(x)	(testbit (block_map, (x) - FIRSTBLOCK))
#define block_is_bad(x)	(testbit (bad_map, (x) - FIRSTBLOCK))

#define mark_inode(x)	(setbit (inode_map, (x) - 1))
#define unmark_inode(x)	(clrbit (inode_map, (x) - 1))

#define mark_block(x)	(setbit (block_map, (x) - FIRSTBLOCK))
#define unmark_block(x)	(clrbit (block_map, (x) - FIRSTBLOCK))

#define mark_bad(x)	(setbit (bad_map, (x) - FIRSTBLOCK))

/*
 * Volatile to let gcc know that this doesn't return.
 */
static volatile void fatal_error (const char * fmt_string)
{
	fprintf (stderr, fmt_string, program_name, device_name);
	exit (1);
}

#define usage()	fatal_error ("usage: %s [-c | -t | -l filename] [-i bytes-per-inode]\n" \
			     "       [-b block-size] [-f fragment-size]\n" \
			     "       [-g blocks-per-group]\n" \
			     "       [-m reserved-blocks-ratio] [-v]\n" \
			     "       /dev/name [blocks]\n")

#define die(str) fatal_error ("%s: " str "\n")

/*
 * Note: this function works only if inodes_per_group is a multiple of 8
 *
 * This should not be a problem since the inodes count is rounded to fully
 * use the inode table blocks, a block is at least 1024 bytes and an inode
 * is 128 bytes
 */
static void write_desc (void)
{
	int i;
	char * block_bitmap = block_map;
	char * inode_bitmap = inode_map;
	char * inode_table = inode_buffer;
	char * full_bitmap = blkbuf;
	char * zeroed_inode_table = alloca (INODESPERGROUP * sizeof (struct ext2_inode));

	if (verbose)
		printf ("Writing bitmaps and tables\n");
	memset (full_bitmap, 0xff, block_size);
	memset (zeroed_inode_table, 0, INODESPERGROUP * sizeof (struct ext2_inode));
	if (INODESPERGROUP % 8)
		die ("inodes_per_group is not a multiple of 8");
	for (i = 0; i < group_desc_count; i++)
	{
		if (verbose)
			printf (" group %d: block bitmap at %d,",
				i, group_desc[i].bg_block_bitmap);
		if (lseek (dev, group_desc[i].bg_block_bitmap * block_size, SEEK_SET) !=
		    group_desc[i].bg_block_bitmap * block_size)
			die ("seek failed in write_desc");
		if (write (dev, full_bitmap, block_size) != block_size)
			die ("write of block bitmap failed in write_desc");
		if (lseek (dev, group_desc[i].bg_block_bitmap * block_size, SEEK_SET) !=
		    group_desc[i].bg_block_bitmap * block_size)
			die ("seek failed in write_desc");
		if (write (dev, block_bitmap, BLOCKSPERGROUP / 8) != BLOCKSPERGROUP / 8)
			die ("write of block bitmap failed in write_desc");
		block_bitmap += BLOCKSPERGROUP / 8;
		if (verbose)
			printf (" inode bitmap at %d,",
				group_desc[i].bg_inode_bitmap);
		if (lseek (dev, group_desc[i].bg_inode_bitmap * block_size, SEEK_SET) !=
		    group_desc[i].bg_inode_bitmap * block_size)
			die ("seek failed in write_desc");
		if (write (dev, full_bitmap, block_size) != block_size)
			die ("write of inode bitmap failed in write_desc");
		if (lseek (dev, group_desc[i].bg_inode_bitmap * block_size, SEEK_SET) !=
		    group_desc[i].bg_inode_bitmap * block_size)
			die ("seek failed in write_desc");
		if (write (dev, inode_bitmap, INODESPERGROUP / 8) != INODESPERGROUP / 8)
			die ("write of inode bitmap failed in write_desc");
		inode_bitmap += INODESPERGROUP / 8;
		if (verbose)
			printf (" inode table at %d\n",
				group_desc[i].bg_inode_table);
		if (lseek (dev, group_desc[i].bg_inode_table * block_size, SEEK_SET) !=
		    group_desc[i].bg_inode_table * block_size)
			die ("seek failed in write_desc");
		if (write (dev, inode_table, inode_blocks_per_group * block_size) != inode_blocks_per_group * block_size)
			die ("write of inode table failed in write_desc");
		if (inode_table == inode_buffer)
			inode_table = zeroed_inode_table;
	}
}

static void write_tables (void)
{
	int i;
	unsigned long block;

	block = FIRSTBLOCK;
	for (i = 0; i < group_desc_count; i++)
	{
		if (EXT2_MIN_BLOCK_SIZE * block != lseek (dev, EXT2_MIN_BLOCK_SIZE * block, SEEK_SET))
			die ("seek failed in write_tables");
		if (EXT2_MIN_BLOCK_SIZE != write (dev, super_block_buffer, EXT2_MIN_BLOCK_SIZE))
			die ("unable to write super-block");
		block += BLOCKSPERGROUP;
	}
	write_desc ();
	block = FIRSTBLOCK + 1;
	for (i = 0; i < group_desc_count; i++)
	{
		
		if (block_size * block != lseek (dev, block_size * block, SEEK_SET))
			die ("seek failed in write_tables");
		if (group_desc_size != write (dev, (char *) group_desc, group_desc_size))
			die ("write of group descriptors failed\n");
		block += BLOCKSPERGROUP;
	}
}

void write_block (int blk, char *buffer)
{
	if (blk * block_size != lseek (dev, blk * block_size, SEEK_SET))
		die ("seek failed in write_block");
	if (block_size != write(dev, buffer, block_size))
	{
		printf ("blk = %d\n", blk);
		die ("write failed in write_block");
	}
}

static int get_free_block (void)
{
	static unsigned long blk = 0;

	if (!blk)
		blk = FIRSTBLOCK;
	while (blk < BLOCKS && block_in_use (blk))
		blk++;
	if (blk >= BLOCKS)
		die ("not enough good blocks");
	mark_block(blk);
	return blk++;
}

static unsigned long next_bad (unsigned long block)
{

	if (!block)
		block = FIRSTBLOCK - 1;
	while (++block < BLOCKS)
		if (block_is_bad (block))
		{
			printf ("%d ", block);
			return block;
		}
	return 0;
}

static void make_bad_inode (void)
{
	struct ext2_inode * inode = &Inode [EXT2_BAD_INO];
	int i, j, k;
	unsigned long block;
	int ind = 0, dind = 0, tind = 0;
	unsigned long *ind_block  = (long *)(blkbuf + block_size);
	unsigned long *dind_block = (long *)(blkbuf + block_size * 2);
	unsigned long *tind_block = (long *)(blkbuf + block_size * 3);

#define NEXT_BAD (block = next_bad (block))

	mark_inode (EXT2_BAD_INO);
	inode->i_links_count = 0;
	inode->i_atime = time(NULL);
	inode->i_ctime = inode->i_atime;
	inode->i_mtime = inode->i_atime;
	inode->i_mode = 0;
	inode->i_size = badblocks * block_size;
	inode->i_blocks = badblocks * frags_per_block;
	if (!badblocks)
		return;
	if (verbose)
		printf ("Recording bad blocks : ");
	block = next_bad (0);
	for (i = 0; i < EXT2_NDIR_BLOCKS; i++)
	{
		inode->i_block[i] = block;
		if (!NEXT_BAD)
			goto end_bad;
	}
	inode->i_block[EXT2_IND_BLOCK] = ind = get_free_block();
	memset (ind_block, 0, block_size);
	for (i = 0; i < EXT2_ADDR_PER_BLOCK; i++)
	{
		ind_block[i] = block;
		if (!NEXT_BAD)
			goto end_bad;
	}
	inode->i_block[EXT2_DIND_BLOCK] = dind = get_free_block();
	memset (dind_block, 0, block_size);
	for (i = 0; i < EXT2_ADDR_PER_BLOCK; i++)
	{
		write_block (ind, (char *) ind_block);
		dind_block[i] = ind = get_free_block();
		memset (ind_block, 0, block_size);
		for (j = 0; j < EXT2_ADDR_PER_BLOCK; j++)
		{
			ind_block[j] = block;
			if (!NEXT_BAD)
				goto end_bad;
		}
	}
	inode->i_block[EXT2_TIND_BLOCK] = tind = get_free_block();
	memset (tind_block, 0, block_size);
	for (i = 0; i < EXT2_ADDR_PER_BLOCK; i++)
	{
		write_block (dind, (char *) dind_block);
		tind_block[i] = dind = get_free_block();
		memset (dind_block, 0, block_size);
		for (j = 0; j < EXT2_ADDR_PER_BLOCK; j++)
		{
			write_block (ind, (char *) ind_block);
			dind_block[j] = ind = get_free_block();
			memset (ind_block, 0 , block_size);
			for (k = 0; k < EXT2_ADDR_PER_BLOCK; k++)
			{
				ind_block[k] = block;
				if (!NEXT_BAD)
					goto end_bad;
			}
		}
	}
	die ("too many bad blocks");
end_bad:
	printf ("\n");
	if (ind)
		write_block (ind, (char *) ind_block);
	if (dind)
		write_block (dind, (char *) dind_block);
	if (tind)
		write_block (tind, (char *) tind_block);
}

/*
 * Create the group descriptors
 */

static void make_group_desc (void)
{
	int i, j, k;
	int block;

	if (verbose)
		printf ("Making group descriptors\n");
	inode_blocks_per_group = (INODES / EXT2_INODES_PER_BLOCK) /
				 group_desc_count;
	for (i = 0, block = FIRSTBLOCK; i < group_desc_count; i++, block += BLOCKSPERGROUP)
	{
		for (j = 0; j < desc_blocks + 1; j++)
			mark_block (block + j);
		j = block + desc_blocks + 1;
		while (j < block + BLOCKSPERGROUP && block_in_use (j))
			j++;
		if (j >= block + BLOCKSPERGROUP)
		{
			printf ("group = %d\n", i);
			die ("Unable to find a block for the block bitmap");
		}
		group_desc[i].bg_block_bitmap = j;
		mark_block(j);
		if (verbose)
			printf (" group %d: block bitmap at %d,",
				i, j);
		while (j < block + BLOCKSPERGROUP && block_in_use (j))
			j++;
		if (j >= block + BLOCKSPERGROUP)
		{
			printf ("group = %d\n", i);
			die ("Unable to find a block for the inode bitmap");
		}
		group_desc[i].bg_inode_bitmap = j;
		mark_block(j);
		if (verbose)
			printf (" inode bitmap at %d,", j);
repeat:
		while (j < block + BLOCKSPERGROUP && block_in_use (j))
			j++;
		if (j >= block + BLOCKSPERGROUP)
		{
			printf ("group = %d\n", i);
			die ("Unable to find a block for the inode table");
		}
		for (k = 0; k < inode_blocks_per_group; k++)
			if (block_in_use (j + k))
			{
				j = j + k;
				goto repeat;
			}
		group_desc[i].bg_inode_table = j;
		for (k = 0; k < inode_blocks_per_group; k++)
			mark_block (j + k);
		if (verbose)
			printf (" inode table at %d (%d)\n",
				j, inode_blocks_per_group);
		group_desc[i].bg_free_blocks_count = 0;
		group_desc[i].bg_free_inodes_count = 0;
		group_desc[i].bg_used_dirs_count = 0;
	}
}

static void count_free_blocks_and_inodes (void)
{
	int i, j;
	int count;
	int dirs_count;
	int block = FIRSTBLOCK;
	int ino = 1;
	int total_blocks_count = 0;
	int total_inodes_count = 0;
	int block_count;

	if (verbose)
		printf ("Computing free blocks and inodes count\n");
	for (i = 0; i < group_desc_count; i++)
	{
		count = 0;
		if (i == group_desc_count - 1)
			block_count = (BLOCKS - FIRSTBLOCK + 1) % BLOCKSPERGROUP;
		else
			block_count = BLOCKSPERGROUP;
		for (j = 0; j < block_count; j++)
		{
			if (!block_in_use(block + j))
				count ++;
		}
		group_desc[i].bg_free_blocks_count = count;
		if (verbose)
			printf (" group %d: %d free blocks,",
				i, group_desc[i].bg_free_blocks_count);
		total_blocks_count += count;
		block += BLOCKSPERGROUP;
		count = 0;
		dirs_count = 0;
		for (j = 0; j < INODESPERGROUP; j++)
			if (!inode_in_use(ino + j))
				count ++;
		group_desc[i].bg_free_inodes_count = count;
		if (verbose)
			printf (" %d free inodes, %d directories\n",
				group_desc[i].bg_free_inodes_count,
				group_desc[i].bg_used_dirs_count);
		total_inodes_count += count;
		ino += INODESPERGROUP;
	}
	FREEBLOCKS = total_blocks_count;
	FREEINODES = total_inodes_count;
}

/*
 * Create the lost+found directory
 *
 * Note: the direct blocks are pre-allocated for this directory so nefsck
 * won't have to allocate space when moving inodes to this directory
 */
static unsigned long make_lpf_inode (void)
{
	unsigned long ino = EXT2_FIRST_INO;
	struct ext2_inode *inode;
	char * lpf_block = blkbuf;
	struct ext2_dir_entry *de;
	int i;

	while (ino <= INODESPERGROUP && inode_in_use (ino))
		ino ++;
	if (ino > INODESPERGROUP)
		return 0;	/* Should never happen */
	if (verbose)
		printf ("Making lost+found (inode %d): blocks", ino);
	mark_inode (ino);
	group_desc[0].bg_used_dirs_count ++;
	inode = &Inode[ino];
	inode->i_links_count = 2;
	inode->i_atime = time(NULL);
	inode->i_ctime = inode->i_atime;
	inode->i_mtime = inode->i_atime;
	inode->i_dtime = 0;
	inode->i_version = 1;
	inode->i_mode = S_IFDIR + 0755;
	inode->i_block[0] = get_free_block ();
	if (verbose)
		printf (" %d", inode->i_block[0]);
	de = (struct ext2_dir_entry *) lpf_block;
	de->inode = ino;
	de->rec_len = 12;
	de->name_len = 1;
	strcpy (de->name, ".");
	de = (struct ext2_dir_entry *) (lpf_block + 12);
	de->inode = EXT2_ROOT_INO;
	de->rec_len = block_size - 12;
	de->name_len = 2;
	strcpy (de->name, "..");
	write_block (inode->i_block[0], lpf_block);
	de = (struct ext2_dir_entry *) lpf_block;
	de->inode = 0;
	de->rec_len = block_size;
	de->name_len = 0;
	for (i = 1; i < EXT2_NDIR_BLOCKS; i++)
	{
		inode->i_block[i] = get_free_block ();
		write_block (inode->i_block[i], lpf_block);
		if (verbose)
			printf (" %d", inode->i_block[i]);
	}
	if (verbose)
		printf ("\n");
	inode->i_blocks = (block_size * EXT2_NDIR_BLOCKS) / 512;
	inode->i_size = block_size * EXT2_NDIR_BLOCKS;
	return ino;
}

static void make_root_inode (unsigned long lpf_ino)
{
	struct ext2_inode *inode = &Inode [EXT2_ROOT_INO];
	struct ext2_dir_entry *de;

	if (verbose)
		printf ("Making root inode (inode %d): ", EXT2_ROOT_INO);
	mark_inode (EXT2_ROOT_INO);
	group_desc[0].bg_used_dirs_count ++;
	inode->i_block[0] = get_free_block();
	if (verbose)
		printf (" block %d\n", inode->i_block[0]);
	inode->i_links_count = 3;
	inode->i_atime = time(NULL);
	inode->i_ctime = inode->i_atime;
	inode->i_mtime = inode->i_atime;
	inode->i_dtime = 0;
	inode->i_size = block_size;
	inode->i_blocks = block_size / 512;
	inode->i_mode = S_IFDIR + 0755;
	inode->i_version = 1;
	if (lpf_ino)
	{
		de = (struct ext2_dir_entry *) (root_block + 24);
		de->rec_len = block_size - 24;
	}
	else
	{
		de = (struct ext2_dir_entry *) (root_block + 12);
		de->rec_len = block_size - 12;
	}
	write_block (inode->i_block[0], root_block);
}

static void setup_tables (void)
{
	int i;
	int log_size = 0;
	int size;
	int block_map_size;

	memset (super_block_buffer, 0, EXT2_MIN_BLOCK_SIZE);
	MTIME = 0;
	WTIME = time (NULL);
	MAGIC = EXT2_SUPER_MAGIC;
	VALID = 1;

	size = block_size >> (EXT2_MIN_BLOCK_LOG_SIZE + 1);
	while (size)
	{
		log_size ++;
		size >>= 1;
	}
	BLOCKSIZE = log_size;

	printf ("Block Log Size = %d\n", log_size);
	size = frag_size >> (EXT2_MIN_FRAG_LOG_SIZE + 1);
	log_size = 0;
	while (size)
	{
		log_size ++;
		size >>= 1;
	}
	FRAGSIZE = log_size;
	printf ("Fragment Log Size = %d\n", log_size);

	BLOCKS = blocks / (block_size / EXT2_MIN_BLOCK_SIZE);
	RBLOCKS = (BLOCKS * reserved_ratio) / 100;
	FRAGSPERGROUP = blocks_per_group * (block_size / frag_size);
	BLOCKSPERGROUP = blocks_per_group;

	group_desc_count = (((blocks - NORM_FIRSTBLOCK) * frags_per_block) / BLOCKSPERGROUP);
	if (group_desc_count * BLOCKSPERGROUP < (blocks - NORM_FIRSTBLOCK) * frags_per_block)
		group_desc_count ++;
	if (group_desc_count % EXT2_DESC_PER_BLOCK)
		desc_blocks = (group_desc_count / EXT2_DESC_PER_BLOCK) + 1;
	else
		desc_blocks = group_desc_count / EXT2_DESC_PER_BLOCK;
	group_desc_size = desc_blocks * block_size;

/* some magic nrs: 1 inode / 4096 bytes (now use the inode ratio) */
	INODES = (blocks * EXT2_MIN_BLOCK_SIZE) / inode_ratio;

/* Round the inodes count to fully use each group descriptor */
	if (INODES % group_desc_count)
		INODESPERGROUP = (INODES / group_desc_count) + 1;
	 else
		INODESPERGROUP = INODES / group_desc_count;

/* Round the inodes count to fully use each block in each descriptor */
	if (INODESPERGROUP % EXT2_INODES_PER_BLOCK)
		INODESPERGROUP = ((INODESPERGROUP / EXT2_INODES_PER_BLOCK) + 1) *
				   EXT2_INODES_PER_BLOCK;
	INODES = INODESPERGROUP * group_desc_count;

	FIRSTBLOCK = NORM_FIRSTBLOCK;

	inode_map = malloc (INODES / 8 + 1);
	if (!inode_map)
		die ("cannot allocate inode bitmap\n");
	memset (inode_map, 0xff, INODES / 8 + 1);
	for (i = EXT2_FIRST_INO; i <= INODES; i++)
		unmark_inode (i);

	block_map_size = (BLOCKS / 8 + 1);
	if (block_map_size % (block_size * 8))
		block_map_size = (block_map_size / (block_size * 8) + 1) *
				 (block_size * 8);
	block_map = malloc (block_map_size);
	if (!block_map)
		die ("cannot allocate block bitmap\n");
	memset (block_map, 0xff, block_map_size);

	bad_map = malloc (block_map_size);
	if (!bad_map)
		die ("cannot allocate bad block bitmap\n");
	memset (bad_map, 0, block_map_size);

	for (i = FIRSTBLOCK; i < BLOCKS; i++)
		unmark_block (i);

	inode_buffer = malloc (INODESPERGROUP * sizeof (struct ext2_inode));
	if (!inode_buffer)
		die ("Unable to allocate buffer for inodes");
	memset (inode_buffer, 0, INODESPERGROUP * sizeof (struct ext2_inode));

	group_desc = malloc (group_desc_size);
	if (!group_desc)
		die ("Unable to allocate buffer for groups descriptors");
	memset (group_desc, 0, group_desc_size);

	printf ("%d inodes\n", INODES);
	printf ("%d blocks\n", BLOCKS);
	printf ("%d blocks reserved for root\n", RBLOCKS);
	printf ("First data block=%d (%d)\n", FIRSTBLOCK, NORM_FIRSTBLOCK);
	printf ("Block size=%d\n", EXT2_MIN_BLOCK_SIZE << BLOCKSIZE);
	printf ("%d blocks group%s\n", group_desc_count, (group_desc_count > 1) ? "s" : "");
	printf ("%d blocks per group\n", BLOCKSPERGROUP);
	printf ("%d fragments per group\n", FRAGSPERGROUP);
	printf ("%d inodes per group\n", INODESPERGROUP);
}

/*
 * Perform a test of a block; return the number of
 * blocks readable/writeable.
 */
static long do_check (char * buffer, int try, unsigned int current_block)
{

	long got;
	
	/* Seek to the correct loc. */
	if (lseek (dev, current_block * block_size, SEEK_SET) !=
                       current_block * block_size)
                 die("seek failed during testing of blocks");

	/* Try the read */
	got = read (dev, buffer, try * block_size);
	if (got < 0)
		got = 0;	
	if (got & (block_size - 1))
		printf("Weird values in do_check: probably bugs\n");
	got /= block_size;
	return got;
}

static unsigned int currently_testing = 0;

static void alarm_intr (int alnum)
{
	if (currently_testing >= BLOCKS)
		return;
	signal (SIGALRM, alarm_intr);
	alarm (5);
	if (!currently_testing)
		return;
	printf ("%d... ", currently_testing * (block_size / EXT2_MIN_BLOCK_SIZE));
	fflush (stdout);
}

static void check_blocks (void)
{

	int try, got;

	if (verbose)
	{
		printf ("Searching for bad blocks ");
		fflush (stdout);
	}
	currently_testing = 0;
	if (verbose)
	{
		signal (SIGALRM, alarm_intr);
		alarm (5);
	}
	try = TEST_BUFFER_BLOCKS;
	while (currently_testing < BLOCKS)
	{
		if (currently_testing + try > BLOCKS)
			try = BLOCKS - currently_testing;
		got = do_check (blkbuf, try, currently_testing);
		currently_testing += got;
		if (got == try)
		{
			try = TEST_BUFFER_BLOCKS;
			continue;
		}
		else
			try = 1;
		if (currently_testing < FIRSTBLOCK)
			die ("bad blocks before data-area: cannot make fs");
		mark_bad (currently_testing);
		mark_block (currently_testing);
		badblocks++;
		currently_testing++;
	}
	if (verbose)
		printf ("\n");
	if (badblocks)
		printf ("%d bad block%s\n", badblocks,
			(badblocks > 1) ? "s" : "");
}

static void get_list_blocks (char *filename)
{

	FILE *listfile;
	unsigned long blockno;

	listfile = fopen (filename,"r");
	if (listfile == (FILE *)NULL)
		die ("Can't open file of bad blocks");
	while (!feof (listfile))
	{
		fscanf (listfile, "%d\n", &blockno);
		mark_bad (blockno);
		mark_block (blockno);
		badblocks++;
	}
	fclose (listfile);
	if (badblocks)
		printf("%d bad block%s\n", badblocks,
			(badblocks>1) ? "s" : "");
}

static int check_size (int size)
{
	int tmp = size;

	while (tmp != 0)
	{
		if ((tmp & 1) && (tmp & ~1))
		{
			printf ("%d is not a power of two\n", size);
			return 0;
		}
		tmp >>= 1;
	}
	return 1;
}

static int valid_offset (int fd, int offset)
{
	char ch;

	if (lseek (fd, offset, 0) < 0 )
		return 0;
	if (read (fd, &ch, 1) < 1 )
		return 0;
	return 1;
}

static int count_blocks (char *filename)
{
	unsigned long high, low;
	int fd;

	if ((fd = open (filename, O_RDONLY)) < 0 )
	{
		perror (filename);
		exit (1);
	}
	low = 0;

	for (high = 1; valid_offset (fd, high); high *= 2 )
		low = high;
	while (low < high - 1)
	{
		const int mid = (low + high) / 2;

		if (valid_offset (fd, mid))
			low = mid;
		else
			high = mid;
	}
	valid_offset (fd, 0);
	close (fd);
	return (low+1) / 1024 ;
}

int main (int argc, char **argv)
{
	char c;
	char * tmp;
	struct stat statbuf;
	char * listfile = NULL;
	unsigned long lpf_ino;

	fprintf (stderr, "mke2fs 0.2d, 93/03/30 for EXT2 FS %s\n",
		 EXT2FS_VERSION);
	if (argc && *argv)
		program_name = *argv;
	if (INODE_SIZE * EXT2_INODES_PER_BLOCK != EXT2_MIN_BLOCK_SIZE)
		die("bad inode size");
	while ((c = getopt (argc, argv, "b:cf:g:i:l:m:tv")) != EOF)
		switch (c)
		{
			case 'b':
				block_size = strtol (optarg, &tmp, 0);
				if (*tmp)
				{
					printf ("bad block size: %s\n",
						optarg);
					usage ();
				}
				if (! check_size (block_size) ||
				    block_size < EXT2_MIN_BLOCK_SIZE ||
				    block_size > EXT2_MAX_BLOCK_SIZE)
					die ("bad block size");
				if (block_size != 1024)
					die ("Only 1024 bytes blocks are supported (yet)");
				break;
			case 'c':
			case 't':
				check = 1;
				break;
			case 'f':
				frag_size = strtol (optarg, &tmp, 0);
				if (*tmp)
				{
					printf ("bad fragment size: %s\n",
						optarg);
					usage ();
				}
				if (! check_size (frag_size) ||
				    frag_size < EXT2_MIN_FRAG_SIZE ||
				    frag_size > EXT2_MAX_FRAG_SIZE)
					die ("bad fragment size");
				printf ("Fragments are not supported yet, ignoring -f %d\n", frag_size);
				frag_size = 1024;
				break;
			case 'g':
				blocks_per_group = strtol (optarg, &tmp, 0);
				if (*tmp || (blocks_per_group % 8) != 0)
				{
					printf ("bad blocks per group: %s\n",
						optarg);
					usage ();
				}
			case 'i':
				inode_ratio = strtol (optarg, &tmp, 0);
				if (*tmp || inode_ratio < 1024)
				{
					printf ("bad inode ratio : %s\n",
						optarg);
					usage ();
				}
				break;
			case 'l':
				listfile = optarg;
				break;
			case 'm':
				reserved_ratio = strtol (optarg, &tmp, 0);
				if (*tmp || reserved_ratio > 50)
				{
					printf ("bad reserved block ratio : %s\n", optarg);
					usage ();
				}
				break;
			case 'v':
				verbose = 1;
				break;
			default:
				usage ();
		}
	device_name = argv [optind];
	if (optind == argc - 2)
		blocks = strtol (argv[optind + 1], &tmp, 0);
	else if (optind == argc - 1)
	{
		blocks = count_blocks (device_name);
		tmp = "";
	}
	else
		usage ();
	if (*tmp)
	{
		printf ("bad block count : %s\n", argv[optind + 1]);
		usage ();
	}
	if (check && listfile)
		die("-c and -l are incompatible\n");
	if (blocks < 10)
		die("number of blocks must be greater than 10.");
	frags_per_block = block_size / frag_size;
	dev = open (device_name, O_RDWR);
	if (dev < 0)
		die("unable to open %s");
	if (fstat (dev, &statbuf) < 0)
		die ("unable to stat %s");
	if (!S_ISBLK (statbuf.st_mode))
		check = 0;
	else if (statbuf.st_rdev == 0x0300 || statbuf.st_rdev == 0x0340)
		die ("will not try to make filesystem on '%s'");
	setup_tables ();
	if (check)
		check_blocks ();
        else if (listfile)
                get_list_blocks (listfile);
	make_group_desc ();
	lpf_ino = make_lpf_inode ();
	make_root_inode (lpf_ino);
	make_bad_inode ();
	count_free_blocks_and_inodes ();
	write_tables ();
	exit (0);
}
