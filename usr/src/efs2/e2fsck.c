/*
 * e2fsck.c - a consistency checker for the new extended file system.
 *
 * Copyright (C) 1992, 1993 Remy Card (card@masi.ibp.fr)
 *
 * Copyright (C) 1991 Linus Torvalds. This file may be redistributed as per
 * the GNU copyright.
 */

/*
 * 09.11.91  -  made the first rudimetary functions
 *
 * 10.11.91  -  updated, does checking, no repairs yet.
 *		Sent out to the mailing-list for testing.
 *
 * 14.11.91  -	Testing seems to have gone well. Added some
 *		correction-code, and changed some functions.
 *
 * 15.11.91  -  More correction code. Hopefully it notices most
 *		cases now, and tries to do something about them.
 *
 * 16.11.91  -  More corrections (thanks to Mika Jalava). Most
 *		things seem to work now. Yeah, sure.
 *
 * 14.07.92  -  Modifications by Remy Card (card@masi.ibp.fr)
 *		Used the fsck source to create efsck.c
 *
 * 19.07.92  -	Many bugs fixed [the first release was full of bugs :-(]
 *		- init_zone_bitmap() and init_inode_bitmap() now check that
 *		  the zones/inodes free lists are not corrupted. If they are,
 *		  all zones/inodes are marked as unused and are corrected (I
 *		  hope) by check_counts).
 *		- init_zone_bitmap() and init_inode_bitmap() now check for
 *		  cycles in the free lists. Before, in case of a cycle, efsck
 *		  entered an infinite loop ...
 *		- salvage_inode_freelist() was wrong. It used inode-1 as index
 *		  instead of inode.
 *		- salvage_zone_freelist() and salvage_inode_freelist() now
 *		  try to keep the same first free zone/inode so there should
 *		  be less problems when checking a mounted file system (for
 *		  a mounted ext file system, the first free zone/inode numbers
 *		  are stored in the copy of the super block in memory and are
 *		  rewritten to disk on sync() and umount(), modifying the
 *		  first free zone/inode can lead to some inconsistency).
 *		- corrected the file name printing in get_inode().
 *		- corrected the "inode not used" test which was never true ...
 *		- added size checks : compare the size of each inode with the
 *		  number of blocks allocated for the inode.
 *		Remaining problem :
 *		- I think that there is some more work to do to correct the
 *		  free lists. Currently, efsck salvages (rebuilds) them and it
 *		  is a very primitive way to handle errors. Perhaps, it should
 *		  act in a more clever way by adding or removing zones/inodes
 *		  from the free lists. I don't know if it is very important ...
 *
 *  21.07.92  - Corrected check_sizes():
 *		- to count the last allocated block and	NOT the allocated
 *		  blocks count, so it now counts holes,
 *		- fixed the bug causing the message 'incorrect size 0
 *		  (counted = 0)'.
 *
 *  26.07.92  - efsck now understands triple indirection
 *
 *  11.08.92  -	Changes by Wayne Davison (davison@borland.com)
 *		- The badblock inode (2 on my system) is always read in and
 *		  the resulting bitmap is used to ensure that files don't use
 *		  these blocks in their data.  A minor tweak keeps efsck from
 *		  complaining about the inode not being referenced.
 *		- The -t option has been added to perform a read test of the
 *		  disk looking for new bad blocks.  Using -t gives efsck write-
 *		  permission for just the bad block changes (which might
 *		  include a rewrite of the free lists, inodes & root).  If no
 *		  repair options were specified and a file uses a bad block
 *		  only a warning is generated.  A block must be either unused
 *		  or "repaired" (dropped from the file) for it to be added to
 *		  the badblock inode.
 *		- Minor changes were made to the buffers to reduce their
 *		  number.
 *		- All the pluralizing outputs were changed so that 0's come out
 *		  plural (e.g. 0 bad blocks, not 0 bad block).
 *		- Fixed an off-by-one error in the INODE code.
 *		- Fixed a bug in the directory loop where it could infinite
 *		  loop
 *		  if we're checking bogus directory data (with a zero rec_len).
 *		- Fixed a bug in the size counting when dealing with the
 *		  triple-indirect zone list.
 *		- Twiddled the verbose code to use the counts stored in the
 *		  super block (which were just verified) rather than counting
 *		  them again.
 *		- The verbose code outputs the number of bad blocks on the
 *		  disk.
 *		- I removed 'm' from the usage list of options.
 *
 *  12.08.92  - Corrected (again) check_sizes() : it now complains only
 *		when the size of a file is less than the last allocated
 *		block.
 *
 *  13.08.92  - Changes to support the existing .badblocks file
 *	      - Added the -S argument which allows salvaging the free lists
 *
 *  16.08.92  - Added some sanity checks on the directory entries
 *	      - corrected the test on rec_len to ensure that it is greater than
 *		or equal to 8 (was 16 before) because there may be 8 bytes
 *		directory entries (the last unused entry in a block can be 8
 *		bytes long)
 *	      - Use getopt() to get arguments
 *
 *  25.08.92  - Changes by Wayne Davison to create a "new" bad blocks inode
 *		from the old ".badblocks" file
 *
 *  27.08.92  - Fixed two stupid errors :
 *		- when the free lists were salvaged, the tables were not
 *		  written back to the disk
 *		- when the free inodes count and/or the free blocks count
 *		  were modified, the tables were not written back to the
 *		  disk
 *
 *  28.08.92  - Corrected init_zone_bitmap() and init_inode_bitmap() : sanity
 *		checks on the free zones/inodes are done BEFORE accessing the
 *		bitmaps.  When it was done after accessing the bitmaps, efsck
 *		could dump core when the list(s) was(were) corrupted.
 *
 *  03.09.92  - Corrected check_sizes() to ignore special files
 *
 *  01.11.92  - return a status code
 *		try to correct bad directory entries by	truncating the
 *		directory
 *
 *  15.11.92  - display ZONES and FIRSTZONE when invalid blocks are detected
 *		check inodes mode
 *		check links to directories (enable detection of cycles)
 *		try to correct bad directory entries in a smarter way
 *
 *  29.11.92  - efsck now understands sockets (Thanks to Juergen Schoenwaeldner
 *		<schoenw@ibr.cs.tu-bs.de>)
 *
 *  06.12.92  - added the '-b filename' option which reads a bad block list
 *		from the file
 *
 *  12.12.92  - corrected a minor bug in the free blocks list initialization
 *		(Thanks to Stephen Tweedie <sct@dcs.ed.ac.uk>)
 *
 *  01.01.93  - adapted to gcc 2.3.3 / libc 4.2
 *
 *  10.01.93  - OK.  I just deleted the previous e2fsck.c (after making a
 *		backup :) and I converted efsck alpha 12 to e2fsck. 
 *		Then, I have added the new functionalities that I had added
 *		to the old e2fsck.
 *
 *  16.01.93  -	added i_blocks check
 *		added free blocks and inodes count checks for each group
 *
 *  17.01.93  -	added block nr checks in check_block_nr()
 *
 *  21.01.93  - corrected an off by one error.  Thanks to Thomas Winder
 *		<tom@vlsivie.tuwien.ac.at> and to Drew Eckhardt
 *		<drew@nag.cs.Colorado.Edu>
 *
 *  24.01.93  - adapted to the super block and descriptors backup
 *
 *  28.02.93  - corrected bugs in set_lost_dir_ent
 *
 *  01.03.93  - added the new used_dirs_count field check
 *
 *  13.03.93  - corrected a bug in set_lost_dir_ent which caused bad
 *		   directory entries
 *		count the fast symbolic links
 *		make -c an alias to -t
 *		change the bad blocks test
 *
 * 14.03.93   -	display the bad blocks list
 *
 * 18.03.93   - check lost+found contents in set_lost_dir_ent
 *
 * 27.03.93   - finally uses *much* less memory than before (but e2fsck
 *		is now slower :-( )
 *
 * I've had no time to add comments - hopefully the function names
 * are comments enough. As with all file system checkers, this assumes
 * the file system is quiescent - don't use it on a mounted device
 * unless you can be sure nobody is writing to it (and remember that the
 * kernel can write to it when it searches for files).
 *
 * Usage: e2fsck [-acdflrstv] [-b file] device
 *	-a for automatic repairs
 *	-c for testing bad blocks on the fs
 *	-b for reading the bad blocks list from a file
 *	-d for debugging this program
 *	-f for checking the fs even if it is marked valid
 *	-l for a listing of all the filenames
 *	-r for repairs (interactive)
 *	-s for super-block info
 *	-t for testing bad blocks on the fs
 *	-v for verbose (tells how many files)
 *
 * The device may be a block device or a image of one, but this isn't
 * enforced (but it's not much fun on a character device :-). 
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include <stdlib.h>
#include <signal.h>
#include <termios.h>
#include <time.h>
#include <sys/stat.h>
#include <getopt.h>

#include <linux/fs.h>
#include <linux/ext2_fs.h>

#ifndef __GNUC__
#error "needs gcc for the bitop-__asm__'s"
#endif

#define TEST_BUFFER_BLOCKS	 16

static char blkbuf[BLOCK_SIZE * TEST_BUFFER_BLOCKS];

#define UPPER(size,n)		((size + ((n) - 1)) / (n))
#define INODE_SIZE		(sizeof (struct ext2_inode))
#define INODE_BLOCKS		UPPER(INODES, EXT2_INODES_PER_BLOCK (Super))
#define INODE_BUFFER_SIZE	(INODE_BLOCKS * EXT2_BLOCK_SIZE (Super))

extern int isatty(int);

static char * program_name = "e2fsck";
static char * listfile = NULL;
static char * device_name = NULL;
static int dev;

/* Command line options */
static int repair = 0;
static int automatic = 0;
static int verbose = 0;
static int list = 0;
static int show = 0;
static int debug = 0;
static int test_disk = 0;
static int force = 0;

static int no_bad_inode = 0;
static int no_lpf = 0;
static int lpf_corrupted = 0;

/* Files counts */
static int directory = 0;
static int regular = 0;
static int blockdev = 0;
static int chardev = 0;
static int links = 0;
static int symlinks = 0;
static int fast_symlinks;
static int fifo = 0;
static int total = 0;
static int badblocks = 0;
static int sockets = 0;

static int changed = 0; /* flags if the filesystem has been changed */

static int inode_blocks_per_group;
static int block_size;
static int addr_per_block;

/* File-name data */
#define MAX_DEPTH 50
static int name_depth = 0;   /* -1 when we're parsing the bad block list */
static char name_list [MAX_DEPTH] [EXT2_NAME_LEN + 1];

#if 0
static char * inode_buffer = NULL;

static struct ext2_inode * Inode;
#endif

static char super_block_buffer[BLOCK_SIZE];

static struct ext2_super_block * Super = (struct ext2_super_block *) super_block_buffer;

#define INODES		(Super->s_inodes_count)
#define BLOCKS		(Super->s_blocks_count)
#define RBLOCKS		(Super->s_r_blocks_count)
#define FREEBLOCKSCOUNT	(Super->s_free_blocks_count)
#define FREEINODESCOUNT	(Super->s_free_inodes_count)
#define FIRSTBLOCK	(Super->s_first_data_block)
#define BLOCKSIZE	(Super->s_log_block_size)
#define FRAGSIZE	(Super->s_log_frag_size)
#define BLOCKSPERGROUP	(Super->s_blocks_per_group)
#define FRAGSPERGROUP	(Super->s_frags_per_group)
#define INODESPERGROUP	(Super->s_inodes_per_group)
#define	MTIME		(Super->s_mtime)
#define	WTIME		(Super->s_wtime)
#define MAGIC		(Super->s_magic)
#define VALID		(Super->s_valid)

#define NORM_FIRSTBLOCK	1

static char * inode_map = NULL;
static char * block_map = NULL;
static char * bad_map = NULL;

static unsigned short * inode_count = NULL;
static unsigned short * block_count = NULL;

static unsigned long group_desc_count;
static unsigned long group_desc_size;
static unsigned long desc_blocks;
static struct ext2_group_desc * group_desc = NULL;

static unsigned long last_block_read;

static void recursive_check (unsigned int ino);

#define bitop(name,op) \
static inline int name(char * addr,unsigned int nr) \
{ \
	int __res; \
	__asm__ __volatile__("bt" op "l %1,%2; adcl $0,%0" \
			     :"=g" (__res) \
			     :"r" (nr),"m" (*(addr)),"0" (0)); \
	return __res; \
}

bitop(testbit, "")
bitop(setbit, "s")
bitop(clrbit, "r")

#define inode_in_use(x)	(testbit (inode_map, (x) - 1))
#define block_in_use(x)	(testbit (block_map, (x) - FIRSTBLOCK))
#define block_is_bad(x)	(testbit (bad_map, (x) - FIRSTBLOCK))

#define mark_inode(x)	(setbit (inode_map, (x) - 1), changed = 1)
#define unmark_inode(x)	(clrbit (inode_map, (x) - 1), changed = 1)

#define mark_block(x)	(setbit (block_map, (x) - FIRSTBLOCK), changed = 1)
#define unmark_block(x)	(clrbit (block_map, (x) - FIRSTBLOCK), changed = 1)

#define mark_bad(x)	(setbit (bad_map, (x) - FIRSTBLOCK), badblocks++)

/*
 * Volatile to let gcc know that this doesn't return.
 */
static volatile void fatal_error (const char * fmt_string)
{
	fflush (stdout);
	fprintf (stderr, fmt_string, program_name, device_name);
	exit (1);
}

#define usage()		fatal_error("Usage: %s [-acdflrstv] [-b filename] /dev/name\n")
#define die(str)	fatal_error("%s: " str "\n")

/*
 * This simply goes through the file-name data and prints out the
 * current file.
 */
static void print_current_name (void)
{
	int i = 0;

	while (i < name_depth)
		printf ("/%.255s", name_list [i++]);
	if (!name_depth)
		printf ("/");
	else if (name_depth == -1)
		printf ("{Bad-Block-List}");
}

static int ask (const char * string, int def)
{
	int c;

	if (!repair)
	{
		printf ("%s? no\n", string);
		return 0;
	}
	if (automatic)
	{
		printf ("%s? %s\n", string, def ? "yes" : "no");
		return def;
	}
	printf (def ? "%s (y/n)? " : "%s (n/y)? ", string);
	for (;;)
	{
		fflush (stdout);
		if ((c = getchar()) == EOF)
			break;
		c = toupper(c);
		if (c == 'Y')
		{
			def = 1;
			break;
		}
		else if (c == 'N')
		{
			def = 0;
			break;
		}
		else if (c == ' ' || c == '\n')
			break;
	}
	if (def)
		printf ("yes\n");
	else
		printf ("nno\n");
	return def;
}
/*
 * These functions manage a 1KB block cache used by the inode cache
 */
static unsigned long cache_block_nr = 0;
static unsigned char cache_block_dirt = 0;
static char cache_buffer [BLOCK_SIZE];

static char * cache_read_block (unsigned long block_nr)
{
	if (block_nr != cache_block_nr)
	{
		if (cache_block_dirt)
		{
			if (cache_block_nr * BLOCK_SIZE !=
			    lseek (dev, cache_block_nr * BLOCK_SIZE, SEEK_SET))
				die ("seek failed in cache_read_block");
			if (BLOCK_SIZE != write (dev, cache_buffer, BLOCK_SIZE))
				die ("write failed in cache_read_block");
		}
		cache_block_dirt = 0;
		cache_block_nr = block_nr;
		if (cache_block_nr * BLOCK_SIZE !=
		    lseek (dev, cache_block_nr * BLOCK_SIZE, SEEK_SET))
			die ("seek failed in cache_read_block");
		if (BLOCK_SIZE != read (dev, cache_buffer, BLOCK_SIZE))
			die ("read failed in cache_read_block");
	}
	return cache_buffer;
}

static void cache_write_block (unsigned long block_nr)
{
	if (block_nr != cache_block_nr)
		die ("cache_write_block: block_nr != cache_block_nr");
	cache_block_dirt = 1;
}

static void cache_flush (void)
{
	if (cache_block_dirt)
	{
		if (cache_block_nr * BLOCK_SIZE !=
		    lseek (dev, cache_block_nr * BLOCK_SIZE, SEEK_SET))
			die ("seek failed in cache_flush");
		if (BLOCK_SIZE != write (dev, cache_buffer, BLOCK_SIZE))
			die ("write failed in cache_flush");
	}
	cache_block_dirt = 0;
}
		
/*
 * These functions manage a cache for the inodes
 */
static void read_inode (unsigned long ino, struct ext2_inode * inode)
{
	unsigned long group;
	unsigned long block;
	unsigned long block_nr;
	int i;
	char * buffer;

	group = (ino - 1) / EXT2_INODES_PER_GROUP(Super);
	block = ((ino - 1) % EXT2_INODES_PER_GROUP(Super)) /
		EXT2_INODES_PER_BLOCK(Super);
	i = ((ino - 1) % EXT2_INODES_PER_GROUP(Super)) %
		EXT2_INODES_PER_BLOCK(Super);
	block_nr = group_desc[group].bg_inode_table + block;
	buffer = cache_read_block (block_nr);
	memcpy (inode, (struct ext2_inode *) buffer + i,
		sizeof (struct ext2_inode));
}

static void write_inode (unsigned long ino, struct ext2_inode * inode)
{
	unsigned long group;
	unsigned long block;
	unsigned long block_nr;
	int i;
	char * buffer;

	group = (ino - 1) / EXT2_INODES_PER_GROUP(Super);
	block = ((ino - 1) % EXT2_INODES_PER_GROUP(Super)) /
		EXT2_INODES_PER_BLOCK(Super);
	i = ((ino - 1) % EXT2_INODES_PER_GROUP(Super)) %
		EXT2_INODES_PER_BLOCK(Super);
	block_nr = group_desc[group].bg_inode_table + block;
	buffer = cache_read_block (block_nr);
	memcpy ((struct ext2_inode *) buffer + i, inode,
		sizeof (struct ext2_inode));
	cache_write_block (block_nr);
	changed = 1;
}

/*
 * check_block_nr checks to see that *nr is a valid block nr. If it
 * isn't, it will possibly be repaired. Check_block_nr returns != 0
 * if it changed the nr.
 */
static int check_block_nr (unsigned long * nr)
{
	unsigned long group;

	if (debug)
		printf ("DEBUG: check_block_nr (&%d)\n", *nr);
	if (!*nr)
		return 0;
	if (*nr < FIRSTBLOCK)
		printf ("Block nr %d < FIRSTBLOCK (%d) in file `", *nr, FIRSTBLOCK);
	else if (*nr >= BLOCKS)
		printf ("Block nr %d > BLOCKS (%d) in file `", *nr, BLOCKS);
	else
	{
		group = (*nr - FIRSTBLOCK) / BLOCKSPERGROUP;
		if (*nr == group_desc[group].bg_block_bitmap)
			printf ("Block nr %d is the block bitmap of group %d in file `",
				*nr, group);
		else if (*nr == group_desc[group].bg_inode_bitmap)
			printf ("Block nr %d is the inode bitmap of group %d in file `",
				*nr, group);
		else if (*nr >= group_desc[group].bg_inode_table &&
			 *nr < group_desc[group].bg_inode_table + inode_blocks_per_group)
			printf ("Block nr %d is in the inode table of group %d in file `",
				*nr, group);
		else
			return 0;
	}
	print_current_name ();
	printf ("'. ");
	if (ask ("Remove block", 1))
	{
		*nr = 0;
		changed = 1;
		return 1;
	}
	return 0;
}

/*
 * read-block reads block *nr into the buffer at addr. It returns
 * 0 if the *nr is unchanged, 1 if it was changed.
 */
static int read_block (unsigned long * nr, char * addr)
{
	int blk_chg = check_block_nr(nr);

	if (debug)
		printf ("DEBUG: read_block (&%d, %d)\n", *nr, (int) addr);
	last_block_read = *nr;

	if (!*nr || *nr >= BLOCKS)
	{
		memset (addr, 0, BLOCK_SIZE);
		return changed;
	}
	if (BLOCK_SIZE * (*nr) != lseek (dev, BLOCK_SIZE * (*nr), SEEK_SET))
		die ("seek failed in read_block");
	if (BLOCK_SIZE != read (dev, addr, BLOCK_SIZE))
	{
		printf ("Read error: bad block in file '");
		print_current_name ();
		printf ("'\n");
		memset(addr, 0, BLOCK_SIZE);
	}
	return blk_chg;
}

/*
 * write_block writes block nr to disk.
 */
static void write_block (unsigned int nr, char * addr)
{
	if (debug)
		printf ("DEBUG: write_block(%d, %d)\n", nr, (int) addr);
	if (!nr)
		return;
	if (nr < FIRSTBLOCK || nr >= BLOCKS)
	{
		printf ("Internal error: trying to write bad block (%d)\n"
			"Write request ignored\n", nr);
		return;
	}
	if (BLOCK_SIZE * nr != lseek (dev, BLOCK_SIZE * nr, SEEK_SET))
		die ("seek failed in write_block");
	if (BLOCK_SIZE != write (dev, addr, BLOCK_SIZE))
	{
		printf ("Write error: bad block (%d) in file '", nr);
		print_current_name ();
		printf ("'\n");
	}
}

/*
 * mapped_read_block reads block nr blknr from the specified file.
 * it returns 1 if the inode has been changed due to bad block nrs
 */
static int mapped_read_block (struct ext2_inode * inode,
			      unsigned int blknr, char * addr)
{
	unsigned long *ind  = (long *)(blkbuf + BLOCK_SIZE);
	unsigned long *dind = (long *)(blkbuf + BLOCK_SIZE * 2);
	unsigned long *tind = (long *)(blkbuf + BLOCK_SIZE * 3);
	int result;

	if (debug)
		printf ("DEBUG: mapped_read_block(%d,%d,%d)\n", 
			(int)inode, blknr, (int) addr);
	if (blknr < EXT2_NDIR_BLOCKS)
		return read_block (blknr + inode->i_block, addr);
	blknr -= EXT2_NDIR_BLOCKS;
	if (blknr < addr_per_block)
	{
		result = read_block (EXT2_IND_BLOCK + inode->i_block,
				     (char *) ind);
		if (read_block (blknr + ind, addr))
		{
			result = 1;
			write_block (inode->i_block[EXT2_IND_BLOCK],
				     (char *) ind);
		}
		return result;
	}
	blknr -= addr_per_block;
	if (blknr < addr_per_block * addr_per_block)
	{
		result = read_block (EXT2_DIND_BLOCK + inode->i_block,
				     (char *) dind);
		if (read_block (blknr / addr_per_block + dind, (char *) ind))
		{
			result = 1;
			write_block (inode->i_block[EXT2_DIND_BLOCK],
				     (char *) dind);
		}
		if (read_block (blknr % addr_per_block + ind, addr))
		{
			result = 1;
			write_block (dind [blknr / addr_per_block],
				     (char *) ind);
		}
		return result;
	}
	blknr -= addr_per_block * addr_per_block;
	result = read_block (EXT2_TIND_BLOCK + inode->i_block,
			     (char *) tind);
	if (read_block ((blknr >> 16) + tind, (char *) dind))
	{
		result = 1;
		write_block (inode->i_block[EXT2_TIND_BLOCK], (char *) tind);
	}
	if (read_block (((blknr >> 8) & (addr_per_block - 1)) + dind,
			(char *) ind))
	{
		result = 1;
		write_block (tind [blknr >> 16], (char *) dind);
	}
	if (read_block (blknr % addr_per_block + ind, addr))
	{
		result = 1;
		write_block (dind [(blknr >> 8) & (addr_per_block - 1)],
			     (char *) ind);
	}
	return result;
}

static void write_inode_bitmap (void)
{
	int i;
	char * inode_bitmap = inode_map;

	if (debug)
		printf ("write_inode_bitmap()\n");
	for (i = 0; i < group_desc_count; i++)
	{
		if (group_desc[i].bg_inode_bitmap * block_size !=
		    lseek (dev, group_desc[i].bg_inode_bitmap * block_size,
			   SEEK_SET))
			die ("seek failed in write_inode_bitmap");
		if (INODESPERGROUP / 8 != write (dev, inode_bitmap,
						 INODESPERGROUP / 8))
			die ("write failed in write_inode_bitmap");
		inode_bitmap += INODESPERGROUP / 8;
	}
}

static void write_block_bitmap (void)
{
	int i;
	char * block_bitmap = block_map;

	if (debug)
		printf ("write_block_bitmap()\n");
	for (i = 0; i < group_desc_count; i++)
	{
		if (group_desc[i].bg_block_bitmap * block_size !=
		    lseek (dev, group_desc[i].bg_block_bitmap * block_size,
			   SEEK_SET))
			die ("seek failed in write_block_bitmap");
		if (BLOCKSPERGROUP / 8 != write (dev, block_bitmap,
						 BLOCKSPERGROUP / 8))
			die ("write failed in write_block_bitmap");
		block_bitmap += BLOCKSPERGROUP / 8;
	}
}

#if 0
static void write_inode_table (void)
{
	int i;
	char * inode_table = inode_buffer;
	unsigned long inode_chars_per_group = inode_blocks_per_group *
					      block_size;

	if (debug)
		printf ("write_inode_table()\n");
	for (i = 0; i < group_desc_count; i++)
	{
		if (group_desc[i].bg_inode_table * block_size !=
		    lseek (dev, group_desc[i].bg_inode_table * block_size,
			   SEEK_SET))
			die ("seek failed in write_inode_table");
		if (inode_chars_per_group != write (dev, inode_table,
						    inode_chars_per_group))
			die ("write failed in write_inode_table");
		inode_table += inode_chars_per_group;
	}
}
#endif

static void write_tables (void)
{
	if (debug)
		printf ("DEBUG: write_tables()\n");
	if (BLOCK_SIZE != lseek (dev, BLOCK_SIZE, SEEK_SET))
		die ("seek failed in write_tables");
	if (BLOCK_SIZE != write (dev, super_block_buffer, BLOCK_SIZE))
		die ("unable to write super-block");
	if (group_desc_size != write (dev, group_desc, group_desc_size))
		die ("Unable to write group descriptors");
	write_inode_bitmap ();
	write_block_bitmap ();
#if 0
	write_inode_table ();
#endif
}

static void read_inode_bitmap (void)
{
	int i;
	char * inode_bitmap = inode_map;

	if (debug)
		printf ("DEBUG: read_inode_bitmap()\n");
	for (i = 0; i < group_desc_count; i++)
	{
		if (debug)
			printf ("Group %d: inode bitmap at %d read at %d\n",
				i, group_desc[i].bg_inode_bitmap,
				inode_bitmap - inode_map);
		if (group_desc[i].bg_inode_bitmap * block_size !=
		    lseek (dev, group_desc[i].bg_inode_bitmap * block_size,
			   SEEK_SET))
			die ("seek failed in read_inode_bitmap");
		if (INODESPERGROUP / 8 != read (dev, inode_bitmap,
						INODESPERGROUP / 8))
			die ("read failed in read_inode_bitmap");
		inode_bitmap += INODESPERGROUP / 8;
	}
}

static void read_block_bitmap (void)
{
	int i;
	char * block_bitmap = block_map;

	if (debug)
		printf ("DEBUG: read_block_bitmap()\n");
	for (i = 0; i < group_desc_count; i++)
	{
		if (debug)
			printf ("Group %d: block bitmap at %d read at %d\n",
				i, group_desc[i].bg_block_bitmap,
				block_bitmap - block_map);
		if (group_desc[i].bg_block_bitmap * block_size !=
		    lseek (dev, group_desc[i].bg_block_bitmap * block_size,
			   SEEK_SET))
			die ("seek failed in read_block_bitmap");
		if (BLOCKSPERGROUP / 8 != read (dev, block_bitmap,
						BLOCKSPERGROUP / 8))
			die ("read failed in read_block_bitmap");
		block_bitmap += BLOCKSPERGROUP / 8;
	}
}

#if 0
static void read_inode_table (void)
{
	int i;
	char * inode_table = inode_buffer;
	unsigned long inode_chars_per_group = inode_blocks_per_group *
					      block_size;

	if (debug)
		printf ("DEBUG: read_inode_table()\n");
	for (i = 0; i < group_desc_count; i++)
	{
		if (debug)
			printf ("Group %d: inode table at %d for %d blocks\n",
				i, group_desc[i].bg_inode_table,
				inode_blocks_per_group /
				block_size);
		if (group_desc[i].bg_inode_table * block_size !=
		    lseek (dev, group_desc[i].bg_inode_table * block_size,
			   SEEK_SET))
			die ("seek failed in read_inode_table");
		if (inode_chars_per_group != read (dev, inode_table,
						   inode_chars_per_group))
			die ("read failed in read_inode_table");
		inode_table += inode_chars_per_group;
	}
}
#endif

static void check_desc (void)
{
	int i;
	int block = FIRSTBLOCK;

	for (i = 0; i < group_desc_count; i++)
	{
		if (group_desc[i].bg_block_bitmap < block ||
		    group_desc[i].bg_block_bitmap > block + BLOCKSPERGROUP)
		{
			printf ("Block bitmap for group %d not in group (%d)!",
				i, group_desc[i].bg_block_bitmap);
			die ("descriptors corrupted");
		}
		if (group_desc[i].bg_inode_bitmap < block ||
		    group_desc[i].bg_inode_bitmap > block + BLOCKSPERGROUP)
		{
			printf ("Inode bitmap for group %d not in group (%d)!",
				i, group_desc[i].bg_inode_bitmap);
			die ("descriptors corrupted");
		}
		if (group_desc[i].bg_inode_table < block ||
		    group_desc[i].bg_inode_table + inode_blocks_per_group >
		    block + BLOCKSPERGROUP)
		{
			printf ("Inode table for group %d not in group (%d)!",
				i, group_desc[i].bg_inode_table);
			die ("descriptors corrupted");
		}
		block += BLOCKSPERGROUP;
	}
}

static void read_tables (void)
{
	int i;

	if (debug)
		printf ("DEBUG: read_tables()\n");
	if (BLOCK_SIZE != lseek (dev, BLOCK_SIZE, SEEK_SET))
		die ("seek failed");
	if (BLOCK_SIZE != read (dev, super_block_buffer, BLOCK_SIZE))
		die ("unable to read super block");
	if (MAGIC != EXT2_SUPER_MAGIC)
		die ("bad magic number in super-block");
	if (VALID && !force)
	{
		printf ("%s is clean, no check.\n", device_name);
		exit (0);
	}
	if (BLOCKSIZE != 0 || BLOCK_SIZE != 1024)
		die("Only 1k blocks supported");

	addr_per_block = EXT2_ADDR_PER_BLOCK (Super);
	block_size = EXT2_BLOCK_SIZE (Super);
	inode_blocks_per_group = INODESPERGROUP /
				 EXT2_INODES_PER_BLOCK (Super);

	group_desc_count = BLOCKS / BLOCKSPERGROUP;
	if ((group_desc_count * BLOCKSPERGROUP) != BLOCKS)
		group_desc_count ++;
	if (group_desc_count % EXT2_DESC_PER_BLOCK (Super))
		desc_blocks = (group_desc_count / EXT2_DESC_PER_BLOCK (Super)) + 1;
	else
		desc_blocks = group_desc_count / EXT2_DESC_PER_BLOCK (Super);
	group_desc_size = desc_blocks * block_size;
	group_desc = malloc (group_desc_size);
	if (!group_desc)
		die ("Unable to allocate buffers for group descriptors");
	if (group_desc_size != read (dev, group_desc, group_desc_size))
		die ("Unable to read group descriptors");

	check_desc ();

	block_count = malloc(BLOCKS * sizeof (*block_count));
	if (!block_count)
		die ("Unable to allocate buffer for block count");

	inode_map = malloc ((INODES / 8) + 1);
	if (!inode_map)
		die ("Unable to allocate inodes bitmap\n");
	memset (inode_map, 0, (INODES / 8) + 1);
	read_inode_bitmap ();

	block_map = malloc (((BLOCKS - FIRSTBLOCK) / 8) + 1);
	if (!block_map)
		die ("Unable to allocate blocks bitmap\n");
	memset (block_map, 0, ((BLOCKS - FIRSTBLOCK) / 8) + 1);
	read_block_bitmap ();

	bad_map = malloc (((BLOCKS - FIRSTBLOCK) / 8) + 1);
	if (!bad_map)
		die ("Unable to allocate bad block bitmap\n");
	memset (bad_map, 0, ((BLOCKS - FIRSTBLOCK) / 8) + 1);

#if 0
	inode_buffer = malloc (INODE_BUFFER_SIZE);
	if (!inode_buffer)
		die ("Unable to allocate buffer for inodes");
	Inode = ((struct ext2_inode *) inode_buffer) - 1;
#endif
	inode_count = malloc ((INODES + 1) * sizeof (*inode_count));
	if (!inode_count)
		die ("Unable to allocate buffer for inode count");
#if 0
	read_inode_table ();
#endif

	if (NORM_FIRSTBLOCK != FIRSTBLOCK)
		printf ("Warning: First block != Norm first block\n");

	if (show)
	{
		printf ("Block size = %d\n", EXT2_BLOCK_SIZE (Super));
		printf ("Fragment size = %d\n", EXT2_FRAG_SIZE (Super));
		printf ("%d inode%s\n", INODES, (INODES != 1) ? "s" : "");
		printf ("%d block%s\n", BLOCKS, (BLOCKS != 1) ? "s" : "");
		printf ("%d reserved block%s\n", RBLOCKS,
			(RBLOCKS != 1) ? "s" : "");
		printf ("First data block = %d (%d)\n",
			FIRSTBLOCK, NORM_FIRSTBLOCK);
		printf ("%d free block%s\n", FREEBLOCKSCOUNT,
			(FREEBLOCKSCOUNT != 1) ? "s" : "");
		printf ("%d free inode%s\n", FREEINODESCOUNT,
			(FREEINODESCOUNT != 1) ? "s" : "");
		printf ("%d group%s (%d descriptors block%s)\n",
			group_desc_count, (group_desc_count != 1) ? "s" : "",
			desc_blocks, (desc_blocks != 1) ? "s" : "");
		for (i = 0; i < group_desc_count; i++)
			printf ("  Group 0: block bitmap at %d, "
				"inode bitmap at %d, "
				"inode table at %d\n",
				group_desc[i].bg_block_bitmap,
				group_desc[i].bg_inode_bitmap,
				group_desc[i].bg_inode_table);
		printf ("%d blocks per group\n", BLOCKSPERGROUP);
		printf ("%d fragments per group\n", FRAGSPERGROUP);
		printf ("%d inodes per group\n", INODESPERGROUP);
	}
}

static struct ext2_inode get_inode (unsigned int nr)
{
	struct ext2_inode inode;

	if (debug)
		printf ("DEBUG: get_inode (%d)\n", nr);
	if (!nr || nr > INODES)
	{
		inode.i_mode = 0;
		return inode;
	}
	total++;
	read_inode (nr, &inode);
	if (!inode_count [nr])
	{
		if (!inode_in_use (nr))
		{
			printf ("Inode %d marked not used, but used for file '",
				nr);
			print_current_name ();
			printf ("'\n");
			if (ask ("Mark in use", 1))
				mark_inode (nr);
		}
		if (S_ISDIR (inode.i_mode))
			directory++;
		else if (S_ISREG (inode.i_mode))
			regular++;
		else if (S_ISCHR (inode.i_mode))
			chardev++;
		else if (S_ISBLK (inode.i_mode))
			blockdev++;
		else if (S_ISLNK (inode.i_mode))
		{
			symlinks++;
			if (!inode.i_blocks)
				fast_symlinks++;
		}
		else if (S_ISFIFO (inode.i_mode))
			fifo++;
		else if (S_ISSOCK (inode.i_mode))
		        sockets++;
		else
		{
			printf ("Inode %d has bad mode (%o) ",
				nr, inode.i_mode);
			if (ask ("Set mode to file", 1))
			{
				inode.i_mode |= S_IFREG;
				write_inode (nr, &inode);
			}
#if 0
			else
				return NULL;
#endif
		}
	}
	else
		links++;
	if (! ++inode_count [nr])
	{
		printf ("Warning: inode count too big.\n");
		inode_count [nr] --;
	}
	return inode;
}

static void check_root (void)
{
	struct ext2_inode inode;

	if (debug)
		printf ("DEBUG: check_root()\n");
	read_inode (EXT2_ROOT_INO, &inode);
	if (!S_ISDIR (inode.i_mode))
		die ("root inode isn't a directory");
}

static int add_block (unsigned long * znr, int badflg)
{
	int result;

	if (debug)
		printf ("DEBUG: add_block(&%d,%d)\n", *znr, badflg);
	result = check_block_nr(znr);
	if (! *znr || *znr >= BLOCKS)
		return result;
	if (!badflg && block_is_bad (*znr))
	{
		printf ("Bad block used in file `");
		print_current_name ();
		printf ("'. ");
		if (ask ("Clear", 1))
		{
			*znr = 0;
			changed = 1;
			return result;
		}
	}
	else if (block_count [*znr])
	{
		printf ("Block %d has been used before. Now in file `", *znr);
		print_current_name ();
		printf ("'. ");
		if (ask ("Clear", 1))
		{
			*znr = 0;
			changed = 1;
			return result;
		}
	}
	if (!block_in_use (*znr))
	{
		printf ("Block %d in file `", *znr);
		print_current_name ();
		printf ("' is marked not in use. ");
		if (ask ("Mark in use", 1))
			mark_block (*znr);
	}
	if (badflg)
	{
		if (debug)
			printf ("DEBUG: Marking %d as bad.\n", *znr);
		mark_bad (*znr);
	}
	if (! ++ block_count [*znr])
		block_count [*znr] --;
	return result;
}

static int add_block_ind (unsigned long * znr, int badflg)
{
	char *blk = blkbuf + BLOCK_SIZE;
	int i, result, chg_blk = 0;

	if (debug)
		printf ("DEBUG: add_block_ind (&%d,%d)\n", *znr, badflg);
	result = add_block (znr, 0);
	if (! *znr || *znr >= BLOCKS || block_is_bad (*znr))
		return result;
	read_block(znr, blk);
	for (i = 0; i < (BLOCK_SIZE >> 2); i++)
		chg_blk |= add_block (i + (unsigned long *) blk, badflg);
	if (chg_blk)
		write_block (*znr, blk);
	return result;
}

static int add_block_dind (unsigned long * znr, int badflg)
{
	char *blk = blkbuf + BLOCK_SIZE * 2;
	int i, result, blk_chg = 0;

	if (debug)
		printf ("DEBUG: add_block_dind (&%d,%d)\n", *znr, badflg);
	result = add_block (znr, 0);
	if (! *znr || *znr >= BLOCKS || block_is_bad (*znr))
		return result;
	read_block(znr, blk);
	for (i = 0; i < (BLOCK_SIZE >> 2); i++)
		blk_chg |= add_block_ind (i + (unsigned long *) blk, badflg);
	if (blk_chg)
		write_block (*znr, blk);
	return result;
}

static int add_block_tind (unsigned long * znr, int badflg)
{
	char *blk = blkbuf + BLOCK_SIZE * 3;
	int i, result, blk_chg = 0;

	if (debug)
		printf ("DEBUG: add_block_tind (&%d,%d)\n", *znr, badflg);
	result = add_block (znr, 0);
	if (! *znr || *znr >= BLOCKS || block_is_bad (*znr))
		return result;
	read_block(znr, blk);
	for (i = 0; i < (BLOCK_SIZE >> 2); i++)
		blk_chg |= add_block_dind (i + (unsigned long *) blk, badflg);
	if (blk_chg)
		write_block (*znr, blk);
	return result;
}

/*
 * Perform a test of a block; return the number of blocks readable/writeable.
 */
static long do_test (char * buffer, int try, unsigned int current_block)
{
	long got;

	if (debug)
		printf ("DEBUG: do_test (buf,%d,%d)\n", try, current_block);
	
	/* Seek to the correct loc. */
	if (lseek (dev, current_block * BLOCK_SIZE, SEEK_SET) !=
                       current_block * BLOCK_SIZE)
                 die("seek failed during testing of blocks");

	/* Try the read */
	got = read (dev, buffer, try * BLOCK_SIZE);
	if (got < 0)
		got = 0;	
	if (got & (BLOCK_SIZE - 1))
		printf ("Weird values in do_test: probably bugs\n");
	got /= BLOCK_SIZE;
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
	printf ("%d... ", currently_testing);
	fflush (stdout);
}

static void test_blocks (void)
{
	int try, got;
	int currently_bad = badblocks;

	if (debug)
		printf ("DEBUG: test_blocks ()\n");
	if (verbose)
		printf ("Testing the disk for bad blocks\n");
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
		if (currently_testing >= FIRSTBLOCK)
		{
			int i;

			/* Skip blocks that are known to be bad */
			while (currently_testing < BLOCKS &&
			       block_is_bad (currently_testing))
				currently_testing++;
			if (currently_testing >= BLOCKS)
				break;
			/* Shorten group if it contains a bad block */
			for (i = try - 1; i; i--)
			{
				if (block_is_bad (currently_testing + try - i))
				{
					try -= i;
					break;
				}
			}
		}
		else if (currently_testing + try > FIRSTBLOCK)
			try = FIRSTBLOCK - currently_testing;
		got = do_test (blkbuf, try, currently_testing);
		currently_testing += got;
		if (got == try)
		{
			try = TEST_BUFFER_BLOCKS;
			continue;
		}
		else
			try = 1;
		if (debug)
			printf ("Only got %d\n", got);
		if (currently_testing < FIRSTBLOCK)
			die ("bad blocks before data-area: cannot fix!");
		mark_bad (currently_testing);
		/* If this block is in use, we'll warn about it later.
		   For now, just make sure it is marked as used. */
		if (!block_in_use (currently_testing))
		{
			mark_block (currently_testing);
			FREEBLOCKSCOUNT--;
		}
		currently_testing++;
	}
	if (verbose)
		printf ("\n");
	if (badblocks - currently_bad)
		printf ("found %d bad block%s\n", badblocks - currently_bad,
			(badblocks - currently_bad != 1) ? "s" : "");
}

static void read_bad_blocks (void)
{
	struct ext2_inode inode;
	int i;

	if (debug)
		printf ("DEBUG: read_bad_blocks()\n");
	if (verbose)
		printf ("Reading bad blocks list\n");
	read_inode (EXT2_BAD_INO, &inode);
	if (inode.i_mode || inode.i_links_count)
	{
		printf ("Note: file system does not have a badblock inode.\n");
		if (test_disk)
			fatal_error ("You can't use -t on this file system.\n");
		no_bad_inode = 1;
		return;
	}
	if (!inode_in_use (EXT2_BAD_INO))
	{
		printf ("The badblock inode is marked as unused. ");
		if (test_disk && !repair)
			fatal_error ("\nYou need to specify -a or -r to repair this.");
		if (ask ("Mark in use", 1))
			mark_inode (EXT2_BAD_INO);
	}

	name_depth = -1;    /* Flag to indicate we're parsing the bad blocks */
	for (i = 0; i < EXT2_NDIR_BLOCKS ; i++)
		add_block (i + inode.i_block, 1);
	add_block_ind (EXT2_IND_BLOCK + inode.i_block, 1);
	add_block_dind (EXT2_DIND_BLOCK + inode.i_block, 1);
	add_block_tind (EXT2_TIND_BLOCK + inode.i_block, 1);
	name_depth = 0;
}

static void read_bad_blocks_from_file (void)
{
	FILE *f;
	unsigned long blockno;

	f = fopen (listfile, "r");
	if (f == (FILE *) NULL)
		die ("Can't open file of bad blocks");
	while (!feof (f))
	{
		fscanf (f, "%d", &blockno);
		mark_bad (blockno);
		if (!block_in_use (blockno))
		{
			mark_block (blockno);
			FREEBLOCKSCOUNT--;
		}
	}
	fclose (f);
}

static void display_bad_blocks (void)
{
	int block;
	int badblocks_count = 0;

	printf ("Bad blocks list:");
	for (block = 0; block < BLOCKS; block++)
		if (block_is_bad (block))
		{
			badblocks_count ++;
			printf (" %d", block);
		}
	if (badblocks_count == 0)
		printf (" none\n");
	else
		printf ("\n");
}

static void trunc_direct (struct ext2_inode * inode)
{
	int i;
#define DIRECT_BLOCK ((inode->i_size + 1023) >> 10)

	for (i = DIRECT_BLOCK ; i < EXT2_NDIR_BLOCKS ; i++)
	{
		unmark_block (inode->i_block[i]);
		inode->i_block[i] = 0;
	}
}

static void trunc_indirect (struct ext2_inode * inode, int offset,
			    unsigned long * p)
{
	int i;
	unsigned int dirty = 0;
	unsigned long *ind = (unsigned long *)(blkbuf + BLOCK_SIZE);
#define INDIRECT_BLOCK (DIRECT_BLOCK - offset)

	if (!*p)
		return;
	read_block (p, (char *) ind);
	for (i = INDIRECT_BLOCK ; i < addr_per_block ; i++)
	{
		if (! ind[i])
			continue;
		dirty = 1;
		unmark_block (ind[i]);
		ind[i] = 0;
	}
	for (i = 0; i < addr_per_block; i++)
		if (ind[i++])
			break;
	if (i >= addr_per_block)
	{
		unmark_block (*p);
		*p = 0;
	}
	else
		if (dirty)
			write_block(*p, (char *) ind);
}

static void trunc_dindirect (struct ext2_inode * inode, int offset,
			     unsigned long * p)
{
	int i;
	unsigned int dirty = 0;
	unsigned long * dind = (unsigned long *)(blkbuf + BLOCK_SIZE * 2);
#define DINDIRECT_BLOCK ((DIRECT_BLOCK - offset) >> 8)

	if (!*p)
		return;
	read_block (p, (char *) dind);
	for (i = DINDIRECT_BLOCK ; i < addr_per_block ; i ++)
	{
		if (!dind[i])
			continue;
		trunc_indirect (inode, offset + (i << 8), &dind[i]);
		dirty = 1;
	}
	for (i = 0; i < addr_per_block; i++)
		if (dind[i])
			break;
	if (i >= addr_per_block)
	{
		unmark_block (*p);
		*p = 0;
	}
	else
		if (dirty)
			write_block (*p, (char *) dind);
}

static void trunc_tindirect (struct ext2_inode * inode)
{
	int i;
	unsigned int dirty = 0;
	unsigned long * tind = (unsigned long *)(blkbuf + BLOCK_SIZE * 3);
#define TINDIRECT_BLOCK	((DIRECT_BLOCK - (addr_per_block * addr_per_block + \
			addr_per_block + EXT2_NDIR_BLOCKS)) >> 16)

	if (!inode->i_block[EXT2_TIND_BLOCK]);
		return;
	read_block (&inode->i_block[EXT2_TIND_BLOCK], (char *) tind);
	for (i = TINDIRECT_BLOCK ; i < addr_per_block ; i ++)
	{
		trunc_dindirect(inode, EXT2_NDIR_BLOCKS + addr_per_block +
				addr_per_block * addr_per_block + (i << 16),
				tind + i);
		dirty = 1;
	}
	for (i = 0; i < addr_per_block; i++)
		if (tind[i])
			break;
	if (i >= addr_per_block)
	{
		unmark_block (inode->i_block[EXT2_TIND_BLOCK]);
		inode->i_block[EXT2_TIND_BLOCK] = 0;
	}
	else
		if (dirty)
			write_block (inode->i_block[EXT2_TIND_BLOCK],
				     (char *) tind);
}

static void truncate_file (struct ext2_inode * inode)
{
	trunc_direct(inode);
	trunc_indirect(inode, EXT2_NDIR_BLOCKS, inode->i_block + 
		       EXT2_IND_BLOCK);
	trunc_dindirect(inode, EXT2_NDIR_BLOCKS + addr_per_block,
			inode->i_block + EXT2_DIND_BLOCK);
	trunc_tindirect(inode);
}

static void check_blocks (unsigned int i)
{
	struct ext2_inode inode;
	int inode_changed = 0;

	if (debug)
		printf ("DEBUG: check_blocks(%d)\n", i);
	if (!i || i > INODES)
		return;
	if (inode_count [i] > 1)  /* have we counted this file already? */
		return;
	read_inode (i, &inode);
	if (!S_ISDIR (inode.i_mode) && !S_ISREG (inode.i_mode) &&
	    !S_ISLNK (inode.i_mode))
		return;
	if (S_ISLNK (inode.i_mode) && inode.i_blocks == 0 &&
	    inode.i_size < EXT2_N_BLOCKS * sizeof (unsigned long))
		return;
	for (i = 0; i < EXT2_NDIR_BLOCKS ; i++)
		inode_changed |= add_block (i + inode.i_block, 0);
	inode_changed |= add_block_ind (EXT2_IND_BLOCK + inode.i_block, 0);
	inode_changed |= add_block_dind (EXT2_DIND_BLOCK + inode.i_block, 0);
	inode_changed |= add_block_tind (EXT2_TIND_BLOCK + inode.i_block, 0);
	if (inode_changed)
		write_inode (i, &inode);
}

static unsigned short check_file (struct ext2_inode * dir, int * dir_changed,
				  unsigned int offset)
{
	struct ext2_inode inode;
	unsigned long ino;
	unsigned short rec_len;
	unsigned short name_len;
	char * name;
	int original_depth = name_depth;

	if (debug)
		printf ("DEBUG: check_file(%d,%d)\n", (int)dir, offset);
	*dir_changed = mapped_read_block (dir, offset / BLOCK_SIZE, blkbuf);
	changed |= *dir_changed;
	name = blkbuf + (offset % BLOCK_SIZE) + 8;
	ino = * (unsigned long *) (name - 8);
	rec_len = * (unsigned short *) (name - 4);
	name_len = * (unsigned short *) (name - 2);
	if (rec_len % 4 != 0 || rec_len < name_len + 8 ||
	    rec_len < 8 ||
	    ((offset % BLOCK_SIZE) + rec_len) > BLOCK_SIZE)
	{
		printf ("Bad directory entry in ");
		print_current_name ();
		printf (" at offset %d\n", offset);
		if (((offset / 1024) + 1) * 1024 > dir->i_size)
			rec_len = dir->i_size - offset;
		else
			rec_len = BLOCK_SIZE - (offset % BLOCK_SIZE);
		if (ask ("Delete directory entry", 1))
		{
			* (unsigned long *) (name - 8) = 0;
			* (unsigned short *) (name - 4) = rec_len;
			* (unsigned short *) (name - 2) = rec_len - 8;
			write_block(last_block_read, blkbuf);
			changed = 1;
		}
		else
			printf ("Skipping to next block\n");
		return rec_len;			
	}
	if (ino && strcmp (".", name) && strcmp ("..", name))
	{
		if (name_depth < MAX_DEPTH)
		{
			strncpy (name_list [name_depth], name, name_len);
			name_list [name_depth] [name_len] = '\0';
		}
		name_depth++;	
	}
	if (!no_bad_inode && ino == EXT2_BAD_INO)
	{
		printf ("File `");
		print_current_name ();
		printf ("' consists of the badblock list. ");
		if (ask ("Delete it", 1))
		{
			/* Zero the inode and write out the dir block */
			* (unsigned long *) (name - 8) = 0;
			write_block(last_block_read, blkbuf);
			changed = 1;
		}
		goto exit;
	}
	inode = get_inode (ino);
	if (!offset)
		if (inode.i_mode == 0 || strcmp (".", name))
		{
			print_current_name ();
			printf (": bad directory: '.' isn't first\n");
		}
		else
			goto exit;
	if (offset == 12)
		if (inode.i_mode == 0 || strcmp ("..", name))
		{
			print_current_name ();
			printf (": bad directory: '..' isn't second\n");
		}
		else
			goto exit;
	if (ino && inode.i_mode == 0)
	{
		printf ("bad inode in directory ");
		print_current_name ();
		printf (" at offset %d ", offset);
		if (ask ("Delete directory entry", 1))
		{
			* (unsigned long *) (name - 8) = 0;
			write_block (last_block_read, blkbuf);
			changed = 1;
		}
	}
	if (inode.i_mode == 0)
		goto exit;
	if (list)
	{
		if (verbose)
			printf ("%6d %07o ", ino, inode.i_mode);
		print_current_name ();
		if (S_ISDIR (inode.i_mode))
			printf (":\n");
		else
			printf ("\n");
	}
	check_blocks (ino);
	if (inode.i_flags)
	{
		printf ("Inode %d has i_flags (currently not used) set. ",
			ino);
		if (ask ("Clear", 1))
		{
			inode.i_flags = 0;
			write_inode (ino, &inode);
		}
	}
	if (inode.i_faddr)
	{
		printf ("Inode %d has i_faddr (currently not used) set. ",
			ino);
		if (ask ("Clear", 1))
		{
			inode.i_faddr = 0;
			write_inode (ino, &inode);
		}
	}
	if (inode.i_frag)
	{
		printf ("Inode %d has i_frag (currently not used) set. ",
			ino);
		if (ask ("Clear", 1))
		{
			inode.i_frag = 0;
			write_inode (ino, &inode);
		}
	}
	if (inode.i_fsize)
	{
		printf ("Inode %d has i_fsize (currently not used) set. ",
			ino);
		if (ask ("Clear", 1))
		{
			inode.i_fsize = 0;
			write_inode (ino, &inode);
		}
	}
	if (inode.i_file_acl)
	{
		printf ("Inode %d has i_file_acl (currently not used) set. ",
			ino);
		if (ask ("Clear", 1))
		{
			inode.i_file_acl = 0;
			write_inode (ino, &inode);
		}
	}
	if (inode.i_dir_acl)
	{
		printf ("Inode %d has i_dir_acl (currently not used) set. ",
			ino);
		if (ask ("Clear", 1))
		{
			inode.i_dir_acl = 0;
			write_inode (ino, &inode);
		}
	}
	if (inode.i_mode != 0 && S_ISDIR (inode.i_mode))
	{
		if (inode_count[ino] > 1)
		{
			printf ("link to a directory ");
			print_current_name ();
			printf ("\n");
			if (ask ("delete directory entry", 1))
			{
				* (unsigned long *) (name - 8) = 0;
				write_block (last_block_read, blkbuf);
				changed = 1;
				goto exit;
			}
		}
		recursive_check (ino);
	}
exit:
	name_depth = original_depth;
	return rec_len;
}

static void recursive_check (unsigned int ino)
{
	struct ext2_inode dir;
	unsigned int offset;
	unsigned short rec_len;
	int dir_changed = 0;

	if (debug)
		printf ("DEBUG: recursive_check(%d)\n", ino);
	read_inode (ino, &dir);
	if (!S_ISDIR (dir.i_mode))
		die ("error in recursive_check: dir is not a directory");
	if (dir.i_size < 24)
	{
		print_current_name ();
		printf (": bad directory: size < 24\n");
		return;
	}
	for (offset = 0; offset < dir.i_size; offset += rec_len)
	{
		rec_len = check_file (&dir, &dir_changed, offset);
		if (dir_changed)
			write_inode (ino, &dir);
		if (rec_len < 8)
		{
			print_current_name ();
			printf (": bad directory: rec_len(%d) too short\n", rec_len);
			if (ask ("Truncate directory", 1))
			{
				dir.i_size = offset;
				truncate_file (&dir);
				write_inode (ino, &dir);
			}	    
			return;
		}
	}
}

static unsigned long get_free_block (void)
{
	static unsigned long blk = 0;

	if (!blk)
		blk = FIRSTBLOCK;
	while (blk < BLOCKS && block_in_use (blk))
		blk++;
	if (blk >= BLOCKS)
		die ("not enough good blocks");
	mark_block(blk);
	block_count [blk]++;
	FREEBLOCKSCOUNT--;
	return blk++;
}

static unsigned long next_new_bad (unsigned long block)
{
	if (!block)
		block = FIRSTBLOCK - 1;
	while (++block < BLOCKS)
		if (block_is_bad (block) && !block_count [block])
		{
			block_count [block]++;
			return block;
		}
	return 0;
}

static int next_block (unsigned long *znr, void *blk, unsigned long *pnr)
{
	if (*znr)
	{
		*pnr = *znr;
		read_block (znr, blk);
		return 0;
	}
	*pnr = *znr = get_free_block();
	memset (blk, 0, BLOCK_SIZE);
	return 1;
}

static void update_bad_block (void)
{
	struct ext2_inode inode;
	int i, j, k;
	unsigned long block;
	int ind, dind, tind;
	int ind_dirty = 0, dind_dirty = 0, tind_dirty = 0;
	unsigned long *ind_block  = (long *)(blkbuf + BLOCK_SIZE);
	unsigned long *dind_block = (long *)(blkbuf + BLOCK_SIZE * 2);
	unsigned long *tind_block = (long *)(blkbuf + BLOCK_SIZE * 3);

	read_inode (EXT2_BAD_INO, &inode);
	inode.i_atime = inode.i_ctime = inode.i_mtime = time(NULL);
	inode.i_size = badblocks * BLOCK_SIZE;
	if (!badblocks)
		return;
	if (!(block = next_new_bad (0)))
		return;
	for (i = 0; i < EXT2_NDIR_BLOCKS; i++)
	{
		if (inode.i_block[i])
			continue;
		inode.i_block[i] = block;
		if (!(block = next_new_bad (block)))
			goto end_bad;
	}
	ind_dirty = next_block (&inode.i_block[EXT2_IND_BLOCK], ind_block,
				&ind);
	for (i = 0; i < addr_per_block; i++)
	{
		if (ind_block[i])
			continue;
		ind_block[i] = block;
		ind_dirty = 1;
		if (!(block = next_new_bad (block)))
			goto end_bad;
	}
	dind_dirty = next_block (&inode.i_block[EXT2_DIND_BLOCK],
				 dind_block, &dind);
	for (i = 0; i < addr_per_block; i++)
	{
		if (ind_dirty)
		{
			write_block (ind, (char *) ind_block);
			ind_dirty = 0;
		}
		dind_dirty |= next_block (&dind_block[i], ind_block, &ind);
		for (j = 0; j < addr_per_block; j++)
		{
			if (ind_block[j])
				continue;
			ind_block[j] = block;
			ind_dirty = 1;
			if (!(block = next_new_bad (block)))
				goto end_bad;
		}
	}
	tind_dirty = next_block (&inode.i_block[EXT2_TIND_BLOCK],
				 tind_block, &tind);
	for (i = 0; i < addr_per_block; i++)
	{
		if (dind_dirty)
		{
			write_block (dind, (char *) dind_block);
			dind_dirty = 0;
		}
		tind_dirty |= next_block (&tind_block[i], dind_block, &dind);
		for (j = 0; j < addr_per_block; j++)
		{
			if (ind_dirty)
			{
				write_block (ind, (char *) ind_block);
				ind_dirty = 0;
			}
			dind_dirty |= next_block(&dind_block[j], ind_block,
						 &ind);
			for (k = 0; k < addr_per_block; k++)
			{
				if (ind_block[k])
					continue;
				ind_block[k] = block;
				ind_dirty = 1;
				if (!(block = next_new_bad (block)))
					goto end_bad;
			}
		}
	}
	printf ("Warning: there are too many bad blocks\n");
end_bad:
	if (ind_dirty)
		write_block (ind, (char *) ind_block);
	if (dind_dirty)
		write_block (dind, (char *) dind_block);
	if (tind_dirty)
		write_block (tind, (char *) tind_block);
	write_inode (EXT2_BAD_INO, &inode);
}

/*
 * Look for the lost+found directory
 */
static int find_lpf_inode (struct ext2_inode * lpf_inode)
{
	struct ext2_inode root_inode;
	char * buffer = blkbuf;
	unsigned long block;
	struct ext2_dir_entry * de;

	read_inode (EXT2_ROOT_INO, &root_inode);
	block = 0;
	while ((block * BLOCK_SIZE) < root_inode.i_size)
	{
		mapped_read_block (&root_inode, block, buffer);
		de = (struct ext2_dir_entry *) buffer;
		while ((char *) de < buffer + BLOCK_SIZE)
			if (de->inode && de->name_len == 10 &&
			    strncmp (de->name, "lost+found", 10) == 0)
			{
				read_inode (de->inode, lpf_inode);
				return de->inode;
			}
			else
				de = (struct ext2_dir_entry *) ((char *) de +
								de->rec_len);
		block ++;
	}
	return 0;
}

/*
 * Add an entry to the lost+found directory
 */
static void set_lost_dir_ent (unsigned long ino)
{
	static struct ext2_inode lpf_inode;
	static int lpf_inode_number = -1;
	int dir_changed;
	unsigned long block;
	char * buffer = &blkbuf[BLOCK_SIZE];
	struct ext2_dir_entry * de;
	struct ext2_dir_entry * de1;
	char name[20];
	int rec_len;

	if (lpf_inode_number == -1)
	{
		lpf_inode_number = find_lpf_inode (&lpf_inode);
		if (lpf_inode_number == 0)
		{
			printf ("No directory lost+found\n");
			no_lpf = 1;
			return;
		}
	}
	block = 0;
	if (mapped_read_block (&lpf_inode, block, buffer))
		write_inode (lpf_inode_number, &lpf_inode);
	de = (struct ext2_dir_entry *) buffer;
	sprintf (name, "%u", ino);
	rec_len = EXT2_DIR_REC_LEN(strlen (name));
	while (block * BLOCK_SIZE < lpf_inode.i_size)
	{
		if (de->rec_len < EXT2_DIR_REC_LEN(1) ||
		    de->rec_len % 4 != 0 ||
		    de->rec_len < EXT2_DIR_REC_LEN(de->name_len))
		{
			printf ("lost+found is corrupted, cannot fix\n");
			lpf_corrupted = 1;
		}
		if ((!de->inode && de->rec_len >= rec_len) ||
		    de->rec_len >= EXT2_DIR_REC_LEN(de->name_len) + rec_len)
		{
			if (de->inode)
			{
				de1 = (struct ext2_dir_entry *) ((char *) de + EXT2_DIR_REC_LEN(de->name_len));
				de1->rec_len = de->rec_len - EXT2_DIR_REC_LEN(de->name_len);
				de->rec_len = EXT2_DIR_REC_LEN(de->name_len);
			}
			else
				de1 = de;
			de1->inode = ino;
			/* de1->rec_len = rec_len; */
			de1->name_len = strlen (name);
			strncpy (de1->name, name, strlen (name));
			write_block (last_block_read, buffer);
			strcpy (name_list[name_depth], name);
			name_depth ++;
			check_file (&lpf_inode, &dir_changed,
				    (block * BLOCK_SIZE) +
				    ((char *) de1 - buffer));
			if (dir_changed)
				write_inode (lpf_inode_number, &lpf_inode);
			name_depth --;
			return;
		}
		else
		{
			de = (struct ext2_dir_entry *) ((char *) de + de->rec_len);
			if ((char *) de > buffer + BLOCK_SIZE)
			{
				block ++;
				if (block * BLOCK_SIZE < lpf_inode.i_size)
				{
					mapped_read_block (&lpf_inode, block,
							   buffer);
					de = (struct ext2_dir_entry *) buffer;
				}
			}
		}
	}
	printf ("directory lost+found full\n");
	return;
}

static void check_connectivity (void)
{
	int i;
	struct ext2_inode inode;

	if (debug)
		printf ("check_connectivity ()\n");
	if (verbose)
		printf ("Searching for lost inodes\n");
	name_depth = 1;
	strcpy (name_list[0], "lost+found");
	for (i = 1; i < INODES; i++)
	{
		if (i == EXT2_BAD_INO ||
		    (i > EXT2_ROOT_INO && i < EXT2_FIRST_INO))
			continue;
		if (!inode_count[i] && inode_in_use (i))
		{
			printf("Inode %d not used, not counted in the bitmap ",
			       i);
			read_inode (i, &inode);
			if (!inode.i_dtime && !no_lpf && !lpf_corrupted)
			{
				if (ask ("Connect to lost+found", 1))
					set_lost_dir_ent (i);
			}
			else
			{
				if (lpf_corrupted)
					printf ("(lost+found is corrupted) ");
				if (no_lpf)
					printf ("(no directory lost+found) ");
				if (ask ("Unmark in the bitmap", 1))
					unmark_inode (i);
			}
		}
	}
}

static void check_counts (void)
{
	int i;
	int free;
	int group_free;
	int dirs_count;
	int group;
	int blocks;
	int inodes;
	struct ext2_inode inode;

	if (debug)
		printf ("DEBUG: check_counts()\n");
	if (verbose)
		printf ("Checking inodes counts\n");
	for (i = 1; i <= INODES; i++)
	{
		if (i == EXT2_BAD_INO || 
		    (i > EXT2_ROOT_INO && i < EXT2_FIRST_INO))
			continue;
		read_inode (i, &inode);
		if (!inode_count[i] && inode_in_use (i))
		{
			printf ("Inode %d not used, "
				"not counted in the bitmap. ", i);
			if (!inode.i_dtime && inode.i_mode && !no_lpf &&
			    !lpf_corrupted)
			{
				if (ask ("Connect to lost+found", 1))
					set_lost_dir_ent (i);
			}
			else
			{
				if (lpf_corrupted)
					printf ("(lost+found is corrupted) ");
				if (no_lpf)
					printf ("(no directory lost+found) ");
				if (ask ("Clear", 1))
					unmark_inode (i);
			}
		}
		else if (inode_count[i] && !inode_in_use (i))
		{
			printf ("Inode %d used, counted in the bitmap. ", i);
			if (ask ("Set", 1))
				mark_inode (i);
		}
		if (!inode_in_use (i))
		{
			if (!inode.i_dtime && inode.i_mode)
			{
				printf ("Inode %d not used with dtime null. ",
					i);
				if (ask ("Set dtime", 1))
				{
					inode.i_dtime = time (NULL);
					write_inode (i, &inode);
				}
			}
		}
		if (inode_in_use (i) && inode.i_links_count != inode_count[i])
		{
			printf ("Inode %d, i_nlinks=%d, counted=%d. ",
				i, inode.i_links_count, inode_count[i]);
			if (ask ("Set i_nlinks to count", 1))
			{
				inode.i_links_count = inode_count[i];
				write_inode (i, &inode);
			}
		}
	}
	for (i = 1, free = 0, group_free = 0, dirs_count = 0, group = 0, inodes = 0;
	     i <= INODES; i++)
	{
		if (!inode_in_use (i))
		{
			group_free ++;
			free ++;
		}
		else
		{
			read_inode (i, &inode);
			if (S_ISDIR(inode.i_mode))
				dirs_count ++;
		}
		inodes ++;
		if (inodes == INODESPERGROUP)
		{
			if (group_free != group_desc[group].bg_free_inodes_count)
			{
				printf ("Free inodes count wrong for group %d (%d, counted=%d) ",
					group, group_desc[group].bg_free_inodes_count,
					group_free);
				if (ask ("Repair", 1))
				{
					group_desc[group].bg_free_inodes_count = group_free;
					changed = 1;
				}
			}
			if (dirs_count != group_desc[group].bg_used_dirs_count)
			{
				printf ("Directories count wrong for group %d (%d, counted=%d) ",
					group, group_desc[group].bg_used_dirs_count,
					dirs_count);
				if (ask ("Repair", 1))
				{
					group_desc[group].bg_used_dirs_count = dirs_count;
					changed = 1;
				}
			}
			group ++;
			inodes = 0;
			group_free = 0;
			dirs_count = 0;
		}
	}
	if (free != FREEINODESCOUNT)
	{
		printf ("Free inodes count wrong (%d, counted=%d). ",
			FREEINODESCOUNT, free);
		if (ask ("Repair", 1))
		{
			FREEINODESCOUNT = free;
			changed = 1;
		}
	}

	if (verbose)
		printf ("Checking blocks count\n");
	for (i = FIRSTBLOCK; i < BLOCKS; i++)
	{
		if (block_in_use (i) == block_count[i])
			continue;

		if (!block_count[i] && block_in_use (i))
		{
			if (block_is_bad (i))
				continue;
			printf ("Block %d: marked in use, no file uses it. ", i);
			if (ask ("Unmark", 1))
				unmark_block (i);
			continue;
		}
		if (block_count[i] && !block_in_use (i))
			printf ("Block %d: %sin use, counted=%d\n",
				i, block_in_use (i) ? "" : "not ", 
				block_count[i]);
	}
	for (i = FIRSTBLOCK, free = 0, group_free = 0, group = 0, blocks = 0;
	     i < BLOCKS; i++)
	{
		if (!block_in_use (i))
		{
			group_free ++;
			free ++;
		}
		blocks ++;
		if (blocks == BLOCKSPERGROUP)
		{
			if (group_free != group_desc[group].bg_free_blocks_count)
			{
				printf ("Free blocks count wrong for group %d (%d, counted=%d) ",
					group, group_desc[group].bg_free_blocks_count,
					group_free);
				if (ask ("Repair", 1))
				{
					group_desc[group].bg_free_blocks_count = group_free;
					changed = 1;
				}
			}
			group ++;
			blocks = 0;
			group_free = 0;
		}
	}
	if (free != FREEBLOCKSCOUNT)
	{
		printf ("Free blocks count wrong (%d, counted=%d). ",
			FREEBLOCKSCOUNT, free);
		if (ask ("Repair", 1))
		{
			FREEBLOCKSCOUNT = free;
			changed = 1;
		}
	}
}

static void count_ind (int *znr, unsigned long *last, unsigned long *count)
{
	char *block = blkbuf + BLOCK_SIZE;
	unsigned long * ptr;
	int i;

	if (!*znr || *znr > BLOCKS)
		return;
	(*count) += block_size / 512;
	read_block (znr, block);
	for (i = 0, ptr = (unsigned long *) block; i < (BLOCK_SIZE >> 2); i++)
		if (ptr[i])
		{
			(*last) = i + 1;
			(*count) += block_size / 512;
		}
}

static void count_dind (int *znr, unsigned long *last, unsigned long *count)
{
	char *block = blkbuf + BLOCK_SIZE * 2;
	unsigned long * ptr;
	unsigned long tmp;
	int i;

	if (!*znr || !znr > BLOCKS)
		return;
	(*count) += block_size / 512;
	read_block (znr, block);
	for (i = 0, ptr = (unsigned long *) block; i < (BLOCK_SIZE >> 2); i++)
	{
		tmp = 0;
		count_ind (ptr + i, &tmp, count);
		if (tmp)
			(*last) = tmp + i * addr_per_block;
	}
}

static void count_tind (int *znr, unsigned long *last, unsigned *count)
{
	char *block = blkbuf + BLOCK_SIZE * 3;
	unsigned long * ptr;
	unsigned long tmp;
	int i;

	if (!*znr || !znr > BLOCKS)
		return;
	(*count) += block_size / 512;
	read_block (znr, block);
	for (i = 0, ptr = (unsigned long *) block; i < (BLOCK_SIZE >> 2); i++)
	{
		tmp = 0;
		count_dind (ptr + i, &tmp, count);
		if (tmp)
			(*last) = tmp + i * addr_per_block * addr_per_block;
	}
}

static void check_sizes (void)
{
	int i;
	int ino;
	struct ext2_inode inode;
	unsigned long count;
	unsigned long last;
	unsigned long tmp;

	if (debug)
		printf ("DEBUG: check_sizes()\n");
	if (verbose)
		printf ("Checking file sizes\n");
	for (ino = 1; ino <= INODES; ino++)
	{
		if (!inode_in_use (ino))
			continue;
		read_inode (ino, &inode);
		if (!S_ISDIR (inode.i_mode) && !S_ISREG (inode.i_mode) &&
		    !S_ISLNK (inode.i_mode))
			continue;
		if (S_ISLNK (inode.i_mode) && inode.i_blocks == 0 &&
		    inode.i_size < EXT2_N_BLOCKS * sizeof (unsigned long))
			continue;
		count = 0;
		last = 0;
		for (i = 0; i < EXT2_NDIR_BLOCKS; i++)
			if (inode.i_block[i])
			{
				last = i + 1;
				count += block_size / 512;
			}
		tmp = 0;
		count_ind (inode.i_block + EXT2_IND_BLOCK, &tmp, &count);
		if (tmp)
			last = tmp + EXT2_NDIR_BLOCKS;
		tmp = 0;
		count_dind (inode.i_block + EXT2_DIND_BLOCK, &tmp, &count);
		if (tmp)
			last = tmp + EXT2_NDIR_BLOCKS + addr_per_block;
		tmp = 0;
		count_tind (inode.i_block + EXT2_TIND_BLOCK, &tmp, &count);
		if (tmp)
			last = tmp + EXT2_NDIR_BLOCKS + addr_per_block +
				addr_per_block * addr_per_block;
		if ((inode.i_size || last) &&
			inode.i_size < (last - 1) * block_size)
		{
			printf ("Inode %d, incorrect size, %d (counted = %d). ",
				ino, inode.i_size, count * block_size);
			if (ask ("Set size to counted", 1))
			{
				inode.i_size = count * block_size;
				write_inode (ino, &inode);
			}
		}
		if (inode.i_blocks != count)
		{
			printf ("Inode %d, i_blocks wrong %d (counted=%d) .",
				ino, inode.i_blocks, count);
			if (ask ("Set i_blocks to counted", 1))
			{
				inode.i_blocks = count;
				write_inode (ino, &inode);
			}
		}
	}
}

static void check (void)
{
	int i, j;
	unsigned long block;

	if (debug)
		printf ("DEBUG: check()\n");
	memset (inode_count, 0, (INODES + 1) * sizeof (*inode_count));
	memset (block_count, 0, BLOCKS * sizeof (*block_count));

	check_desc ();
	block = FIRSTBLOCK;
	for (i = 0; i < group_desc_count; i++)
	{
		block_count[group_desc[i].bg_block_bitmap] = 1;
		block_count[group_desc[i].bg_inode_bitmap] = 1;
		for (j = 0; j < inode_blocks_per_group; j++)
			block_count[group_desc[i].bg_inode_table + j] = 1;
		block_count[block] = 1;
		for (j = 0; j < desc_blocks; j++)
			block_count[block + j + 1] = 1;
		block += BLOCKSPERGROUP;
	}
	read_bad_blocks ();
	if (test_disk)
		test_blocks ();
	if (listfile)
		read_bad_blocks_from_file ();
	if (verbose)
		display_bad_blocks ();
	check_blocks (EXT2_ROOT_INO);
	if (verbose)
		printf ("Reading directories and checking files\n");
	recursive_check (EXT2_ROOT_INO);
	if (test_disk || listfile)
		update_bad_block ();
	check_connectivity ();
	check_counts ();
	check_sizes ();
}

int main (int argc, char ** argv)
{
	struct termios termios, tmp;
	int count;
	char c;

	fprintf (stderr, "e2fsck 0.2d, 93/03/30 for EXT2 FS %s\n",
		 EXT2FS_VERSION);
	if (argc && *argv)
		program_name = *argv;
	if (INODE_SIZE * (BLOCK_SIZE / INODE_SIZE) != BLOCK_SIZE)
		die("bad inode size");
	while ((c = getopt (argc, argv, "ab:cdflrstv")) != EOF)
		switch (c)
		{
			case 'a':
				automatic = 1;
				repair = 1;
				break;
			case 'b':
				listfile = optarg;
				break;
			case 'c':
			case 't':
				test_disk = 1;
				break;
			case 'd':
				debug = 1;
				break;
			case 'f':
				force = 1;
				break;
			case 'l':
				list = 1;
				break;
			case 'r':
				automatic = 0;
				repair = 1;
				break;
			case 's':
				show = 1;
				break;
			case 'v':
				verbose = 1;
				break;
			default:
				usage ();
		}
	if (optind != argc - 1)
		usage ();
	device_name = argv[optind];
	if (repair && !automatic)
	{
		if (! isatty (0) || ! isatty (1))
			die ("need terminal for interactive repairs");
		tcgetattr (0, &termios);
		tmp = termios;
		tmp.c_lflag &= ~(ICANON | ECHO);
		tcsetattr (0, TCSANOW, &tmp);
	}
	dev = open (device_name, repair || test_disk || listfile ? O_RDWR : O_RDONLY);
	if (dev < 0)
		die("unable to open '%s'");
	for (count = 0 ; count < 3; count++)
		sync();
	if (verbose)
		printf ("Reading tables\n");
	read_tables ();
	if (verbose)
		printf ("Checking root directory\n");
	check_root ();
	check ();
	if (changed)
	{
		write_tables ();
		cache_flush ();
		printf ("----------------------------\n"
			"FILE SYSTEM HAS BEEN CHANGED\n"
			"----------------------------\n");
		for (count = 0; count < 3; count++)
			sync ();
	}
	if (verbose)
	{
		int free;

		free = FREEINODESCOUNT;
		printf ("\n%6d inode%s used (%d%%)\n", (INODES - free),
			((INODES - free) != 1) ? "s" : "",
			100 * (INODES - free) / INODES);
		free = FREEBLOCKSCOUNT;
		printf ("%6d block%s used (%d%%)\n"
			"%6d bad block%s\n", (BLOCKS - free),
			((BLOCKS - free) != 1) ? "s" : "",
			100 * (BLOCKS - free) / BLOCKS, badblocks,
			badblocks != 1 ? "s" : "");
		printf ("\n%6d regular file%s\n"
			"%6d director%s\n"
			"%6d character device file%s\n"
			"%6d block device file%s\n"
			"%6d fifo%s\n"
			"%6d link%s\n"
			"%6d symbolic link%s (%d fast symbolic link%s)\n"
			"%6d socket%s\n"
			"------\n"
			"%6d file%s\n",
			regular, (regular != 1) ? "s" : "",
			directory, (directory != 1) ? "ies" : "y",
			chardev, (chardev != 1) ? "s" : "",
			blockdev, (blockdev != 1) ? "s" : "",
			fifo, (fifo != 1) ? "s" : "",
			links - 2 * directory + 1,
			((links - 2 * directory + 1) != 1) ? "s" : "",
			symlinks, (symlinks != 1) ? "s" : "",
			fast_symlinks, (fast_symlinks != 1) ? "s" : "",
			sockets, (sockets != 1) ? "s" : "",
			total - 2 * directory + 1,
			((total - 2 * directory + 1) != 1) ? "s" : "");
	}
	close (dev);
	if (repair && !automatic)
		tcsetattr (0, TCSANOW, &termios);
	return (changed);
}
