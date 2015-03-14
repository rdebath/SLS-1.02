#ifndef _EXT2_FS_SB
#define _EXT2_FS_SB

#define EXT2_MAX_GROUP_DESC	4
#define EXT2_MAX_GROUP_LOADED	8

/*
 * second extended-fs super-block data in memory
 */
struct ext2_sb_info {
	unsigned long s_inodes_count;	/* Inodes count */
	unsigned long s_blocks_count;	/* Blocks count */
	unsigned long s_r_blocks_count;	/* Reserved blocks count */
	unsigned long s_first_data_block;/* First data block */
	unsigned long s_log_block_size;	/* Log of block size */
	long s_log_frag_size;		/* Log of fragment size */
	unsigned long s_frag_size;	/* Size of a fragment in bytes */
	unsigned long s_frags_per_block;/* Number of fragments per block */
	unsigned long s_inodes_per_block;/* Number of inodes per block */
	unsigned long s_frags_per_group;/* Number of fragments in a group */
	unsigned long s_blocks_per_group;/* Number of blocks in a group */
	unsigned long s_inodes_per_group;/* Number of inodes in a group */
	unsigned long s_desc_per_block;	/* Number of group descriptors per block */
	unsigned long s_groups_count;	/* Number of groups in the fs */
	struct buffer_head * s_sbh;	/* Buffer containing the super block */
	struct buffer_head * s_group_desc[EXT2_MAX_GROUP_DESC];
	unsigned short s_loaded_inode_bitmaps;
	unsigned short s_loaded_block_bitmaps;
	unsigned long s_inode_bitmap_number[EXT2_MAX_GROUP_LOADED];
	struct buffer_head * s_inode_bitmap[EXT2_MAX_GROUP_LOADED];
	unsigned long s_block_bitmap_number[EXT2_MAX_GROUP_LOADED];
	struct buffer_head * s_block_bitmap[EXT2_MAX_GROUP_LOADED];
	int s_rename_lock;
	int s_was_mounted_valid;
	struct wait_queue * s_rename_wait;
};

#endif
