#ifndef _LINUX_EXT2_FS_H
#define _LINUX_EXT2_FS_H

/*
 * The second extended filesystem constants/structures
 */

/*
 * Define EXT2FS_DEBUG to produce debug messages
 */
#undef EXT2FS_DEBUG

/*
 * Define EXT2FS_PRE_02B_COMPAT to convert ext 2 fs prior to 0.2b
 */
#undef EXT2FS_PRE_02B_COMPAT

/*
 * Define DONT_USE_DCACHE to inhibit the directory cache
 */
#undef DONT_USE_DCACHE

/*
 * Define EXT2FS_DEBUG_CACHE to produce cache debug messages
 */
#undef EXT2FS_DEBUG_CACHE

/*
 * The second extended file system version
 */
#define EXT2FS_VERSION	"0.3, 93/04/22"

/*
 * Special inodes numbers
 */
#define	EXT2_BAD_INO		 1	/* Bad blocks inode */
#define EXT2_ROOT_INO		 2	/* Root inode */
#define EXT2_ACL_INO		 3	/* ACL inode */
#define EXT2_FIRST_INO		11	/* First non reserved inode */

/*
 * The second extended file system magic number
 */
#define EXT2_OLD_SUPER_MAGIC	0xEF51
#define EXT2_SUPER_MAGIC	0xEF53

/*
 * Maximal count of links to a file
 */
#define EXT2_LINK_MAX		32000

/*
 * Macro-instructions used to manage several block sizes
 */
#define EXT2_MIN_BLOCK_SIZE		1024
#define	EXT2_MAX_BLOCK_SIZE		4096
#define EXT2_MIN_BLOCK_LOG_SIZE		  10
#ifdef KERNEL
# define EXT2_BLOCK_SIZE(s)		((s)->s_blocksize)
#else
# define EXT2_BLOCK_SIZE(s)		(EXT2_MIN_BLOCK_SIZE << (s)->s_log_block_size)
#endif
#define EXT2_ACLE_PER_BLOCK(s)		(EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_acl_entry))
#define	EXT2_ADDR_PER_BLOCK(s)		(EXT2_BLOCK_SIZE(s) / sizeof (unsigned long))
#ifdef KERNEL
# define EXT2_BLOCK_SIZE_BITS(s)	((s)->u.ext2_sb.s_log_block_size + 10)
#else
# define EXT2_BLOCK_SIZE_BITS(s)	((s)->s_log_block_size + 10)
#endif
#define	EXT2_INODES_PER_BLOCK(s)	(EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_inode))

/*
 * Macro-instructions used to manage fragments
 */
#define EXT2_MIN_FRAG_SIZE		1024
#define	EXT2_MAX_FRAG_SIZE		1024
#define EXT2_MIN_FRAG_LOG_SIZE		  10
#ifdef KERNEL
# define EXT2_FRAG_SIZE(s)		((s)->u.ext2_sb.s_frag_size)
# define EXT2_FRAGS_PER_BLOCK(s)	((s)->u.ext2_sb.s_frags_per_block)
#else
# define EXT2_FRAG_SIZE(s)		(EXT2_MIN_FRAG_SIZE << (s)->s_log_frag_size)
# define EXT2_FRAGS_PER_BLOCK(s)	(EXT2_BLOCK_SIZE(s) / EXT2_FRAG_SIZE(s))
#endif

/*
 * ACL structures
 */

struct ext2_acl_header	/* Header of Access Control Lists */
{
	unsigned long aclh_file_count;
	unsigned long aclh_acle_count;
	unsigned long aclh_first_acle;
	unsigned long aclh_reserved;
};

struct ext2_acl_entry	/* Access Control List Entry */
{
	unsigned short acle_perms;	/* Access permissions */
	unsigned short acle_type;	/* Type of entry */
	unsigned short acle_tag;	/* User or group identity */
	unsigned short acle_pad1;
	unsigned long acle_reserved;
	unsigned long acle_next;	/* Pointer on next entry for the */
					/* same inode or on next free entry */
};

/*
 * Structure of a blocks group descriptor
 */
struct ext2_old_group_desc
{
	unsigned long bg_block_bitmap;		/* Blocks bitmap block */
	unsigned long bg_inode_bitmap;		/* Inodes bitmap block */
	unsigned long bg_inode_table;		/* Inodes table block */
	unsigned short bg_free_blocks_count;	/* Free blocks count */
	unsigned short bg_free_inodes_count;	/* Free inodes count */
};

struct ext2_group_desc
{
	unsigned long bg_block_bitmap;		/* Blocks bitmap block */
	unsigned long bg_inode_bitmap;		/* Inodes bitmap block */
	unsigned long bg_inode_table;		/* Inodes table block */
	unsigned short bg_free_blocks_count;	/* Free blocks count */
	unsigned short bg_free_inodes_count;	/* Free inodes count */
	unsigned short bg_used_dirs_count;	/* Directories count */
	unsigned short bg_pad;
	unsigned long bg_reserved[3];
};

/*
 * Macro-instructions used to manage group descriptors
 */
#ifdef KERNEL
# define EXT2_BLOCKS_PER_GROUP(s)	((s)->u.ext2_sb.s_blocks_per_group)
# define EXT2_DESC_PER_BLOCK(s)		((s)->u.ext2_sb.s_desc_per_block)
# define EXT2_INODES_PER_GROUP(s)	((s)->u.ext2_sb.s_inodes_per_group)
#else
# define EXT2_BLOCKS_PER_GROUP(s)	((s)->s_blocks_per_group)
# define EXT2_DESC_PER_BLOCK(s)		(EXT2_BLOCK_SIZE(s) / sizeof (struct ext2_group_desc))
# define EXT2_INODES_PER_GROUP(s)	((s)->s_inodes_per_group)
#endif

/*
 * Constants relative to the data blocks
 */
#define	EXT2_NDIR_BLOCKS	12
#define	EXT2_IND_BLOCK		EXT2_NDIR_BLOCKS
#define	EXT2_DIND_BLOCK		(EXT2_IND_BLOCK + 1)
#define	EXT2_TIND_BLOCK		(EXT2_DIND_BLOCK + 1)
#define	EXT2_N_BLOCKS		(EXT2_TIND_BLOCK + 1)

/*
 * Structure of an inode on the disk
 */
struct ext2_inode {
	unsigned short i_mode;		/* File mode */
	unsigned short i_uid;		/* Owner Uid */
	unsigned long i_size;		/* Size in bytes */
	unsigned long i_atime;		/* Access time */
	unsigned long i_ctime;		/* Creation time */
	unsigned long i_mtime;		/* Modification time */
	unsigned long i_dtime;		/* Deletion Time */
	unsigned short i_gid;		/* Group Id */
	unsigned short i_links_count;	/* Links count */
	unsigned long i_blocks;		/* Blocks count */
	unsigned long i_flags;		/* File flags */
	unsigned long i_reserved1;
	unsigned long i_block[EXT2_N_BLOCKS];/* Pointers to blocks */
	unsigned long i_version;	/* File version (for NFS) */
	unsigned long i_file_acl;	/* File ACL */
	unsigned long i_dir_acl;	/* Directory ACL */
	unsigned long i_faddr;		/* Fragment address */
	unsigned char i_frag;		/* Fragment number */
	unsigned char i_fsize;		/* Fragment size */
	unsigned short i_pad1;
	unsigned long i_reserved2[2];
};

/*
 * Structure of the super block
 */
struct ext2_super_block {
	unsigned long s_inodes_count;	/* Inodes count */
	unsigned long s_blocks_count;	/* Blocks count */
	unsigned long s_r_blocks_count;	/* Reserved blocks count */
	unsigned long s_free_blocks_count;/* Free blocks count */
	unsigned long s_free_inodes_count;/* Free inodes count */
	unsigned long s_first_data_block;/* First Data Block */
	unsigned long s_log_block_size;	/* Block size */
	long s_log_frag_size;		/* Fragment size */
	unsigned long s_blocks_per_group;/* # Blocks per group */
	unsigned long s_frags_per_group;/* # Fragments per group */
	unsigned long s_inodes_per_group;/* # Inodes per group */
	unsigned long s_mtime;		/* Mount time */
	unsigned long s_wtime;		/* Write time */
	unsigned long s_pad;		/* Padding to get the magic signature*/
					/* at the same offset as in the */
					/* previous ext fs */
	unsigned short s_magic;		/* Magic signature */
	unsigned short s_valid;		/* Flag */
	unsigned long s_reserved[243];	/* Padding to the end of the block */
};

/*
 * Structure of a directory entry
 */
#define EXT2_NAME_LEN 255

struct ext2_dir_entry {
	unsigned long inode;		/* Inode number */
	unsigned short rec_len;		/* Directory entry length */
	unsigned short name_len;	/* Name length */
	char name[EXT2_NAME_LEN];	/* File name */
};

/*
 * EXT2_DIR_PAD defines the directory entries boundaries
 *
 * NOTE: It must be a multiple of 4
 */
#define EXT2_DIR_PAD		 	4
#define EXT2_DIR_ROUND 			(EXT2_DIR_PAD - 1)
#define EXT2_DIR_REC_LEN(name_len)	(((name_len) + 8 + EXT2_DIR_ROUND) & \
					 ~EXT2_DIR_ROUND)

/*
 * Function prototypes
 */

/* acl.c */
extern int ext2_permission (struct inode *, int);

/* balloc.c */
extern int ext2_new_block (struct super_block *, unsigned long);
extern void ext2_free_block (struct super_block *, unsigned long);
extern unsigned long ext2_count_free_blocks (struct super_block *);

/* bitmap.c */
extern unsigned long ext2_count_free (struct buffer_head *, unsigned);

/* dcache.c */
extern void ext2_dcache_invalidate (unsigned short);
extern unsigned long ext2_dcache_lookup (unsigned short, unsigned long,
					 const char *, int);
extern void ext2_dcache_add (unsigned short, unsigned long, const char *,
			     int, int);
extern void ext2_dcache_remove (unsigned short, unsigned long, const char *,
				int);

/* dir.c */
extern int ext2_check_dir_entry (char *, struct inode *,
				 struct ext2_dir_entry *, struct buffer_head *,
				 unsigned int);

/* file.c */
extern int ext2_read (struct inode *, struct file *, char *, int);
extern int ext2_write (struct inode *, struct file *, char *, int);

/* ialloc.c */
extern struct inode * ext2_new_inode (const struct inode *, int);
extern void ext2_free_inode (struct inode *);
extern unsigned long ext2_count_free_inodes (struct super_block *);

/* inode.c */
extern int ext2_bmap (struct inode *, int);

extern struct buffer_head * ext2_getblk (struct inode *, int, int, int *);
extern struct buffer_head * ext2_bread (struct inode *, int, int, int *);

extern void ext2_put_super (struct super_block *);
extern void ext2_write_super (struct super_block *);
extern struct super_block * ext2_read_super (struct super_block *,void *,int);
extern void ext2_read_inode (struct inode *);
extern void ext2_write_inode (struct inode *);
extern void ext2_put_inode (struct inode *);
extern void ext2_statfs (struct super_block *, struct statfs *);

/* ioctl.c */
extern int ext2_ioctl (struct inode *, struct file *, unsigned int,
		       unsigned long);

/* namei.c */
extern int ext2_open (struct inode *, struct file *);
extern void ext2_release (struct inode *, struct file *);
extern int ext2_lookup (struct inode *,const char *, int, struct inode **);
extern int ext2_create (struct inode *,const char *, int, int,
			struct inode **);
extern int ext2_mkdir (struct inode *, const char *, int, int);
extern int ext2_rmdir (struct inode *, const char *, int);
extern int ext2_unlink (struct inode *, const char *, int);
extern int ext2_symlink (struct inode *, const char *, int, const char *);
extern int ext2_link (struct inode *, struct inode *, const char *, int);
extern int ext2_mknod (struct inode *, const char *, int, int, int);
extern int ext2_rename (struct inode *, const char *, int,
			struct inode *, const char *, int);

/* truncate.c */
extern void ext2_truncate (struct inode *);

/*
 * Inodes and files operations
 */

/* dir.c */
extern struct inode_operations ext2_dir_inode_operations;
extern struct file_operations ext2_dir_operations;

/* file.c */
extern struct inode_operations ext2_file_inode_operations;
extern struct file_operations ext2_file_operations;

/* symlink.c */
extern struct inode_operations ext2_symlink_inode_operations;

#endif
