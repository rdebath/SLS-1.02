/*
 *  linux/fs/ext2/namei.c
 *
 *  Copyright (C) 1992, 1993  Remy Card (card@masi.ibp.fr)
 *
 *  from
 *
 *  linux/fs/minix/namei.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linux/sched.h>
#include <linux/ext2_fs.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/stat.h>
#include <linux/fcntl.h>
#include <linux/errno.h>

#include <asm/segment.h>

/*
 * comment out this line if you want names > EXT2_NAME_LEN chars to be
 * truncated. Else they will be disallowed.
 */
/* #define NO_TRUNCATE */
	
/*
 * ok, we cannot use strncmp, as the name is not in our data space.
 * Thus we'll have to use ext2_match. No big problem. ext2_match also makes
 * some sanity tests.
 *
 * NOTE! unlike strncmp, ext2_match returns 1 for success, 0 for failure.
 */
static int ext2_match (int len, const char * const name,
		       struct ext2_dir_entry * de)
{
	unsigned char same;

	if (!de || !de->inode || len > EXT2_NAME_LEN)
		return 0;
	/* "" means "." ---> so paths like "/usr/lib//libc.a" work */
	if (!len && de->name_len == 1 && (de->name[0] == '.') &&
	   (de->name[1] == '\0'))
		return 1;
	if (len != de->name_len)
		return 0;
	__asm__("cld\n\t"
		"repe ; cmpsb\n\t"
		"setz %0"
		:"=q" (same)
		:"S" ((long) name), "D" ((long) de->name), "c" (len)
		:"cx", "di", "si");
	return (int) same;
}

/*
 *	ext2_find_entry()
 *
 * finds an entry in the specified directory with the wanted name. It
 * returns the cache buffer in which the entry was found, and the entry
 * itself (as a parameter - res_dir). It does NOT read the inode of the
 * entry - you'll have to do that yourself if you want to.
 *
 * In the ext2 file system, this function also returns a pointer on the
 * previous directory entry because functions which remove directory
 * entries need it
 */
static struct buffer_head * ext2_find_entry (struct inode * dir,
					     const char * const name, int namelen,
					     struct ext2_dir_entry ** res_dir,
					     struct ext2_dir_entry ** prev_dir)
{
	long offset;
	struct buffer_head * bh;
	struct ext2_dir_entry * de;
	struct super_block * sb;
	int err;

	*res_dir = NULL;
	if (!dir)
		return NULL;
	sb = dir->i_sb;
#ifdef NO_TRUNCATE
	if (namelen > EXT2_NAME_LEN)
		return NULL;
#else
	if (namelen > EXT2_NAME_LEN)
		namelen = EXT2_NAME_LEN;
#endif
	bh = ext2_bread (dir, 0, 0, &err);
	if (!bh)
		return NULL;
	if (prev_dir)
		*prev_dir = NULL;
	offset = 0;
	de = (struct ext2_dir_entry *) bh->b_data;
	while (offset < dir->i_size) {
		if (!bh || (char *)de >= sb->s_blocksize + bh->b_data) {
			brelse (bh);
			bh = ext2_bread (dir, offset >> EXT2_BLOCK_SIZE_BITS(sb), 0, &err);
			if (!bh) {
				offset += sb->s_blocksize;
				continue;
			}
			de = (struct ext2_dir_entry *) bh->b_data;
			if (prev_dir)
				*prev_dir = NULL;
		}
		if (! ext2_check_dir_entry ("ext2_find_entry", dir, de, bh,
					    offset)) {
			brelse (bh);
			return NULL;
		}
		if (ext2_match (namelen, name, de)) {
			*res_dir = de;
			return bh;
		}
		offset += de->rec_len;
		if (prev_dir)
			*prev_dir = de;
		de = (struct ext2_dir_entry *) ((char *) de + de->rec_len);
	}
	brelse (bh);
	return NULL;
}

int ext2_lookup (struct inode * dir, const char * name, int len,
		 struct inode ** result)
{
	int ino;
	struct ext2_dir_entry * de;
	struct buffer_head * bh;

	*result = NULL;
	if (!dir)
		return -ENOENT;
	if (!S_ISDIR(dir->i_mode)) {
		iput (dir);
		return -ENOENT;
	}
#ifndef DONT_USE_DCACHE
	if (!(ino = ext2_dcache_lookup (dir->i_dev, dir->i_ino, name, len))) {
#endif
		if (!(bh = ext2_find_entry (dir, name, len, &de, NULL))) {
			iput (dir);
			return -ENOENT;
		}
		ino = de->inode;
#ifndef DONT_USE_DCACHE
		ext2_dcache_add (dir->i_dev, dir->i_ino, de->name,
				 de->name_len, ino);
#endif
		brelse (bh);
#ifndef DONT_USE_DCACHE
	}
#endif
	if (!(*result = iget (dir->i_sb, ino))) {
		iput (dir);
		return -EACCES;
	}
	iput (dir);
	return 0;
}

/*
 *	ext2_add_entry()
 *
 * adds a file entry to the specified directory, using the same
 * semantics as ext2_find_entry(). It returns NULL if it failed.
 *
 * NOTE!! The inode part of 'de' is left at 0 - which means you
 * may not sleep between calling this and putting something into
 * the entry, as someone else might have used it while you slept.
 */
static struct buffer_head * ext2_add_entry (struct inode * dir,
					    const char * name, int namelen,
					    struct ext2_dir_entry ** res_dir,
					    int *err)
{
	int i;
	long offset;
	unsigned short rec_len;
	struct buffer_head * bh;
	struct ext2_dir_entry * de, * de1;
	struct super_block * sb;

	*err = -EINVAL;
	*res_dir = NULL;
	if (!dir)
		return NULL;
	sb = dir->i_sb;
#ifdef NO_TRUNCATE
	if (namelen > EXT2_NAME_LEN)
		return NULL;
#else
	if (namelen > EXT2_NAME_LEN)
		namelen = EXT2_NAME_LEN;
#endif
	if (!namelen)
		return NULL;
	bh = ext2_bread (dir, 0, 0, err);
	if (!bh)
		return NULL;
	rec_len = EXT2_DIR_REC_LEN(namelen);
	offset = 0;
	de = (struct ext2_dir_entry *) bh->b_data;
	*err = -ENOSPC;
	while (1) {
		if ((char *)de >= sb->s_blocksize + bh->b_data) {
			brelse (bh);
			bh = NULL;
			bh = ext2_bread (dir, offset >> EXT2_BLOCK_SIZE_BITS(sb), 1, err);
			if (!bh)
				return NULL;
			if (dir->i_size <= offset) {
#ifdef EXT2FS_DEBUG
				printk ("ext2_add_entry: creating next block\n");
#endif
				de = (struct ext2_dir_entry *) bh->b_data;
				de->inode = 0;
				de->rec_len = sb->s_blocksize;
				dir->i_size = offset + sb->s_blocksize;
				dir->i_dirt = 1;
				dir->i_ctime = CURRENT_TIME;
			} else {
#ifdef EXT2FS_DEBUG
				printk ("ext2_add_entry: skipping to next block\n");
#endif
				de = (struct ext2_dir_entry *) bh->b_data;
			}
		}
		if (! ext2_check_dir_entry ("ext2_add_entry", dir, de, bh,
					    offset)) {
			*err = -ENOENT;
			brelse (bh);
			return NULL;
		}
		if (de->inode) {
			if (ext2_match (namelen, name, de)) {
				*err = -EEXIST;
				brelse (bh);
				return NULL;
			}
		}
		if ((!de->inode && de->rec_len >= rec_len) ||
		    (de->rec_len >= EXT2_DIR_REC_LEN(de->name_len) + rec_len)) {
			offset += de->rec_len;
			if (de->inode) {
				de1 = (struct ext2_dir_entry *) ((char *) de +
					EXT2_DIR_REC_LEN(de->name_len));
				de1->rec_len = de->rec_len -
					EXT2_DIR_REC_LEN(de->name_len);
				de->rec_len = EXT2_DIR_REC_LEN(de->name_len);
				de = de1;
			}
			de->inode = 0;
			de->name_len = namelen;
			for (i = 0; i < namelen ; i++)
				de->name[i] = name [i];
			dir->i_mtime = dir->i_ctime = CURRENT_TIME;
			bh->b_dirt = 1;
			*res_dir = de;
			return bh;
		}
		offset += de->rec_len;
		de = (struct ext2_dir_entry *) ((char *) de + de->rec_len);
	}
	brelse (bh);
	return NULL;
}

/*
 * ext2_delete_entry deletes a directory entry by merging it with the
 * previous entry
 */
static void ext2_delete_entry (struct ext2_dir_entry * dir,
			       struct ext2_dir_entry * prev_dir)
{
	if (prev_dir != NULL)
		prev_dir->rec_len += dir->rec_len;
	else
		dir->inode = 0;
}

int ext2_create (struct inode * dir,const char * name, int len, int mode,
		 struct inode ** result)
{
	struct inode * inode;
	struct buffer_head * bh;
	struct ext2_dir_entry * de;
	int err;

	*result = NULL;
	if (!dir)
		return -ENOENT;
	inode = ext2_new_inode (dir, mode);
	if (!inode) {
		iput (dir);
		return -ENOSPC;
	}
	inode->i_op = &ext2_file_inode_operations;
	inode->i_mode = mode;
	inode->i_dirt = 1;
	bh = ext2_add_entry (dir, name, len, &de, &err);
	if (!bh) {
		inode->i_nlink --;
		inode->i_dirt = 1;
		iput (inode);
		iput (dir);
		return err;
	}
	de->inode = inode->i_ino;
#ifndef DONT_USE_DCACHE
	ext2_dcache_add (dir->i_dev, dir->i_ino, de->name, de->name_len,
			 de->inode);
#endif
	bh->b_dirt = 1;
	brelse (bh);
	iput (dir);
	*result = inode;
	return 0;
}

int ext2_mknod (struct inode * dir, const char * name, int len, int mode,
		int rdev)
{
	struct inode * inode;
	struct buffer_head * bh;
	struct ext2_dir_entry * de;
	int err;

	if (!dir)
		return -ENOENT;
	bh = ext2_find_entry (dir, name, len, &de, NULL);
	if (bh) {
		brelse (bh);
		iput (dir);
		return -EEXIST;
	}
	inode = ext2_new_inode (dir, mode);
	if (!inode) {
		iput (dir);
		return -ENOSPC;
	}
	inode->i_uid = current->euid;
	inode->i_mode = mode;
	inode->i_op = NULL;
	if (S_ISREG(inode->i_mode))
		inode->i_op = &ext2_file_inode_operations;
	else if (S_ISDIR(inode->i_mode)) {
		inode->i_op = &ext2_dir_inode_operations;
		if (dir->i_mode & S_ISGID)
			inode->i_mode |= S_ISGID;
	}
	else if (S_ISLNK(inode->i_mode))
		inode->i_op = &ext2_symlink_inode_operations;
	else if (S_ISCHR(inode->i_mode))
		inode->i_op = &chrdev_inode_operations;
	else if (S_ISBLK(inode->i_mode))
		inode->i_op = &blkdev_inode_operations;
	else if (S_ISFIFO(inode->i_mode)) 
		init_fifo(inode);
	if (S_ISBLK(mode) || S_ISCHR(mode))
		inode->i_rdev = rdev;
	inode->i_mtime = inode->i_atime = CURRENT_TIME;
	inode->i_dirt = 1;
	bh = ext2_add_entry (dir, name, len, &de, &err);
	if (!bh) {
		inode->i_nlink --;
		inode->i_dirt = 1;
		iput (inode);
		iput (dir);
		return err;
	}
	de->inode = inode->i_ino;
#ifndef DONT_USE_DCACHE
	ext2_dcache_add (dir->i_dev, dir->i_ino, de->name, de->name_len,
			 de->inode);
#endif
	bh->b_dirt = 1;
	brelse (bh);
	iput (dir);
	iput (inode);
	return 0;
}

int ext2_mkdir (struct inode * dir, const char * name, int len, int mode)
{
	struct inode * inode;
	struct buffer_head * bh, * dir_block;
	struct ext2_dir_entry * de;
	int err;

	if (!dir)
		return -ENOENT;
	bh = ext2_find_entry (dir, name, len, &de, NULL);
	if (bh) {
		brelse (bh);
		iput (dir);
		return -EEXIST;
	}
	if (dir->i_nlink >= EXT2_LINK_MAX) {
		iput (dir);
		return -EMLINK;
	}
	inode = ext2_new_inode (dir, S_IFDIR);
	if (!inode) {
		iput (dir);
		return -ENOSPC;
	}
	inode->i_op = &ext2_dir_inode_operations;
	inode->i_size = inode->i_sb->s_blocksize;
	inode->i_mtime = inode->i_atime = CURRENT_TIME;
	dir_block = ext2_bread (inode, 0, 1, &err);
	if (!dir_block) {
		iput (dir);
		inode->i_nlink --;
		inode->i_dirt = 1;
		iput (inode);
		return err;
	}
	inode->i_blocks = inode->i_sb->s_blocksize / 512;
	de = (struct ext2_dir_entry *) dir_block->b_data;
	de->inode = inode->i_ino;
	de->name_len = 1;
	de->rec_len = EXT2_DIR_REC_LEN(de->name_len);
	strcpy (de->name, ".");
	de = (struct ext2_dir_entry *) ((char *) de + de->rec_len);
	de->inode = dir->i_ino;
	de->rec_len = inode->i_sb->s_blocksize - EXT2_DIR_REC_LEN(1);
	de->name_len = 2;
	strcpy (de->name, "..");
	inode->i_nlink = 2;
	dir_block->b_dirt = 1;
	brelse (dir_block);
	inode->i_mode = S_IFDIR | (mode & 0777 & ~current->umask);
	if (dir->i_mode & S_ISGID)
		inode->i_mode |= S_ISGID;
	inode->i_dirt = 1;
	bh = ext2_add_entry (dir, name, len, &de, &err);
	if (!bh) {
		iput (dir);
		inode->i_nlink = 0;
		iput (inode);
		return err;
	}
	de->inode = inode->i_ino;
#ifndef DONT_USE_DCACHE
	ext2_dcache_add (dir->i_dev, dir->i_ino, de->name, de->name_len,
			 de->inode);
#endif
	bh->b_dirt = 1;
	dir->i_nlink ++;
	dir->i_dirt = 1;
	iput (dir);
	iput (inode);
	brelse (bh);
	return 0;
}

/*
 * routine to check that the specified directory is empty (for rmdir)
 */
static int empty_dir (struct inode * inode)
{
	unsigned long offset;
	struct buffer_head * bh;
	struct ext2_dir_entry * de, * de1;
	struct super_block * sb;
	int err;

	sb = inode->i_sb;
	if (inode->i_size < EXT2_DIR_REC_LEN(1) + EXT2_DIR_REC_LEN(2) ||
	    !(bh = ext2_bread (inode, 0, 0, &err))) {
	    	printk ("warning - bad directory (dev %04x, dir %d)\n",
			inode->i_dev, inode->i_ino);
		return 1;
	}
	de = (struct ext2_dir_entry *) bh->b_data;
	de1 = (struct ext2_dir_entry *) ((char *) de + de->rec_len);
	if (de->inode != inode->i_ino || !de1->inode || 
	    strcmp (".", de->name) || strcmp ("..", de1->name)) {
	    	printk ("warning - bad directory (dev %04x, dir %d)\n",
			inode->i_dev, inode->i_ino);
		return 1;
	}
	offset = de->rec_len + de1->rec_len;
	de = (struct ext2_dir_entry *) ((char *) de1 + de1->rec_len);
	while (offset < inode->i_size ) {
		if ((void *) de >= (void *) (bh->b_data + sb->s_blocksize)) {
			brelse (bh);
			bh = ext2_bread (inode, offset >> EXT2_BLOCK_SIZE_BITS(sb), 1, &err);
			if (!bh) {
				offset += sb->s_blocksize;
				continue;
			}
			de = (struct ext2_dir_entry *) bh->b_data;
		}
		if (! ext2_check_dir_entry ("empty_dir", inode, de, bh,
					    offset)) {
			brelse (bh);
			return 1;
		}
		if (de->inode) {
			brelse (bh);
			return 0;
		}
		offset += de->rec_len;
		de = (struct ext2_dir_entry *) ((char *) de + de->rec_len);
	}
	brelse (bh);
	return 1;
}

int ext2_rmdir (struct inode * dir, const char * name, int len)
{
	int retval;
	struct inode * inode;
	struct buffer_head * bh;
	struct ext2_dir_entry * de, * pde;

	if (!dir)
		return -ENOENT;
	inode = NULL;
	bh = ext2_find_entry (dir, name, len, &de, &pde);
	retval = -ENOENT;
	if (!bh)
		goto end_rmdir;
	retval = -EPERM;
	if (!(inode = iget (dir->i_sb, de->inode)))
		goto end_rmdir;
	if ((dir->i_mode & S_ISVTX) && current->euid &&
	    inode->i_uid != current->euid)
		goto end_rmdir;
	if (inode->i_dev != dir->i_dev)
		goto end_rmdir;
	if (inode == dir)	/* we may not delete ".", but "../dir" is ok */
		goto end_rmdir;
	if (!S_ISDIR(inode->i_mode)) {
		retval = -ENOTDIR;
		goto end_rmdir;
	}
	if (!empty_dir (inode)) {
		retval = -ENOTEMPTY;
		goto end_rmdir;
	}
	if (inode->i_count > 1) {
		retval = -EBUSY;
		goto end_rmdir;
	}
	if (inode->i_nlink != 2)
		printk ("empty  directory has nlink!=2 (%d)\n", inode->i_nlink);
#ifndef DONT_USE_DCACHE
	ext2_dcache_remove (dir->i_dev, dir->i_ino, de->name, de->name_len);
#endif
	ext2_delete_entry (de, pde);
	bh->b_dirt = 1;
	inode->i_nlink = 0;
	inode->i_dirt = 1;
	dir->i_nlink --;
	dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	dir->i_dirt = 1;
	retval = 0;
end_rmdir:
	iput (dir);
	iput (inode);
	brelse (bh);
	return retval;
}

int ext2_unlink (struct inode * dir, const char * name, int len)
{
	int retval;
	struct inode * inode;
	struct buffer_head * bh;
	struct ext2_dir_entry * de, * pde;

	if (!dir)
		return -ENOENT;
	retval = -ENOENT;
	inode = NULL;
	bh = ext2_find_entry (dir, name, len, &de, &pde);
	if (!bh)
		goto end_unlink;
	if (!(inode = iget (dir->i_sb, de->inode)))
		goto end_unlink;
	retval = -EPERM;
	if ((dir->i_mode & S_ISVTX) && !suser() &&
	    current->euid != inode->i_uid &&
	    current->euid != dir->i_uid)
		goto end_unlink;
	if (S_ISDIR(inode->i_mode))
		goto end_unlink;
	if (!inode->i_nlink) {
		printk ("Deleting nonexistent file (%04x:%d), %d\n",
			inode->i_dev, inode->i_ino, inode->i_nlink);
		inode->i_nlink = 1;
	}
#ifndef DONT_USE_DCACHE
	ext2_dcache_remove (dir->i_dev, dir->i_ino, de->name, de->name_len);
#endif
	ext2_delete_entry (de, pde);
	bh->b_dirt = 1;
	dir->i_ctime = dir->i_mtime = CURRENT_TIME;
	dir->i_dirt = 1;
	inode->i_nlink --;
	inode->i_dirt = 1;
	inode->i_ctime = CURRENT_TIME;
	retval = 0;
end_unlink:
	brelse (bh);
	iput (inode);
	iput (dir);
	return retval;
}

int ext2_symlink (struct inode * dir, const char * name, int len,
		  const char * symname)
{
	struct ext2_dir_entry * de;
	struct inode * inode = NULL;
	struct buffer_head * bh = NULL, * name_block = NULL;
	char * link;
	int i, err;
	int l;
	char c;

	if (!(inode = ext2_new_inode (dir, S_IFLNK))) {
		iput (dir);
		return -ENOSPC;
	}
	inode->i_mode = S_IFLNK | 0777;
	inode->i_op = &ext2_symlink_inode_operations;
	for (l = 0; l < inode->i_sb->s_blocksize - 1 &&
	     symname [l]; l++)
		;
	if (l >= EXT2_N_BLOCKS * sizeof (unsigned long)) {
#ifdef EXT2FS_DEBUG
		printk ("ext2_symlink: l=%d, normal symlink\n", l);
#endif
		name_block = ext2_bread (inode, 0, 1, &err);
		if (!name_block) {
			iput (dir);
			inode->i_nlink --;
			inode->i_dirt = 1;
			iput (inode);
			return err;
		}
		link = name_block->b_data;
	} else {
		link = (char *) inode->u.ext2_i.i_data;
#ifdef EXT2FS_DEBUG
		printk ("ext2_symlink: l=%d, fast symlink\n", l);
#endif
	}
	i = 0;
	while (i < inode->i_sb->s_blocksize - 1 && (c = *(symname ++)))
		link[i++] = c;
	link[i] = 0;
	if (name_block) {
		name_block->b_dirt = 1;
		brelse (name_block);
	}
	inode->i_size = i;
	inode->i_dirt = 1;
	bh = ext2_find_entry (dir, name, len, &de, NULL);
	if (bh) {
		inode->i_nlink --;
		inode->i_dirt = 1;
		iput (inode);
		brelse (bh);
		iput (dir);
		return -EEXIST;
	}
	bh = ext2_add_entry (dir, name, len, &de, &err);
	if (!bh) {
		inode->i_nlink --;
		inode->i_dirt = 1;
		iput (inode);
		iput (dir);
		return err;
	}
	de->inode = inode->i_ino;
#ifndef DONT_USE_DCACHE
	ext2_dcache_add (dir->i_dev, dir->i_ino, de->name, de->name_len,
			 de->inode);
#endif
	bh->b_dirt = 1;
	brelse (bh);
	iput (dir);
	iput (inode);
	return 0;
}

int ext2_link (struct inode * oldinode, struct inode * dir,
	       const char * name, int len)
{
	struct ext2_dir_entry * de;
	struct buffer_head * bh;
	int err;

	if (S_ISDIR(oldinode->i_mode)) {
		iput (oldinode);
		iput (dir);
		return -EPERM;
	}
	if (oldinode->i_nlink > EXT2_LINK_MAX) {
		iput (oldinode);
		iput (dir);
		return -EMLINK;
	}
	bh = ext2_find_entry (dir, name, len, &de, NULL);
	if (bh) {
		brelse (bh);
		iput (dir);
		iput (oldinode);
		return -EEXIST;
	}
	bh = ext2_add_entry (dir, name, len, &de, &err);
	if (!bh) {
		iput (dir);
		iput (oldinode);
		return err;
	}
	de->inode = oldinode->i_ino;
#ifndef DONT_USE_DCACHE
	ext2_dcache_add (dir->i_dev, dir->i_ino, de->name, de->name_len,
			 de->inode);
#endif
	bh->b_dirt = 1;
	brelse (bh);
	iput (dir);
	oldinode->i_nlink ++;
	oldinode->i_ctime = CURRENT_TIME;
	oldinode->i_dirt = 1;
	iput (oldinode);
	return 0;
}

static int subdir (struct inode * new, struct inode * old)
{
	int ino;
	int result;

	new->i_count ++;
	result = 0;
	for (;;) {
		if (new == old) {
			result = 1;
			break;
		}
		if (new->i_dev != old->i_dev)
			break;
		ino = new->i_ino;
		if (ext2_lookup (new, "..", 2, &new))
			break;
		if (new->i_ino == ino)
			break;
	}
	iput (new);
	return result;
}

#define PARENT_INO(buffer) \
	((struct ext2_dir_entry *) ((char *) buffer + \
	((struct ext2_dir_entry *) buffer)->rec_len))->inode

#define PARENT_NAME(buffer) \
	((struct ext2_dir_entry *) ((char *) buffer + \
	((struct ext2_dir_entry *) buffer)->rec_len))->name

/*
 * rename uses retrying to avoid race-conditions: at least they should be
 * minimal.
 * it tries to allocate all the blocks, then sanity-checks, and if the sanity-
 * checks fail, it tries to restart itself again. Very practical - no changes
 * are done until we know everything works ok.. and then all the changes can be
 * done in one fell swoop when we have claimed all the buffers needed.
 *
 * Anybody can rename anything with this: the permission checks are left to the
 * higher-level routines.
 */
static int do_ext2_rename (struct inode * old_dir, const char * old_name,
			   int old_len, struct inode * new_dir,
			   const char * new_name, int new_len)
{
	struct inode * old_inode, * new_inode;
	struct buffer_head * old_bh, * new_bh, * dir_bh;
	struct ext2_dir_entry * old_de, * new_de, * pde;
	int retval;

	goto start_up;
try_again:
	brelse (old_bh);
	brelse (new_bh);
	brelse (dir_bh);
	iput (old_inode);
	iput (new_inode);
	current->counter = 0;
	schedule ();
start_up:
	old_inode = new_inode = NULL;
	old_bh = new_bh = dir_bh = NULL;
	old_bh = ext2_find_entry (old_dir, old_name, old_len, &old_de, &pde);
	retval = -ENOENT;
	if (!old_bh)
		goto end_rename;
	old_inode = iget (old_dir->i_sb, old_de->inode);
	if (!old_inode)
		goto end_rename;
	retval = -EPERM;
	if ((old_dir->i_mode & S_ISVTX) && 
	    current->euid != old_inode->i_uid &&
	    current->euid != old_dir->i_uid && !suser())
		goto end_rename;
	new_bh = ext2_find_entry (new_dir, new_name, new_len, &new_de, NULL);
	if (new_bh) {
		new_inode = iget (new_dir->i_sb, new_de->inode);
		if (!new_inode) {
			brelse (new_bh);
			new_bh = NULL;
		}
	}
	if (new_inode == old_inode) {
		retval = 0;
		goto end_rename;
	}
	if (new_inode && S_ISDIR(new_inode->i_mode)) {
		retval = -EISDIR;
		if (!S_ISDIR(old_inode->i_mode))
			goto end_rename;
		retval = -EINVAL;
		if (subdir (new_dir, old_inode))
			goto end_rename;
		retval = -ENOTEMPTY;
		if (!empty_dir (new_inode))
			goto end_rename;
		retval = -EBUSY;
		if (new_inode->i_count > 1)
			goto end_rename;
	}
	retval = -EPERM;
	if (new_inode && (new_dir->i_mode & S_ISVTX) &&
	    current->euid != new_inode->i_uid &&
	    current->euid != new_dir->i_uid && !suser())
		goto end_rename;
	if (S_ISDIR(old_inode->i_mode)) {
		retval = -ENOTDIR;
		if (new_inode && !S_ISDIR(new_inode->i_mode))
			goto end_rename;
		retval = -EINVAL;
		if (subdir (new_dir, old_inode))
			goto end_rename;
		dir_bh = ext2_bread (old_inode, 0, 0, &retval);
		if (!dir_bh)
			goto end_rename;
		if (PARENT_INO(dir_bh->b_data) != old_dir->i_ino)
			goto end_rename;
		retval = -EMLINK;
		if (!new_inode && new_dir->i_nlink >= EXT2_LINK_MAX)
			goto end_rename;
	}
	if (!new_bh)
		new_bh = ext2_add_entry (new_dir, new_name, new_len, &new_de,
					 &retval);
	if (!new_bh)
		goto end_rename;
/* sanity checking before doing the rename - avoid races */
	if (new_inode && (new_de->inode != new_inode->i_ino))
		goto try_again;
	if (new_de->inode && !new_inode)
		goto try_again;
	if (old_de->inode != old_inode->i_ino)
		goto try_again;
/* ok, that's it */
	new_de->inode = old_inode->i_ino;
#ifndef DONT_USE_DCACHE
	ext2_dcache_remove (old_dir->i_dev, old_dir->i_ino, old_de->name,
			    old_de->name_len);
	ext2_dcache_add (new_dir->i_dev, new_dir->i_ino, new_de->name,
			 new_de->name_len, new_de->inode);
#endif
	if (old_bh->b_blocknr == new_bh->b_blocknr &&
	    ((char *) new_de) + new_de->rec_len == (char *) old_de)
		new_de->rec_len += old_de->rec_len;
	else
		ext2_delete_entry (old_de, pde);
	if (new_inode) {
		new_inode->i_nlink --;
		new_inode->i_dirt = 1;
	}
	old_bh->b_dirt = 1;
	new_bh->b_dirt = 1;
	if (dir_bh) {
		PARENT_INO(dir_bh->b_data) = new_dir->i_ino;
		dir_bh->b_dirt = 1;
		old_dir->i_nlink --;
		new_dir->i_nlink ++;
		old_dir->i_dirt = 1;
		new_dir->i_dirt = 1;
	}
	retval = 0;
end_rename:
	brelse (dir_bh);
	brelse (old_bh);
	brelse (new_bh);
	iput (old_inode);
	iput (new_inode);
	iput (old_dir);
	iput (new_dir);
	return retval;
}

/*
 * Ok, rename also locks out other renames, as they can change the parent of
 * a directory, and we don't want any races. Other races are checked for by
 * "do_rename()", which restarts if there are inconsistencies.
 *
 * Note that there is no race between different filesystems: it's only within
 * the same device that races occur: many renames can happen at once, as long
 * as they are on different partitions.
 *
 * In the second extended file system, we use a lock flag stored in the memory
 * super-block.  This way, we really lock other renames only if they occur
 * on the same file system
 */
int ext2_rename (struct inode * old_dir, const char * old_name, int old_len,
		 struct inode * new_dir, const char * new_name, int new_len)
{
	int result;

	while (old_dir->i_sb->u.ext2_sb.s_rename_lock)
		sleep_on (&old_dir->i_sb->u.ext2_sb.s_rename_wait);
	old_dir->i_sb->u.ext2_sb.s_rename_lock = 1;
	result = do_ext2_rename (old_dir, old_name, old_len, new_dir,
				 new_name, new_len);
	old_dir->i_sb->u.ext2_sb.s_rename_lock = 0;
	wake_up (&old_dir->i_sb->u.ext2_sb.s_rename_wait);
	return result;
}
