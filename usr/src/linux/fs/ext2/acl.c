/*
 * linux/fs/ext2/acl.c
 *
 * Copyright (C) 1993  Remy Card (card@masi.ibp.fr)
 */

/*
 * This file will contain the Access Control Lists management for the
 * second extended file system.
 */

#include <linux/sched.h>
#include <linux/ext2_fs.h>
#include <linux/errno.h>

/*
 * ext2_permission ()
 *
 * Check for access rights
 */
int ext2_permission (struct inode * inode, int mask)
{
	int mode = inode->i_mode;

	/* Special case, access is always granted for root */
	if (suser ())
		return 1;
	/* If no ACL, checks using the file mode */
	else if (current->euid == inode->i_uid)
		mode >>= 6;
	else if (in_group_p (inode->i_gid))
		mode >>= 3;
	if (((mode & mask & 0007) == mask))
		return 1;
	else
		return 0;
}
