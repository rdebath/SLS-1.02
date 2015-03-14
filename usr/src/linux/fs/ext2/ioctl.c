/*
 * linux/fs/ext2/ioctl.c
 *
 * Copyright (C) 1993  Remy Card (card@masi.ibp.fr)
 */

#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/ext2_fs.h>

int ext2_ioctl (struct inode * inode, struct file * filp, unsigned int cmd,
		unsigned long arg)
{
#ifdef EXT2FS_DEBUG
	printk ("ext2_ioctl: cmd = %d, arg = %d\n", cmd, arg);
#endif
	return -EINVAL;
}
