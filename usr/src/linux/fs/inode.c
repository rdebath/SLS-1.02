/*
 *  linux/fs/inode.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linux/stat.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/string.h>

#include <asm/system.h>

static struct inode * inode_table;
static struct inode * last_inode;
static struct wait_queue * inode_wait = NULL;

unsigned long inode_init(unsigned long start, unsigned long end)
{
	start += 0x0000000f;
	start &= 0xfffffff0;
	inode_table = (struct inode *) start;
	last_inode = inode_table;
	start = (unsigned long) (inode_table + NR_INODE);
	memset(inode_table,0,NR_INODE*sizeof(struct inode));
	return start;
}

static void __wait_on_inode(struct inode *);

static inline void wait_on_inode(struct inode * inode)
{
	if (inode->i_lock)
		__wait_on_inode(inode);
}

static inline void lock_inode(struct inode * inode)
{
	wait_on_inode(inode);
	inode->i_lock = 1;
}

static inline void unlock_inode(struct inode * inode)
{
	inode->i_lock = 0;
	wake_up(&inode->i_wait);
}

/*
 * Note that we don't want to disturb any wait-queues when we discard
 * an inode.
 *
 * Argghh. Got bitten by a gcc problem with inlining: no way to tell
 * the compiler that the inline asm function 'memset' changes 'inode'.
 * I've been searching for the bug for days, and was getting desperate.
 * Finally looked at the assembler output... Grrr.
 *
 * The solution is the weird use of 'volatile'. Ho humm. Have to report
 * it to the gcc lists, and hope we can do this more cleanly some day..
 */
void clear_inode(struct inode * inode)
{
	struct wait_queue * wait;

	wait_on_inode(inode);
	wait = ((volatile struct inode *) inode)->i_wait;
	memset(inode,0,sizeof(*inode));
	((volatile struct inode *) inode)->i_wait = wait;
}

int fs_may_mount(dev_t dev)
{
	struct inode * inode;

	for (inode = inode_table+0 ; inode < inode_table+NR_INODE ; inode++) {
		if (inode->i_dev != dev)
			continue;
		if (inode->i_count || inode->i_dirt || inode->i_lock)
			return 0;
		clear_inode(inode);
	}
	return 1;
}

int fs_may_umount(dev_t dev, struct inode * mount_root)
{
	struct inode * inode;

	for (inode = inode_table+0 ; inode < inode_table+NR_INODE ; inode++) {
		if (inode->i_dev==dev && inode->i_count)
			if (inode == mount_root && inode->i_count == 1)
				continue;
			else
				return 0;
	}
	return 1;
}

static void write_inode(struct inode * inode)
{
	if (!inode->i_dirt)
		return;
	wait_on_inode(inode);
	if (!inode->i_dirt)
		return;
	if (!inode->i_sb || !inode->i_sb->s_op || !inode->i_sb->s_op->write_inode) {
		inode->i_dirt = 0;
		return;
	}
	inode->i_lock = 1;	
	inode->i_sb->s_op->write_inode(inode);
	unlock_inode(inode);
}

static void read_inode(struct inode * inode)
{
	lock_inode(inode);
	if (inode->i_sb && inode->i_sb->s_op && inode->i_sb->s_op->read_inode)
		inode->i_sb->s_op->read_inode(inode);
	unlock_inode(inode);
}

/*
 * notify_change is called for inode-changing operations such as
 * chown, chmod, utime, and truncate.  It is guaranteed (unlike
 * write_inode) to be called from the context of the user requesting
 * the change.  It is not called for ordinary access-time updates.
 * NFS uses this to get the authentication correct.  -- jrs
 */

int notify_change(int flags, struct inode * inode)
{
	if (inode->i_sb && inode->i_sb->s_op  &&
	    inode->i_sb->s_op->notify_change)
		return inode->i_sb->s_op->notify_change(flags, inode);
	return 0;
}

/*
 * bmap is needed for demand-loading and paging: if this function
 * doesn't exist for a filesystem, then those things are impossible:
 * executables cannot be run from the filesystem etc...
 *
 * This isn't as bad as it sounds: the read-routines might still work,
 * so the filesystem would be otherwise ok (for example, you might have
 * a DOS filesystem, which doesn't lend itself to bmap very well, but
 * you could still transfer files to/from the filesystem)
 */
int bmap(struct inode * inode, int block)
{
	if (inode->i_op && inode->i_op->bmap)
		return inode->i_op->bmap(inode,block);
	return 0;
}

void invalidate_inodes(dev_t dev)
{
	int i;
	struct inode * inode;

	inode = 0+inode_table;
	for(i=0 ; i<NR_INODE ; i++,inode++) {
		wait_on_inode(inode);
		if (inode->i_dev == dev) {
			if (inode->i_count) {
				printk("VFS: inode busy on removed device %d/%d\n",
							MAJOR(dev), MINOR(dev));
				continue;
			}
			clear_inode(inode);
		}
	}
}

void sync_inodes(dev_t dev)
{
	struct inode * inode;

	for(inode = 0+inode_table ; inode < NR_INODE+inode_table ; inode++) {
		if (dev && inode->i_dev != dev)
			continue;
		wait_on_inode(inode);
		if (inode->i_dirt)
			write_inode(inode);
	}
}

void iput(struct inode * inode)
{
	if (!inode)
		return;
	wait_on_inode(inode);
	if (!inode->i_count) {
		printk("VFS: iput: trying to free free inode\n");
		printk("VFS: device %d/%d, inode %d, mode=0%07o\n",
			MAJOR(inode->i_rdev), MINOR(inode->i_rdev),
					inode->i_ino, inode->i_mode);
		return;
	}
	if (inode->i_pipe) {
		wake_up(&PIPE_READ_WAIT(*inode));
		wake_up(&PIPE_WRITE_WAIT(*inode));
	}
repeat:
	if (inode->i_count>1) {
		inode->i_count--;
		return;
	}
	wake_up(&inode_wait);
	if (inode->i_pipe) {
		unsigned long page = (unsigned long) PIPE_BASE(*inode);
		PIPE_BASE(*inode) = NULL;
		free_page(page);
	}
	if (inode->i_sb && inode->i_sb->s_op && inode->i_sb->s_op->put_inode) {
		inode->i_sb->s_op->put_inode(inode);
		if (!inode->i_nlink)
			return;
	}
	if (inode->i_dirt) {
		write_inode(inode);	/* we can sleep - so do again */
		wait_on_inode(inode);
		goto repeat;
	}
	inode->i_count--;
	return;
}

struct inode * get_empty_inode(void)
{
	struct inode * inode;
	int i;

repeat:
	inode = NULL;
	for (i = NR_INODE; i ; i--) {
		if (++last_inode >= inode_table + NR_INODE)
			last_inode = inode_table;
		if (!last_inode->i_count) {
			inode = last_inode;
			if (!inode->i_dirt && !inode->i_lock)
				break;
		}
	}
	if (!inode) {
		printk("VFS: No free inodes - contact Linus\n");
		sleep_on(&inode_wait);
		goto repeat;
	}
	if (inode->i_lock) {
		wait_on_inode(inode);
		goto repeat;
	}
	if (inode->i_dirt) {
		write_inode(inode);
		goto repeat;
	}
	if (inode->i_count)
		goto repeat;
	clear_inode(inode);
	inode->i_count = 1;
	inode->i_nlink = 1;
	return inode;
}

struct inode * get_pipe_inode(void)
{
	struct inode * inode;

	if (!(inode = get_empty_inode()))
		return NULL;
	if (!(PIPE_BASE(*inode) = (char *) get_free_page(GFP_USER))) {
		inode->i_count = 0;
		return NULL;
	}
	inode->i_count = 2;	/* sum of readers/writers */
	PIPE_READ_WAIT(*inode) = PIPE_WRITE_WAIT(*inode) = NULL;
	PIPE_HEAD(*inode) = PIPE_TAIL(*inode) = 0;
	PIPE_RD_OPENERS(*inode) = PIPE_WR_OPENERS(*inode) = 0;
	PIPE_READERS(*inode) = PIPE_WRITERS(*inode) = 1;
	inode->i_pipe = 1;
	inode->i_mode |= S_IFIFO;
	inode->i_uid = current->euid;
	inode->i_gid = current->egid;
	inode->i_atime = inode->i_mtime = inode->i_ctime = CURRENT_TIME;
	return inode;
}

struct inode * iget(struct super_block * sb,int nr)
{
	struct inode * inode, * empty;

	if (!sb)
		panic("VFS: iget with sb==NULL");
	empty = get_empty_inode();
	inode = inode_table;
	while (inode < NR_INODE+inode_table) {
		if (inode->i_dev != sb->s_dev || inode->i_ino != nr) {
			inode++;
			continue;
		}
		wait_on_inode(inode);
		if (inode->i_dev != sb->s_dev || inode->i_ino != nr) {
			inode = inode_table;
			continue;
		}
		inode->i_count++;
		if (inode->i_mount) {
			int i;

			for (i = 0 ; i<NR_SUPER ; i++)
				if (super_block[i].s_covered==inode)
					break;
			if (i >= NR_SUPER) {
				printk("VFS: Mounted inode hasn't got sb\n");
				if (empty) {
					if (last_inode > inode_table)
						--last_inode;
					else
						last_inode
						= inode_table + NR_INODE;
					iput(empty);
				}
				return inode;
			}
			iput(inode);
			if (!(inode = super_block[i].s_mounted))
				printk("VFS: Mounted device %d/%d has no rootinode\n",
					MAJOR(inode->i_dev), MINOR(inode->i_dev));
			else {
				inode->i_count++;
				wait_on_inode(inode);
			}
		}
		if (empty) {
			if (last_inode > inode_table)
				--last_inode;
			else
				last_inode
				= inode_table + NR_INODE;
			iput(empty);
		}
		return inode;
	}
	if (!empty)
		return (NULL);
	inode = empty;
	inode->i_sb = sb;
	inode->i_dev = sb->s_dev;
	inode->i_ino = nr;
	inode->i_flags = sb->s_flags;
	read_inode(inode);
	return inode;
}

/*
 * The "new" scheduling primitives (new as of 0.97 or so) allow this to
 * be done without disabling interrupts (other than in the actual queue
 * updating things: only a couple of 386 instructions). This should be
 * much better for interrupt latency.
 */
static void __wait_on_inode(struct inode * inode)
{
	struct wait_queue wait = { current, NULL };

	add_wait_queue(&inode->i_wait, &wait);
repeat:
	current->state = TASK_UNINTERRUPTIBLE;
	if (inode->i_lock) {
		schedule();
		goto repeat;
	}
	remove_wait_queue(&inode->i_wait, &wait);
	current->state = TASK_RUNNING;
}
