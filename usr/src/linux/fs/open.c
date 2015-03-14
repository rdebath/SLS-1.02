/*
 *  linux/fs/open.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linux/vfs.h>
#include <linux/types.h>
#include <linux/utime.h>
#include <linux/errno.h>
#include <linux/fcntl.h>
#include <linux/stat.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/signal.h>
#include <linux/tty.h>
#include <linux/time.h>

#include <asm/segment.h>

extern void fcntl_remove_locks(struct task_struct *, struct file *);

int sys_ustat(int dev, struct ustat * ubuf)
{
	return -ENOSYS;
}

int sys_statfs(const char * path, struct statfs * buf)
{
	struct inode * inode;
	int error;

	error = verify_area(VERIFY_WRITE, buf, sizeof(struct statfs));
	if (error)
		return error;
	error = namei(path,&inode);
	if (error)
		return error;
	if (!inode->i_sb->s_op->statfs) {
		iput(inode);
		return -ENOSYS;
	}
	inode->i_sb->s_op->statfs(inode->i_sb, buf);
	iput(inode);
	return 0;
}

int sys_fstatfs(unsigned int fd, struct statfs * buf)
{
	struct inode * inode;
	struct file * file;
	int error;

	error = verify_area(VERIFY_WRITE, buf, sizeof(struct statfs));
	if (error)
		return error;
	if (fd >= NR_OPEN || !(file = current->filp[fd]))
		return -EBADF;
	if (!(inode = file->f_inode))
		return -ENOENT;
	if (!inode->i_sb->s_op->statfs)
		return -ENOSYS;
	inode->i_sb->s_op->statfs(inode->i_sb, buf);
	return 0;
}

int sys_truncate(const char * path, unsigned int length)
{
	struct inode * inode;
	int error;

	error = namei(path,&inode);
	if (error)
		return error;
	if (S_ISDIR(inode->i_mode) || !permission(inode,MAY_WRITE)) {
		iput(inode);
		return -EACCES;
	}
	if (IS_RDONLY(inode)) {
		iput(inode);
		return -EROFS;
	}
	inode->i_size = length;
	if (inode->i_op && inode->i_op->truncate)
		inode->i_op->truncate(inode);
	inode->i_atime = inode->i_mtime = CURRENT_TIME;
	inode->i_dirt = 1;
	error = notify_change(NOTIFY_SIZE, inode);
	iput(inode);
	return error;
}

int sys_ftruncate(unsigned int fd, unsigned int length)
{
	struct inode * inode;
	struct file * file;

	if (fd >= NR_OPEN || !(file = current->filp[fd]))
		return -EBADF;
	if (!(inode = file->f_inode))
		return -ENOENT;
	if (S_ISDIR(inode->i_mode) || !(file->f_mode & 2))
		return -EACCES;
	inode->i_size = length;
	if (inode->i_op && inode->i_op->truncate)
		inode->i_op->truncate(inode);
	inode->i_atime = inode->i_mtime = CURRENT_TIME;
	inode->i_dirt = 1;
	return notify_change(NOTIFY_SIZE, inode);
}

/* If times==NULL, set access and modification to current time,
 * must be owner or have write permission.
 * Else, update from *times, must be owner or super user.
 */
int sys_utime(char * filename, struct utimbuf * times)
{
	struct inode * inode;
	long actime,modtime;
	int error;

	error = namei(filename,&inode);
	if (error)
		return error;
	if (IS_RDONLY(inode)) {
		iput(inode);
		return -EROFS;
	}
	if (times) {
		if ((current->euid != inode->i_uid) && !suser()) {
			iput(inode);
			return -EPERM;
		}
		actime = get_fs_long((unsigned long *) &times->actime);
		modtime = get_fs_long((unsigned long *) &times->modtime);
		inode->i_ctime = CURRENT_TIME;
	} else {
		if ((current->euid != inode->i_uid) &&
		    !permission(inode,MAY_WRITE)) {
			iput(inode);
			return -EACCES;
		}
		actime = modtime = inode->i_ctime = CURRENT_TIME;
	}
	inode->i_atime = actime;
	inode->i_mtime = modtime;
	inode->i_dirt = 1;
	error = notify_change(NOTIFY_TIME, inode);
	iput(inode);
	return error;
}

/*
 * XXX we should use the real ids for checking _all_ components of the
 * path.  Now we only use them for the final compenent of the path.
 */
int sys_access(const char * filename,int mode)
{
	struct inode * inode;
	int res, i_mode;

	if (mode != (mode & 0007))	/* where's F_OK, X_OK, W_OK, R_OK? */
		return -EINVAL;
	res = namei(filename,&inode);
	if (res)
		return res;
	i_mode = inode->i_mode;
	res = i_mode & 0777;
	if (current->uid == inode->i_uid)
		res >>= 6;
	else if (in_group_p(inode->i_gid))
		res >>= 3;
	iput(inode);
	if ((res & mode) == mode)
		return 0;
	/*
	 * XXX we are doing this test last because we really should be
	 * swapping the effective with the real user id (temporarily),
	 * and then calling suser() routine.  If we do call the
	 * suser() routine, it needs to be called last. 
	 *
	 * XXX nope.  suser() is inappropriate and swapping the ids while
	 * decomposing the path would be racy.
	 */
	if ((!current->uid) &&
	    (S_ISDIR(i_mode) || !(mode & 1) || (i_mode & 0111)))
		return 0;
	return -EACCES;
}

int sys_chdir(const char * filename)
{
	struct inode * inode;
	int error;

	error = namei(filename,&inode);
	if (error)
		return error;
	if (!S_ISDIR(inode->i_mode)) {
		iput(inode);
		return -ENOTDIR;
	}
	if (!permission(inode,MAY_EXEC)) {
		iput(inode);
		return -EACCES;
	}
	iput(current->pwd);
	current->pwd = inode;
	return (0);
}

int sys_chroot(const char * filename)
{
	struct inode * inode;
	int error;

	error = namei(filename,&inode);
	if (error)
		return error;
	if (!S_ISDIR(inode->i_mode)) {
		iput(inode);
		return -ENOTDIR;
	}
	if (!suser()) {
		iput(inode);
		return -EPERM;
	}
	iput(current->root);
	current->root = inode;
	return (0);
}

int sys_fchmod(unsigned int fd, mode_t mode)
{
	struct inode * inode;
	struct file * file;

	if (fd >= NR_OPEN || !(file = current->filp[fd]))
		return -EBADF;
	if (!(inode = file->f_inode))
		return -ENOENT;
	if ((current->euid != inode->i_uid) && !suser())
		return -EPERM;
	if (IS_RDONLY(inode))
		return -EROFS;
	inode->i_mode = (mode & 07777) | (inode->i_mode & ~07777);
	if (!suser() && !in_group_p(inode->i_gid))
		inode->i_mode &= ~S_ISGID;
	inode->i_ctime = CURRENT_TIME;
	inode->i_dirt = 1;
	return notify_change(NOTIFY_MODE, inode);
}

int sys_chmod(const char * filename, mode_t mode)
{
	struct inode * inode;
	int error;

	error = namei(filename,&inode);
	if (error)
		return error;
	if ((current->euid != inode->i_uid) && !suser()) {
		iput(inode);
		return -EPERM;
	}
	if (IS_RDONLY(inode)) {
		iput(inode);
		return -EROFS;
	}
	inode->i_mode = (mode & 07777) | (inode->i_mode & ~07777);
	if (!suser() && !in_group_p(inode->i_gid))
		inode->i_mode &= ~S_ISGID;
	inode->i_ctime = CURRENT_TIME;
	inode->i_dirt = 1;
	error = notify_change(NOTIFY_MODE, inode);
	iput(inode);
	return error;
}

int sys_fchown(unsigned int fd, uid_t user, gid_t group)
{
	struct inode * inode;
	struct file * file;

	if (fd >= NR_OPEN || !(file = current->filp[fd]))
		return -EBADF;
	if (!(inode = file->f_inode))
		return -ENOENT;
	if (IS_RDONLY(inode))
		return -EROFS;
	if (user == (uid_t) -1)
		user = inode->i_uid;
	if (group == (gid_t) -1)
		group = inode->i_gid;
	if ((current->euid == inode->i_uid && user == inode->i_uid &&
	     (in_group_p(group) || group == inode->i_gid)) ||
	    suser()) {
		inode->i_uid = user;
		inode->i_gid = group;
		inode->i_ctime = CURRENT_TIME;
		inode->i_dirt = 1;
		return notify_change(NOTIFY_UIDGID, inode);
	}
	return -EPERM;
}

int sys_chown(const char * filename, uid_t user, gid_t group)
{
	struct inode * inode;
	int error;

	error = lnamei(filename,&inode);
	if (error)
		return error;
	if (IS_RDONLY(inode)) {
		iput(inode);
		return -EROFS;
	}
	if (user == (uid_t) -1)
		user = inode->i_uid;
	if (group == (gid_t) -1)
		group = inode->i_gid;
	if ((current->euid == inode->i_uid && user == inode->i_uid &&
	     (in_group_p(group) || group == inode->i_gid)) ||
	    suser()) {
		inode->i_uid = user;
		inode->i_gid = group;
		inode->i_ctime = CURRENT_TIME;
		inode->i_dirt = 1;
		error = notify_change(NOTIFY_UIDGID, inode);
		iput(inode);
		return error;
	}
	iput(inode);
	return -EPERM;
}

/*
 * Note that while the flag value (low two bits) for sys_open means:
 *	00 - read-only
 *	01 - write-only
 *	10 - read-write
 *	11 - special
 * it is changed into
 *	00 - no permissions needed
 *	01 - read-permission
 *	10 - write-permission
 *	11 - read-write
 * for the internal routines (ie open_namei()/follow_link() etc). 00 is
 * used by symlinks.
 */
int sys_open(const char * filename,int flags,int mode)
{
	struct inode * inode;
	struct file * f;
	char * tmp;
	int flag,error,fd;

	for(fd=0 ; fd<NR_OPEN ; fd++)
		if (!current->filp[fd])
			break;
	if (fd>=NR_OPEN)
		return -EMFILE;
	FD_CLR(fd,&current->close_on_exec);
	f = get_empty_filp();
	if (!f)
		return -ENFILE;
	current->filp[fd] = f;
	f->f_flags = flag = flags;
	f->f_mode = (flag+1) & O_ACCMODE;
	if (f->f_mode)
		flag++;
	if (flag & (O_TRUNC | O_CREAT))
		flag |= 2;
	error = getname(filename,&tmp);
	if (!error) {
		error = open_namei(tmp,flag,mode,&inode,NULL);
		putname(tmp);
	}
	if (error) {
		current->filp[fd]=NULL;
		f->f_count--;
		return error;
	}
	if (flag & O_TRUNC) {
		inode->i_size = 0;
		if (inode->i_op && inode->i_op->truncate)
			inode->i_op->truncate(inode);
		if ((error = notify_change(NOTIFY_SIZE, inode))) {
			iput(inode);
			current->filp[fd] = NULL;
			f->f_count--;
			return error;
		}
		inode->i_dirt = 1;
	}
	f->f_inode = inode;
	f->f_pos = 0;
	f->f_reada = 0;
	f->f_op = NULL;
	if (inode->i_op)
		f->f_op = inode->i_op->default_file_ops;
	if (f->f_op && f->f_op->open) {
		error = f->f_op->open(inode,f);
		if (error) {
			iput(inode);
			f->f_count--;
			current->filp[fd]=NULL;
			return error;
		}
	}
	f->f_flags &= ~(O_CREAT | O_EXCL | O_NOCTTY | O_TRUNC);
	return (fd);
}

int sys_creat(const char * pathname, int mode)
{
	return sys_open(pathname, O_CREAT | O_WRONLY | O_TRUNC, mode);
}

int close_fp(struct file *filp)
{
	struct inode *inode;

	if (filp->f_count == 0) {
		printk("VFS: Close: file count is 0\n");
		return 0;
	}
	inode = filp->f_inode;
	if (inode && S_ISREG(inode->i_mode))
		fcntl_remove_locks(current, filp);
	if (filp->f_count > 1) {
		filp->f_count--;
		return 0;
	}
	if (filp->f_op && filp->f_op->release)
		filp->f_op->release(inode,filp);
	filp->f_count--;
	filp->f_inode = NULL;
	iput(inode);
	return 0;
}

int sys_close(unsigned int fd)
{	
	struct file * filp;

	if (fd >= NR_OPEN)
		return -EBADF;
	FD_CLR(fd, &current->close_on_exec);
	if (!(filp = current->filp[fd]))
		return -EBADF;
	current->filp[fd] = NULL;
	return (close_fp (filp));
}

/*
 * This routine simulates a hangup on the tty, to arrange that users
 * are given clean terminals at login time.
 */
int sys_vhangup(void)
{
	struct tty_struct *tty;

	if (!suser())
		return -EPERM;
	/* See if there is a controlling tty. */
	if (current->tty < 0)
		return 0;
	tty = TTY_TABLE(MINOR(current->tty));
	tty_vhangup(tty);
	return 0;
}
