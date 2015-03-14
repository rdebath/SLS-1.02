/*
 *	linux/mm/mmap.c
 *
 * Written by obz.
 */
#include <linux/stat.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/errno.h>
#include <linux/mman.h>
#include <linux/string.h>

#include <asm/segment.h>
#include <asm/system.h>

/*
 * description of effects of mapping type and prot in current implementation.
 * this is due to the current handling of page faults in memory.c. the expected
 * behavior is in parens:
 *
 * map_type	prot
 *		PROT_NONE	PROT_READ	PROT_WRITE	PROT_EXEC
 * MAP_SHARED	r: (no) yes	r: (yes) yes	r: (no) yes	r: (no) no
 *		w: (no) yes	w: (no) copy	w: (yes) yes	w: (no) no
 *		x: (no) no	x: (no) no	x: (no) no	x: (yes) no
 *		
 * MAP_PRIVATE	r: (no) yes	r: (yes) yes	r: (no) yes	r: (no) no
 *		w: (no) copy	w: (no) copy	w: (copy) copy	w: (no) no
 *		x: (no) no	x: (no) no	x: (no) no	x: (yes) no
 *
 */

#define CODE_SPACE(addr) ((((addr)+4095)&~4095) < \
			  current->start_code + current->end_code)

int sys_mmap(unsigned long *buffer)
{
	unsigned long base, addr;
	unsigned long len, limit, off;
	int prot, flags, mask, fd, error;
	struct file *file;

	addr = (unsigned long)	get_fs_long(buffer);	/* user address space*/
	len = (size_t)		get_fs_long(buffer+1);	/* nbytes of mapping */
	prot = (int)		get_fs_long(buffer+2);	/* protection */
	flags = (int)		get_fs_long(buffer+3);	/* mapping type */
	fd = (int) 		get_fs_long(buffer+4);	/* object to map */
	off = (unsigned long)	get_fs_long(buffer+5);	/* offset in object */

	if (fd >= NR_OPEN || fd < 0 || !(file = current->filp[fd]))
		return -EBADF;
	if (addr > TASK_SIZE || len > TASK_SIZE || addr > TASK_SIZE-len)
		return -EINVAL;

	/*
	 * do simple checking here so the lower-level routines won't have
	 * to. we assume access permissions have been handled by the open
	 * of the memory object, so we don't do any here.
	 */

	switch (flags & MAP_TYPE) {
	case MAP_SHARED:
		if ((prot & PROT_WRITE) && !(file->f_mode & 2))
			return -EINVAL;
		/* fall through */
	case MAP_PRIVATE:
		if (!(file->f_mode & 1))
			return -EINVAL;
		break;

	default:
		return -EINVAL;
	}

	/*
	 * obtain the address to map to. we verify (or select) it and ensure
	 * that it represents a valid section of the address space. we assume
	 * that if PROT_EXEC is specified this should be in the code segment.
	 */
	if (prot & PROT_EXEC) {
		base = get_base(current->ldt[1]);	/* cs */
		limit = get_limit(0x0f);		/* cs limit */
	} else {
		base = get_base(current->ldt[2]);	/* ds */
		limit = get_limit(0x17);		/* ds limit */
	}

	if (flags & MAP_FIXED) {
		/*
		 * if MAP_FIXED is specified, we have to map exactly at this
		 * address. it must be page aligned and not ambiguous.
		 */
		if ((addr & 0xfff) || addr > 0x7fffffff || addr == 0 ||
		    (off & 0xfff))
			return -EINVAL;
		if (addr + len > limit)
			return -ENOMEM;
	} else {
		/*
		 * we're given a hint as to where to put the address.
		 * that we still need to search for a range of pages which
		 * are not mapped and which won't impact the stack or data
		 * segment.
		 * in linux, we only have a code segment and data segment.
		 * since data grows up and stack grows down, we're sort of
		 * stuck. placing above the data will break malloc, below
		 * the stack will cause stack overflow. because of this
		 * we don't allow nonspecified mappings...
		 */
		return -ENOMEM;
	}

	/*
	 * determine the object being mapped and call the appropriate
	 * specific mapper. the address has already been validated, but
	 * not unmapped
	 */
	if (!file->f_op || !file->f_op->mmap)
		return -ENODEV;
	mask = 0;
	if (prot & (PROT_READ | PROT_EXEC))
		mask |= PAGE_READONLY;
	if (prot & PROT_WRITE)
		mask |= PAGE_RW;
	if (!mask)
		return -EINVAL;
	if ((flags & MAP_TYPE) == MAP_PRIVATE) {
		mask |= PAGE_COW;
		mask &= ~PAGE_RW;
	}
	error = file->f_op->mmap(file->f_inode, file, base + addr, len, mask, off);
	if (error)
		return error;
	return addr;
}

int sys_munmap(unsigned long addr, size_t len)
{
	unsigned long base, limit;

	base = get_base(current->ldt[2]);	/* map into ds */
	limit = get_limit(0x17);		/* ds limit */

	if ((addr & 0xfff) || addr > 0x7fffffff || addr == 0 ||
	    addr + len > limit)
		return -EINVAL;
	if (unmap_page_range(base + addr, len))
		return -EAGAIN; /* should never happen */
	return 0;
}
