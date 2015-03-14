#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/fs.h>
#include <linux/hdreg.h>
#include <linux/errno.h>

#include <asm/segment.h>

#include "../blk.h"
#include "scsi.h"
#include "hosts.h"
#include "sd.h"

extern int scsi_ioctl (Scsi_Device *dev, int cmd, void *arg);
extern int revalidate_scsidisk(int, int);

int sd_ioctl(struct inode * inode, struct file * file, unsigned int cmd, unsigned long arg)
{
	int dev = inode->i_rdev;
	int host, error;
	int diskinfo[4];
	struct hd_geometry *loc = (void *) arg;

	switch (cmd) {
         	case HDIO_REQ:   /* Return BIOS disk parameters */
			if (!loc)  return -EINVAL;
			error = verify_area(VERIFY_WRITE, loc, sizeof(*loc));
			if (error)
				return error;
			host = rscsi_disks[MINOR(dev) >> 4].device->host_no;
			diskinfo[0] = 0;
			diskinfo[1] = 0;
			diskinfo[2] = 0;
			if(scsi_hosts[host].bios_param != NULL)
			      scsi_hosts[host].bios_param(rscsi_disks[MINOR(dev) >> 4].capacity,
							  dev,
							  &diskinfo[0]);
			put_fs_byte(diskinfo[0],
				(char *) &loc->heads);
			put_fs_byte(diskinfo[1],
				(char *) &loc->sectors);
			put_fs_word(diskinfo[2],
				(short *) &loc->cylinders);
			put_fs_long(sd[MINOR(inode->i_rdev)].start_sect,
				(long *) &loc->start);
			return 0;
         	case BLKGETSIZE:   /* Return device size */
			if (!arg)  return -EINVAL;
			error = verify_area(VERIFY_WRITE, (long *) arg, sizeof(long));
			if (error)
				return error;
			put_fs_long(sd[MINOR(inode->i_rdev)].nr_sects,
				(long *) arg);
			return 0;
		case BLKRRPART: /* Re-read partition tables */
			return revalidate_scsidisk(dev, 1);
		default:
			return scsi_ioctl(rscsi_disks[MINOR(dev) >> 4].device , cmd, (void *) arg);
	}
}
