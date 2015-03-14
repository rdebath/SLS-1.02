SLS 1.02
========

![SLS 1.02](ISO/sls-1.02.png)

This was the distribution to have in 1993, it was my first Linux
distribution. To this day I haven't reinstalled the system that this was
installed on. The machine has been upgraded, hard disks, motherboard,
tape drives and so on. At some point during the nineties I upgraded the
OS to use Debian Linux and since then it has been upgraded normally. But
... there are still a few signs of the original SLS 1.02 install.

To recreate the CD-ROM ISO file you can use the "jigdo-file" tool ...

`$ jigdo-file make-image --jigdo=ISO/SLS-1.02.iso.jigdo *`

But this probably isn't very useful as the CD-ROM is not bootable.
The CD-ROM contains all the binaries and source uncompressed on the CD-ROM.
There are floppy disk images for both 3½ and and 5¼ inch floppy drives.

I haven't installed this to a qemu VM, mostly because I'm not looking forward
to swapping those floppies!
