SLS attempts to impose a very simple structure upon Linux to produce
a usable operating system that can be installed/manipulated/removed,
component by component.  The following documents this structure.

the disks:
----------
Each disk in SLS contains about <= 1185K of stuff.  The files on these 
disks must have names that are all lower case and be able to map 
directly to DOS files (ie, 8.3 names).  Further, the distribution is
subdivided into a collection of series denoted by a single lowercase 
letter.  The disks in a series have a number appended for serialization.

*.tgz files:
--------------
The ".tgz" suffix is just a short form for the conventional ".tar.z" 
suffix (formerly it was ".taz" short for ".tar.Z", but now gzip is
used).  Since such name do map to/from the DOS file namespace, this 
means that all packages in SLS can be manipulated the same as (and 
even be renamed to) ".tar.z" archives.  These archives need only be
unarchived from the root directory ("/"), and should be ready to 
use.  Large packages however, had to be split into several packages,
and currently there is no enforcment provided for coordinating
the multiple pieces.

sysinstall:
-----------
The "sysinstall" script controls the installation/removal of software
from a linux system.  It can also be used to extract a package from 
the live system.  This is actually how SLS can be tested in an 
integrated environment before packaging.  Instead of building the
package and then installing it to see if it works, the software is
installed and tested and then packaged for distribution.  Sysinstall
installs either single packages ("-install"), all packages on a given
disk ("-disk"), all packages in a series N ("-series N"), or any
of the collections of series known about (ie, "a", "b", "c", "x").
Sysinstall looks for the file "diskXN" where 'X' is the series and
'N' is the disk number within that series.  Sysinstall, simply
keeps installing disks in a series until after it has installed
a disk with the file "install.end" on it.  So if a disk is added
to a series, that file must be moved to the new last disk.

/install/doinst.sh:
----------
When a package "pkg.tgz" is installed, the list of files it contains are stored
in the file "/install/installed/pkg".  This file can later be used to
remove the package from the harddisk (with the exception that directories
are not removed).  In addition, if the file "/install/doinst.sh" is
present in a package, it is executed with the "-install" option, and
then renamed to "/install/scripts/pkg".  Similarly, upon removing a
package, it is executed with the "-remove" option.  Two more options
"-extract" and "-retract" are used during the extract process, because
the script must be moved to "/install/doinst.sh" for tarring.  Currently
however, the only packages using this in SLS are the "z?fix.tgz" packages,
talked about in the next section.  One rule of thumb to follow though is 
that all files referenced should assume from the root, but not include the
root prefix (ie, usr/bin/bash not /usr/bin/bash), because install may have
the hard drive mounted from a floppy/ramdisk.

package descriptions:
---------------------
Disk contents are logged in /install/disks/diskXN.

z?fix.tgz files:
----------------
Although the best way to fix a problem with a package is to extract a new package,
in the real world of limited bandwidth, it easier to post just the fixes. In
"z?fix.tgz" files, the "z" prefix plus being on the last disk of a series
ensures that other packages get installed first, and the "?" is the name of the 
disk series.   Typically, fixed or new files are put here, as well as the shell
script "/install/doinst.sh" as described above.

sysbuild:
---------
The bootdisk loads a small file system into a ramdisk.  The "/usr/bin/sysbuild"
utility can be used to put this filesystem at the correct starting address.  The 
relevant arguments are: getroot, putroot, getflop,  and putflop.  The last
two are for putting the root.fs onto a temporary floppy for mounting.  To build
your own ramdisk bootimage, just recompile the kernel with RAMDISK defined in the 
makefile, after setting kernel/blk_dev/ramdisk with rdev, if necessary.
Also in sysbuild are some utilities used to build/maintain the SLS master 
distribution.  They are not supported or documented, but feel free to peruse them.
