.\" $XConsortium: RELNOTES.ms,v 1.23 91/08/29 09:45:58 rws Exp $
.\" use ms macros
.if n \{
.nr LL 79n
.ll 79n
.nr FL 79n
.nr LT 79n
.\}
.nr PS 11
.de Ds
.LP
.in +.5i
.nf
..
.de De
.fi
.in -.5i
..
.de Ip
.LP
.ne 3
\fB\\$1\fP
.br
.IP
..
.ds LF \fBX Window System Release Notes\fP
.ds RF \fBX Version 11, Release 5\fP
.ce 4
\s+2\fBX Window System, Version 11, Release 5\fP\s-2

\s+1\fBRelease Notes\fP\s-1

\fIMIT X Consortium staff\fP

MIT Laboratory for Computer Science
.sp 3
Copyright \(co\ 1991 by the Massachusetts Institute of Technology.
.nr PS 9
.LP
Permission to use, copy, modify, and distribute this document 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice
appear in all copies, and that the name of MIT not be used 
in advertising or publicity pertaining to this document without 
specific, written prior permission.
MIT makes no representations about the suitability of this
document for any purpose.
It is provided ``as is'' without express or implied warranty.
.nr PS 11
.LP
\fIX Window System\fP is a trademark of MIT.
.sp 3
This document describes how to build, install, and get started with
Release 5 of the X Window System from MIT
and gives a brief overview of the contents of the release.
.NH 1
For the Impatient Explorer
.LP
For those of you who will try to build the distribution without reading the
entire release notes first, here is a quick summary of what to do.
.LP
If you want to build with \fBgcc\fP, edit \fBmit/config/site.def\fP by
uncommenting the \fBHasGcc\fP line.
.LP
If you want to install into somewhere other than \fB/usr/bin/X11\fP,
\fB/usr/include/X11\fP, etc., edit \fBmit/config/site.def\fP by uncommenting
the \fBProjectRoot\fP lines and changing "/usr/X11R5" to whatever directory
you want to install into.  (Do \fInot\fP use \fBDESTDIR\fP.)
.LP
Check the appropriate \fBmit/config/\fP\fIvendor\fP\fB.cf\fP file to
make sure that \fBOSMajorVersion\fP and \fBOSMinorVersion\fP are
set correctly (change them if necessary).
.LP
Find the \fBBootstrapCFlags\fP line, if any, in the \fIvendor\fP\fB.cf\fP file.
If there isn't one, \fBcd\fP to the \fBmit\fP directory and type:
.Ds
make World >& world.log
.De
.LP
If there is a \fBBootstrapCFlags\fP, take its value\s-2\u1\d\s0
.FS
\s+2\&1.  If you are using the \fBx386.cf\fP file, you will have to compute the
correct value.\s-2
.FE
and type:
.Ds
make World BOOTSTRAPCFLAGS="\fIvalue\fP" >& world.log
.De
.LP
Do not call the output file ``make.log'', or it will be deleted.
If the build is successful, you can install most of it with:
.Ds
make install >& install.log
.De
.LP
You can install man pages with:
.Ds
make install.man >& man.log
.De
.LP
You can install lint libraries (if desired) with:
.Ds
make install.ln >& lintlib.log
.De
.LP
If things fail, read the rest of the release notes.
.NH 1
Brief Overview of the Distribution
.LP
(If you want, you can skip to the next chapter first, and get your
build started.  While it is compiling you will have plenty of time
to read the rest of the release notes.)
.LP
There are two parts to the Release 5 distribution: MIT software and
documentation, and user-contributed software and documentation.
The MIT part contains:
.Ip "X Consortium Standards"
The MIT X Consortium produces standards:  documents which define
network protocols, programming interfaces, and other aspects of
the X environment.  See the \fBXStandards\fP man page for a
list of standards.  See the \fBXConsortium\fP man page for
information about the X Consortium.
.Ip "Sample Implementations"
For most of our standards, we provide \fIsample\fP implementations
to demonstrate proof of concept.  These are not \fIreference\fP
implementations; the written specifications define the standards.
.Ip "Fonts"
A collection of bitmap and outline fonts are included in the
distribution, contributed by various individuals and companies.
.Ip "Utility Libraries"
A number of libraries, such as the \fIAthena Widget Set\fP, are
included.  These are not standards, but are used in building
MIT applications and may be useful in building other applications.
.Ip "Sample Programs"
We also provide a number of application programs.
A few of these programs, such as \fBxdm\fP,
should be considered essential in almost all environments.
The rest of the applications carry no special status, they
are simply programs that have been developed and/or maintained
by MIT X Consortium staff.
In some cases, you will find better substitutes for these
programs in the user-contributed software.
.LP
The user-contributed part contains whatever people contribute.
You find a variety of software and documentation here:
application programs, demos, examples, libraries, Asian input methods,
X server extensions, etc.
.NH 2
Structure of the MIT Sources
.LP
All of the MIT sources are under a single directory, \fBmit\fP.
Sources are organized into the following subdirectories:
.Ip "clients"
This directory contains most of the sample applications.
See the program man pages for details.
.Ip "config"
This directory contains configuration files and the \fBimake\fP
program used to build the release.
Details are covered in other sections below.
.Ip "demos"
This directory contains a small collection of graphics demonstration programs,
a few utility/test programs, and some performance demonstration programs.
These are by no means the ``best'' demo programs around, they just happen
to be ones we try to maintain.
.Ip "doc"
This directory contains \fBtroff\fP sources to X Consortium standards,
server internals documentation,
documentation for various utility libraries,
some useful tutorial material.
.Ip "extensions"
This directory contains implementations of X server extensions,
both the server internals and the application programming libraries,
and some test programs.  Of particular note here, new in Release 5,
is PEX, the PHIGS Extension to X used for 3D graphics, and the PHIGS
programming library which interfaces to the PEX protocol.
.Ip "fonts"
This directory contains bitmap fonts in source form,
some outline fonts,
a sample font server,
a utility font library used by the X server and font server,
a client font library for interacting with the font server,
and programs for building fonts and querying the font server.
.Ip "hardcopy"
This directory contains pre-generated PostScript files for
the client man pages and for
most of the documentation found in the \fBdoc\fP directory.
The files are compressed with \fBcompress\fP to save disk space.
If you do not have \fBcompress\fP on your system, you will
find a version in the \fBmit/util/compress\fP directory.
.Ip "include"
This directory contains various library-independent C header files
and a collection of bitmap files.
.Ip "lib"
This directory contains programming libraries and support files.
Of note are Xlib (the lowest-level C programming interface to X),
Xt (the X Toolkit Intrinsics), Xmu (an eclectic set of utility functions),
Xaw (the Athena Widget Set), and CLX (a Common Lisp interface to X).
.Ip "man"
This directory contains a few top-level man pages about the release
(general information, server access control mechanisms, the X Consortium,
and X Consortium standards), and man pages for some of the programming
libraries.
.Ip "rgb"
This directory contains a program to generate the color database used
by the X server and sample databases.
.Ip "server"
This directory contains the X server sources, both device-independent
(\fBdix\fP) and device-dependent (\fBddx\fP).  In this release, there
is support for building the following servers:
.Ds
DECstation 2100/3100 monochrome and color displays
DECstation 5000 CX and MX displays
IBM RS/6000 skyway adapter
Apple Macintosh monochrome and color displays
MIPS monochrome and color displays
OMRON LUNA monochrome displays (color displays operate in monochrome)
Tektronix 4319 color display
VAXstation QVSS and QDSS displays
Sun monochrome and 8-bit color displays (with GX support)
Various VGA displays under System V/386
.De
.IP
If your favorite hardware is not listed above, please do not blame us at MIT,
we ship what Consortium members provide.  Only in a few cases do we try to
maintain device-specific software for our own development needs.
.\".Ip "util"
.LP
\fButil\fP
.br
.IP
This directory contains miscellaneous utility programs and shell scripts
used to build, maintain, and install the release.
.NH 1
Building the Release
.LP
The core distribution (code under the \fBmit\fP directory) has been
built and tested at MIT on the following systems:
.Ds
AIX 3.1.5, on IBM RS/6000
Apollo SR10.3 (very minimal testing, bsd4.3 only)
AT&T Unix System V Release 4 V2, on AT&T WGS6386
A/UX 2.0.1
HP-UX 7.0, on HP9000/s300
IRIX 4.0
Mach 2.5 Version 1.13, on OMRON Luna 88k
NEWS-OS 4.1, on Sony NWS-1850
NEWS-OS 5.0U, on Sony NWS-3710
SunOS 4.1.1, on Sun 3, Sparc 1, and Sparc 2
Ultrix-32 4.2, VAX and RISC
UNICOS 5.1
UTek 4.0
VAX 4.3bsd (with unknown local changes)
.De
.LP
In somes cases, we have not used the most recent version of the operating
system (sorry).  Support for earlier versions of the operating systems
listed is not claimed, and not guaranteed.
.LP
In addition to the systems above, support has been provided by vendors for:
.Ds
AIX 2.2 and AOS 4.3, on IBM RT
AIX 1.2.1, on IBM PS/2
ConvexOS V9.0
DG/UX 4.32
INTERACTIVE UNIX Version 2.2.1
Mach 2.5 Version 1.40, on OMRON Luna 68k
Motorola R32V2/R3V6.2 and R40V1
RISCOS 4.50
UNIOS-B 4.3BSD UNIX: 2.00
Unix System V/386 Release 3.2, on ESIX, SCO, and AT&T (``work in progress'')
Unix System V/386 Release 4.0, on DELL
.De
.NH 2
Unpacking the Distribution
.LP
The distribution normally comes as multiple tar files, either on
tape or across a network.  Create a directory to hold the distribution,
\fBcd\fP to it, and untar everything from that directory.  For example:
.Ds
mkdir \fIsourcedir\fP
cd \fIsourcedir\fP
tar xfp \fItar-file-or-tape-device\fP
.De
.LP
If you have obtained compressed and split tar files over the network,
then the sequence for each part of the \fBmit\fP directory might be:
.Ds
cd \fIftp-dir\fP/mit-\fIN\fP
cat mit-\fIN\fP.?? | uncompress | (cd \fIsourcedir\fP; tar xfp -)
.De
.LP
The sequence for each part of the \fBcontrib\fP directory might be:
.Ds
cd \fIftp-dir\fP/contrib-\fIN\fP
cat contrib-\fIN\fP.?? | uncompress | (cd \fIsourcedir\fP; tar xfp -)
.De
.LP
The \fIsourcedir\fP directory you choose can be anywhere in any of your
filesystems that is
convenient to you.  After extracting the release, you should end up with an
\fBmit\fP subdirectory, and a \fBcontrib\fP subdirectory if you unpack
user-contributed software.  You will need about 100 megabytes of disk space
to unpack the \fBmit\fP directory contents; building it will of course
require more disk space.
.NH 2
Symbolic Link Trees
.LP
If you expect to build the distribution on more than one machine using
a shared source tree, or you just want to keep the source tree pure,
you may want to use the shell script \fBmit/util/scripts/lndir.sh\fP to create
a symbolic link tree on each build machine.  This is fairly expensive in
disk space, however.  To do this, create a directory for the build, \fBcd\fP
to it, and type:
.Ds
\fIsourcedir\fP/mit/util/scripts/lndir.sh \fIsourcedir\fP
.De
.LP
where \fIsourcedir\fP is the pathname of the
directory where you stored the sources.  All of the build instructions
given below should then be done in the build directory on each machine,
rather than in the source directory.
.LP
The shell script is reasonably portable but quite slow to execute.
If you want you can instead try compiling a similar C program, but
it is slightly tricky to do before the distribution is built;
\fBcd\fP to the directory \fBmit/util/progs\fP and try typing:
.Ds
ln -s ../../include X11
cc -o lndir -I. lndir.c
.De
.LP
If it compiles and links, it will probably work; otherwise you can
try typing:
.Ds
cc -o lndir -I. -DNOSTDHDRS lndir.c
.De
.LP
If it still fails, use the shell script.
.NH 2
Setting Configuration Parameters
.LP
You will notice that few if any of the subdirectories under \fBmit\fP
contain a \fBMakefile\fP, but they do contain an \fBImakefile\fP.
The \fBImakefile\fP is a template file used to create a \fBMakefile\fP
containing build rules and variables appropriate for the target machine.
The \fBMakefile\fP is generated by the program \fBimake\fP.
Most of the configuration work prior to building the release is to
set parameters so that \fBimake\fP will generate correct files.
.LP
The directory \fBmit/config\fP contains configuration
files that control how the distribution is built.  On systems directly
supported by this distribution, only minimal editing of these files
should be necessary.  If your system is not supported by the
distribution but conforms to ANSI C and POSIX.1 and has socket-style
networking, then you should be able to build a new configuration
file relatively easily.  Otherwise, edits to many files throughout
the system may be necessary.  We only deal with minor editing for
supported systems here.
.LP
The main files to be concerned with in the \fBmit/config\fP directory
are \fBsite.def\fP and one of the \fIvendor\fP\fB.cf\fP files.  The
\fBsite.def\fP file should be used for most site-specific configuration
customizations.  The \fB.cf\fP file should normally only need to be edited
if you are using a different release of the operating system.
.NH 3
The vendor.cf File
.LP
Find the appropriate \fB.cf\fP file from this table:
.Ds
.ta 2.5i
AIX	ibm.cf
AOS	ibm.cf
Apollo	apollo.cf
AT&T Unix SVR4	att.cf
A/UX	macII.cf
BSD	bsd.cf
ConvexOS	convex.cf
DG/UX	DGUX.cf
HP-UX	hp.cf
INTERACTIVE	x386.cf
IRIX	sgi.cf
Mach (Luna)	luna.cf
Motorola	moto.cf
NEWS-OS	sony.cf
RISCOS	Mips.cf
SunOS	sun.cf
Ultrix	ultrix.cf
UNICOS	cray.cf
UTek	pegasus.cf
UNIOS-B	luna.cf
Unix System V/386	x386.cf
.De
.LP
Look through this file, and check the \fBOSMajorVersion\fP and
\fBOSMinorVersion\fP values.  The numbers have been preset
to what was tested at MIT or what was supplied by the vendor.
If the version numbers match the operating system you are
currently running, all is well.  If they do not, you
will need to edit to file to make them correct.  In a few cases
(specifically
changing UNICOS from 5.1 to 6.0) there should not be a problem
in moving the version numbers forward to a newer release.
However, if you are moving the version numbers backwards,
or moving forward to a version that hasn't been pre-tested,
you may have problems, and you have have to edit other parts
of the file (and possibly other files) to get things to work.
.LP
You can browse through the rest of the items in the \fB.cf\fP file,
but most of them you should not need to edit.
.NH 3
The site.def File
.LP
There are two main variables to set in the \fBsite.def\fP file:
\fBHasGcc\fP and \fBProjectRoot\fP.  If you are going to compile
the distribution with \fBgcc\fP, find the line that looks like
.Ds
/* #define HasGcc YES */
.De
.LP
and remove the comment markers, turning it into
.Ds
#define HasGcc YES
.De
.LP
If you are sharing a single \fBsite.def\fP across multiple systems,
you can do something more complicated.
For example, if you only want to use \fBgcc\fP on a Sun 3 (but not
on Sparcs) you might use this:
.Ds
#ifdef SunArchitecture
#define HasGcc mc68000
#endif
.De
.LP
The most common error when using \fBgcc\fP is to fail to run
the \fBfixincludes\fP script (from the \fBgcc\fP distribution)
when installing \fBgcc\fP.  Make sure you have done this before
compiling the release.  Another common error is likely to be
using \fBgcc\fP ANSI C include files when the vendor operating system
supplies correct ones.  The \fBgcc\fP include files \fBassert.h\fP,
\fBlimits.h\fP, and \fBstddef.h\fP are prime candidates for not installing.
.LP
The \fBProjectRoot\fP variable controls where the software will
eventually be installed.  The default as distributed for most
systems is to install into ``system'' directories: \fB/usr/bin/X11\fP,
\fB/usr/include/X11\fP, \fB/usr/lib\fP, and \fB/usr/man\fP (this
is the behaviour when \fBProjectRoot\fP is not defined).
If you prefer to install into alternate directories, the simplest
thing to do is to set \fBProjectRoot\fP.  Find the four \fBProjectRoot\fP
lines in the \fBsite.def\fP file, and again remove the ``/*'' and ``*/''
comment markers that surround them.  You will see a default choice
for \fBProjectRoot\fP of \fB/usr/X11R5\fP; if you don't like that one,
replace it with another.  Assuming you have set the variable to some
value /\fIpath\fP, files will be installed into
/\fIpath\fP/bin, /\fIpath\fP/include/X11, /\fIpath\fP/lib, and
/\fIpath\fP/man.
.LP
Note that in a few cases (\fBibm.cf\fP and \fBx386.cf\fP) the
vendor-supplied \fB.cf\fP file supplies a \fBProjectRoot\fP by default.
If you want to accept this one, do not uncomment the one in \fBsite.def\fP;
otherwise the one you place in \fBsite.def\fP will
override the default setting.
.LP
The directories where the software will be installed are compiled in
to various programs and files during the build process, so it is
important that you get the configuration correct at the outset.
If you change your mind later, you will want to do a ``make Everything''
to rebuild correctly.
.LP
Notice that the \fBsite.def\fP file was two parts, one protected with
``#ifdef BeforeVendorCF'' and one with ``#ifdef AfterVendorCF''.
The file is actually processed twice, once before the \fB.cf\fP file
and once after.  About the only thing you need to set in the ``before''
section is \fBHasGcc\fP; just about everything else can be set in the
``after'' section.
.LP
There are a large number of parameters that you can modify to change
what gets built and how it gets built.  An exhaustive list and explanation
will not be given here; you can browse through \fBmit/config/README\fP
to see a list of parameters.  However, here are some notable parameters
that you can set in the ``after'' section:
.Ip "BuildXsi\fP and \fBBuildXimp"
New in this release, Xlib contains support for internationalized
input methods, using library- or network-based implementation methods.
The implementation details internal to Xlib can vary considerably
depending on the types of input methods supported.  In this release,
two different implementations are supported, named \fBXsi\fP and
\fBXimp\fP.  As distributed, the default on all systems except Sony
is \fBXsi\fP.  If you want to use \fBXimp\fP instead, add this:
.Ds
#define BuildXimp YES
.De
.Ip "BuildServer"
This controls whether or not an X server is built.  If the variable
is not set to \fBNO\fP in the \fB.cf\fP file, then the default is
to build a server.  If you want to disable the server, add this:
.Ds
#define BuildServer NO
.De
.Ip "BuildFonts"
Compiled fonts take up a lot of disk space.  In this release, the
compiled form (called ``pcf'') can be shared across all machines
of all architectures, so you may only want to build the fonts on
one machine.  To disable font building, add this:
.Ds
#define BuildFonts NO
.De
.Ip "BuildPex"
PEX is an X extension supporting 3-D graphics and the PHIGS programming
interface.  The PEX sources are known to cause some compilers to
exceed their static symbol table sizes.  If this happens to you, you
can disable PEX by adding this:
.Ds
#define BuildPex NO
.De
.Ip "ManSuffix"
User program man pages are installed by default in subdirectory
``mann'' with suffix ``.n''.
You can change this to ``man1'' and ``.1'', for example, by adding this:
.Ds
#define ManSuffix 1
.De
.Ip "InstallLibManPages"
By default, the programming library man pages (Xlib, Xt, various
extensions) are installed along with all of the other man pages.
The library pages constitute a considerable number of files.
If you do not expect to be programming with X, or prefer
using other forms of documentation, you can disable installation of
the library pages by adding this:
.Ds
#define InstallLibManPages NO
.De
.Ip "InstallXdmConfig\fP and \fBInstallXinitConfig"
The \fBxdm\fP and \fBxinit\fP programs are the normal ways to run X servers.
By default, the configuration files for these programs are not installed,
to avoid inadvertently destroying existing configuration files.
If you are not yet using \fBxdm\fP or \fBxinit\fP, or will be installing
into a new destination, or do not wish to retain your old configuration
files, add these:
.Ds
#define InstallXdmConfig YES
#define InstallXinitConfig YES
.De
.Ip "XdmServersType"
Some of the \fBxdm\fP config files are generated based on configuration
parameters.  One of the files controls whether an X server is started
by default.  By default the choice is made based on whether an X server
is built as part of this distribution (the \fBBuildServer\fP parameter).
If you are not building a server, but you will be running a product server
on the workstation under \fBxdm\fP, you should add this:
.Ds
#define XdmServersType ws
.De
.Ip "HasXdmAuth"
This release supports a DES-based form of authorization called
XDM-AUTHORIZATION-1.  The source file \fBmit/lib/Xdmcp/Wraphelp.c\fP,
which is necessary for this to compile, might not be included in your
distribution due to possible export regulations; if it is not
included and you are a US citizen, you should be able to obtain the
file over the network.  To enable building of this mechanism, add this:
.Ds
#define HasXdmAuth YES
.De
.Ip "InstallFSConfig"
New to this release is a network font server, \fBfs\fP.  By default,
the configuration files for the font server are not installed.  To
have them installed, add this:
.Ds
#define InstallFSConfig YES
.De
.Ip "MotifBC"
If you want to use the Release 5 Xlib and Xt with Motif 1.1, you
will need to enable a backward compatibility flag, by adding this:
.Ds
#define MotifBC YES
.De
.NH 2
System Pitfalls
.LP
On a few systems, you are likely to have build problems unless you make some
minor changes to the system.  Naturally, you should exercise caution
before making changes to system files, but these are our recommendations
based on our experience.
.LP
On VAX Ultrix systems, you may find that \fB<stdlib.h>\fP
contains declarations of \fBmalloc\fP, \fBcalloc\fP, and \fBrealloc\fP
with a return value of ``void *''.  You may find this causes problems
when compiling with a non-ANSI-C compiler, in which case a workaround
is to change the return values to ``char\&*'' in the ``#else'' section.
.LP
Ultrix may not provide \fB<locale.h>\fP unless you load the
Internationalization subset.  You will need this file to compile
the distribution (or else you will need to reset a configuration
parameter, see below).
.LP
On SunOS systems, you may find that statically linking (when debugging)
against both Xlib and the \fBlibc\fP will result in unresolved symbols to
dynamic linker functions, because Xlib contains calls to \fBwcstombs\fP.
Either link dynamically against \fBlibc\fP, or compile and link
the stub routines in \fBmit/util/misc/dlsym.c\fP.
.LP
On Sun 3s, the default is to compile library files with no special floating
point assumptions.  If all of your Sun 3s have floating point hardware, you
may want to change this, for better performance of Xlib color functions.
For example, in the ``after'' section of your \fBsite.def\fP file,
you might add:
.Ds
#if defined(SunArchitecture) && defined(mc68000)
#undef LibraryCCOptions
#define SharedLibraryCCOptions -f68881 -pipe
#endif
.De
.LP
On AOS, you may find that \fB<stdarg.h>\fP is missing.
In that case, you should be able to copy \fBmit/util/misc/rt.stdarg.h\fP
to create the file.
.LP
On some System V/386 systems, you may find when using \fBgcc\fP in
ANSI mode that there are inconsistent declarations between \fB<memory.h>\fP
and \fB<string.h>\fP.  In that case, you may find it convenient to
remove \fB<memory.h>\fP and make it a link to \fB<string.h>\fP.
.LP
On some System V/386 systems, you may need to build and install a
\fBdbm\fP library before building the X server and RGB database.
One can be found in \fBcontrib/util/sdbm\fP.
.NH 3
Internationalization
.LP
This release has support for internationalization, based on the
ANSI C and POSIX locale model.  On some systems, you may discover
that while the locale interface is supported, only the ``C'' locale
is actually provided in the base operating system.  If you have
such a system, and would like to experiment with a broader set
of locales, the Xlib implementation contains support you can use,
although use of this override has not really be tested.  You
need to add the following defines to the \fBStandardDefines\fP
parameter:
.Ds
-DX_WCHAR -DX_LOCALE
.De
.LP
In most cases you will have to directly edit the \fB.cf\fP file to do this,
or else you will have to know what the rest of the values are supposed to
be, and add this to \fBsite.def\fP:
.Ds
#undef StandardDefines
#define StandardDefines \fIprevious-values\fP -DX_WCHAR -DX_LOCALE
.De
.LP
It is also possible to directly edit the file \fBmit/include/Xosdefs.h\fP,
but this is not recommended.
.LP
With this setup, you will have to be careful that the system's
declaration of \fBwchar_t\fP (in \fB<stddef.h>\fP) never gets used;
this might be tricky.
.NH 2
Typing ``make World''
.LP
One more piece of information is required before building, at least
on some systems: bootstrap flags.  Look in your \fB.cf\fP file for
a line of the form
.Ds
#define BootstrapCFlags \fIvalue\fP
.De
.LP
If there isn't one things are simple, otherwise things
are only slightly more complicated.  If there is more than one
(for example, in \fBibm.cf\fP, \fBmoto.cf\fP, and \fBsony.cf\fP),
then you need to select the right one; it should be pretty obvious by the
grouping according to operating system type.  Note that on
A/UX you only need this value if you are using \fBgcc\fP,
and that on a Sun you only need this value if you are using
an earlier version of the operating system.
.LP
If you are using \fBx386.cf\fP, you will have to ``compute'' the value
from the information given in the file.  You may also need to do other
preparatory work; please read \fBmit/server/ddx/x386/README\fP.
.LP
If no value is required on your system, you can \fBcd\fP to the \fBmit\fP
directory and start the build with:
.Ds
make World >& world.log
.De
.LP
If a value is required, start the build with:
.Ds
make World BOOTSTRAPCFLAGS="\fIvalue\fP" >& world.log
.De
.LP
You can call the output file something other than ``world.log'', but
do not call it ``make.log'' because files with this name are
automatically deleted during the ``cleaning'' stage of the build.
.LP
Because the build can take several hours to complete, you will probably
want to run it in the background, and keep a watch on the output.
For example:
.Ds
make World >& world.log &
tail -f world.log
.De
.LP
If something goes wrong, the easiest thing is to just start over
(typing ``make World'' again) once you have corrected the problem.
It is possible that a failure will corrupt the top-level \fBMakefile\fP.
If that happens, simply delete the file and recreate a workable
substitute with:
.Ds
cp Makefile.ini Makefile
.De
.LP
When the build completes, examine the \fBworld.log\fP file for
errors.  If you search for `:' (colon) characters, and skip
the obvious compile lines, it is usually pretty easy to spot
any errors.\s-2\u2\d\s0
.FS
\s+2\&2.  Searching for colon does not work particularly well on the RS/6000
because it appears in command lines when building shared libraries.
Try searching for colon followed by space.\s-2
.FE
.NH 1
Installing the Release
.LP
Although it is possible to test the release before installing it,
it is a lot easier to test after it has been installed.
If everything is built successfully, you can install the software
by typing the following as root, from the \fBmit\fP directory:
.Ds
make install >& install.log
.De
.LP
Again, you might want to run this in the background and use \fBtail\fP
to watch the progress.
.LP
You can install the man pages by typing the following as root,
from the \fBmit\fP directory:
.Ds
make install.man >& man.log
.De
.LP
You can install lint libraries (useful if your systems does does not have
an ANSI C compiler) by typing the following as root,
from the \fBmit\fP directory:
.Ds
make install.ln >& lintlib.log
.De
.NH 2
Setting Up xterm
.LP
If your \fB/etc/termcap\fP and \fB/usr/lib/terminfo\fP databases do not have 
correct entries for \fBxterm\fP, sample entries are provided in the directory
\fBmit/clients/xterm/\fP.  System V users may need to compile and install the 
\fBterminfo\fP entry with the \fBtic\fP utility.
.LP
Since each \fBxterm\fP will need a separate pseudoterminal,
you need a reasonable number of them for normal execution.
You probably will want at least 32 on a small, multiuser system.
On most systems, each pty has two devices, a master and a slave,
which are usually named /dev/tty[pqrstu][0-f] and /dev/pty[pqrstu][0-f].
If you don't have at least the ``p'' and ``q'' sets configured
(try typing ``ls /dev/?ty??''), you should have your system administrator
add them.  This is commonly done by running the \fBMAKEDEV\fP script in
the \fB/dev\fP directory with appropriate arguments.
.NH 2
Starting Servers at System Boot
.LP
The \fBxdm\fP program is designed to be run automatically at system startup.
Please read the \fBxdm\fP man page for details on setting up configuration
files; reasonable sample files are in \fBmit/clients/xdm/config\fP.
If your system uses an \fB/etc/rc\fP file at boot time, you can
usually enable \fBxdm\fP by placing the following at or near the end of
the file:
.Ds
if [ -f /usr/bin/X11/xdm ]; then
        /usr/bin/X11/xdm; echo -n ' xdm'
fi
.De
.LP
The example here uses \fB/usr/bin/X11\fP, but if you have installed into
a different directory (for example by setting \fBProjectRoot\fP) then you
need to substitute the correct directory.
.LP
If you are going to use the font server, you can also start it at boot time
by adding this:
.Ds
if [ -f /usr/bin/X11/fs ]; then
        /usr/bin/X11/fs &; echo -n ' fs'
fi
.De
.LP
If you are unsure about how system boot works, or if your system does
not use \fB/etc/rc\fP, consult your system administrator for help.
.NH 1
Rebuilding the Release
.LP
You shouldn't need this right away, but eventually you are probably
going to make changes to the sources, for example by applying
public patches distributed by MIT.  If only C source files are
changed, you should be able to rebuild just by going to the \fBmit\fP
directory and typing:
.Ds
make >& make.log
.De
.LP
If configuration files are changed, the safest thing to do is type:
.Ds
make Everything >& every.log
.De
.LP
``Everything'' is similar to ``World'' in that it rebuilds every
\fBMakefile\fP, but unlike ``World'' it does not delete the
existing objects, libraries, and executables, and only rebuilds
what is out of date.
.LP
Note that in both kinds of rebuilds you do not need to supply the
\fBBootstrapCFlags\fP value any more, the information is already recorded.
.NH 1
Building Contributed Software
.LP
The software in \fBcontrib\fP is not set up to have everything
built automatically.  It is assumed that you will build individual
pieces as you find the desire, time, and/or disk space.  You need
to have the MIT software built and installed before building the
contributed software.  To build a program or library in \fBcontrib\fP,
look in its directory for any special build instructions (for example,
a \fBREADME\fP file).  If there are none, and there is an \fBImakefile\fP,
\fBcd\fP to the directory and type:
.Ds
xmkmf -a
make >& make.log
.De
.LP
This will build a \fBMakefile\fP in the directory and all subdirectories,
and then build the software.  If the build is successful, you should be
able to install it using the same commands used for the \fBmit\fP
software:
.Ds
make install >& install.log
make install.man >& man.log
.De
.NH 1
Filing Bug Reports
.LP
If you find a reproducible bug in software in the \fBmit\fP directory,
or find bugs in the \fBmit\fP documentation, please send a bug report
to MIT using the form in the file \fBmit/bug-report\fP and the
destination address:
.Ds
xbugs@expo.lcs.mit.edu
.De
.LP
Please try to provide all of the information requested on the form if it is
applicable; the little extra time you spend on the report will make it
much easier for us to reproduce, find, and fix the bug.  Receipt of bug
reports is generally acknowledged, but sometimes it can be delayed by a
few weeks.
.LP
Bugs in \fBcontrib\fP software should not be reported to MIT.  Consult
the documentation for the individual software to see where (if anywhere)
to report the bug.
.NH 1
Public Fixes
.LP
We occasionally put out patches to the MIT software, to fix any
serious problems that are discovered.  Such fixes (if any) can be found
on \fBexport.lcs.mit.edu\fP, in the directory \fBpub/R5/fixes\fP,
using anonymous \fBftp\fP.  Fixes are applied using the \fBpatch\fP
program; a copy of it is included in the directory \fBmit/util/patch\fP.
.LP
For those without \fBftp\fP access, individual fixes can be obtained by
electronic mail by sending a message to
.Ds
xstuff@expo.lcs.mit.edu
.De
.LP
(Note that the host here is ``expo'', not ``export''.)  In the usual case,
the message should have a subject line and no body, or a single-line body and
no subject, in either case the line looking like:
.Ds
send fixes \fInumber\fP
.De
.LP
where \fInumber\fP is a decimal number, starting from one.  To get a
summary of available fixes, make the line:
.Ds
index fixes
.De
.LP
If you need help, make the line:
.Ds
help
.De
.LP
Some mailers produce mail headers that are unusable for extracting return
addresses.  If you use such a mailer, you won't get any response.  If you
happen to know an explicit return path, you can include include one in the
body of your message, and the daemon will use it.  For example:
.Ds
path \fIuser\fP%\fIhost\fP.bitnet@mitvma.mit.edu
    or
path \fIhost1\fP!\fIhost2\fP!\fIuser\fP@uunet.uu.net
.De
.NH 1
Configuring for a New Architecture
.LP
Here is a very brief overview of the files that \fBimake\fP reads.
All the files are in the \fBmit/config\fP directory, except
for the \fBImakefile\fP in the directory for which the \fBMakefile\fP
is being created.  The processing order is:
.Ds
.ta 2.5i
Imake.tmpl	variables not related specifically to X
    site.def	site-specific BeforeVendorCF part
    *.cf	machine-specific
        *Lib.rules	shared library rules
    site.def	site-specific AfterVendorCF part
    Project.tmpl	X-specific variables
        *Lib.tmpl	shared library variables
    Imake.rules		rules
Imakefile		specific to the program or library
    Library.tmpl	library rules
    Server.tmpl	server rules
.De
The indentation levels indicate what files include other files.
.NH 2
Imake.tmpl
.LP
The first part of \fBImake.tmpl\fP determines which \fB.cf\fP
file to include.  If your \fBcpp\fP defines a unique symbol,
that should be used to select the file.
Otherwise, you should place a -D symbol definition
in \fBBootstrapCFlags\fP in your \fB.cf\fP file and use that.
The canonical code to add to \fBImake.tmpl\fP is:
.Ds
#ifdef \fIsymbol\fP
#define MacroIncludeFile <\fIsymbol\fP.cf>
#define MacroFile \fIsymbol\fP.cf
#undef \fIsymbol\fP
#define \fISymbol\fPArchitecture
#endif /* \fIsymbol\fP */
.De
.NH 2
imakemdep.h
.LP
You also need to edit the file \fBimakemdep.h\fP.  There are
three parts to this file.  The first contains defines (beyond
\fBBootstrapCFlags\fP) or compiler options that are required to get
\fBimake\fP itself built the first time.
.LP
The next section is for \fBimake\fP itself.
There is a hook in case your \fBcpp\fP collapses tabs down to single spaces.
There is also a way to override the \fBcpp\fP to use.
Finally, add specific defines to pass to \fBcpp\fP
when processing configuration files.
.LP
The last section is for \fBmakedepend\fP, to tell it about
predefined symbols that will be used to control inclusion of header files.
.LP
.NH 2
vendor.cf
.LP
Most of the rest of your vendor-specific configuration information goes here.
We won't try to tell you everything you need; study the other \fB.cf\fP
files and copy from systems that are similar.  One good rule to follow
is to not define anything that will get the correct default value from
somewhere else; this will make it easier to see what is special, and will
make it easier for sites to customize in their \fBsite.def\fP.
.LP
If you have shared libraries, the convention is to place the
configuration rules and standard parameters in a file named
\fIos\fP\fBLib.rules\fP, and to place version number parameters
and \fBmake\fP variables in a file named \fIos\fP\fBLib.tmpl\fP.
Look at the existing files and mimic them.
.NH 2
Other Files
.LP
Unfortunately, for a new system there are a potentially large number of files
that you may have to modify.  Only the most prominent ones are listed here.
.Ds
mit/include/Xfuncs.h
mit/include/Xmd.h
mit/include/Xos.h
mit/include/Xosdefs.h
mit/clients/xload/get_load.c
mit/clients/xman/vendor.c
mit/clients/xman/vendor.h
mit/clients/xterm/main.c
mit/lib/X/Xlibnet.h
mit/server/include/servermd.h
.De
.NH 1
Writing Portable Code
.LP
In this section we give a brief introduction to using various header
files to aid in writing portable code.
.NH 2
<X11/Xosdefs.h>
.LP
The file \fB<X11/Xosdefs.h>\fP defines symbols that describe the system
environment for ANSI C and POSIX.  We likely will extend it to other
standards in the future.  We have found these symbols useful in
writing portable code, and hope that other writers of X software will
use them as well.
This file is not part of any X Consortium standard,
it is simply part of our software distribution.
.LP
\fB<X11/Xosdefs.h>\fP can be included directly by a file, or it
will be automatically included when you include \fB<X11/Xos.h>\fP.
.LP
The symbols in \fB<X11/Xosdefs.h>\fP tell when you can, for example, do
.Ds
#include <stdlib.h>
.De
.LP
without getting a ``no such header file'' error from the compiler.
If the system provides a declaration for a function or value for a
constant, it is important to use the system's definition rather than
providing your own, particularly because you might not use function
prototypes and the system might, or vice versa.
.LP
\fB<X11/Xosdefs.h>\fP currently controls two symbols:
\fBX_NOT_STDC_ENV\fP and \fBX_NOT_POSIX\fP.
.LP
\fBX_NOT_STDC_ENV\fP means the system does not have ANSI C header files.
Thus, for example, if \fBX_NOT_STDC_ENV\fP is not defined, it is safe to
include \fB<stdlib.h>\fP.  Do not confuse this symbol with \fB_\^_STDC_\^_\fP,
which says whether the compiler itself supports ANSI C semantics.
\fBX_NOT_STDC_ENV\fP is independent, and tells what header files it
is safe to include.
.LP
Lack of the symbol \fBX_NOT_STDC_ENV\fP does \fInot\fP mean that the
system has \fB<stdarg.h>\fP.  This header file is part of ANSI C,
but we have found it more useful to check for it separately because
many systems have all the ANSI C files we need except this one.
\fB_\^_STDC_\^_\fP is used to control inclusion of this file.
.LP
An example of using \fBX_NOT_STDC_ENV\fP might be to know when the system
declares \fBgetenv\fP:
.Ds
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *getenv();
#endif
.De
.LP
We usually put the standard case first in our code, using ``#ifndef''.
.LP
\fBX_NOT_POSIX\fP means the system does not have POSIX.1 header files.
Lack of this symbol does \fInot\fP mean that the POSIX environment is
the default.  You may still have to define \fB_POSIX_SOURCE\fP before
including the header file to get POSIX definitions.\s-2\u3\d\s0
.FS
\s+2\&3.  We have found it very unfortunate that POSIX did not define
a standard symbol that means ``give me POSIX, plus any non-conflicting
vendor-specific definitions''.\s-2
.FE
.LP
An example of using \fBX_NOT_POSIX\fP might be to determine the type that
\fBgetuid\fP would be declared by in \fB<pwd.h>\fP:
.Ds
#include <pwd.h>
#ifndef X_NOT_POSIX
    uid_t uid;
#else
    int uid;
    extern int getuid();
#endif
    uid = getuid();
.De
.LP
Note that both of these symbols, when declared, state a
non-compliance.  This was chosen so that porting to a new, standard
platform would be easier.  Only non-standard platforms need to add
themselves to \fB<X11/Xosdefs.h>\fP to turn on the appropriate symbols.
.LP
Not all systems for which we leave these symbols undefined strictly
adhere to the relevant standards.  Thus you will sometimes see checks
for a specific O/S near a check for one of the Xosdefs.h symbols.
However, we have found it most useful to label systems as conforming
even if they have some holes in their compliance.  Presumably these
holes will become fewer as time goes on.
.NH 2
<X11/Xos.h>
.LP
In general, \fB<X11/Xos.h>\fP should be used instead of the
following header files:
.Ds
<string.h>
<strings.h>
<sys/types.h>
<sys/file.h>
<fcntl.h>
<sys/time.h>
<unistd.h>
.De
This file is not part of any X Consortium standard,
it is simply part of our software distribution.
.LP
Some common routines for which you need to include
\fB<X11/Xos.h>\fP before using are:
.Ds
index
rindex
strchr
strrchr
(all the other standard string routines)
gettimeofday
time
.De
.LP
Data types and constants that should be obtained with \fB<X11/Xos.h>\fP are:
.Ds
caddr_t
O_RDONLY
O_RDWR
(and other \fBopen\fP constants)
R_OK
W_OK
X_OK
(and other \fBfcntl\fP constants)
.De
.LP
Unfortunately, we did not create a header file for declaring \fBmalloc\fP
correctly, and it can be a bit tricky.  You can use what we currently
have by copying, for example, from \fBmit/lib/Xt/Alloc.c\fP:
.Ds
#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
char *malloc(), *realloc(), *calloc();
#endif
#if defined(macII) && !defined(__STDC__)  /* stdlib.h fails to define these */
char *malloc(), *realloc(), *calloc();
#endif /* macII */
.De
.NH 2
<X11/Xfuncs.h>
.LP
This file contains definitions of \fBbcopy\fP, \fBbzero\fP,
and \fBbcmp\fP.\s-2\u4\d\s0
.FS
\s+2\&4.  Yes, we should have used the ANSI C function names,
but we thought we had too much existing code using the BSD names.\s-2
.FE
You should include this header in any file that uses these functions.
This file is not part of any X Consortium standard,
it is simply part of our software distribution.
.NH 2
<X11/Xfuncproto.h>
.LP
This file contains definitions for writing function declarations
to get function prototypes to work right.
It deals with ANSI C compilers as well as pre-ANSI C compilers
that have parts of function prototypes implemented.
This file is not part of any X Consortium standard,
it is simply part of our software distribution.
.LP
For external header files that might get used from C++,
you should wrap all of your function declarations like this:
.Ds
_XFUNCPROTOBEGIN
\fIfunction declarations\fP
_XFUNCPROTOEND
.De
When in doubt, assume that the header file might get used from C++.
.LP
A typical function declaration uses \fBNeedFunctionPrototypes\fP, like this:
.Ds
extern Atom XInternAtom(
#if NeedFunctionPrototypes
    Display*		/* display */,
    _Xconst char*	/* atom_name */,
    Bool		/* only_if_exists */		 
#endif
);
.De
.LP
If there are \fBconst\fP parameters, use the symbol \fB_Xconst\fP instead,
as above.
If it is plausible to pass a string literal to a char* parameter, then
it is a good idea to declare the parameter with \fB_Xconst\fP, so that
literals can be passed in C++.
.LP
If there are nested function prototypes, use \fBNeedNestedPrototypes\fP:
.Ds
extern Bool XCheckIfEvent(
#if NeedFunctionPrototypes
    Display*		/* display */,
    XEvent*		/* event_return */,
    Bool (*) (
#if NeedNestedPrototypes
	       Display*			/* display */,
               XEvent*			/* event */,
               XPointer			/* arg */
#endif
             )		/* predicate */,
    XPointer		/* arg */
#endif
);
.De
.LP
If there is a variable argument list, use \fBNeedVarargsPrototypes\fP:
.Ds
extern char *XGetIMValues(
#if NeedVarargsPrototypes
    XIM /* im */, ...
#endif
);
.De
.LP
If you have parameter types that will widen in K&R C, then you should
use \fBNeedWidePrototypes\fP:
.Ds
extern XModifierKeymap *XDeleteModifiermapEntry(
#if NeedFunctionPrototypes
    XModifierKeymap*	/* modmap */,
#if NeedWidePrototypes
    unsigned int	/* keycode_entry */,
#else
    KeyCode		/* keycode_entry */,
#endif
    int			/* modifier */
#endif
);
.De
.LP
If you use \fB_Xconst\fP, \fBNeedNestedPrototypes\fP,
\fBNeedVarargsPrototypes\fP, or \fBNeedWidePrototypes\fP,
then your function implementation also has to have a
function prototype.  For example:
.Ds
#if NeedFunctionPrototypes
Atom XInternAtom (
    Display *dpy,
    _Xconst char *name,
    Bool onlyIfExists)
#else
Atom XInternAtom (dpy, name, onlyIfExists)
    Display *dpy;
    char *name;
    Bool onlyIfExists;
#endif
{
    ...
}
.De
.LP
Actually, anytime you use a function prototype in a header file,
you should use a function prototype in the implementation, as
required by ANSI C.  The MIT X sources do not follow this (we've
never had time to make all the changes), and there are almost
certainly compilers that will complain if the implementation
does not match the declaration.
.NH 2
Other Symbols
.LP
Do not use the names \fBclass\fP, \fBnew\fP, or \fBindex\fP as
variables or struct members.  The names \fBclass\fP and \fBnew\fP
are reserved words in C++, and you may find your header files
used by a C++ program someday.
Depending on your system, \fBindex\fP can be defined as \fBstrchr\fP
or a macro in \fB<X11/Xos.h>\fP; this may cause problems if you
include this header file.
.LP
The following system-specific symbols are commonly used in X sources
where OS dependencies intrude:\s-2\u5\d\s0
.FS
\s+2\&5.  At most \fIone\fP of these symbols should be defined
on a given system!\s-2
.FE
.Ds
.ta 1i
USG	based on System V Release 2
SYSV	based on System V Release 3
SVR4	System V Release 4
.De
.LP
For other system-specific symbols, look at the \fBStandardDefines\fP
parameters in the \fBmit/config/*.cf\fP files.
.NH 1
What's New, What's Changed
.LP
In this section we briefly describe some of the more significant
new features of Release 5.
.NH 2
New standards
.LP
The following standards are new in Release 5:
.Ip "X Font Service Protocol"
Instead of forcing each X server to read all fonts from the
filesystem, the X Font Server Protocol makes it
possible to manage fonts separately from the X server, directing the X
server to request fonts via this new Consortium standard network protocol from
a font server.
In addition, for fonts which take a long time to
open, this allows the X server to continue with other clients while
the font server services the font requests.
.Ip "XLFD changes for scalable fonts"
The X Logical Font Description standard has been compatibly enhanced
to allow clients to specify and use scalable fonts.
.Ip "X Input Device Extension"
This extension has been promoted from Draft Standard to full
Consortium Standard with this release.
.Ip "Inter-Client Communications Conventions"
This standard has been updated to cover the new X Device Color
Characterization Conventions for device-independent color support in Xlib.
.NH 2
General
.LP
We have tried hard with this release to make our code use standard
features from POSIX.1 and ANSI C when possible.  A new include file
\fB<X11/Xosdefs.h>\fP describes which systems comply or do not comply with
these standards.
.LP
Two new X authorization schemes are included, a DES based private-key system
which was described in the R4 XDMCP document - XDM-AUTHORIZATION-1 (along
with the associated XDMCP authentication system XDM-AUTHENTICATION-1) and
the Sun Secure RPC based SUN-DES-1 system, which uses the SunOS supplied
security system.
.NH 2
Clients
.LP
Most clients participate in the WM_DELETE_WINDOW protocol.
.LP
New clients: \fBeditres\fP, \fBviewres\fP, \fBxconsole\fP, \fBxcmsdb\fP.
New demos: \fBbeach_ball\fP, \fBauto_box\fP, \fBgpc\fP,
\fBxcmstest\fP, \fBxgas\fP, \fBx11perf\fP.
\fBXlswins\fP has been removed; it is replaced by \fBxwininfo -tree\fP.
Moved to contrib: \fBmuncher\fP, \fBplaid\fP.
Completely new implementation: \fBbitmap\fP and \fBxmag\fP.
Other changes of note:
.Ip editres
\fBEditres\fP is a tool that allows users and application developers to view
the full widget hierarchy of any X Toolkit client that speaks the
Editres protocol.  In addition \fBeditres\fP will help the user construct
resource specifications, allow the user to apply the resource to
the application and view the results dynamically.  Once the user is
happy with a resource specification, editres will append the resource
string to the user's resources file.
.Ip xdm
\fBXdm\fP can now display a menu of hosts for XDMCP-capable terminals using the
new \fIchooser\fP client.  This is useful for X terminals that
do not themselves offer such a menu.
XDMCP works with STREAMS.
A new setup program is invoked by \fBxdm\fP prior to putting up the
login window; this program can be used to run \fBxsetroot\fP, \fBxcmsdb\fP,
and do any other custom initialization required.
.Ip xterm
Cuts of wrapped lines are now treated as a single line.
Cuts of multi-page regions now work and highlight correctly.
Pasting large amounts of data into \fBxterm\fP now works (on systems with
properly-working pty implementations).
New arguments have been added to the send-signal action: quit, alarm.
The \fBtiteInibit\fP resource has been modified to also inhibit
the escape sequence
which switches to the alternate screen.
Two new items have been added to the VT Fonts menu: 
5x7 (Tiny) and 10x20 (Huge).
The following resources have been added: \fBresizeGravity\fP,
\fBbellSuppressTime\fP,
\fBappcursorDefault\fP, \fBappkeypadDefault\fP,
\fBginTerminator\fP, \fBautoWrap\fP.
The \fIXterm Control Sequences\fP document is up to date.
\fBXterm\fP is installed securely when made setuid on SunOS 4.1.1
with shared libraries.
.Ip xmh
\fBXmh\fP now uses the \fBMH\fP environment variable, if set.
\fBXmh\fP now supports checking for mail in multiple maildrops.
Enhanced participation in WM_PROTOCOLS has been added.
New resources have been added, including:
\fBcheckpointInterval\fP,
\fBcheckpointNameFormat\fP,
\fBmailInterval\fP,
\fBrescanInterval\fP, \fBshowOnInc\fP, \fBnoMailBitmap\fP,
\fBnewMailBitmap\fP, \fBnewMailIconBitmap\fP, and \fBnoMailIconBitmap\fP.
New actions have been added:
XmhWMProtocols, XmhShellCommand, XmhCheckForNewMail,
XmhViewMarkDelete.
Better recovery from unexpected inconsistencies with the filesystem
has been implemented.
Better POP support has been added.
See the file \fBmit/clients/xmh/CHANGES\fP for more details.
.Ip oclock
\fBOclock\fP has a new \fB\-transparent\fP option.
.Ip xload
\fBXload\fP is secure on SunOS 4.1.1 with shared libraries.
.Ip xditview
\fBXditview\fP now supports \fBpic\fP, scalable fonts, settable
device resolution, and has a better user interface.
.NH 2
Libraries in General
.LP
All of the useful libraries now use function prototypes by default for
systems which support them.
SunOS shared libraries now use much less swap space than in R4.
In addition, System V Release 4 and AIX 3.1 shared libraries are also
supported now.  Configuring new shared library systems should be much
easier than before.
.NH 2
Xlib
.LP
Two new major pieces of functionality have been added to Xlib:
device independent color, and internationalization (i18n).
In addition, a few other additions and improvements have been made.
.NH 3
Xlib Manual
.LP
The Xlib manual has been reorganized for Release 5.
Unfortunately, this may cause considerable confusion for a while
when people quote section numbers without reference to the release.
However, we feel that the new organization is a considerable improvement.
.NH 3
Device-independent Color
.LP
The \fBXcms\fP (X Color Management System) functions in Xlib
support device-independent color spaces derivable from the CIE XYZ
color space.
This includes the CIE XYZ, xyY, L*u*v*, and L*a*b* color spaces as well as
the TekHVC color space.
In addition, linear RGB intensity value space has been added, as
well as gamma correction for device RGB values,
and a uniform syntax has been adopted for specifying colors in strings.
Xlib now supports client-side color name databases, and the
existing Xlib functions that use color names (e.g.,
\fBXLookupColor\fP and \fBXAllocNamedColor\fP) now handle all
color spaces, so that the contrivance of using \fBXParseColor\fP
followed by \fBXAllocColor\fP is no longer necessary.
Xlib provides direct programming interfaces for dealing with color
values in different spaces, and for converting between spaces.
New device-independent color spaces can also be added.
For details on the new color functionality, read Chapter 6 of
the new Xlib manual.
.LP
Monitors are characterized by data stored on root window properties;
the new \fBxcmsdb\fP program can be used to set these properties.
Unfortunately, you need a color analyzer instrument to generate
characterizations; choosing a random one will almost certainly
produce inaccurate colors.  However, you will find some sample
database files in \fBmit/clients/xcmsdb/datafiles\fP and in
\fBcontrib/clients/ca100/monitors\fP.
.NH 3
Internationalization
.LP
An internationalized application is one which is adaptable to the requirements
of different native languages, local customs, and character string encodings.
The process of adapting the operation to a particular native language,
local custom, or string encoding is called localizaton.
A goal of internationalization is to permit localization
without program source modifications or recompilation.
.LP
Internationalization in Xlib is based on the concept of a \fIlocale\fP.
A locale defines the ``localized'' behavior of a program at run-time.
Locales affect Xlib in its:
.IP \(bu 5
Encoding and processing of input method text
.IP \(bu 5
Encoding of resource files and values
.IP \(bu 5
Encoding and imaging of text strings
.IP \(bu 5
Encoding and decoding for inter-client text communication
.LP
Characters from various languages are represented in a computer
using an encoding.
Different languages have different encodings,
and there are even different encodings for the same characters
in the same language.
.LP
Xlib provides support for localized text imaging and text input.
Sets of functions are provided for multibyte (char\&*) text as well as
wide character (\fBwchar_t\fP) text in the form supported
by the host C language environment.
For details on the new internationalization functionality,
read Chapter 13 of the new Xlib manual.
.LP
Two sample implementations of the internationalization mechanisms
exist in this release, one called \fBXsi\fP and one called \fBXimp\fP.
You will find documentation for them in \fBmit/doc/I18N\fP and
\fBcontrib/im\fP, and locale definition files in \fBmit/lib/nls\fP.
In \fBcontrib/im\fP you will also find network-based
Input Method servers.
.LP
Unfortunately, none of the programs in the MIT software use the new
internationalization facilities.  However, you will find some sample
clients in \fBcontrib/im\fP, and internationalized versions of some
of the MIT clients in \fBcontrib/clients\fP.
.NH 3
Keysyms
.LP
By default a database of all registered vendor-private keysyms
gets installed, so that Xlib can map between keysym values and names.
.NH 3
Resource Databases
.LP
A new SCREEN_RESOURCES property has been defined, permitting
screen-specific resources to be set, so that (for example)
colors can be specified for a color screen and not effect
a monochrome screen on the same server.  The \fBxrdb\fP program
has been enhanced to ``do the right thing'' automatically
in most cases.
.LP
New functions have been defined to merge a resource database
directly from a file, and to combine two databases with either
``augment'' or ``override'' semantics.
.LP
A ``#include'' syntax is now supported in resource files.
A specific example of using this is to have a customized
app-defaults file (in an Xt application) include the base
app-defaults file.
.LP
A new reserved component name, ``?'', has been defined
that matches a single level in the resource hierarchy.
This makes it easier to override resources specified in
app-defaults files.
.LP
A new function, \fBXrmEnumerateDatabase\fP, has been defined
to search for matching entries in a resource database.
The \fBappres\fP program has been enhanced in this release
to become a convenient interface to this function.
.LP
A new function, \fBXrmPermStringToQuark\fP, has been
introduced to avoid having Xlib allocate needless storage
for constant strings.
.NH 3
Extensions
.LP
A new function has been added to permit an extension to convert
errors with additional data into Xlib format, and one has been
added to permit an extension to print out the values in an
error handler.
.NH 3
Miscellaneous
.LP
A new type \fBXPointer\fP has been introduced, replacing
uses of the non-standard type \fBcaddr_t\fP.  Old programs
using \fBcaddr_t\fP will still work, of course.
.NH 3
Performance
.LP
The new color and internationalization facilities have the unfortunate
effect of making executables quite a bit larger on systems that do
not have shared libraries.
.LP
The resource database functions have been completely rewritten for
this release.  Databases should be significantly smaller in memory,
and loading and parsing resources should be faster.
.NH 2
Xt Intrinsics
.LP
At the data structure level, Release 5 retains complete binary
compatibility with Release 4.  The specification of the
\fBObjectPart\fP,
\fBRectObjPart\fP,
\fBCorePart\fP,
\fBCompositePart\fP,
\fBShellPart\fP,
\fBWMShellPart\fP,
\fBTopLevelShellPart\fP,
and
\fBApplicationShellPart\fP
instance records was made less strict to permit implementations to
add internal fields to these structures.  Any implementation that
chooses to do so would, of course, force a recompilation.
The Xlib specification for
\fBXrmValue\fP
and
\fBXrmOptionDescRec\fP
was updated to use a new type,
\fBXPointer\fP,
for the \fIaddr\fP and \fIvalue\fP fields respectively, to avoid
ANSI C conformance problems.  The definition of
\fBXPointer\fP
is binary compatible with the previous implementation.
.NH 3
baseTranslations Resource
.LP
A new pseudo-resource, \fBXtNbaseTranslations\fP, was defined to permit
application developers to specify translation tables in application
defaults files while still giving end users the ability to augment
or override individual event sequences.  This change will affect
only those applications that wish to take advantage of the new
functionality, or those widgets that may have previously defined
a resource named ``baseTranslations''.
.LP
Applications wishing to take advantage of the new functionality
would change their application defaults file, e.g., from
.Ds
app.widget.translations: \fIvalue\fP
.De
to
.Ds
app.widget.baseTranslations: \fIvalue\fP
.De
If it is important to the application to preserve complete
compatibility of the defaults file between different versions
of the application running under Release 4 and Release 5,
the full translations can be replicated in both the ``translations''
and the ``baseTranslations'' resource.
.NH 3
Resource File Search Path
.LP
The current specification allows implementations greater flexibility
in defining the directory structure used to hold the application class
and per-user application defaults files.  Previous specifications
required the substitution strings to appear in the default path in a
certain order, preventing sites from collecting all the files for
a specific application together in one directory.  The Release 5
specification allows the default path to specify the substitution
strings in any order within a single path entry.  Users will need to
pay close attention to the documentation for the specific
implementation to know where to find these files and how to specify
their own
\fBXFILESEARCHPATH\fP
and
\fBXUSERFILESEARCHPATH\fP
values when overriding the system defaults.
.NH 3
Customization Resource
.LP
\fBXtResolvePathname\fP
supports a new substitution string, %C, for specifying separate
application class resource files according to arbitrary user-specified
categories.  The primary motivation for this addition was separate
monochrome and color application class defaults files.  The
substitution value is obtained by querying the current resource
database for the application resource name ``customization'', class
``Customization''.  Any application that previously used this
resource name and class will need to be aware of the possibly
conflicting semantics.
.NH 3
Per-Screen Resource Database
.LP
To allow a user to specify separate preferences for each screen of a
display, a per-screen resource specification string has been added and
multiple resource databases are created; one for each screen.  This
will affect any application that modified the (formerly unique)
resource database associated with the display subsequent to the Intrinsics
database initialization.  Such applications will need to be aware
of the particular screen on which each shell widget is to be created.
.LP
Although the wording of the specification changed substantially in the
description of the process by which the resource database(s) is
initialized, the net effect is the same as in prior releases with the
exception of the added per-screen resource specification and the new
customization substitution string in
\fBXtResolvePathname\fP.
.NH 3
Internationalization of Applications
.LP
Internationalization as defined by ANSI is a technology that
allows support of an application in a single locale.  In
adding support for internationalization to the Intrinsics
the restrictions of this model have been followed.
In particular, the new Intrinsics interfaces are designed to not
preclude an application from using other alternatives.
For this reason, no Intrinsics routine makes a call to establish the
locale.   However, a convenience routine to establish the
locale at initialize time has been provided, in the form
of a default procedure that must be explicitly installed
if the application desires ANSI C locale behavior.
.LP
As many objects in X, particularly resource databases, now inherit
the global locale when they are created, applications wishing to use
the ANSI C locale model should use the new function
\fBXtSetLanguageProc\fP
to do so.
.LP
The internationalization additions also define event filters
as a part of the Xlib Input Method specifications.  The
Intrinsics enable the use of event filters through additions to
\fBXtDispatchEvent\fP.
Applications that may not be dispatching all events through
\fBXtDispatchEvent\fP
should be reviewed in the context of this new input method mechanism.
.LP
In order to permit internationalization of error messages the name
and path of the error database file is now allowed to be implementation
dependent.  No adequate standard mechanism has yet been suggested to
allow the Intrinsics to locate the database from localization information
supplied by the client.
.LP
The previous specification for the syntax of the language string
specified by
\fBxnlLanguage\fP
has been dropped to avoid potential conflicts with other standards.
The language string syntax is now implementation-defined.
The example syntax cited is consistent with the previous
specification.
.NH 3
Permanently Allocated Strings
.LP
In order to permit additional memory savings, an Xlib interface was
added to allow the resource manager to avoid copying certain string
constants.  The Intrinsics specification was updated to explicitly require
the Object \fIclass_name\fP, \fIresource_name\fP, \fIresource_class\fP,
\fIresource_type\fP, \fIdefault_type\fP in resource tables, Core \fIactions\fP
\fIstring\fP field, and Constraint \fIresource_name\fP, \fIresource_class\fP,
\fIresource_type\fP, and \fIdefault_type\fP resource fields to be
permanently allocated.  This explicit requirement is expected to
affect only applications that may create and destroy classes
on the fly.
.NH 3
Arguments to Existing Functions
.LP
The \fIargs\fP argument to
\fBXtAppInitialize\fP,
\fBXtVaAppInitialize\fP,
\fBXtOpenDisplay\fP,
\fBXtDisplayInitialize\fP,
and
\fBXtInitialize\fP
were changed from
\fBCardinal\fP*
to int* to conform to pre-existing convention and avoid otherwise
annoying typecasting in ANSI C environments.
.NH 3
Implementation
.LP
Function prototypes are now fully supported in the header files.
.LP
\fB<X11/Intrinsic.h>\fP no longer includes \fB<X11/Xos.h>\fP by default.
Inclusion of this file was a bug in earlier releases.  If you have
old code that depends on this bug, you can define -DXT_BC when you compile
to get back the old behaviour.
.LP
String constants are now defined in a single array, saving memory and external
symbols.  Note that because the new implementation uses #defines, string
constants in widget header files which duplicate a constant defined by Xt
should either be removed or protected from a collision.
.LP
The translation manager facilities have been completely reimplemented
in this release, resulting in substantially less memory consumed by
some applications.
A number of other memory-saving changes have been implemented, and
in a few cases execution time should be faster.
.LP
The default keycode to keysym translator deals with all Latin keysyms.
.NH 3
Extension Events
.LP
Unfortunately, the Xt standard as of R5 still does not address the
issues of integrating events from protocol extensions into the
normal Xt dispatch mechanism.  The adventurous will find a set
of patches to Xt in \fBcontrib/lib/Xt\fP that attempt to address
this problem.  These patches are non-standard, experimental,
subject to change, not guaranteed, may adversely affect your ability to
apply public patches from MIT, and have not reviewed by the X Consortium.
.NH 2
PEX
.LP
The PEX Sample Implementation (SI) is composed of several parts. The
major components are the extension to the X Server, which implements
the PEX protocol, and the client side Application Protocol Interface (API),
which provides a mechanism by which clients can generate PEX protocol.
The API provided with the PEX-SI is the ISO IS PHIGS Binding and the
yet to be standardized PHIGS PLUS Binding.
.LP
In addition to these major components, several other minor components are
provided.  These include documentation, 3D fonts for PEX, demos, and
a verification suite called InsPEX.  Also provided in \fBcontrib\fP
are additional example programs and demos.
.LP
These elements are located in the following area:
.LP
The PEX server extension is located in the directories under
\fBmit/extensions/server/PEX\fP.  Device independent portions are located
in \fBmit/extensions/server/PEX/dipex\fP.  Device dependent functionality
appears in \fBmit/extensions/server/PEX/ddpex\fP.  Operating system font
dependent code appears in \fBmit/extensions/server/PEX/ospex\fP.  General
purpose server include files are in \fBmit/extensions/server/PEX/include\fP.
.LP
The API code is located under the directory \fBmit/extensions/lib/PEX\fP.
The PHIGS/PHIGS PLUS Binding routines are in the \fBc_binding\fP subdirectory.
The PHIGS Monitor (PM), a separate process started at client runtime
to handle PHIGS Input functionality, is in the \fBcp\fP subdirectory.
Other code located in the various subdirectories handles PHIGS archival,
error handling, and comprises the internal library level that PHIGS calls
to generate the PEX Protocol.
.LP
All PEX documentation is located in the directory \fBmit/doc/extensions/PEX\fP,
with pregenerated PostScript files in \fBmit/hardcopy/extensions/PEX\fP.
The PEX Protocol Specification itself is in the \fBProto\fP subdirectory.  All
SI documentation is in the \fBSI\fP subdirectory.  Three subdirectories there
contain an Architecture Specification, a Porting Guide (with implementation
details), and a User's Guide.  The sources and programs used to generate
these files are located in the \fBmit/doc/extensions/PEX/SI\fP directory.
Also located there is the \fBPHIGS\fP subdirectory which contains PHIGS man
pages and macros for printing these pages.
.LP
Font source for PEX and utilities to build them are located in the directory 
\fBmit/fonts/PEX\fP.  Two stroke fonts are supplied.
.LP
The PEX verification tool InsPEX can be found in the
\fBmit/extensions/test/InsPEX\fP directory.
Shell scripts are provided there to run InsPEX.
More information on InsPEX is available in the User's Guide.
.LP
Demos for PEX can be found in the \fBmit/demos\fP directory.
Two demos and the
NCGA Graphics Performance Characterization (GPC) Suite can be found
there.  The demos are in the \fBmit/demos/auto_box\fP and
\fBmit/demos/beach_ball\fP directories, and are named \fBauto_box\fP
and \fBbeach_ball\fP respectively.
The GPC suite is found in \fBmit/demos/gpc\fP.
This suite consists of demos (in the \fBobjects\fP subdirectory),
benchmarks (various directories below \fBbenchmarks\fP) and tests
(in \fBtests\fP).  For more information on how to run these demos see the
User's Guide.
.LP
There are also several unsupported demos and examples available in
\fBcontrib\fP.  In \fBcontrib/demos/beach_ball2\fP a newer version 
of the \fBbeach_ball\fP demo with enhanced functionality can be found.
In \fBcontrib/examples/PEX\fP various PHIGS based clients that
demonstrate how to use PEX via the PHIGS API are available.
.NH 3
Standards and Functionality
.LP
This release conforms to the PEX Protocol Specification V5.0P.
The release comes with 2 fonts, Roman and Roman_M (see the User's Guide for
more details).  It implements the minimum required HLHSR (Hidden Line/
Hidden Surface Removal) for PHIGS compliance (i.e., NONE).  The release
only supports 8-bit color.
.LP
The API binding has been updated to the ISO IS PHIGS binding.
The directory \fBmit/util/PEX\fP contains \fBsed\fP scripts for
converting programs
from the previous binding to the new binding.  These scripts do most of 
the work, but some manual editing is still needed.  There is a \fBREADME\fP
file in this directory with notes and information.
.LP
The PHIGS Binding provides most PHIGS/PHIGS PLUS functionality.  The full
PHIGS Input Model (Valuator, Locator, Stroke, Choice, String, Pick)
is implemented in a device independent manner using the Athena Widget Set.
PHIGS/PHIGS PLUS functionality includes, but is not limited to the following
graphical primitives:  Polylines, Polymarkers, Fill Areas, Triangle Strips,
NURBS Curves and Surfaces, 2D and 3D Text.  Other operations include
Depth Cueing, Modelling Clip, Backface removal, Lighting Models and
Surface Reflection.
.LP
Functionality not completed in this release is as follows: 
.LP
In the API:
.Ds
Mapping of PHIGS font ids to PEX fonts
.De
.LP
In the Server:
.Ds
Backface Attributes and Distinguish Flag
Font sharing between clients
Patterns, Hatches and associated attributes
Color Interpolation
Transparency
Depth Cueing for Markers
Z-buffering
Double Buffering
.De
.LP
In InsPEX:
.Ds
Completion of port to ISO IS PHIGS Binding
.De
.NH 3
PEX and PHIGS Documents
.LP
The following documents are provided with this release:
.Ds
PEX-SI User Guide
PEX-SI Graphics Library Manual Pages
PEX-SI Architecture Specification
PEX-SI Porting Guide
.De
.LP
They are located in subdirectories of \fBmit/doc/extensions/PEX\fP.
Please read the PEX-SI User's Guide for descriptions of the documents and
how to use them.  Instructions for printing the documents are provided
in a README file in each of the document directories.
.LP
The User's Guide is provided as a starting point in the documentation.
It describes the various documents provided with the release, and includes
instructions on using the clients, the API and the server code.
It also includes specifications for the server functionality and archive
format.
.LP
The Graphics Library Manual Pages are for the client-side library,
written to the ISO IS binding.
.LP
The Architecture Specification describes the PEX-SI architecture
at a high level.
.LP
The Porting Guide is intended as an extension to the Architecture
Specification.  There is a lot of good information in this document,
and it is organized fairly well, but it lacks some polish.  It is not a
complete document.
.NH 3
InsPEX
.LP
This release of InsPEX includes coverage of all the PHIGS PLUS graphics
primitives, such as fill area sets with data, quadrilateral meshes,
triangle strips, and NURBS.  PHIGS PLUS attributes such as direct color
specification, depth cuing, and lighting are also exercised.
.LP
The testing of input is somewhat limited by the problem of simulating
mouse and keyboard input in a reliable and portable fashion.  For the
pick, locator, and stroke devices, simulating the mouse events is
straightforward, but since the string, valuator, and choice devices
are built upon a toolkit (Athena Widgets in the PEX-SI's case), getting
window id's for the appropriate windows and sending mouse
clicks to the right place on those windows is more difficult,
and probably impossible to do in a way that could be quickly
ported to another toolkit along with these input devices.
The technology for automatic testing of software using
a graphical user interface under X has not progressed to the
point where this functionality could be tested in a way
that would be useful to all the potential users of InsPEX.
.LP
For nearly all of the tests that use image comparison
to verify graphical output, reference images have been supplied.
Due to outstanding bugs in the code, however, some tests are shipped without
reference images.  Also, since back-facing attributes are not implemented,
the images saved for these tests are actually incorrect.  These have been
shipped, however, because they still can be helpful to someone porting
the PEX-SI.  It should be expected that when backfacing attributes are
implemented, these tests will fail and image comparison will be required.
.LP
Along with the \fBREADME\fP in the main InsPEX directory, there is a sample
log file, sample.log, and an automatically-generated summary of all the
current tests, \fBtest_descrip\fP.  See the \fBREADME\fP for a description
of how that file was generated.
.NH 2
Athena Widget Set
.LP
Many minor bugs have been fixed.
The Xaw examples have been moved to \fBcontrib\fP.
However, please note that the Athena Widgets have been and continue
to be low on our priority list, so many bugs remain (particularly
in the Text widget) and many requests for enhancements have not
been implemented.  Because some incompatible
changes have been made, the shared library major version number on
Suns has been incremented.
.Ip "Header Files"
Function prototypes have been added to the public interfaces.
.Ip "AsciiSrc"
No warning message is printed when the file cannot be
written to; the return value should be enough info.
\fBGetValues\fP on the string resource was failing when
"useStringInPlace" was set to true; fixed.
A memory leak when freeing pieces in a source of type "ascii String"
has been plugged.
The buffer is now updated whenever the "string" resource is set using
\fBXtSetValues\fP.  If the type is file then the file is read in again.
.Ip "Box"
\fBBox.h\fP now includes \fB<X11/Xmu/Converters.h>\fP for the 
orientation resources.
.Ip "Clock"
Changed to be a subclass of \fBSimple\fP instead of \fBCore\fP.
.Ip "Command"
A bug in changing the shape style back to Rectangular has been fixed.
.Ip "Dialog"
The Icon resource type has changed from \fBPixmap\fP to \fBBitmap\fP.
.Ip "Form"
The geometry handler now will now disallow geometry management
requests that will force the child outside the \fBForm\fP's window.
EdgeType names have been changed to have prefix ``Xaw'' instead of ``Xt'', but
the old definitions are still around with a #define.
The string-to-widget converter no longer caches resources.
.Ip "Logo"
Changed to be a subclass of \fBSimple\fP instead of \fBCore\fP.
Reverse video now works correctly.
.Ip "Mailbox"
Changed to be a subclass of \fBSimple\fP instead of \fBCore\fP.
Reverse video now works correctly.
.Ip "MenuButton"
The \fBMenuButton\fP widget no longer places a server grab on itself.
Instead, \fBPopupMenu\fP is registered as a grab action.  As a result
of this, clients which popped up menus without using \fBXtMenuPopup\fP
or \fBMenuPopup\fP or \fBPopupMenu\fP in the menu button translations will
fail to have a grab active.  They should make a call to
\fBXtRegisterGrabAction\fP on the appropriate action in the application
initialization routine, or use a different translation.
.Ip "Paned"
\fBPaned.h\fP now includes \fB<X11/Xmu/Converters.h>\fP for the 
orientation resources.
.Ip "Panner"
This widget is new in R5, see the Xaw manual for details.
.Ip "Porthole"
This widget is new in R5, see the Xaw manual for details.
.Ip "Repeater"
This widget is new in R5, see the Xaw manual for details.
.Ip "Scrollbar"
Changed to be a subclass of \fBSimple\fP instead of \fBCore\fP.
The type of thumb resource has changes from \fBPixmap\fP to \fBBitmap\fP.
However, if applications provide the resource conversion, the 
\fBSetValues\fP method can still handle pixmaps of correct depth.
.Ip "Simple"
A color cursor converter has been added, as well as the new
new resource types: \fBXtNpointerColor\fP,
\fBXtNpointerColorBackground\fP, and \fBXtNcursorName\fP.
.Ip "SmeBSB"
The Right bitmaps are now painted in the correct location.
Right and Left Bitmaps can be specified in resource files, and
at startup time.
.Ip "Text"
If there is no current selection the the selection extends
from the insert point, rather than some random location.
Forward (Backward) Paragraph works at the paragraph boundaries now.
Selecting a word now transitions correctly at both end points.
An infinite loop when using fill paragraph in a read only text widget
has been found and fixed.
When the "resize" resource is set the text will start up with
exactly enough space to contain the text in the widget.
A bug that could cause an infinite loop when Meta-Q was used
to invoke the form-paragraph function on a read-only text widget has
been fixed.
Problems dealing with exposure events have been fixed.
In \fBTextP.h\fP, the names of the following symbolic constants
have each had the prefix Xaw added to them:
\fBXawLF\fP, \fBXawCR\fP, \fBXawTAB\fP, \fBXawBS\fP, \fBXawSP\fP,
\fBXawDEL\fP, and \fBXawBSLASH\fP.
.Ip "Toggle"
The widget state is preserved across changes in sensitivity.
A string-to-widget converter is registered for radio groups.
.Ip "Tree"
This widget is new in R5, see the Xaw manual for details.
.Ip "Paned"
A bug that caused \fBXtGeometryYes\fP returns to have bogus values,
and caused panes to get random sizes, has been fixed.
.Ip "Vendor"
Support has been added for the \fBeditres\fP protocol.
All applications using the Athena Widgets are automatically
editable with \fBeditres\fP.
A bug that cause \fBShell\fP to ignore all but first child has been fixed.
.Ip "Viewport"
\fBXawPannerReport\fP support has been added.
.NH 2
X Server
.LP
Considerably more work speeding up the server has been done,
particularly in the \fBcfb\fP and \fBmfb\fP code.
The font interfaces are completely new.
Compressed fonts are not supported in the release.
Other changes are documented in \fBmit/doc/Server/r5.tbl.ms\fP.
.NH 2
Fonts
.Ip "Font Server"
A sample implementation of the server side of the X Font Service Protocol
is provided in a new program, \fBfs\fP.
On the Macintosh, a special version of the server called \fBMacFS\fP
can be used to serve TrueType fonts.
.Ip "New Font Format"
Both the X server and the font server use a
new font format, \fIpcf\fP (portable compiled font).
Pcf is readable across different machines and contains more information
about a font than the old snf format.  Fonts in snf format can still
be used.
.Ip "Font Applications"
The following new utilities talk to the font server:
\fBfsinfo\fP, \fBfslsfonts\fP, \fBfstobdf\fP, and \fBshowfont\fP.
To build pcf fonts, \fBbdftosnf\fP has been replaced by \fBbdftopcf\fP.
.Ip "Scalable Fonts"
Bitstream, Inc. has donated an outline scaling engine along with
a set of sample fonts, matching the donated bitmap fonts included with
Release 4.  In addition, a usable (but not very pretty) bitmap scaling
engine has been implemented which
allows the use of all other fonts at arbitrary point sizes.
.Ip "Font Changes"
Many \fBmisc\fP fonts now have ISO Latin-1 upper half characters and
some tuning.
The following sets of fonts are new:
Latin/Hebrew in ISO8859-8 encoding,
Kanji in JISX0208.1983-0 encoding,\s-2\u6\d\s0
.FS
\s+2\&6.  The JIS Kanji fonts were purchased on behalf of the MIT X Consortium
from the Japanese Standards Association, 1-24, Akasaka 4,
Minato-ku, Tokyo 107, Japan.  They were converted to BDF format,
keeping within the JIS rules.  In keeping with
JIS rules, these fonts should not be transformed into other encodings, they
should only be used in the JISX0208.1983-0 encoding.  It is also strongly
recommended that companies wishing to use these fonts in commercial products
should purchase the original JIS font standards directly from JSA.  The
purchase price is nominal.\s-2
.FE
Hangul in KSC5601.1987-0 encoding.
.NH 1
Acknowledgements
.LP
The MIT Release 5 distribution is brought to you by the MIT X Consortium.
A cast of thousands, literally, have made this release possible.
We cannot possibly acknowledge them all here.
The names of all people who made it a reality will be found in the
individual documents and source files.
We greatly appreciate the work that everyone has put into this release.
.sp 1
.in +3i
.nf
Hoping you enjoy Release 5,

Donna Converse
Stephen Gildea
Susan Hardy
Jay Hersh
Keith Packard
David Sternlicht
Bob Scheifler
Ralph Swick

(R5 Survival Club)
