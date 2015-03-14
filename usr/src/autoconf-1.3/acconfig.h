/* Sample config.h
   This file is used by Autoheader.  It is in the public domain.
   It provides sample descriptive text for the C preprocessor macros
   that the distributed Autoconf macros can define.
   No software package will use all of them, but this file is provided
   so you can copy the ones you use into your own configuration header files.
   The entries are in alphabetical order.  */

/* Define as the proper declaration for yytext.  */
#undef DECLARE_YYTEXT

/* Define if you have dirent.h.  */
#undef DIRENT

/* Define if you have alloca.h and it should be used (not Ultrix).  */
#undef HAVE_ALLOCA_H

/* Define if you don't have vprintf but do have _doprnt.  */
#undef HAVE_DOPRNT

/* Define if you support file names longer than 14 characters.  */
#undef HAVE_LONG_FILE_NAMES

/* Define if you have netdb.h (and thus the rexec function).  */
#undef HAVE_NETDB_H

/* Define if system calls automatically restart after interruption
   by a signal.  */
#undef HAVE_RESTARTABLE_SYSCALLS

/* Define if you have the strcoll function and it is properly defined.  */
#undef HAVE_STRCOLL

/* Define if your struct stat has st_blksize.  */
#undef HAVE_ST_BLKSIZE

/* Define if your struct stat has st_blocks.  */
#undef HAVE_ST_BLOCKS

/* Define if your struct stat has st_rdev.  */
#undef HAVE_ST_RDEV

/* Define if you have sys/mtio.h.  */
#undef HAVE_SYS_MTIO_H

/* Define if your struct tm has tm_zone.  */
#undef HAVE_TM_ZONE

/* Define if you don't have tm_zone but do have the external array
   tzname.  */
#undef HAVE_TZNAME

/* Define if you have unistd.h.  */
#undef HAVE_UNISTD_H

/* Define if utime(file, NULL) sets file's timestamp to the present.  */
#undef HAVE_UTIME_NULL

/* Define if you have vfork.h.  */
#undef HAVE_VFORK_H

/* Define vfork as fork if vfork does not work.  */
#undef vfork

/* Define if you have the vprintf function.  */
#undef HAVE_VPRINTF

/* Define if you have the wait3 system call.  */
#undef HAVE_WAIT3

/* Define if int is 16 bits instead of 32.  */
#undef INT_16_BITS

/* Define if major, minor, and makedev are declared in mkdev.h.  */
#undef MAJOR_IN_MKDEV

/* Define if major, minor, and makedev are declared in sysmacros.h.  */
#undef MAJOR_IN_SYSMACROS

/* Define if you have memory.h, and string.h doesn't declare the
   mem* functions.  */
#undef NEED_MEMORY_H

/* Define if your struct nlist has an n_un member.  */
#undef NLIST_NAME_UNION

/* Define if you have nlist.h.  */
#undef NLIST_STRUCT

/* Define if you can't use the address of an argument to a function
   as the start of an array.  */
#undef NO_ARG_ARRAY

/* Define if your C compiler doesn't accept -c and -o together.  */
#undef NO_MINUS_C_MINUS_O

/* Define if you have neither a remote shell nor the rexec function.  */
#undef NO_REMOTE

/* Define as the return type of signal handlers (int or void).  */
#undef RETSIGTYPE

/* Define if the setvbuf function takes the buffering type as its second
   argument and the buffer pointer as the third, as on System V
   before release 3.  */
#undef SETVBUF_REVERSED

/* Define if you have the ANSI C header files.  */
#undef STDC_HEADERS

/* Define if you don't have dirent.h, but have sys/dir.h.  */
#undef SYSDIR

/* Define if you don't have dirent.h, but have sys/ndir.h.  */
#undef SYSNDIR

/* Define if your sys/time.h declares struct tm.  */
#undef TM_IN_SYS_TIME

/* Define if you do not have strings.h, index, bzero, etc..  */
#undef USG

/* Define if the closedir function returns void instead of int.  */
#undef VOID_CLOSEDIR

/* Define if your processor stores words with the most significant
   byte first (like Motorola and Sparc, unlike Intel and VAX).  */
#undef WORDS_BIGENDIAN

/* Define if on AIX 3.  */
#undef _ALL_SOURCE

/* Define if on MINIX.  */
#undef _MINIX

/* Define if on MINIX.  */
#undef _POSIX_1_SOURCE

/* Define if you need to in order for stat and other things to work.  */
#undef _POSIX_SOURCE

/* Define if type char is unsigned and you are not using gcc.  */
#undef __CHAR_UNSIGNED__

/* Define if you have a <sys/wait.h> with `union wait'.  */
#undef HAVE_SYS_WAIT

/* Define if you have the POSIX.1 `waitpid' function.  */
#undef HAVE_WAITPID

/* Define on System V Release 4.  */
#undef SVR4

/* Define for Encore UMAX.  */
#undef UMAX

/* Define for Encore UMAX 4.3 that has <inq_status/cpustats.h>
   instead of <sys/cpustats.h>.  */
#undef UMAX4_3

/* Define to `int' if <sys/types.h> doesn't define.  */
#undef uid_t
#undef gid_t
#undef pid_t

/* Define to empty if the keyword does not work.  */
#undef const

/* Define for DGUX with <sys/dg_sys_info.h>.  */
#undef DGUX

/* Define if the `getloadavg' function needs to be run setuid or setgid.  */
#undef GETLOADAVG_PRIVILEGED
