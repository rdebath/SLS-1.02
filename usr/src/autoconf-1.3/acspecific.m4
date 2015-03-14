dnl Macros that test for specific features.
dnl This file is part of Autoconf.
dnl Copyright (C) 1992, 1993 Free Software Foundation, Inc.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
dnl
dnl Written by David MacKenzie, with help from 
dnl Franc,ois Pinard, Karl Berry, Richard Pixley, and Ian Lance Taylor.
dnl
dnl
dnl checks for programs
dnl
dnl
define(AC_PROG_CC,
[AC_BEFORE([$0], [AC_PROG_CPP])AC_PROVIDE([$0])AC_PROGRAM_CHECK(CC, gcc, gcc, cc)
# Find out if we are using GNU C, under whatever name.
cat > conftest.c <<EOF
#ifdef __GNUC__
  yes
#endif
EOF
${CC-cc} -E conftest.c > conftest.out 2>&1
if egrep yes conftest.out >/dev/null 2>&1; then
  GCC=1 # For later tests.
fi
rm -f conftest*
])dnl
dnl
define(AC_GCC_TRADITIONAL,
[AC_REQUIRE([AC_PROG_CC])AC_REQUIRE([AC_PROG_CPP])if test -n "$GCC"; then
  echo checking whether -traditional is needed
changequote(,)dnl
  pattern="Autoconf.*'x'"
changequote([,])dnl
  prog='#include <sgtty.h>
Autoconf TIOCGETP'
  AC_PROGRAM_EGREP($pattern, $prog, need_trad=1)

  if test -z "$need_trad"; then
    prog='#include <termio.h>
Autoconf TCGETA'
    AC_PROGRAM_EGREP($pattern, $prog, need_trad=1)
  fi
  test -n "$need_trad" && CC="$CC -traditional"
fi
])dnl
dnl
define(AC_MINUS_C_MINUS_O,
[echo 'foo(){}' > conftest.c
if ${CC-cc} -c conftest.c -o conftesto >/dev/null 2>&1 && test -f conftesto
then
  :
else
  AC_DEFINE(NO_MINUS_C_MINUS_O)
fi
rm -f conftest*
])dnl
dnl
define(AC_PROG_RANLIB, [AC_PROGRAM_CHECK(RANLIB, ranlib, ranlib, @:)])dnl
dnl
define(AC_PROG_AWK, [AC_PROGRAMS_CHECK(AWK, mawk gawk nawk awk,)])dnl
dnl
define(AC_PROG_YACC,
[AC_PROGRAM_CHECK(YACC, bison, bison -y, )
AC_PROGRAM_CHECK(YACC, byacc, byacc, yacc)
])dnl
dnl
define(AC_PROG_CPP,
[AC_PROVIDE([$0])echo checking how to run the C preprocessor
if test -z "$CPP"; then
  CPP='${CC-cc} -E'
  AC_TEST_CPP([#include <stdio.h>], , CPP=/lib/cpp)
fi
AC_SUBST(CPP)dnl
])dnl
dnl
define(AC_PROG_LEX,
[AC_PROVIDE([$0])AC_PROGRAM_CHECK(LEX, flex, flex, lex)
if test -z "$LEXLIB"
then
  case "$LEX" in
  flex*)
    if test -r /usr/local/lib/libfl.a
    then LEXLIB=/usr/local/lib/libfl.a
    elif test -r ${prefix}/lib/libfl.a
    then LEXLIB=${prefix}/lib/libfl.a
    else LEXLIB="-lfl"
    fi
    ;;
  *) LEXLIB="-ll" ;;
  esac
fi
AC_SUBST(LEXLIB)])dnl
dnl
define(AC_DECLARE_YYTEXT,
[AC_REQUIRE([AC_PROG_CPP])AC_REQUIRE([AC_PROG_LEX])echo checking how to declare yytext
# Figure out what yytext is by creating a minimal parser and
# examining the (preprocessed, in case macros are used) output.
if test -z "$DECLARE_YYTEXT"
then
  echo "%%" | $LEX
  DECLARE_YYTEXT=`eval $CPP lex.yy.c |
    sed -n '/extern.*yytext/s/^.*extern/extern/p'`
  rm -f lex.yy.c
fi
AC_DEFINE(DECLARE_YYTEXT, "$DECLARE_YYTEXT")
])dnl
dnl
define(AC_PROG_INSTALL,
[# Make sure to not get the incompatible SysV /etc/install and
# /usr/sbin/install, which might be in PATH before a BSD-like install,
# or the SunOS /usr/etc/install directory, or the AIX /bin/install,
# or the AFS install, which mishandles nonexistent args.  (Sigh.)
if test -z "$INSTALL"; then
  echo checking for install
  saveifs="$IFS"; IFS="${IFS}:"
  for dir in $PATH; do
    test -z "$dir" && dir=.
    case $dir in
    /etc|/usr/sbin|/usr/etc|/usr/afsws/bin) ;;
    *)
      if test -f $dir/install; then
	if grep dspmsg $dir/install >/dev/null 2>&1; then
	  : # AIX
	else
	  INSTALL="$dir/install -c"
	  INSTALL_PROGRAM='$(INSTALL)'
	  INSTALL_DATA='$(INSTALL) -m 644'
	  break
	fi
      fi
      ;;
    esac
  done
  IFS="$saveifs"
fi
INSTALL=${INSTALL-cp}
AC_SUBST(INSTALL)dnl
INSTALL_PROGRAM=${INSTALL_PROGRAM-'$(INSTALL)'}
AC_SUBST(INSTALL_PROGRAM)dnl
INSTALL_DATA=${INSTALL_DATA-'$(INSTALL)'}
AC_SUBST(INSTALL_DATA)dnl
])dnl
dnl
define(AC_LN_S,
[echo checking for ln -s
rm -f conftestdata
if ln -s X conftestdata 2>/dev/null
then
  rm -f conftestdata
  LN_S="ln -s"
else
  LN_S=ln
fi
AC_SUBST(LN_S)
])dnl
dnl
define(AC_RSH,
[echo checking for remote shell
if test -f /usr/ucb/rsh || test -f /usr/bin/remsh || test -f /usr/bin/rsh ||
  test -f /usr/bsd/rsh || test -f /usr/bin/nsh; then
  RTAPELIB=rtapelib.o
else
  AC_HEADER_CHECK(netdb.h, AC_DEFINE(HAVE_NETDB_H) RTAPELIB=rtapelib.o,
    AC_DEFINE(NO_REMOTE))
fi
AC_SUBST(RTAPELIB)dnl
])dnl
dnl
dnl
dnl checks for header files
dnl
dnl
define(AC_STDC_HEADERS,
[echo checking for ANSI C header files
AC_TEST_CPP([#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <float.h>],
# SunOS string.h does not declare mem*, contrary to ANSI.
AC_HEADER_EGREP(memchr, string.h,
# SGI's /bin/cc from Irix-4.0.5 gets non-ANSI ctype macros unless using -ansi.
AC_TEST_PROGRAM([#include <ctype.h>
#define ISLOWER(c) ('a' <= (c) && (c) <= 'z')
#define TOUPPER(c) (ISLOWER(c) ? 'A' + ((c) - 'a') : (c))
#define XOR(e,f) (((e) && !(f)) || (!(e) && (f)))
int main () { int i; for (i = 0; i < 256; i++)
if (XOR (islower (i), ISLOWER (i)) || toupper (i) != TOUPPER (i)) exit(2);
exit (0); }
],AC_DEFINE(STDC_HEADERS))))
])dnl
dnl
define(AC_UNISTD_H, [AC_HEADER_CHECK(unistd.h,
  AC_DEFINE(HAVE_UNISTD_H))])dnl
dnl
define(AC_USG,
[AC_COMPILE_CHECK([BSD string and memory functions],
[#include <strings.h>], [rindex(0, 0); bzero(0, 0);], , AC_DEFINE(USG))])dnl
dnl
dnl
dnl If memchr and the like aren't declared in <string.h>, include <memory.h>.
dnl To avoid problems, don't check for gcc2 built-ins.
define(AC_MEMORY_H,
[echo checking whether string.h declares mem functions
AC_HEADER_EGREP(memchr, string.h, , 
  AC_HEADER_CHECK(memory.h, AC_DEFINE(NEED_MEMORY_H)))]
)dnl
dnl
define(AC_MAJOR_HEADER,
[AC_COMPILE_CHECK([major, minor and makedev header],
[#include <sys/types.h>],
[return makedev(0, 0);], makedev=1)
if test -z "$makedev"; then
AC_HEADER_CHECK(sys/mkdev.h, AC_DEFINE(MAJOR_IN_MKDEV) makedev=1)
fi
if test -z "$makedev"; then
AC_HEADER_CHECK(sys/sysmacros.h, AC_DEFINE(MAJOR_IN_SYSMACROS))
fi]
)dnl
dnl
define(AC_DIR_HEADER,
[AC_PROVIDE([$0])echo checking for directory library header
AC_HEADER_CHECK(dirent.h, AC_DEFINE(DIRENT) dirheader=dirent.h)
if test -z "$dirheader"; then
AC_HEADER_CHECK(sys/ndir.h, AC_DEFINE(SYSNDIR) dirheader=sys/ndir.h)
fi
if test -z "$dirheader"; then
AC_HEADER_CHECK(sys/dir.h, AC_DEFINE(SYSDIR) dirheader=sys/dir.h)
fi
if test -z "$dirheader"; then
AC_HEADER_CHECK(ndir.h, AC_DEFINE(NDIR) dirheader=ndir.h)
fi

echo checking for closedir return value
AC_TEST_PROGRAM([#include <sys/types.h>
#include <$dirheader>
int closedir(); main() { exit(0); }], , AC_DEFINE(VOID_CLOSEDIR))
])dnl
dnl
dnl
dnl checks for typedefs
dnl
dnl
define(AC_UID_T,
[echo checking for uid_t in sys/types.h
AC_HEADER_EGREP(uid_t, sys/types.h, ,
  AC_DEFINE(uid_t, int) AC_DEFINE(gid_t, int))])dnl
dnl
define(AC_SIZE_T,
[echo checking for size_t in sys/types.h
AC_HEADER_EGREP(size_t, sys/types.h, , AC_DEFINE(size_t, unsigned))])dnl
dnl
define(AC_PID_T,
[echo checking for pid_t in sys/types.h
AC_HEADER_EGREP(pid_t, sys/types.h, , AC_DEFINE(pid_t, int))])dnl
dnl
define(AC_MODE_T,
[echo checking for mode_t in sys/types.h
AC_HEADER_EGREP(mode_t, sys/types.h, , AC_DEFINE(mode_t, int))])dnl
dnl
define(AC_RETSIGTYPE,
[AC_COMPILE_CHECK([return type of signal handlers],
[#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
extern void (*signal ()) ();],
[int i;],
[AC_DEFINE(RETSIGTYPE, void)],
[AC_DEFINE(RETSIGTYPE, int)])]
)dnl
dnl
dnl
dnl checks for functions
dnl
dnl
define(AC_VPRINTF,
[AC_COMPILE_CHECK([vprintf], , [vprintf();], AC_DEFINE(HAVE_VPRINTF),
vprintf_missing=1)
if test -n "$vprintf_missing"; then
AC_COMPILE_CHECK([_doprnt], , [_doprnt();], AC_DEFINE(HAVE_DOPRNT))
fi
])dnl
dnl
define(AC_VFORK,
[AC_HEADER_CHECK(vfork.h, AC_DEFINE(HAVE_VFORK_H))
echo checking for working vfork
AC_TEST_PROGRAM([/* Thanks to Paul Eggert for this test.  */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
main() {
  pid_t parent = getpid();
  pid_t child = vfork();

  if (child == 0) {
    /* On sparc systems, changes by the child to local and incoming
       argument registers are propagated back to the parent.
       The compiler is told about this with #include <vfork.h>,
       but some compilers (e.g. gcc -O) don't grok <vfork.h>.
       Test for this by using lots of local variables, at least
       as many local variables as main has allocated so far
       including compiler temporaries.  4 locals are enough for
       gcc 1.40.3 on a sparc, but we use 8 to be safe.
       A buggy compiler should reuse the register of parent
       for one of the local variables, since it will think that
       parent can't possibly be used any more in this routine.
       Assigning to the local variable will thus munge parent
       in the parent process.  */
    pid_t
      p = getpid(), p1 = getpid(), p2 = getpid(), p3 = getpid(),
      p4 = getpid(), p5 = getpid(), p6 = getpid(), p7 = getpid();
    /* Convince the compiler that p..p7 are live; otherwise, it might
       use the same hardware register for all 8 local variables.  */
    if (p != p1 || p != p2 || p != p3 || p != p4
	|| p != p5 || p != p6 || p != p7)
      _exit(1);

    /* On some systems (e.g. IRIX 3.3),
       vfork doesn't separate parent from child file descriptors.
       If the child closes a descriptor before it execs or exits,
       this munges the parent's descriptor as well.
       Test for this by closing stdout in the child.  */
    _exit(close(fileno(stdout)) != 0);
  } else {
    int status;
    struct stat st;

    while (wait(&status) != child)
      ;
    exit(
	 /* Was there some problem with vforking?  */
	 child < 0

	 /* Did the child fail?  (This shouldn't happen.)  */
	 || status

	 /* Did the vfork/compiler bug occur?  */
	 || parent != getpid()

	 /* Did the file descriptor bug occur?  */
	 || fstat(fileno(stdout), &st) != 0
	 );
  }
}], , AC_DEFINE(vfork, fork))
])dnl
dnl
define(AC_WAIT3,
[echo checking for wait3 that fills in rusage
AC_TEST_PROGRAM([#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
/* HP-UX has wait3 but does not fill in rusage.  */
main() {
  struct rusage r;
  int i;
  r.ru_nvcsw = 0;
  switch (fork()) {
  case 0: /* Child.  */
    sleep(1); /* Give up the CPU.  */
    _exit(0);
  case -1: _exit(0); /* What can we do?  */
  default: /* Parent.  */
    wait3(&i, 0, &r);
    exit(r.ru_nvcsw == 0);
  }
}], AC_DEFINE(HAVE_WAIT3))
])dnl
dnl
define(AC_ALLOCA,
[# The Ultrix 4.2 mips builtin alloca declared by alloca.h only works
# for constant arguments.  Useless!
AC_COMPILE_CHECK(working alloca.h, [#include <alloca.h>],
  [char *p = alloca(2 * sizeof(int));], AC_DEFINE(HAVE_ALLOCA_H))
decl="#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#if HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
char *alloca ();
#endif
#endif
#endif
"
AC_COMPILE_CHECK([alloca], $decl,
[char *p = (char *) alloca(1);], , [alloca_missing=1])
if test -n "$alloca_missing"; then
  # The SVR3 libPW and SVR4 libucb both contain incompatible functions
  # that cause trouble.  Some versions do not even contain alloca or
  # contain a buggy version.  If you still want to use their alloca,
  # use ar to extract alloca.o from them instead of compiling alloca.c.
  ALLOCA=alloca.o
fi
AC_SUBST(ALLOCA)dnl
])dnl
dnl
define(AC_GETLOADAVG,
[# Some definitions of getloadavg require that the program be installed setgid.
AC_SUBST(NEED_SETGID)NEED_SETGID=false

# Check for the 4.4BSD definition of getloadavg.
AC_HAVE_LIBRARY(utils, LIBS="$LIBS -lutils")

# There is a commonly available library for RS/6000 AIX.  Is it installed?
AC_HAVE_LIBRARY(getloadavg, LIBS="$LIBS -lgetloadavg", [dnl
# Since it is not a standard part of AIX, it might be installed locally.
LIBS_save="$LIBS"
LIBS="$LIBS /usr/local/lib/libgetloadavg.a"
AC_COMPILE_CHECK([local getloadavg library], ,
  [double load; int nelem = getloadavg (&load, 1);],
  [getloadavg_missing=false], [getloadavg_missing=true])dnl
if $getloadavg_missing; then
  # It wasn't there.  Restore the old value of LIBS.
  LIBS="$LIBS_save"
fi])
AC_REPLACE_FUNCS(getloadavg)

case "$LIBOBJS" in
*getloadavg*)
AC_HEADER_CHECK(sys/dg_sys_info.h, AC_DEFINE(DGUX) have_sym=1)
if test -z "$have_sym"; then
AC_HEADER_CHECK(dwarf.h, AC_DEFINE(SVR4) LIBS="$LIBS -lelf" have_sym=1)
fi
if test -z "$have_sym"; then
# Solaris 2 does not use dwarf, but it's still SVR4.
AC_HEADER_CHECK(elf.h, AC_DEFINE(SVR4) LIBS="$LIBS -lelf" have_sym=1
  AC_HAVE_LIBRARY(kvm, LIBS="$LIBS -lkvm"))
fi
if test -z "$have_sym"; then
AC_HEADER_CHECK(inq_stats/cpustats.h, AC_DEFINE(UMAX4_3) AC_DEFINE(UMAX) have_sym=1)
fi
if test -z "$have_sym"; then
AC_HEADER_CHECK(sys/cpustats.h, AC_DEFINE(UMAX) have_sym=1)
fi
dnl I think MIPS is supposed to be defined on non-DEC MIPS systems.
dnl But I don't know how to identify them.
dnl AC_HEADER_CHECK(who-knows.h, AC_DEFINE(MIPS) have_sym=1)
if test -z "$have_sym"; then
AC_HEADER_CHECK(nlist.h,
[AC_DEFINE(NLIST_STRUCT)
AC_COMPILE_CHECK(n_un in struct nlist, [#include <nlist.h>],
[struct nlist n; n.n_un.n_name = 0;],
AC_DEFINE(NLIST_NAME_UNION))])dnl
fi

# Figure out whether we will need to install setgid.
AC_PROGRAM_EGREP([Yowza Am I SETGID yet], [dnl
#include "${srcdir}/getloadavg.c"
#ifdef LDAV_PRIVILEGED
Yowza Am I SETGID yet
#endif], [AC_DEFINE(GETLOADAVG_PRIVILEGED) NEED_SETGID=true])dnl
;;
esac
])dnl
dnl
define(AC_UTIME_NULL,
[echo checking utime with null argument
rm -f conftestdata; > conftestdata
# Sequent interprets utime(file, 0) to mean use start of epoch.  Wrong.
AC_TEST_PROGRAM([#include <sys/types.h>
#include <sys/stat.h>
main() {
struct stat s, t;
exit(!(stat ("conftestdata", &s) == 0 && utime("conftestdata", (long *)0) == 0
&& stat("conftestdata", &t) == 0 && t.st_mtime >= s.st_mtime
&& t.st_mtime - s.st_mtime < 120));
}], AC_DEFINE(HAVE_UTIME_NULL))
rm -f core
])dnl
dnl
define(AC_STRCOLL,
[AC_COMPILE_CHECK(strcoll, [#include <string.h>], [strcoll ("abc", "abc");],
  AC_DEFINE(HAVE_STRCOLL))])dnl
dnl
define(AC_SETVBUF_REVERSED,
[AC_TEST_PROGRAM([#include <stdio.h>
main () {
  /* If setvbuf has the reversed format, exit 0. */
  setvbuf(stdout, _IOLBF, (char *) 0, BUFSIZ); /* The reversed way.  */
  putc('\r', stdout);
  exit(0);			/* Non-reversed systems segv here.  */
}], AC_DEFINE(SETVBUF_REVERSED))
rm -f core
])dnl
dnl
dnl
dnl checks for structure members
dnl
dnl
define(AC_STRUCT_TM,
[AC_PROVIDE([$0])AC_COMPILE_CHECK([struct tm in time.h],
[#include <sys/types.h>
#include <time.h>],
[struct tm *tp;], , AC_DEFINE(TM_IN_SYS_TIME))])dnl
dnl
define(AC_TIMEZONE,
[AC_REQUIRE([AC_STRUCT_TM])decl='#include <sys/types.h>
'
case "$DEFS" in
  *TM_IN_SYS_TIME*) decl="$decl #include <sys/time.h>
" ;;
  *) decl="$decl #include <time.h>
" ;;
esac
AC_COMPILE_CHECK([tm_zone in struct tm], $decl,
[struct tm tm; tm.tm_zone;], AC_DEFINE(HAVE_TM_ZONE), no_tm_zone=1)
if test -n "$no_tm_zone"; then
AC_COMPILE_CHECK(tzname, [#include <time.h>
#ifndef tzname /* For SGI.  */
changequote(,)dnl
extern char *tzname[]; /* RS6000 and others want it this way.  */
changequote([,])dnl
#endif], [atoi(*tzname);], AC_DEFINE(HAVE_TZNAME))
fi
])dnl
dnl
define(AC_ST_BLOCKS,
[AC_COMPILE_CHECK([st_blocks in struct stat],
[#include <sys/types.h>
#include <sys/stat.h>], [struct stat s; s.st_blocks;],
AC_DEFINE(HAVE_ST_BLOCKS), LIBOBJS="$LIBOBJS fileblocks.o")dnl
AC_SUBST(LIBOBJS)dnl
])dnl
dnl
define(AC_ST_BLKSIZE,
[AC_COMPILE_CHECK([st_blksize in struct stat],
[#include <sys/types.h>
#include <sys/stat.h>], [struct stat s; s.st_blksize;],
AC_DEFINE(HAVE_ST_BLKSIZE))])dnl
dnl
define(AC_ST_RDEV,
[AC_COMPILE_CHECK([st_rdev in struct stat],
[#include <sys/types.h>
#include <sys/stat.h>], [struct stat s; s.st_rdev;],
AC_DEFINE(HAVE_ST_RDEV))])dnl
dnl
dnl
dnl checks for compiler characteristics
dnl
dnl
define(AC_CROSS_CHECK,
[AC_PROVIDE([$0])echo checking whether cross-compiling
# If we cannot run a trivial program, we must be cross compiling.
AC_TEST_PROGRAM([main(){exit(0);}], , cross_compiling=1)
])dnl
dnl
define(AC_CHAR_UNSIGNED,
[echo checking for unsigned characters
AC_TEST_PROGRAM(
[/* volatile prevents gcc2 from optimizing the test away on sparcs.  */
#if !__STDC__
#define volatile
#endif
main() {
#ifdef __CHAR_UNSIGNED__
  exit(1); /* No need to redefine it.  */
#else
  volatile char c = 255; exit(c < 0);
#endif
}], AC_DEFINE(__CHAR_UNSIGNED__))]
)dnl
dnl
define(AC_INT_16_BITS,
[echo checking integer size
AC_TEST_PROGRAM([main() { exit(!(sizeof(long) > sizeof(int))); }],
 AC_DEFINE(INT_16_BITS))
])dnl
dnl
define(AC_WORDS_BIGENDIAN,
[echo checking byte ordering
AC_TEST_PROGRAM([main () {
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  exit (u.c[sizeof (long) - 1] == 1);
}], , AC_DEFINE(WORDS_BIGENDIAN))
])dnl
dnl
define(AC_ARG_ARRAY,
[echo checking whether the address of an argument can be used as an array
AC_TEST_PROGRAM([main() {
/* Return 0 iff arg arrays are ok.  */
exit(!x(1, 2, 3, 4));
}
x(a, b, c, d) {
  return y(a, &b);
}
/* Return 1 iff arg arrays are ok.  */
y(a, b) int *b; {
  return a == 1 && b[0] == 2 && b[1] == 3 && b[2] == 4;
}], , AC_DEFINE(NO_ARG_ARRAY))
rm -f core
])dnl
dnl
define(AC_INLINE,
[AC_REQUIRE([AC_PROG_CC])if test -n "$GCC"; then
AC_COMPILE_CHECK([inline], , [} inline foo() {], , AC_DEFINE(inline, __inline))
fi
])dnl
define(AC_CONST,
[changequote(,)dnl
prog='/* Ultrix mips cc rejects this.  */
typedef int charset[2]; const charset x;
/* SunOS 4.1.1 cc rejects this. */
char const *const *p;
char **p2;
/* HPUX 7.0 cc rejects these. */
++p;
p2 = (char const* const*) p;'
changequote([,])dnl
AC_COMPILE_CHECK([working const], , [$prog], , AC_DEFINE(const,))])dnl
dnl
dnl
dnl checks for operating system services
dnl
dnl
define(AC_REMOTE_TAPE,
[echo checking for remote tape and socket header files
AC_HEADER_CHECK(sys/mtio.h, AC_DEFINE(HAVE_SYS_MTIO_H) have_mtio=1)
if test -n "$have_mtio"; then
AC_TEST_CPP([#include <sgtty.h>
#include <sys/socket.h>], PROGS="$PROGS rmt")
fi
])dnl
dnl
define(AC_LONG_FILE_NAMES,
[echo checking for long file names
(echo 1 > conftest9012345) 2>/dev/null
(echo 2 > conftest9012346) 2>/dev/null
val=`cat conftest9012345 2>/dev/null`
test -f conftest9012345 && test "$val" = 1 && AC_DEFINE(HAVE_LONG_FILE_NAMES)
rm -f conftest9012345 conftest9012346
])dnl
dnl
define(AC_RESTARTABLE_SYSCALLS,
[echo checking for restartable system calls
AC_TEST_PROGRAM(
[/* Exit 0 (true) if wait returns something other than -1,
   i.e. the pid of the child, which means that wait was restarted
   after getting the signal.  */
#include <sys/types.h>
#include <signal.h>
ucatch (isig) { }
main () {
  int i = fork (), status;
  if (i == 0) { sleep (3); kill (getppid (), SIGINT); sleep (3); exit (0); }
  signal (SIGINT, ucatch);
  status = wait(&i);
  if (status == -1) wait(&i);
  exit (status == -1);
}
], AC_DEFINE(HAVE_RESTARTABLE_SYSCALLS))
])dnl
dnl
dnl
dnl checks for UNIX variants
dnl
dnl
define(AC_AIX,
[echo checking for AIX
AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_BEFORE([$0], [AC_TEST_CPP])AC_PROGRAM_EGREP(yes,
[#ifdef _AIX
  yes
#endif
], AC_DEFINE(_ALL_SOURCE))
])dnl
dnl
define(AC_MINIX,
[AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_BEFORE([$0], [AC_TEST_CPP])AC_HEADER_CHECK(minix/config.h, MINIX=1)
# The Minix shell can't assign to the same variable on the same line!
if test -n "$MINIX"; then
  AC_DEFINE(_POSIX_SOURCE)
  AC_DEFINE(_POSIX_1_SOURCE, 2)
  AC_DEFINE(_MINIX)
fi
])dnl
dnl
define(AC_ISC_POSIX,
[AC_BEFORE([$0], [AC_COMPILE_CHECK])AC_BEFORE([$0], [AC_TEST_PROGRAM])AC_BEFORE([$0], [AC_HEADER_EGREP])AC_BEFORE([$0], [AC_TEST_CPP])echo checking for POSIXized ISC
if test -d /etc/conf/kconfig.d &&
  grep _POSIX_VERSION [/usr/include/sys/unistd.h] >/dev/null 2>&1
then
  ISC=1 # If later tests want to check for ISC.
  AC_DEFINE(_POSIX_SOURCE)
  if test -n "$GCC"; then
    CC="$CC -posix"
  else
    CC="$CC -Xp"
  fi
fi
])dnl
dnl
define(AC_XENIX_DIR,
[AC_REQUIRE([AC_DIR_HEADER])echo checking for Xenix
AC_PROGRAM_EGREP(yes,
[#if defined(M_XENIX) && !defined(M_UNIX)
  yes
#endif
], XENIX=1)
if test -n "$XENIX"; then
  AC_DEFINE(VOID_CLOSEDIR)
  LIBS="$LIBS -lx"
  case "$DEFS" in
  *SYSNDIR*) ;;
  *) LIBS="-ldir $LIBS" ;; # Make sure -ldir precedes any -lx.
  esac
fi
])dnl
dnl
define(AC_SCO_INTL,
[echo checking for SCO UNIX libintl
AC_PROGRAM_EGREP(yes,
[#if defined(M_UNIX)
  yes
#endif
], SCO_UNIX=1)
test -n "$SCO_UNIX" && test -f /lib/libintl.a &&
  LIBS="$LIBS -lintl" # For strftime.
])dnl
dnl
define(AC_IRIX_SUN,
[echo checking for IRIX libsun
if test -f /usr/lib/libsun.a; then
  LIBS="$LIBS -lsun"
fi
])dnl
dnl
define(AC_DYNIX_SEQ,
[echo checking for DYNIX/ptx libseq
AC_PROGRAM_EGREP(yes,
[#if defined(_SEQUENT_)
  yes
#endif
], SEQUENT=1)
test -n "$SEQUENT" && test -f /usr/lib/libseq.a &&
  LIBS="$LIBS -lseq"
])dnl
