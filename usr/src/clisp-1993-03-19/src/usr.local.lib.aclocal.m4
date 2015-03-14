dnl local autoconf macros
dnl Bruno Haible 18.3.1993
dnl
define(CL_CC_GCC,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo checking whether using GNU C
AC_PROGRAM_EGREP(yes,[#ifdef __GNUC__
  yes
#endif
], CC_GCC=true, CC_GCC=false)dnl
AC_SUBST(CC_GCC)dnl
])dnl
dnl
define(CL_CC_ANSI,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo checking whether using ANSI C
AC_PROGRAM_EGREP(yes,[#ifdef __STDC__
  yes
#endif
], CC_ANSI=true, CC_ANSI=false)dnl
AC_SUBST(CC_ANSI)dnl
])dnl
dnl
define(CL_CC_NEED_CCPAUX,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo checking whether CPP likes indented directives
AC_PROGRAM_EGREP([#.*foo],[ #define foo], CC_NEED_CCPAUX=true, CC_NEED_CCPAUX=false)dnl
AC_SUBST(CC_NEED_CCPAUX)dnl
])dnl
dnl
define(CL_CC_NEED_DEELIF,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo "checking whether CPP understands #elif"
AC_TEST_CPP([#if 0
#elif 1
#else
#endif], CC_NEED_DEELIF=false, CC_NEED_DEELIF=true)dnl
AC_SUBST(CC_NEED_DEELIF)dnl
])dnl
dnl
define(CL_CC_NEED_DEERROR,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo "checking whether CPP understands #error"
AC_TEST_CPP([#if 0
#error "bla"
#endif], CC_NEED_DEERROR=false, CC_NEED_DEERROR=true)dnl
AC_SUBST(CC_NEED_DEERROR)dnl
])dnl
dnl
define(CL_CC_NEED_DEEMA,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo checking whether CPP likes empty macro arguments
AC_TEST_CPP([#define divide(x,y,q_zuw,r_zuw) (r_zuw(x)-(q_zuw(x)/(y))*(y))
foo(x,y) int x,y; { int q; divide(x,y,q=,); return q; }],
CC_NEED_DEEMA=false, CC_NEED_DEEMA=true)dnl
AC_SUBST(CC_NEED_DEEMA)dnl
])dnl
dnl
define(CL_CC_NEED_MERGESTRINGS,
[AC_BEFORE([AC_PROG_CPP], [$0])AC_PROVIDE([$0])
echo checking whether CC merges adjacent strings
changequote(,)dnl
echo "char foo [] = \"abc\" \"def\"; main() { exit(0); }" > conftest.c
changequote([,])dnl
if eval $compile; then
  CC_NEED_MERGESTRINGS=false
else
  CC_NEED_MERGESTRINGS=true
fi
AC_SUBST(CC_NEED_MERGESTRINGS)dnl
])dnl
dnl
define(CL_AS_UNDERSCORE,
[echo checking for underscore in external names
cat > conftest.c <<EOF
int foo() { return 0; }
EOF
${CC-cc} -c conftest.c >/dev/null 2>&1
if nm conftest.o | grep _foo >/dev/null 2>&1 ; then
  AS_UNDERSCORE=true
else
  AS_UNDERSCORE=false
fi
AC_SUBST(AS_UNDERSCORE)dnl
rm -f conftest*
])dnl
dnl
define(CL_PROG_RANLIB, [AC_PROGRAM_CHECK(RANLIB, ranlib, ranlib, [\\\#])])dnl
dnl
define(CL_CP,
[echo checking how to copy files
echo "blabla" > conftest.x
err=`/bin/sh -c "cp -p conftest.x conftest.y 2>&1"`
if test -z "$err"; then
  CP='cp -p'
else
  CP='cp'
fi
rm -f conftest*
AC_SUBST(CP)dnl
])dnl
dnl
define(CL_HPUX,
[echo checking for broken HP-UX shell
# A program that outputs its argument count:
cat > conftest.c <<EOF
#include <stdio.h>
main (argc,argv) int argc; char** argv; { printf("%d\n",argc); return 0; }
EOF
eval $compile
# How can a shell script forward its arguments to another program?
#                                    $ * "$ *" "$ @"
# conftest.sh                         1    2     1 (*)
# conftest.sh foo                     2    2     2
# conftest.sh foo bar                 3    2     3
# conftest.sh "foo bar"               3    2     2
# (*): 2 with HP-UX /bin/sh. We must use /bin/ksh instead.
psubs='"$''@"'
cat > conftest.sh <<EOF
#!/bin/sh
exec ./conftest $psubs
EOF
chmod a+x conftest.sh
if test `./conftest.sh` = "1"; then
  GOOD_SH='/bin/sh'
else
  GOOD_SH='/bin/ksh'
  AC_DEFINE(UNIX_USE_KSH)
fi
AC_SUBST(GOOD_SH)dnl
rm -f conftest*
])dnl
dnl
define(RL_VOID,
[AC_COMPILE_CHECK([working void], ,
[void f();
typedef void x; x g();
typedef void* y; y a;
], , AC_DEFINE(void,char))dnl
])dnl
dnl
define(CL_VOID,
[AC_COMPILE_CHECK([working void], ,
[void f();
typedef void x; x g();
typedef void* y; y a;
], have_void=1, AC_DEFINE(void,char))dnl
if test -n "$have_void"; then
AC_COMPILE_CHECK([working \"return void\"],
[void f(); typedef void x; x g() { return f(); }], [;],
AC_DEFINE(return_void,[return]))dnl
fi
])dnl
dnl
define(CL_LONGLONG,
[echo checking for long long type
AC_TEST_PROGRAM([main()
{ long x = 944938507; long y = 737962842; long z = 162359677;
  exit(!(((long long) x)*((long long) y)>>32 == z)); }],
AC_DEFINE(HAVE_LONGLONG))
])dnl
dnl
define(CL_STDC_HEADERS,
dnl This is AC_STDC_HEADERS from Autoconf 1.2. The AC_STDC_HEADERS from
dnl Autoconf 1.3 fails on 386BSD because it checks for correct ANSI ctype
dnl macros and 386BSD (as well as SGI's /bin/cc from Irix-4.0.5) doesn't
dnl have them. But we don't need them!
dnl The same holds for the mem* functions in <string.h> and SunOS.
[echo checking for ANSI C header files
AC_TEST_CPP([#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <float.h>
#include <limits.h>], AC_DEFINE(STDC_HEADERS))
])dnl
dnl
define(CL_OPENFLAGS,
dnl BSD systems require #include <sys/file.h> for O_RDWR etc. being defined.
[AC_BEFORE([$0], [CL_MMAP])AC_PROVIDE([$0])
AC_HEADER_CHECK(sys/file.h, have_sysfile=1)dnl
if test -n "$have_sysfile"; then
openflags_decl='#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#include <fcntl.h>
'
openflags_prog='int x = O_RDWR | O_RDONLY | O_WRONLY | O_CREAT | O_TRUNC;'
AC_COMPILE_CHECK([O_RDWR in fcntl.h], $openflags_decl, $openflags_prog, openflags_ok=1)dnl
if test -z "$openflags_ok"; then
dnl AC_COMPILE_CHECK([O_RDWR in sys/file.h], $openflags_decl[#include <sys/file.h>], $openflags_prog,
AC_DEFINE(NEED_SYS_FILE_H)
dnl openflags_ok=1)dnl
fi
fi
])dnl
dnl
define(CL_UTSNAME,
[AC_HAVE_HEADERS(sys/utsname.h)]
)dnl
dnl
define(CL_NETDB,
[AC_BEFORE([$0], [CL_GETHOSTBYNAME])AC_PROVIDE([$0])
AC_HAVE_HEADERS(netdb.h)]
)dnl
dnl
define(CL_SHM,
[AC_HEADER_CHECK(sys/shm.h, , no_shm=1)dnl
if test -z "$no_shm"; then
AC_HEADER_CHECK(sys/ipc.h, , no_shm=1)dnl
fi
if test -z "$no_shm"; then
AC_DEFINE(HAVE_SHM)
AC_HAVE_HEADERS(sys/sysmacros.h)dnl
fi
])dnl
dnl
define(RL_TERM,
[AC_HAVE_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
case "$DEFS" in
  *HAVE_TERMIOS_H*)
AC_HAVE_FUNCS(tcgetattr tcflow)dnl
  ;;
  *) ;;
esac
AC_HAVE_HEADERS(sys/stream.h sys/ptem.h)dnl
ioctl_decl='#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#if defined(HAVE_TERMIOS_H) && defined(HAVE_TCGETATTR) && defined(HAVE_TCFLOW)
#include <termios.h>
#else
#if defined(HAVE_TERMIO_H) || defined(HAVE_SYS_TERMIO_H)
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#else
#include <termio.h>
#endif
#else
#include <sgtty.h>
#include <sys/ioctl.h>
#endif
#endif
#ifdef HAVE_SYS_STREAM_H
#include <sys/stream.h>
#endif
#ifdef HAVE_SYS_PTEM_H
#include <sys/ptem.h>
#endif
'
ioctl_prog='int x = FIONREAD;'
AC_COMPILE_CHECK([FIONREAD], $ioctl_decl, $ioctl_prog, ioctl_ok=1)dnl
if test -z "$ioctl_ok"; then
AC_COMPILE_CHECK([FIONREAD in sys/filio.h],
$ioctl_decl[#include <sys/filio.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_FILIO_H)
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
AC_COMPILE_CHECK([FIONREAD in sys/ioctl.h],
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H)
ioctl_ok=1)dnl
fi
])dnl
dnl
define(CL_TERM,
[AC_BEFORE([$0], [CL_IOCTL])AC_PROVIDE([$0])
AC_HAVE_HEADERS(termios.h termio.h sys/termio.h sgtty.h)dnl
case "$DEFS" in
  *HAVE_TERMIOS_H*)
AC_COMPILE_CHECK([TCSAFLUSH in termios.h], [#include <termios.h>],
[int x = TCSAFLUSH;], AC_DEFINE(HAVE_TCSAFLUSH))dnl
  ;;
  *) ;;
esac
])dnl
dnl
define(CL_OFF_T,
[echo checking for off_t in sys/types.h
AC_HEADER_EGREP(off_t, sys/types.h, , AC_DEFINE(off_t, int))]
)dnl
dnl
define(CL_CADDR_T,
[echo checking for caddr_t in sys/types.h
AC_HEADER_EGREP(caddr_t, sys/types.h, , AC_DEFINE(caddr_t, void*))]
)dnl
dnl
define(CL_CLOCK_T,
[echo checking for clock_t in sys/types.h etc.
AC_HEADER_EGREP(clock_t, sys/types.h, have_clock=1)dnl
if test -z "$have_clock"; then
AC_HEADER_EGREP(clock_t, sys/times.h, have_clock=1)dnl
fi
if test -z "$have_clock"; then
AC_HEADER_EGREP(clock_t, time.h, have_clock=1)dnl
fi
if test -z "$have_clock"; then
AC_DEFINE(clock_t, int)
fi
])dnl
dnl
define(CL_DIRENT_WITHOUT_NAMLEN,
[AC_COMPILE_CHECK([d_namlen in struct dirent], [#include <dirent.h>],
[struct dirent d; d.d_namlen;], , AC_DEFINE(DIRENT_WITHOUT_NAMLEN))]
)dnl
dnl
define(CL_MALLOC,
[echo checking for malloc declaration
AC_HEADER_EGREP([void.*\*.*malloc], stdlib.h, malloc_void=1)dnl
if test -z "$malloc_void"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
extern void* malloc();
main() { exit(0); }],
malloc_void=1)
fi
if test -n "$malloc_void"; then
AC_DEFINE(RETMALLOCTYPE,[void*])
else
AC_DEFINE(RETMALLOCTYPE,[char*])
fi
])dnl
dnl
define(CL_FREE,
[echo checking for free declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
extern int free();
main() { exit(0); }],
AC_DEFINE(RETFREETYPE,[int]),
AC_DEFINE(RETFREETYPE,[void]))]
)dnl
dnl
define(CL_ALLOCA,
[# The Ultrix 4.2 mips builtin alloca declared by alloca.h only works
# for constant arguments.  Useless!
AC_COMPILE_CHECK(working alloca.h, [#include <alloca.h>],
  [char *p = alloca(2 * sizeof(int));], AC_DEFINE(HAVE_ALLOCA_H))dnl
decl="#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef HAVE_ALLOCA_H
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
[char *p = (char *) alloca(1);], , [alloca_missing=1])dnl
if test -n "$alloca_missing"; then
  # The SVR3 libPW and SVR4 libucb both contain incompatible functions
  # that cause trouble.  Some versions do not even contain alloca or
  # contain a buggy version.  If you still want to use their alloca,
  # use ar to extract alloca.o from them instead of compiling alloca.c.
  ALLOCA=alloca.o
  AC_DEFINE(NO_ALLOCA)
fi
AC_SUBST(ALLOCA)dnl
])dnl
dnl
define(CL_SETJMP,
[AC_FUNC_CHECK(_setjmp, , no__jmp=1)dnl
if test -z "$no__jmp"; then
AC_FUNC_CHECK(_longjmp, , no__jmp=1)dnl
fi
if test -z "$no__jmp"; then
AC_DEFINE(HAVE__JMP)
fi
AC_HEADER_EGREP([void.* longjmp], setjmp.h, , AC_DEFINE(LONGJMP_RETURNS))
])dnl
dnl
define(RL_RETSIGTYPE,
[AC_COMPILE_CHECK([return type of signal handlers],
[#include <sys/types.h>
#include <signal.h>
#ifdef signal
#undef signal
#endif
extern int (*signal ()) ();],
[int i;], , AC_DEFINE(RETSIGTYPE_VOID))
])dnl
dnl
define(CL_SIGNAL_REINSTALL,
[echo checking whether signals handlers need to be reinstalled
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
int gotsig=0;
RETSIGTYPE sigalrm_handler() { gotsig=1; }
typedef RETSIGTYPE (*signal_handler) ();
main() { /* returns 0 if they need not to be reinstalled */
  signal(SIGALRM,(signal_handler)sigalrm_handler); alarm(1); while (!gotsig);
  exit(!( (signal_handler)signal(SIGALRM,(signal_handler)sigalrm_handler)
          == (signal_handler)sigalrm_handler
      ) );
}], , AC_DEFINE(SIGNAL_NEED_REINSTALL))
])dnl
dnl
define(CL_SIGNALBLOCK,
[AC_FUNC_CHECK(sighold, AC_DEFINE(SIGNALBLOCK_SYSV), )dnl
AC_HEADER_EGREP(sigset_t, signal.h, , signals_not_posix=1)dnl
if test -z "$signals_not_posix"; then
AC_FUNC_CHECK(sigprocmask, AC_DEFINE(SIGNALBLOCK_POSIX), )dnl
fi
AC_FUNC_CHECK(sigblock, AC_DEFINE(SIGNALBLOCK_BSD), )dnl
])dnl
dnl
define(CL_SIGPROCMASK,
[case "$DEFS" in
  *SIGNALBLOCK_POSIX*)
echo checking for sigprocmask declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#ifdef __STDC__
extern int sigprocmask (int how, sigset_t* set, sigset_t* oset);
#else
extern int sigprocmask();
#endif
main() { exit(0); }], , AC_DEFINE(SIGPROCMASK_CONST,const))
  ;;
  *) ;;
esac
])dnl
dnl
define(CL_ABORT,
[echo checking for abort declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
extern int abort();
main() { exit(0); }],
AC_DEFINE(RETABORTTYPE,[int]),
AC_DEFINE(RETABORTTYPE,[void]))]
)dnl
dnl
define(CL_SYS_ERRLIST,
[echo checking for sys_errlist declaration
changequote(,)dnl
brackets='[]'
changequote([,])dnl
AC_TEST_PROGRAM([
#include <errno.h>
extern char* sys_errlist $brackets ;
main() { exit(0); }], , AC_DEFINE(SYS_ERRLIST_CONST,const))
])dnl
dnl
define(CL_GETENV,
[echo checking for getenv declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern char* getenv (char* name);
#else
extern char* getenv();
#endif
main() { exit(0); }], , AC_DEFINE(GETENV_CONST,const))
])dnl
dnl
define(CL_VFORK,
[echo checking for vfork declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif
#ifdef __STDC__
extern pid_t vfork (void);
#else
extern pid_t vfork();
#endif
main() { exit(0); }],
AC_DEFINE(RETVFORKTYPE,pid_t),
AC_DEFINE(RETVFORKTYPE,int))
])dnl
dnl
define(CL_EXECV,
[echo checking for execv declaration
for z in '' 'const'; do
for y in '' 'const'; do
for x in '' 'const'; do
if test -z "$have_execv"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int execv ($x char* path, $y char* $z argv[]);
#else
extern int execv();
#endif
main() { exit(0); }],
AC_DEFINE(EXECV_CONST,$x)
AC_DEFINE(EXECV1_CONST,$y)
AC_DEFINE(EXECV2_CONST,$z)
have_execv=1)
fi
done
done
done
])dnl
dnl
define(CL_EXECL,
[echo checking for execl declaration
for x in '' 'const'; do
if test -z "$have_execl"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int execl (EXECV_CONST char* path, $x char* arg, ...);
#else
extern int execl();
#endif
main() { exit(0); }],
AC_DEFINE(EXECL_CONST,$x)
AC_DEFINE(EXECL_DOTS)
have_execl=1)
fi
done
for x in '' 'const'; do
if test -z "$have_execl"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int execl (EXECV_CONST char* path, $x char* arg0, $x char* arg1, $x char* arg2, $x char* arg3);
#else
extern int execl();
#endif
main() { exit(0); }],
AC_DEFINE(EXECL_CONST,$x)
have_execl=1)
fi
done
])dnl
dnl
define(CL_WAITPID,
[AC_FUNC_CHECK(waitpid, AC_DEFINE(HAVE_WAITPID) have_waitpid=1, )dnl
if test -n "$have_waitpid"; then
echo checking for waitpid declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern pid_t waitpid (pid_t pid, int* statusp, int options);
#else
extern pid_t waitpid();
#endif
main() { exit(0); }], AC_DEFINE(PID_T,pid_t), AC_DEFINE(PID_T,int))
fi
])dnl
dnl
define(CL_RUSAGE,
[AC_HAVE_HEADERS(sys/resource.h sys/times.h)dnl
case "$DEFS" in
  *HAVE_SYS_RESOURCE_H*)
    AC_COMPILE_CHECK([getrusage], [#include <sys/time.h>
#include <sys/resource.h>],
      [struct rusage x; int y = RUSAGE_SELF; getrusage(y,&x);],
      AC_DEFINE(HAVE_GETRUSAGE))dnl
    ;;
  *) ;;
esac
])dnl
dnl
define(CL_GETWD,
[AC_COMPILE_CHECK([getwd], , [getwd();], AC_DEFINE(HAVE_GETWD), )]
)dnl
dnl
define(CL_GETCWD,
[case "$DEFS" in
  *HAVE_GETWD*) ;;
  *)
echo checking for getcwd declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern char* getcwd (char* buf, int bufsize);
#else
extern char* getcwd();
#endif
main() { exit(0); }],
AC_DEFINE(GETCWD_SIZE_T,[int]), AC_DEFINE(GETCWD_SIZE_T,[size_t]))
  ;;
esac
])dnl
dnl
define(CL_CHDIR,
[echo checking for chdir declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int chdir (char* path);
#else
extern int chdir();
#endif
main() { exit(0); }], , AC_DEFINE(CHDIR_CONST,const))
])dnl
dnl
define(CL_MKDIR,
[echo checking for mkdir declaration
AC_HEADER_EGREP(mode_t, sys/types.h,
dnl mode_t defined. check if it is really used by mkdir() :
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef __STDC__
extern int mkdir (char* path, mode_t mode);
#else
extern int mkdir();
#endif
main() { exit(0); }],
mode_t_unneeded=1)
if test -z "$mode_t_unneeded"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef __STDC__
extern int mkdir (const char* path, mode_t mode);
#else
extern int mkdir();
#endif
main() { exit(0); }],
mode_t_unneeded=1)
fi)dnl
if test -n "$mode_t_unneeded"; then
AC_DEFINE(MODE_T, mode_t)
else
AC_DEFINE(MODE_T, int)
fi
dnl Now MODE_T should be correct, check for const:
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef __STDC__
extern int mkdir (char* path, MODE_T mode);
#else
extern int mkdir();
#endif
main() { exit(0); }], , AC_DEFINE(MKDIR_CONST,const))
])dnl
dnl
define(CL_RMDIR,
[echo checking for rmdir declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int rmdir (char* path);
#else
extern int rmdir();
#endif
main() { exit(0); }], , AC_DEFINE(RMDIR_CONST,const))
])dnl
dnl
define(CL_STAT,
[echo checking for stat declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef __STDC__
extern int stat (char* path, struct stat * buf);
#else
extern int stat();
#endif
main() { exit(0); }], , AC_DEFINE(STAT_CONST,const))
])dnl
dnl
define(CL_LSTAT,
[echo checking for lstat declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef __STDC__
extern int lstat (char* path, struct stat * buf);
#else
extern int lstat();
#endif
main() { exit(0); }], , AC_DEFINE(LSTAT_CONST,const))
])dnl
dnl
define(CL_READLINK,
[echo checking for readlink declaration
for z in 'int' 'size_t'; do
for y in 'char*' 'void*'; do
for x in '' 'const'; do
if test -z "$have_rl"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int readlink ($x char* path, $y buf, $z bufsiz);
#else
extern int readlink();
#endif
main() { exit(0); }],
AC_DEFINE(READLINK_CONST,$x)
AC_DEFINE(READLINK_BUF_T,$y)
AC_DEFINE(READLINK_SIZE_T,$z)
have_rl=1)
fi
done
done
done
])dnl
dnl
define(CL_OPENDIR,
[case "$DEFS" in
  *DIRENT*)
echo checking for opendir declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <dirent.h>
#ifdef __STDC__
extern DIR* opendir (char* dirname);
#else
extern DIR* opendir();
#endif
main() { exit(0); }], , AC_DEFINE(OPENDIR_CONST,const))
  ;;
  *) ;;
esac
])dnl
dnl
define(CL_OPEN,
[echo checking for open declaration
for y in 'MODE_T mode' '...'; do
for x in '' 'const'; do
if test -z "$have_open"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef __STDC__
extern int open ($x char* path, int flags, $y);
#else
extern int open();
#endif
main() { exit(0); }],
AC_DEFINE(OPEN_CONST,$x)
if test "$y" = "..."; then
AC_DEFINE(OPEN_DOTS)
fi
have_open=1)
fi
done
done
])dnl
dnl
define(CL_READ_WRITE,
[echo checking for read, write declarations
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
extern int read();
main() { exit(0); }],
AC_DEFINE(RETRWTYPE,[int]),AC_DEFINE(RETRWTYPE,[long]))
for y in 'int' 'size_t' 'unsigned RETRWTYPE' 'off_t'; do
for x in 'char*' 'void*'; do
if test -z "$have_rw"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern RETRWTYPE read (int fd, $x buf, $y count);
#else
extern RETRWTYPE read();
#endif
main() { exit(0); }],
AC_DEFINE(RW_BUF_T,$x)
AC_DEFINE(RW_SIZE_T,$y)
have_rw=1)
fi
done
done
])dnl
dnl
define(CL_WRITE,
[echo checking for write declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern RETRWTYPE write (int fd, RW_BUF_T buf, RW_SIZE_T count);
#else
extern RETRWTYPE write();
#endif
main() { exit(0); }], , AC_DEFINE(WRITE_CONST,const))
])dnl
dnl
define(CL_RENAME,
[echo checking for rename declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#ifdef __STDC__
extern int rename (char* oldpath, char* newpath);
#else
extern int rename();
#endif
main() { exit(0); }], , AC_DEFINE(RENAME_CONST,const))
])dnl
dnl
define(CL_UNLINK,
[echo checking for unlink declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int unlink (char* path);
#else
extern int unlink();
#endif
main() { exit(0); }], , AC_DEFINE(UNLINK_CONST,const))
])dnl
dnl
define(CL_FSYNC,
[AC_FUNC_CHECK(fsync, AC_DEFINE(HAVE_FSYNC), )]
)dnl
dnl
define(CL_IOCTL,
[AC_REQUIRE([CL_TERM])
ioctl_decl='#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif
#ifndef HAVE_TCSAFLUSH
#undef HAVE_TERMIOS_H
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#else
#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#else
#ifdef HAVE_TERMIO_H
#include <termio.h>
#else
#ifdef HAVE_SGTTY_H
#include <sgtty.h>
#include <sys/ioctl.h>
#endif
#endif
#endif
#endif
'
echo checking for ioctl declaration
for y in 'caddr_t arg' '...'; do
for x in 'int' 'unsigned long'; do
if test -z "$have_ioctl"; then
AC_TEST_PROGRAM($ioctl_decl[
#ifdef __STDC__
extern int ioctl (int fd, $x request, $y);
#else
extern int ioctl();
#endif
main() { exit(0); }],
AC_DEFINE(IOCTL_REQUEST_T,$x)
if test "$y" = "..."; then
AC_DEFINE(IOCTL_DOTS)
fi
have_ioctl=1)
fi
done
done
ioctl_prog='int x = FIONREAD;'
AC_COMPILE_CHECK([FIONREAD], $ioctl_decl, $ioctl_prog, ioctl_ok=1)dnl
if test -z "$ioctl_ok"; then
AC_COMPILE_CHECK([FIONREAD in sys/filio.h],
$ioctl_decl[#include <sys/filio.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_FILIO_H)
ioctl_ok=1)dnl
fi
if test -z "$ioctl_ok"; then
AC_COMPILE_CHECK([FIONREAD in sys/ioctl.h],
$ioctl_decl[#include <sys/ioctl.h>], $ioctl_prog,
AC_DEFINE(NEED_SYS_IOCTL_H)
ioctl_ok=1)dnl
fi
if test -n "$ioctl_ok"; then
AC_DEFINE(HAVE_FIONREAD)
fi
])dnl
dnl
define(CL_SELECT,
[AC_FUNC_CHECK(select, AC_DEFINE(HAVE_SELECT), have_select=1)
if test -z "$have_select"; then
echo checking for select declaration
for z in '' 'const'; do
for y in 'fd_set' 'int'; do
for x in 'int' 'size_t'; do
if test -z "$have_select"; then
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/time.h>
#ifdef __STDC__
extern int select ($x width, $y * readfds, $y * writefds, $y * exceptfds, $z struct timeval * timeout);
#else
extern int select();
#endif
main() { exit(0); }],
AC_DEFINE(SELECT_WIDTH_T,$x)
AC_DEFINE(SELECT_SET_T,$y)
AC_DEFINE(SELECT_CONST,$z)
have_select=1)
fi
done
done
done
fi
])dnl
dnl
define(CL_UALARM,
[AC_FUNC_CHECK(ualarm, AC_DEFINE(HAVE_UALARM), )]
)dnl
dnl
define(CL_SETITIMER,
[AC_FUNC_CHECK(setitimer, AC_DEFINE(HAVE_SETITIMER) [have_setitimer=1], )
if test -n "$have_setitimer"; then
echo checking for setitimer declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/time.h>
#ifdef __STDC__
extern int setitimer (int which, struct itimerval * ivalue, struct itimerval * ovalue);
#else
extern int setitimer();
#endif
main() { exit(0); }], , AC_DEFINE(SETITIMER_CONST,const))
fi
])dnl
dnl
define(CL_USLEEP,
[AC_FUNC_CHECK(usleep, AC_DEFINE(HAVE_USLEEP), )]
)dnl
dnl
define(CL_LOCALTIME,
[echo checking for localtime declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef TM_IN_SYS_TIME
#include <sys/time.h>
#else
#include <time.h>
#endif
#ifdef __STDC__
extern struct tm * localtime (time_t* clock);
#else
extern struct tm * localtime();
#endif
main() { exit(0); }], , AC_DEFINE(LOCALTIME_CONST,const))
])dnl
dnl
define(CL_GETPWNAM,
[echo checking for getpwnam declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <pwd.h>
#ifdef __STDC__
extern struct passwd * getpwnam (char* name);
#else
extern struct passwd * getpwnam();
#endif
main() { exit(0); }], , AC_DEFINE(GETPWNAM_CONST,const))
])dnl
dnl
define(CL_GETHOSTNAME,
[AC_FUNC_CHECK(gethostname, AC_DEFINE(HAVE_GETHOSTNAME) have_gethostname=1, )dnl
if test -n "$have_gethostname"; then
echo checking for gethostname declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __STDC__
extern int gethostname (char* name, int namelen);
#else
extern int gethostname();
#endif
main() { exit(0); }],
AC_DEFINE(GETHOSTNAME_SIZE_T,[int]),
AC_DEFINE(GETHOSTNAME_SIZE_T,[size_t]))
fi
])dnl
dnl
define(CL_GETHOSTBYNAME,
[AC_REQUIRE([CL_NETDB])
case "$DEFS" in
  *HAVE_NETDB_H*) have_netdb=1 ;;
  *) AC_HEADER_CHECK(sun/netdb.h, have_netdb=1) ;;
esac
if test -n "$have_netdb"; then
AC_DEFINE(HAVE_GETHOSTBYNAME)
echo checking for gethostbyname declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_NETDB_H
#include <sys/socket.h>
#include <netdb.h>
#else
#include <sun/netdb.h>
#endif
#ifdef __STDC__
extern struct hostent * gethostbyname (char* name);
#else
extern struct hostent * gethostbyname();
#endif
main() { exit(0); }], , AC_DEFINE(GETHOSTBYNAME_CONST,const))
fi
])dnl
dnl
define(CL_GETPAGESIZE,
[AC_COMPILE_CHECK([getpagesize], , [getpagesize();],
AC_DEFINE(HAVE_GETPAGESIZE) [have_getpagesize=1])dnl
if test -n "$have_getpagesize"; then
echo checking for getpagesize declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
extern int getpagesize();
main() { exit(0); }],
AC_DEFINE(RETGETPAGESIZETYPE,[int]),
AC_DEFINE(RETGETPAGESIZETYPE,[size_t]))
fi
])dnl
dnl
define(CL_VADVISE,
[AC_COMPILE_CHECK([vadvise], [#include <sys/vadvise.h>], [vadvise(0);],
AC_DEFINE(HAVE_VADVISE)
)])dnl
dnl
define(CL_MMAP,
[AC_REQUIRE([CL_OPENFLAGS])
AC_HEADER_CHECK(sys/mman.h, , no_mmap=1)dnl
if test -z "$no_mmap"; then
AC_FUNC_CHECK(mmap, , no_mmap=1)dnl
if test -z "$no_mmap"; then
AC_DEFINE(HAVE_MMAP)
echo checking for mmap declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/mman.h>
#ifdef __STDC__
extern caddr_t mmap (caddr_t addr, int length, int prot, int flags, int fd, off_t off);
#else
extern caddr_t mmap();
#endif
main() { exit(0); }],
AC_DEFINE(MMAP_SIZE_T,[int]), AC_DEFINE(MMAP_SIZE_T,[size_t]))
echo checking for working mmap
mmap_prog_1='
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef NEED_SYS_FILE_H
#include <sys/file.h>
#endif
#include <sys/types.h>
#include <sys/mman.h>
#ifdef __STDC__
extern caddr_t mmap (caddr_t addr, MMAP_SIZE_T length, int prot, int flags, int fd, off_t off);
#else
extern caddr_t mmap();
#endif
main () {
'
mmap_prog_2='
#define my_shift 24
#define my_size  4*1024*1024
#define my_low   1
#define my_high  63
 {long i;
  for (i=my_low; i<=my_high; i++)
    { caddr_t addr = (caddr_t)(i << my_shift);
      if (mmap(addr,my_size,PROT_READ|PROT_WRITE,flags|MAP_FIXED,fd,0) < 0) exit(1);
    }
#define x(i)  *(unsigned char *) ((i<<my_shift) + (i*i))
#define y(i)  (unsigned char)((3*i-4)*(7*i+3))
  for (i=my_low; i<=my_high; i++) { x(i) = y(i); }
  for (i=my_high; i>=my_low; i--) { if (x(i) != y(i)) exit(1); }
  exit(0);
 }
'
AC_TEST_PROGRAM([$mmap_prog_1
#ifdef MAP_ANON
  int flags = MAP_ANON | MAP_PRIVATE;
  int fd = -1;
$mmap_prog_2
#else
 exit(1);
#endif
}], AC_DEFINE(HAVE_MMAP_ANON))
AC_TEST_PROGRAM([$mmap_prog_1
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
  int flags = MAP_FILE | MAP_PRIVATE;
  int fd = open("/dev/zero",O_RDONLY,0666);
  if (fd<0) exit(1);
$mmap_prog_2
}], AC_DEFINE(HAVE_MMAP_DEVZERO))
fi
fi
])dnl
dnl
define(CL_SHMAT,
[case "$DEFS" in
  *HAVE_SHM*)
echo checking for shmat declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
extern void* shmat();
main() { exit(0); }],
AC_DEFINE(RETSHMATTYPE,[void*]),
AC_DEFINE(RETSHMATTYPE,[char*]))
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#ifdef __STDC__
extern RETSHMATTYPE shmat (int shmid, RETSHMATTYPE shmaddr, int shmflg);
#else
extern RETSHMATTYPE shmat();
#endif
main() { exit(0); }], , AC_DEFINE(SHMAT_CONST,const))
  ;;
  *) ;;
esac
])dnl
dnl
define(CL_SHMCTL,
[case "$DEFS" in
  *HAVE_SHM*)
echo checking for shmctl declaration
AC_TEST_PROGRAM([
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#ifdef __STDC__
extern int shmctl (int shmid, int cmd, struct shmid_ds * buf);
#else
extern int shmctl();
#endif
main() { exit(0); }], , AC_DEFINE(SHMCTL_DOTS))
  ;;
  *) ;;
esac
])dnl
dnl
define(CL_MACHINE,
[echo checking for integer types and behaviour
cp ${srcdir}/machine.c conftest.c
ORIGCC="$CC"
if test -n "$GCC"; then
# gcc -O (gcc version <= 2.3.2) crashes when compiling long long shifts for
# target 80386. Strip "-O".
CC=`echo "$CC " | sed -e 's/-O //g'`
fi
eval $compile
CC="$ORIGCC"
if test -s conftest; then
  echo creating machine.h
  ./conftest > conftest.h
  if cmp -s machine.h conftest.h 2>/dev/null; then
    # The file exists and we would not be changing it
    rm -f conftest.h
  else
    rm -f machine.h
    mv conftest.h machine.h
  fi
fi
rm -f conftest*
])dnl
dnl
define(CL_WORDS_LITTLEENDIAN,
[echo checking byte ordering
AC_TEST_PROGRAM([main () {
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  exit (u.c[0] == 1);
}], , AC_DEFINE(WORDS_LITTLEENDIAN))])dnl
dnl
