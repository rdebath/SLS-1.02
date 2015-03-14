dnl Parameterized macros that do not check for something specific.
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
changequote([,])dnl
undefine([eval])dnl
undefine([include])dnl
dnl
dnl
dnl Utility functions for stamping the configure script.
dnl
dnl
define(AC_ACVERSION, 1.3)dnl
dnl This is defined by the --version option of the autoconf script.
ifdef([AC_PRINT_VERSION], [errprint(Autoconf version AC_ACVERSION
)])dnl
dnl
dnl These are currently not used, for the sake of people who diff
dnl configure scripts and don't want spurious differences.
dnl But they are too clever to just delete.
dnl
define(AC_USER, [esyscmd(
changequote({,})dnl
# Extract the user name from the first pair of parentheses.
({sedcmd='s/[^(]*(\([^)]*\)).*/\1/';}
changequote([,])dnl
whoami || id|sed "$sedcmd") 2>/dev/null|tr -d '\012')])dnl
dnl
define(AC_HOST, [esyscmd((hostname || uname -n) 2>/dev/null|tr -d '\012')])dnl
dnl
define(AC_DATE, [esyscmd(date|tr -d '\012')])dnl
dnl
dnl
dnl Controlling Autoconf operation
dnl
dnl
dnl This is separate from AC_INIT to prevent GNU m4 1.0 from coredumping
dnl when AC_CONFIG_HEADER is used.
define(AC_NOTICE,
[# Guess values for system-dependent variables and create Makefiles.
# Generated automatically using autoconf.
dnl [#] Generated automatically using autoconf version AC_ACVERSION
dnl [#] by AC_USER@AC_HOST on AC_DATE
# Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Usage: configure [--srcdir=DIR] [--host=HOST] [--gas] [--nfp] [--no-create]
#        [--prefix=PREFIX] [--exec-prefix=PREFIX] [--with-PACKAGE] [TARGET]
# Ignores all args except --srcdir, --prefix, --exec-prefix, --no-create, and
# --with-PACKAGE unless this script has special code to handle it.
])dnl
dnl
define(AC_PARSEARGS,[
for arg
do
  # Handle --exec-prefix with a space before the argument.
  if test x$next_exec_prefix = xyes; then exec_prefix=$arg; next_exec_prefix=
  # Handle --host with a space before the argument.
  elif test x$next_host = xyes; then next_host=
  # Handle --prefix with a space before the argument.
  elif test x$next_prefix = xyes; then prefix=$arg; next_prefix=
  # Handle --srcdir with a space before the argument.
  elif test x$next_srcdir = xyes; then srcdir=$arg; next_srcdir=
  else
    case $arg in
     # For backward compatibility, also recognize exact --exec_prefix.
     -exec-prefix=* | --exec_prefix=* | --exec-prefix=* | --exec-prefi=* | --exec-pref=* | --exec-pre=* | --exec-pr=* | --exec-p=* | --exec-=* | --exec=* | --exe=* | --ex=* | --e=*)
changequote(,)dnl
	exec_prefix=`echo $arg | sed 's/[-a-z_]*=//'` ;;
changequote([,])dnl
     -exec-prefix | --exec_prefix | --exec-prefix | --exec-prefi | --exec-pref | --exec-pre | --exec-pr | --exec-p | --exec- | --exec | --exe | --ex | --e)
	next_exec_prefix=yes ;;

     -gas | --gas | --ga | --g) ;;

     -host=* | --host=* | --hos=* | --ho=* | --h=*) ;;
     -host | --host | --hos | --ho | --h)
	next_host=yes ;;

     -nfp | --nfp | --nf) ;;

     -no-create | --no-create | --no-creat | --no-crea | --no-cre | --no-cr | --no-c | --no- | --no)
        no_create=1 ;;

     -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
changequote(,)dnl
	prefix=`echo $arg | sed 's/[-a-z_]*=//'` ;;
changequote([,])dnl
     -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
	next_prefix=yes ;;

     -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=* | --s=*)
changequote(,)dnl
	srcdir=`echo $arg | sed 's/[-a-z_]*=//'` ;;
changequote([,])dnl
     -srcdir | --srcdir | --srcdi | --srcd | --src | --sr | --s)
	next_srcdir=yes ;;

     -with-* | --with-*)
       package=`echo $arg|sed 's/-*with-//'`
       # Delete all the valid chars; see if any are left.
changequote(,)dnl
       if test -n "`echo $package|sed 's/[-a-zA-Z0-9_]*//g'`"; then
changequote([,])dnl
         echo "configure: $package: invalid package name" >&2; exit 1
       fi
       eval "with_`echo $package|sed s/-/_/g`=1" ;;

     *) ;;
    esac
  fi
done
])dnl
dnl
define(AC_INIT,
[#!/bin/sh
AC_NOTICE
AC_PARSEARGS
AC_PREPARE($1)])dnl
dnl
define(AC_PREPARE,
[trap 'rm -f conftest* core; exit 1' 1 3 15

rm -f conftest*
compile='${CC-cc} $DEFS conftest.c -o conftest $LIBS >/dev/null 2>&1'

# A filename unique to this package, relative to the directory that
# configure is in, which we can look for to find out if srcdir is correct.
unique_file=$1

# Find the source files, if location was not specified.
if test -z "$srcdir"; then
  srcdirdefaulted=yes
  # Try the directory containing this script, then `..'.
  prog=[$]0
changequote(,)dnl
  confdir=`echo $prog|sed 's%/[^/][^/]*$%%'`
changequote([,])dnl
  test "X$confdir" = "X$prog" && confdir=.
  srcdir=$confdir
  if test ! -r $srcdir/$unique_file; then
    srcdir=..
  fi
fi
if test ! -r $srcdir/$unique_file; then
  if test x$srcdirdefaulted = xyes; then
    echo "configure: Can not find sources in \`${confdir}' or \`..'." 1>&2
  else
    echo "configure: Can not find sources in \`${srcdir}'." 1>&2
  fi
  exit 1
fi
# Preserve a srcdir of `.' to avoid automounter screwups with pwd.
# But we can't avoid them for `..', to make subdirectories work.
case $srcdir in
  .|/*|~*) ;;
  *) srcdir=`cd $srcdir; pwd` ;; # Make relative path absolute.
esac
])dnl
dnl
dnl Protects the argument from being diverted twice
dnl if this macro is called twice for it.
dnl Diversion 0 is the normal output.
dnl Diversion 1 is sed substitutions for output files.
dnl Diversion 2 is variable assignments for config.status.
define(AC_SUBST,
[ifdef([AC_SUBST_$1], ,
[define([AC_SUBST_$1], )dnl
divert(1)dnl
s%@$1@%[$]$1%g
divert(2)dnl
$1='[$]$1'
divert(0)dnl
])])dnl
dnl
define(AC_WITH,
[echo checking whether --with-$1 was given
withvar=`echo with_$1|sed s/-/_/g`
withval="`eval echo '$'$withvar`"
if test -n "$withval"; then
  ifelse([$2], , :, [$2])
ifelse([$3], , , [else 
  $3
])dnl
fi
])dnl
dnl
dnl Guess the value for the `prefix' variable by looking for
dnl the argument program along PATH and taking its parent.
dnl Example: if the argument is `gcc' and we find /usr/local/gnu/bin/gcc, 
dnl set `prefix' to /usr/local/gnu.
define(AC_PREFIX,
[if test -z "$prefix"
then
  echo checking for $1 to derive installation directory prefix
  saveifs="$IFS"; IFS="$IFS:"
  for dir in $PATH; do
    test -z "$dir" && dir=.
    if test $dir != . && test -f $dir/$1; then
changequote(,)dnl
      # Not all systems have dirname.
      prefix=`echo $dir|sed 's%/[^/][^/]*$%%'`
changequote([,])dnl
      break
    fi
  done
  IFS="$saveifs"
fi
])dnl
dnl
define(AC_CONFIG_HEADER, [define(AC_CONFIG_NAME, $1)])dnl
dnl
dnl Don't compare $2 to a blank, so we can support "-Dfoo=".
dnl If creating a configuration header file, we add
dnl commands to SEDDEFS to define the variable.  SED[due][ABCD]
dnl get defined in config.status.  Here we just insert the
dnl variable parts of the string: the variable name to define
dnl and the value to give it.
define(AC_DEFINE,
[ifelse($#, 2, ifelse(regexp($2, \W), -1, DEFS="$DEFS -D$1=$2", DEFS="$DEFS -D'$1=$2'"), DEFS="$DEFS -D$1=1")ifdef([AC_CONFIG_NAME],
[
ifelse($#, 2, [SEDDEFS="${SEDDEFS}\${SEDdA}$1\${SEDdB}$1\${SEDdC}$2\${SEDdD}
\${SEDuA}$1\${SEDuB}$1\${SEDuC}$2\${SEDuD}
\${SEDeA}$1\${SEDeB}$1\${SEDeC}$2\${SEDeD}
"], [SEDDEFS="${SEDDEFS}\${SEDdA}$1\${SEDdB}$1\${SEDdC}1\${SEDdD}
\${SEDuA}$1\${SEDuB}$1\${SEDuC}1\${SEDuD}
\${SEDeA}$1\${SEDeB}$1\${SEDeC}1\${SEDeD}
"])])])dnl
dnl
define(AC_BEFORE,
[ifdef([AC_PROVIDE_$2], [errprint(__file__:__line__: [$2 was called before $1
])])])dnl
dnl
define(AC_REQUIRE,
[ifdef([AC_PROVIDE_$1],,[indir([$1])
])])dnl
dnl
define(AC_PROVIDE,
[define([AC_PROVIDE_$1],)])dnl
dnl
dnl
dnl Checks for kinds of features
dnl
dnl
define(AC_PROGRAM_CHECK,
[if test -z "[$]$1"; then
  echo checking for $2
  saveifs="$IFS"; IFS="${IFS}:"
  for dir in $PATH; do
    test -z "$dir" && dir=.
    if test -f $dir/$2; then
      $1="$3"
      break
    fi
  done
  IFS="$saveifs"
fi
test -z "[$]$1" && $1="$4"
AC_SUBST($1)dnl
])dnl
dnl
define(AC_PROGRAMS_CHECK,
[for p in $2
do
AC_PROGRAM_CHECK($1, [$]p, [$]p, )
test -n "[$]$1" && break
done
])dnl
dnl
define(AC_HEADER_EGREP,
[AC_REQUIRE([AC_PROG_CPP])AC_PROVIDE([$0])echo '#include <$2>' > conftest.c
eval "$CPP $DEFS conftest.c > conftest.out 2>&1"
if egrep "$1" conftest.out >/dev/null 2>&1; then
  ifelse([$3], , :, [$3])
ifelse([$4], , , [else 
  $4
])dnl
fi
rm -f conftest*
])dnl
dnl
dnl Because this macro is used by AC_GCC_TRADITIONAL, which must come early,
dnl it is not included in AC_BEFORE checks.
define(AC_PROGRAM_EGREP,
[AC_REQUIRE([AC_PROG_CPP])AC_PROVIDE([$0])cat > conftest.c <<EOF
[$2]
EOF
eval "$CPP $DEFS conftest.c > conftest.out 2>&1"
if egrep "$1" conftest.out >/dev/null 2>&1; then
  ifelse([$3], , :, [$3])
ifelse([$4], , , [else 
  $4
])dnl
fi
rm -f conftest*
])dnl
dnl
define(AC_HEADER_CHECK,
[echo checking for $1
ifelse([$3], , [AC_TEST_CPP([#include <$1>], [$2])],
[AC_TEST_CPP([#include <$1>], [$2], [$3])])
])dnl
dnl
define(AC_COMPILE_CHECK,
[AC_PROVIDE([$0])echo checking for $1
cat > conftest.c <<EOF
[$2]
main() { exit(0); } 
t() { [$3] }
EOF
dnl Don't try to run the program, which would prevent cross-configuring.
if eval $compile; then
  ifelse([$4], , :, [$4])
ifelse([$5], , , [else
  $5
])dnl
fi
rm -f conftest*]
)dnl
dnl
define(AC_TEST_PROGRAM,
[AC_PROVIDE([$0])ifelse([$4], , , [AC_REQUIRE([AC_CROSS_CHECK])if test -n "$cross_compiling"
then
  $4
else
])dnl
cat > conftest.c <<EOF
[$1]
EOF
eval $compile
if test -s conftest && (./conftest; exit) 2>/dev/null; then
  ifelse([$2], , :, [$2])
ifelse([$3], , , [else
  $3
])dnl
fi
ifelse([$4], , , fi
)dnl
rm -f conftest*])dnl
dnl
define(AC_TEST_CPP,
[AC_REQUIRE([AC_PROG_CPP])cat > conftest.c <<EOF
[$1]
EOF
err=`eval "$CPP $DEFS conftest.c 2>&1 >/dev/null"`
if test -z "$err"; then
  ifelse([$2], , :, [$2])
ifelse([$3], , , [else
  $3
])dnl
fi
rm -f conftest*])dnl
dnl
define(AC_REPLACE_FUNCS,
[for func in $1
do
AC_COMPILE_CHECK([${func}], ,
[
/* Override any gcc2 internal prototype to avoid an error.  */
extern char ${func}(); ${func}();], , [LIBOBJS="$LIBOBJS ${func}.o"])
done
AC_SUBST(LIBOBJS)dnl
])dnl
dnl
define(AC_FUNC_CHECK,
[ifelse([$3], , [AC_COMPILE_CHECK($1, [#include <stdio.h>], [
#ifdef __stub_$1
choke me
#else
/* Override any gcc2 internal prototype to avoid an error.  */
extern char $1(); $1();
#endif
],
$2)], [AC_COMPILE_CHECK($1, [#include <stdio.h>], [
#ifdef __stub_$1
choke me
#else
/* Override any gcc2 internal prototype to avoid an error.  */
extern char $1(); $1();
#endif
],
$2, $3)])dnl
#endif
])dnl
dnl
define(AC_HAVE_FUNCS,
[for func in $1
do
changequote(,)dnl
trfunc=HAVE_`echo $func | tr '[a-z]' '[A-Z]'`
changequote([,])dnl
AC_FUNC_CHECK(${func},
AC_DEFINE(${trfunc}))dnl
done
])dnl
dnl
define(AC_HAVE_HEADERS,
[for hdr in $1
do
changequote(,)dnl
trhdr=HAVE_`echo $hdr | tr '[a-z]./' '[A-Z]__'`
changequote([,])dnl
AC_HEADER_CHECK(${hdr},
AC_DEFINE(${trhdr}))dnl
done
])dnl
dnl
define(AC_HAVE_LIBRARY, [dnl
changequote(,)dnl
libname=`echo "$1" | sed 's/lib\([^\.]*\)\.a/\1/;s/-l//'`
changequote([,])dnl
LIBS_save="${LIBS}"
LIBS="${LIBS} -l${libname}"
have_lib=""
AC_COMPILE_CHECK([-l$1], , [main();], [have_lib="1"])dnl
LIBS="${LIBS_save}"
ifelse($#, 1, [dnl
if test -n "${have_lib}"; then
   changequote(,)dnl
   deflibname=`echo "${libname}" | tr '[a-z]' '[A-Z]'`
   changequote([,])dnl
   AC_DEFINE(HAVE_LIB${deflibname})
   LIBS="${LIBS} -l${libname}"
fi
], [dnl
if test -n "${have_lib}"; then
   :; $2
else
   :; $3
fi
])])dnl
dnl
dnl
dnl The big finish
dnl
dnl
define(AC_OUTPUT,
[changequote(,)dnl
if test -n "$prefix"; then
  test -z "$exec_prefix" && exec_prefix='${prefix}'
  prsub="s%^prefix\\([ 	]*\\)=\\([ 	]*\\).*$%prefix\\1=\\2$prefix%"
fi
if test -n "$exec_prefix"; then
  prsub="$prsub
s%^exec_prefix\\([ 	]*\\)=\\([ 	]*\\).*$%\
exec_prefix\\1=\\2$exec_prefix%"
fi
changequote([,])dnl
AC_SUBST(LIBS)dnl
AC_SUBST(srcdir)dnl
ifdef([AC_CONFIG_NAME],
[divert(1)dnl
s%@DEFS@%-DHAVE_CONFIG_H%],
[divert(1)dnl
s%@DEFS@%$DEFS%]
[divert(2)dnl
DEFS='$DEFS'
])dnl
divert(2)dnl
prefix='$prefix'
exec_prefix='$exec_prefix'
prsub='$prsub'
divert(0)dnl

trap 'rm -f config.status; exit 1' 1 3 15
echo creating config.status
rm -f config.status
cat > config.status <<EOF
#!/bin/sh
# Generated automatically by configure.
# Run this file to recreate the current configuration.
# This directory was configured as follows,
# on host `(hostname || uname -n) 2>/dev/null`:
#
[#] [$]0 [$]*

for arg
do
  case "[\$]arg" in
    -recheck | --recheck | --rechec | --reche | --rech | --rec | --re | --r)
    exec /bin/sh [$]0 [$]* ;;
    *) echo "Usage: config.status [--recheck]" 2>&1; exit 1 ;;
  esac
done

ifdef([AC_CONFIG_NAME],
[trap 'rm -f $1 AC_CONFIG_NAME conftest*; exit 1' 1 3 15],
[trap 'rm -f $1; exit 1' 1 3 15])
undivert(2)dnl
EOF
cat >> config.status <<\EOF

top_srcdir=$srcdir
changequote(,)dnl
for file in .. $1; do if [ "x$file" != "x.." ]; then
  srcdir=$top_srcdir
  # Remove last slash and all that follows it.  Not all systems have dirname.
  dir=`echo $file|sed 's%/[^/][^/]*$%%'`
changequote([,])dnl
  if test "$dir" != "$file"; then
    test "$top_srcdir" != . && srcdir=$top_srcdir/$dir
    test ! -d $dir && mkdir $dir
  fi
  echo creating $file
  rm -f $file
  echo "# Generated automatically from `echo $file|sed 's|.*/||'`.in by configure." > $file
  sed -e "
$prsub
undivert(1)dnl
" $top_srcdir/${file}.in >> $file
fi; done

ifdef([AC_CONFIG_NAME],
[echo creating AC_CONFIG_NAME
changequote(<<,>>)dnl
# These sed commands are put into SEDDEFS when defining a macro.
# They are broken into pieces to make the sed script easier to manage.
# They are passed to sed as "A NAME B NAME C VALUE D", where NAME
# is the cpp macro being defined and VALUE is the value it is being given.
# Each defining turns into a single global substitution command.
#
# SEDd sets the value in "#define NAME VALUE" lines.
SEDdA='s@^\([ 	]*\)#\([ 	]*define[ 	][ 	]*\)'
SEDdB='\([ 	][ 	]*\)[^ 	]*@\1#\2'
SEDdC='\3'
SEDdD='@g'
# SEDu turns "#undef NAME" with trailing blanks into "#define NAME VALUE".
SEDuA='s@^\([ 	]*\)#\([ 	]*\)undef\([ 	][ 	]*\)'
SEDuB='\([ 	]\)@\1#\2define\3'
SEDuC=' '
SEDuD='\4@g'
# SEDe turns "#undef NAME" without trailing blanks into "#define NAME VALUE".
SEDeA='s@^\([ 	]*\)#\([ 	]*\)undef\([ 	][ 	]*\)'
SEDeB='<<$>>@\1#\2define\3'
SEDeC=' '
SEDeD='@g'
changequote([,])dnl
rm -f conftest.sed
cat > conftest.sed <<CONFEOF
EOF
# Turn off quoting long enough to insert the sed commands.
cat >> config.status <<EOF
$SEDDEFS
EOF
cat >> config.status <<\EOF
CONFEOF
rm -f conftest.h
# Break up the sed commands because old seds have small limits.
cp $top_srcdir/AC_CONFIG_NAME.in conftest.h1
while :
do
  lines=`grep -c . conftest.sed`
  if test -z "$lines" || test "$lines" -eq 0; then break; fi
  rm -f conftest.s1 conftest.s2 conftest.h2
  sed 40q conftest.sed > conftest.s1 # Like head -40.
  sed 1,40d conftest.sed > conftest.s2 # Like tail +41.
  sed -f conftest.s1 < conftest.h1 > conftest.h2
  rm -f conftest.s1 conftest.h1 conftest.sed
  mv conftest.h2 conftest.h1
  mv conftest.s2 conftest.sed
done
rm -f conftest.sed conftest.h
echo "/* AC_CONFIG_NAME.  Generated automatically by configure.  */" > conftest.h
cat conftest.h1 >> conftest.h
rm -f conftest.h1
if cmp -s AC_CONFIG_NAME conftest.h 2>/dev/null; then
  # The file exists and we would not be changing it.
  rm -f conftest.h
else
  rm -f AC_CONFIG_NAME
  mv conftest.h AC_CONFIG_NAME
fi

])dnl
EOF
chmod +x config.status
test -n "$no_create" || ./config.status
])dnl
dnl
