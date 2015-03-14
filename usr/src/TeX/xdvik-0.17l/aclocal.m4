dnl Additional Autoconf definitions.
dnl
define(AC_PROG_GCC_G,
[AC_REQUIRE([AC_PROG_CC])dnl
if test -n "$GCC"
then CC="`echo $CC | sed s/-O/-g/`"
fi
])dnl
dnl
dnl Need to link with more libraries to use X under ISC.
define(AC_ISC_WLIBS,
[echo checking for ISC X window libraries
if test -n "$ISC"
then wlibs="$wlibs -lnsl_s -linet"
fi
AC_SUBST(wlibs)dnl
])dnl
dnl
dnl Look for the X11 include files in various places, in case they're
dnl not in the compiler's path.  Substitute for `xincludedir' and `xlibdir'.
define(AC_X11_LOCATION,
[echo checking for X11 headers and libraries
dir=""
AC_TEST_CPP([#include <X11/Xaw/Box.h>], :,
  if test -r /usr/local/[[include]]/X11/Xaw/Box.h
  then dir=/usr/local/[[include]]
  elif test -r /usr/lpp/X11/Xamples/[[include]]/Box.h
  then dir=/usr/lpp/X11/Xamples/[[include]]
  elif test -r /usr/[[include]]/X11R4/Xaw/Box.h
  then dir=/usr/[[include]]/X11R4
  elif test -r /usr/[[include]]/X11R5/Xaw/Box.h
  then dir=/usr/[[include]]/X11R5
  elif test -r /usr/X11/[[include]]/Box.h
  then dir=/usr/X11/[[include]]
  elif test -r /usr/X11R5/[[include]]/Box.h
  then dir=/usr/X11R5/[[include]]
  fi
)
dnl Can't use AC_SUBST inside AC_TEST_CPP.
if test -n "$dir"
then xincludedir=-I$dir
     AC_SUBST(xincludedir)
fi
# Now check for the libraries.  Amazing how every single X vendor puts
# these in a different place, and all because MIT thought they should go
# in /usr/lib.
dir1=""
if test -r /usr/local/lib/libXaw.a
then dir1=/usr/local/lib
elif test -r /usr/lpp/X11/Xamples/lib/Xaw/libXaw.a
then dir1=/usr/lpp/X11/Xamples/lib/Xaw
elif test -r /usr/lib/X11R4/libX11.sl
then dir1=/usr/lib/X11R4
elif test -r /usr/X11/lib/libXaw.a
then dir1=/usr/X11/lib
elif test -r /usr/X11R5/lib/libXaw.a
then dir1=/usr/X11R5/lib
fi
dir2=""
if test -r /usr/lpp/X11/Xamples/lib/Xmu/libXmu.a
then dir2=/usr/lpp/X11/Xamples/lib/Xmu
fi
test -n "$dir1" && xlibdir=-L$dir1
test -n "$dir2" && xlibdir="$xlibdir -L$dir2"
AC_SUBST(xlibdir)dnl
])dnl
