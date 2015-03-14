dnl Local additions to Autoconf macros.
dnl Copyright (C) 1992 Free Software Foundation, Inc.
dnl Francois Pinard <pinard@iro.umontreal.ca>, 1992.
dnl
define(FP_PROTOTYPES,
[echo checking for ANSI prototypes
AC_PROGRAM_EGREP(yes,
[#ifdef __STDC__
  yes
#endif
], , U=_)
AC_SUBST(U)])dnl
dnl
define(FP_VARLENGTH_ARRAYS,
[echo checking for variable length arrays
AC_TEST_PROGRAM([f(n)int n;{int array[n];}main(){f(7);exit(0);}
], AC_DEFINE(HAVE_VARLENGTH_ARRAYS))
])dnl
