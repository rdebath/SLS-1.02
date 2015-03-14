/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/

/* This is the top-level configuration file for the metamail distribution. */

/* NOTE:  The RESET_PROGRAM resets the terminal to a "normal" state 
   If you comment out the definition, all will be well except that metamail's
   -R switch won't work, and metamail-called programs might be more likely
   to screw up your terminal state */

#ifdef SYSV
#define RESET_PROGRAM "tput clear"
#else
#define RESET_PROGRAM "/usr/ucb/reset"
#endif

#ifdef SYSV
#define index strchr
#define rindex strrchr
#define NO_RLIMITS 1
#define sigtype void
#endif

#ifdef __hpux
#define NO_RLIMITS 1
/* I've gotten conflicting reports about the best way to reset the terminal 
  state under hpux.  I'm now leaving "tput clear" as the default, but there 
  have been two other suggestions as well.  Your mileage may vary! -- NSB */
#undef RESET_PROGRAM
#define RESET_PROGRAM "tput clear"
/* #define RESET_PROGRAM "/bin/reset" */
/* #define RESET_PROGRAM "/usr/bin/reset" */
#endif

#ifdef __MSDOS__
#undef RESET_PROGRAM
#include <string.h>
#define index  strchr
#define rindex strrchr
#define popen  fopen
#define pclose fclose
#define NO_RLIMITS 1
#endif

#ifdef AMIGA
#undef RESET_PROGRAM
#define index  strchr
#define rindex strrchr
#define NO_RLIMITS 1
#define DEFAULT_SPLIT_SIZE 95000
#endif

/* The following defines the default size at which long
    messages will be split into multiple messages of type
    "message/partial" by the mailto and splitmail commands,
    at least. */
#ifndef DEFAULT_SPLIT_SIZE
#define DEFAULT_SPLIT_SIZE 250000
#endif

#ifndef sigtype
#define sigtype int
#endif

#ifdef __MSDOS__
#define PATH_SEPARATOR ';'
#ifndef STDPATH
#define STDPATH ".\\mailcap;\\mailcap"
#endif
#else
#ifdef AMIGA
#define PATH_SEPARATOR ' '
#ifndef STDPATH
#define STDPATH "uulib:mailcap"
#endif
#else
#define PATH_SEPARATOR ':'
#ifndef STDPATH
#define STDPATH "/.mailcap:/usr/local/etc/mailcap:/usr/etc/mailcap:/etc/mailcap:/usr/public/lib/mailcap"
#endif
#endif
#endif
