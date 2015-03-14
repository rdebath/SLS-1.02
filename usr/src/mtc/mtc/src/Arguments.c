/* $Id: Arguments.c,v 1.4 1991/12/05 18:09:30 grosch rel $ */

#include "SYSTEM_.h"

#ifndef DEFINITION_Arguments
#include "Arguments.h"
#endif

void BEGIN_Arguments ()
{
  static BOOLEAN has_been_called = FALSE;

  if (! has_been_called) {
    has_been_called = TRUE;
  }
}

/*
 *	Implementation of procedures GetArgs and GetEnv
 *	from FOREIGN module Arguments
 */

void GetArgs
# ifdef __STDC__
(SHORTCARD *argc, Arguments_ArgTable *argv)
# else
(argc, argv) SHORTCARD *argc; Arguments_ArgTable *argv;
# endif
{
  *argc = SYSTEM_argc;
  *argv = (Arguments_ArgTable) SYSTEM_argv;
}

void GetEnv
# ifdef __STDC__
(Arguments_ArgTable *env)
# else
(env)
Arguments_ArgTable *env;
# endif
{
  *env = (Arguments_ArgTable) SYSTEM_envp;
}
