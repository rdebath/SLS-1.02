/* main.c - main() and some logging routines for expect

Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#include <stdio.h>
#ifndef M_XENIX
  /* netdb.h is not available on SCO XENIX 386, unless you bought
     the TCP/IP Development Package.  <pf@artcom0.north.de> 26-Jan-92 : */
#include <netdb.h>
#endif
#include <sys/types.h>
#ifdef CRAY
#include <sys/inode.h>	/* Cray 5.1 needs this, I don't know why */
#endif
#include <sys/file.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <sys/time.h>
#include <errno.h>
#include "exp_tty.h"
#include "tcl.h"
#include "string.h"

#if 0
#include "tclInt.h"

#ifndef va_dcl
/* unfortunately, tclInt.h includes varargs, which on some systems, namely */
/* SunOS 4.0.3, does protect against reinclusion.  Do so here. */
#include <varargs.h>
#endif

#endif

#include "exp_global.h"
#include "exp_rename.h"
#include "exp_command.h"
#include "exp_log.h"
#include "exp_main.h"

void
main(argc, argv)
int argc;
char *argv[];
{
	Tcl_Interp *interp = Tcl_CreateInterp();

	exp_init(interp);
	exp_parse_argv(interp,argc,argv);

	/* become interactive if requested or "nothing to do" */
	if (exp_interactive || (!exp_cmdfile && !exp_cmdlinecmds))
		(void) exp_interpreter(interp);

	if (exp_cmdfile) exp_interpret_cmdfile(interp,exp_cmdfile);

	exp_exit(interp,0);
}

