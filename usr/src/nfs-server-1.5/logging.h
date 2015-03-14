/*
 * logging.h	Definitions for the logging functions.
 *
 * Version:	@(#)logging.h	1.5	93/04/10
 *
 * Authors:	Donald J. Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */


/* Global Function prototypes. */
extern _PRO( void log_open, (char *progname)				);
extern _PRO( void toggle_logging, (int sig)				);
extern _PRO( void dprintf, (int level, const char *fmt, ...)		);
extern _PRO( void log_call, (struct svc_req *rqstp, char *name,		\
			    char *arg)					);
extern _PRO( char *pr_void, (void)					);
extern _PRO( char *pr_nfs_fh, (nfs_fh *argp)				);
extern _PRO( char *pr_sattrargs, (sattrargs *argp)			);
extern _PRO( char *pr_diropargs, (diropargs *argp)			);
extern _PRO( char *pr_readargs, (readargs *argp)			);
extern _PRO( char *pr_writeargs, (writeargs *argp)			);
extern _PRO( char *pr_createargs, (createargs *argp)			);
extern _PRO( char *pr_renameargs, (renameargs *argp)			);
extern _PRO( char *pr_linkargs, (linkargs *argp)			);
extern _PRO( char *pr_symlinkargs, (symlinkargs *argp)			);
extern _PRO( char *pr_readdirargs, (readdirargs *argp)			);

/* End of LOGGING.H */
