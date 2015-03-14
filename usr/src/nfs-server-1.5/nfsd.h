/*
 * nfsd		This program implements a user-space NFS server.
 *
 * Version:	@(#)nfsd.h	1.5	93/04/10
 *
 * Authors:	Mark A. Shand, May 1988
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988 Mark A. Shand
 *
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */
#include <sys/param.h>			/* needed for MAXPATHLEN	*/
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>			/* needed for select() et al	*/
#include <sys/ioctl.h>			/* needed for ioctl(2)		*/
#include <sys/socket.h>
#include <arpa/nameser.h>		/* needed for <resolv.h>	*/
#include <arpa/inet.h>			/* for inet_ntoa(3)		*/
#include <rpc/rpc.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>			/* needed for gethostbyname	*/
#include <resolv.h>			/* needed for h_errno	YUCK	*/
#include <signal.h>
#include <stdarg.h>			/* needed for va_arg et al	*/
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <time.h>			/* needed for time_t / ctime(3)	*/
#include <unistd.h>			/* needed for lseek(2) et al	*/
#include "mount.h"
#include "nfs_prot.h"


union argument_types {
    nfs_fh	nfsproc_getattr_2_arg;
    sattrargs	nfsproc_setattr_2_arg;
    diropargs	nfsproc_lookup_2_arg;
    nfs_fh	nfsproc_readlink_2_arg;
    readargs	nfsproc_read_2_arg;
    writeargs	nfsproc_write_2_arg;
    createargs	nfsproc_create_2_arg;
    diropargs	nfsproc_remove_2_arg;
    renameargs	nfsproc_rename_2_arg;
    linkargs	nfsproc_link_2_arg;
    symlinkargs	nfsproc_symlink_2_arg;
    createargs	nfsproc_mkdir_2_arg;
    diropargs	nfsproc_rmdir_2_arg;
    readdirargs nfsproc_readdir_2_arg;
    nfs_fh	nfsproc_statfs_2_arg;
};

union result_types {
  attrstat	attrstat;
  diropres	diropres;
  readlinkres	readlinkres;
  readres	readres;
  nfsstat	nfsstat;
  readdirres	readdirres;
  statfsres	statfsres;
};

typedef struct options {
  enum {map_daemon, identity}	uidmap;	/* uid/gid mapping functions	*/
  int				root_squash;
  int				secure_port;	/* client options	*/
  int				read_only;
  int				link_relative;
} options;

typedef struct clnt_param {
  struct clnt_param	*next;
  struct in_addr	clnt_addr;
  char			*clnt_name;
  char			*mount_point;
  options		o;
} clnt_param;


/* Include the other module definitions. */
#ifdef ANSI
#   define _PRO(f, a)	f a
#else
#   define _PRO(f, a)	f ()
#endif
#include "auth.h"
#include "fh.h"
#include "logging.h"


/* Global variables. */
extern union argument_types argument;
extern union result_types result;


/* Global Function prototypes. */
extern _PRO( void mallocfailed, (void)					);
extern _PRO( nfsstat getattr, (nfs_fh *fh, fattr *attr,			\
			       struct stat *stat_optimize)		);


/* End of NFSD.H */
