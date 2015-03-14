#include <sys/types.h>
#include <sys/socket.h>

#include <rpc/rpc.h>
#include "nfs_prot.h"

#ifndef HNFSD_H
#define HNFSD_H

union argument_types {
    nfs_fh nfsproc_getattr_2_arg;
    sattrargs nfsproc_setattr_2_arg;
    diropargs nfsproc_lookup_2_arg;
    nfs_fh nfsproc_readlink_2_arg;
    readargs nfsproc_read_2_arg;
    writeargs nfsproc_write_2_arg;
    createargs nfsproc_create_2_arg;
    diropargs nfsproc_remove_2_arg;
    renameargs nfsproc_rename_2_arg;
    linkargs nfsproc_link_2_arg;
    symlinkargs nfsproc_symlink_2_arg;
    createargs nfsproc_mkdir_2_arg;
    diropargs nfsproc_rmdir_2_arg;
    readdirargs nfsproc_readdir_2_arg;
    nfs_fh nfsproc_statfs_2_arg;
};
extern union argument_types argument;

union result_types {
    attrstat	attrstat;
    diropres	diropres;
    readlinkres readlinkres;
    readres	readres;
    nfsstat	nfsstat;
    readdirres	readdirres;
    statfsres	statfsres;
};
extern union result_types result;

typedef struct options
{
	/* uid/gid mapping functions */
	enum {map_daemon, identity}	uidmap;
	int	root_squash;
	/* client options */
	int	secure_port;
	int	read_only;
	int	link_relative;
}
	options;

typedef struct clnt_param
{
	struct clnt_param	*next;

	struct in_addr	clnt_addr;
	char	*clnt_name;
	char	*mount_point;
	options	o;
}
	clnt_param;

/* Defined in main.c */
extern void mallocfailed(void);

/* Defined in auth_clnt.c */
extern clnt_param *auth_clnt(struct svc_req *, char *path);

/* Defined in init_auth.c */
void init_auth(char *filename);

#endif /* HNFSD_H */


