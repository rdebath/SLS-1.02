#include <stdio.h>		/* Just for FILE *debuglog */
#include <syslog.h>
#include "nfsd.h"
#include "libc.h"


union argument_types 	argument;
union result_types	result;
clnt_param *cp;			/* To pass the current option list around */

extern FILE *debuglog;
extern void logcall(struct svc_req *rqstp, char *name, char *arg);

extern int
    nfsd_nfsproc_null_2(), nfsd_nfsproc_getattr_2(), nfsd_nfsproc_setattr_2(),
    nfsd_nfsproc_root_2(), nfsd_nfsproc_lookup_2(), nfsd_nfsproc_readlink_2(),
    nfsd_nfsproc_read_2(), nfsd_nfsproc_writecache_2(), nfsd_nfsproc_write_2(),
    nfsd_nfsproc_create_2(), nfsd_nfsproc_remove_2(), nfsd_nfsproc_rename_2(),
    nfsd_nfsproc_link_2(), nfsd_nfsproc_symlink_2(), nfsd_nfsproc_mkdir_2(),
    nfsd_nfsproc_rmdir_2(), nfsd_nfsproc_readdir_2(), nfsd_nfsproc_statfs_2();

extern char *print_void(), *print_nfs_fh(), *print_diropargs(),
    *print_readargs(), *print_writeargs(), *print_createargs(),
    *print_renameargs(), *print_linkargs(), *print_symlinkargs(),
    *print_createargs(), *print_readdirargs(), *print_sattrargs();


/* Allow the following to be over-ridden at compile time. */

#define ROOT_UID	0		/* Root's user id */

#ifndef NOBODY_UID
#define NOBODY_UID	((uid_t) -2)	/* The Unpriviledged User */
#endif

#ifndef NOBODY_GID
#define NOBODY_GID	((gid_t) -2)	/* The Unpriviledged Group */
#endif

int eff_uid = 0;		/* Current effective IDs */
int eff_gid = 0;

/* This is a dispatch table to simplify error checking, and supply return */
/* attributes for NFS functions. */

#define table_ent(auth, ro, cred, res_type, arg_type, funct) \
    { auth, ro, cred, sizeof(res_type), sizeof(arg_type),\
    xdr_##res_type, xdr_##arg_type,\
    nfsd_nfsproc_##funct##_2, #funct, print_##arg_type }

struct dispatch_entry {
    int authenticate;		/* Zero if op permitted to any requestor. */
    int read_only;		/* Zero if op permitted on RO filesystems. */
    int credentials;		/* Zero if no creditials are needed. */
    int res_size, arg_size;	/* Sizeof the result/argument structures. */
    bool_t (*xdr_result)(), (*xdr_argument)();
    int (*funct)();
    char *name;
    char *(*log_print)();
};

static struct dispatch_entry dtable[] = {
    table_ent(0, 0, 0,	void,		void,		null),
    table_ent(1, 0, 1,	attrstat,	nfs_fh,		getattr),
    table_ent(1, 1, 1,	attrstat,	sattrargs,	setattr),
    table_ent(0, 0, 0,	void,		void,		root),
    table_ent(1, 0, 1,	diropres,	diropargs,	lookup),
    table_ent(1, 0, 1,	readlinkres,	nfs_fh,		readlink),
    table_ent(1, 0, 1,	readres,	readargs,	read),
    table_ent(0, 0, 0,	void,		void,		writecache),
    table_ent(1, 1, 1,	attrstat,	writeargs,	write),
    table_ent(1, 1, 1,	diropres,	createargs,	create),
    table_ent(1, 1, 1,	nfsstat,	diropargs,	remove),
    table_ent(1, 1, 1,	nfsstat,	renameargs,	rename),
    table_ent(1, 1, 1,	nfsstat,	linkargs,	link),
    table_ent(1, 1, 1,	nfsstat,	symlinkargs,	symlink),
    table_ent(1, 1, 1,	diropres,	createargs,	mkdir),
    table_ent(1, 1, 1,	nfsstat,	diropargs,	rmdir),
    table_ent(1, 0, 1,	readdirres,	readdirargs,	readdir),
    table_ent(1, 0, 0,	statfsres,	nfs_fh,		statfs),
};

void
nfs_dispatch(rqstp, transp)
     struct svc_req *rqstp;
     SVCXPRT *transp;
{
    unsigned int proc_index = rqstp->rq_proc;
    struct dispatch_entry *dent;
    
    if (proc_index >= (sizeof(dtable)/sizeof(dtable[0]))) {
	svcerr_noproc(transp);
	return;
    }
    dent = &dtable[proc_index];
    
    bzero(&argument, dent->arg_size);
    if (!svc_getargs(transp, dent->xdr_argument, &argument)) {
	svcerr_decode(transp);
	return;
    }

    /* Clear the result structure. */
    bzero(&result, dent->res_size);

    /* Log the call, if we're currently logging. */
    if (debuglog)
	logcall(rqstp, dent->name, dent->log_print(&argument));

    /* Check the authentication for most functions. */
    if (dent->authenticate  &&  ((cp = auth_clnt(rqstp, NULL)) == NULL)) {
	result.nfsstat = NFSERR_ACCES;
	goto report_status;
    }

    /* Punt a write-type request if we are READ_ONLY or a RO mount. */
#ifdef READ_ONLY
    if (dent->read_only) {
	result.nfsstat = NFSERR_ROFS;
	goto report_status;
    }
#else
    if (dent->read_only  &&  cp->o.read_only) {
	result.nfsstat = NFSERR_ROFS;
	goto report_status;
    }
#endif

    /* Establish the credentials and set the effective user IDs. */
    if (dent->credentials) {
	int cred_uid, cred_gid;
	if (rqstp->rq_cred.oa_flavor == AUTH_UNIX) {
	    struct authunix_parms *unix_cred;
	    unix_cred = (struct authunix_parms *) rqstp->rq_clntcred;
	    cred_uid = unix_cred->aup_uid;
	    cred_gid = unix_cred->aup_gid;
	} else {
	    cred_uid = NOBODY_UID;
	    cred_gid = NOBODY_GID;
	}
	/* To set the group ID we first need to be root.  What a pain. */
	if (eff_gid != cred_gid) {
	    if (eff_uid != ROOT_UID) {
		if (setreuid(-1, ROOT_UID) < 0)
		    syslog(LOG_DAEMON|LOG_ERR, "Unable to reset my uid to root: %s.",
			   sys_errlist[errno]);
		else
		    eff_uid = ROOT_UID;
	    }
	    if (setregid(-1, cred_gid) < 0)
		syslog(LOG_DAEMON|LOG_ERR,
		       "Unable to setregid:  regid = %d,%d  target gid = %d  %s.",
		       getgid(), getegid(), cred_gid, sys_errlist[errno]);
	    else
		eff_gid = cred_gid;
	}
	if (eff_uid != cred_uid) {
	    if (eff_uid != ROOT_UID  &&  setreuid(-1, ROOT_UID) < 0)
		    syslog(LOG_DAEMON|LOG_ERR, "Unable to reset my uid to root: %s.",
			   sys_errlist[errno]);
	    if (setreuid(-1, cred_uid) < 0)
		syslog(LOG_DAEMON|LOG_ERR,
		       "Unable to setreuid:  reuid = %d,%d  target uid:%d  %s.",
		       getuid(), geteuid(), cred_uid, sys_errlist[errno]);
	    else
		eff_uid = cred_uid;
	}
    }

    /* Do the function function call. */
    result.nfsstat = (*dent->funct)(&argument);

 report_status:
    if (debuglog)
	fprintf(debuglog, "    result:%d\n", result.nfsstat);
    if (result.nfsstat >= 0
	&& !svc_sendreply(transp, dent->xdr_result, (caddr_t) &result)) {
	svcerr_systemerr(transp);
    }
    if (!svc_freeargs(transp, dent->xdr_argument, &argument)) {
	syslog(LOG_DAEMON|LOG_ERR, "unable to free RPC arguments, exiting");
	exit(1);
    }
}


	    
