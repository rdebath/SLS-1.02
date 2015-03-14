/*
 *  linux/fs/nfs/proc.c
 *
 *  Copyright (C) 1992  Rick Sladkey
 *
 *  OS-independent nfs remote procedure call functions
 */

/*
 * Defining NFS_PROC_DEBUG causes a lookup of a file named
 * "xyzzy" to toggle debugging.  Just cd to an NFS-mounted
 * filesystem and type 'ls xyzzy' to turn on debugging.
 */

#if 0
#define NFS_PROC_DEBUG
#endif

#include <linux/param.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/nfs_fs.h>
#include <linux/utsname.h>
#include <linux/errno.h>
#include <linux/string.h>

#include <netinet/in.h>

#ifdef NFS_PROC_DEBUG
static int proc_debug = 0;
#define PRINTK if (proc_debug) printk
#else
#define PRINTK if (0) printk
#endif

static int *nfs_rpc_header(int *p, int procedure);
static int *nfs_rpc_verify(int *p);
static int nfs_stat_to_errno(int stat);

/*
 * Here are a bunch of xdr encode/decode functions that convert
 * between machine dependent and xdr data formats.
 */

static inline int *xdr_encode_fhandle(int *p, struct nfs_fh *fhandle)
{
	*((struct nfs_fh *) p) = *fhandle;
	p += (sizeof (*fhandle) + 3) >> 2;
	return p;
}

static inline int *xdr_decode_fhandle(int *p, struct nfs_fh *fhandle)
{
	*fhandle = *((struct nfs_fh *) p);
	p += (sizeof (*fhandle) + 3) >> 2;
	return p;
}

static inline int *xdr_encode_string(int *p, const char *string)
{
	int len, quadlen;
	
	len = strlen(string);
	quadlen = (len + 3) >> 2;
	*p++ = htonl(len);
	memcpy((char *) p, string, len);
	memset(((char *) p) + len, '\0', (quadlen << 2) - len);
	p += quadlen;
	return p;
}

static inline int *xdr_decode_string(int *p, char *string, int maxlen)
{
	unsigned int len;

	len = ntohl(*p++);
	if (len > maxlen)
		return NULL;
	memcpy(string, (char *) p, len);
	string[len] = '\0';
	p += (len + 3) >> 2;
	return p;
}

static inline int *xdr_encode_data(int *p, char *data, int len)
{
	int quadlen;
	
	quadlen = (len + 3) >> 2;
	*p++ = htonl(len);
	memcpy((char *) p, data, len);
	memset(((char *) p) + len, '\0', (quadlen << 2) - len);
	p += quadlen;
	return p;
}

static inline int *xdr_decode_data(int *p, char *data, int *lenp, int maxlen)
{
	unsigned int len;

	len = *lenp = ntohl(*p++);
	if (len > maxlen)
		return NULL;
	memcpy(data, (char *) p, len);
	data[len] = '\0';
	p += (len + 3) >> 2;
	return p;
}

static int *xdr_decode_fattr(int *p, struct nfs_fattr *fattr)
{
	fattr->type = ntohl(*p++);
	fattr->mode = ntohl(*p++);
	fattr->nlink = ntohl(*p++);
	fattr->uid = ntohl(*p++);
	fattr->gid = ntohl(*p++);
	fattr->size = ntohl(*p++);
	fattr->blocksize = ntohl(*p++);
	fattr->rdev = ntohl(*p++);
	fattr->blocks = ntohl(*p++);
	fattr->fsid = ntohl(*p++);
	fattr->fileid = ntohl(*p++);
	fattr->atime.seconds = ntohl(*p++);
	fattr->atime.useconds = ntohl(*p++);
	fattr->mtime.seconds = ntohl(*p++);
	fattr->mtime.useconds = ntohl(*p++);
	fattr->ctime.seconds = ntohl(*p++);
	fattr->ctime.useconds = ntohl(*p++);
	return p;
}

static int *xdr_encode_sattr(int *p, struct nfs_sattr *sattr)
{
	*p++ = htonl(sattr->mode);
	*p++ = htonl(sattr->uid);
	*p++ = htonl(sattr->gid);
	*p++ = htonl(sattr->size);
	*p++ = htonl(sattr->atime.seconds);
	*p++ = htonl(sattr->atime.useconds);
	*p++ = htonl(sattr->mtime.seconds);
	*p++ = htonl(sattr->mtime.useconds);
	return p;
}

static int *xdr_decode_entry(int *p, struct nfs_entry *entry)
{
	entry->fileid = ntohl(*p++);
	if (!(p = xdr_decode_string(p, entry->name, NFS_MAXNAMLEN)))
		return NULL;
	entry->cookie = ntohl(*p++);
	entry->eof = 0;
	return p;
}

static int *xdr_decode_fsinfo(int *p, struct nfs_fsinfo *res)
{
	res->tsize = ntohl(*p++);
	res->bsize = ntohl(*p++);
	res->blocks = ntohl(*p++);
	res->bfree = ntohl(*p++);
	res->bavail = ntohl(*p++);
	return p;
}

int nfs_proc_getattr(struct nfs_server *server, struct nfs_fh *fhandle,
		     struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  getattr\n");
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_GETATTR);
	p = xdr_encode_fhandle(p, fhandle);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fattr(p, fattr);
		PRINTK("NFS reply getattr\n");
	}
	else
		PRINTK("NFS reply getattr failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_setattr(struct nfs_server *server, struct nfs_fh *fhandle,
		     struct nfs_sattr *sattr, struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  setattr\n");
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_SETATTR);
	p = xdr_encode_fhandle(p, fhandle);
	p = xdr_encode_sattr(p, sattr);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fattr(p, fattr);
		PRINTK("NFS reply setattr\n");
	}
	else
		PRINTK("NFS reply setattr failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_lookup(struct nfs_server *server, struct nfs_fh *dir, const char *name,
		    struct nfs_fh *fhandle, struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  lookup %s\n", name);
#ifdef NFS_PROC_DEBUG
	if (!strcmp(name, "xyzzy"))
		proc_debug = 1 - proc_debug;
#endif
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_LOOKUP);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fhandle(p, fhandle);
		p = xdr_decode_fattr(p, fattr);
		PRINTK("NFS reply lookup\n");
	}
	else
		PRINTK("NFS reply lookup failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_readlink(struct nfs_server *server, struct nfs_fh *fhandle,
		      char *res)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  readlink\n");
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_READLINK);
	p = xdr_encode_fhandle(p, fhandle);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		if (!(p = xdr_decode_string(p, res, NFS_MAXPATHLEN))) {
			printk("nfs_proc_readlink: giant pathname\n");
			status = NFSERR_IO;
		}
		else
			PRINTK("NFS reply readlink %s\n", res);
	}
	else
		PRINTK("NFS reply readlink failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_read(struct nfs_server *server, struct nfs_fh *fhandle,
		  int offset, int count, char *data, struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;
	int len = 0; /* = 0 is for gcc */

	PRINTK("NFS call  read %d @ %d\n", count, offset);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_READ);
	p = xdr_encode_fhandle(p, fhandle);
	*p++ = htonl(offset);
	*p++ = htonl(count);
	*p++ = htonl(count); /* traditional, could be any value */
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fattr(p, fattr);
		if (!(p = xdr_decode_data(p, data, &len, count))) {
			printk("nfs_proc_read: giant data size\n"); 
			status = NFSERR_IO;
		}
		else
			PRINTK("NFS reply read %d\n", len);
	}
	else
		PRINTK("NFS reply read failed = %d\n", status);
	free_page((long) p0);
	return (status == NFS_OK) ? len : -nfs_stat_to_errno(status);
}

int nfs_proc_write(struct nfs_server *server, struct nfs_fh *fhandle,
		   int offset, int count, char *data, struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  write %d @ %d\n", count, offset);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_WRITE);
	p = xdr_encode_fhandle(p, fhandle);
	*p++ = htonl(offset); /* traditional, could be any value */
	*p++ = htonl(offset);
	*p++ = htonl(count); /* traditional, could be any value */
	p = xdr_encode_data(p, data, count);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fattr(p, fattr);
		PRINTK("NFS reply write\n");
	}
	else
		PRINTK("NFS reply write failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_create(struct nfs_server *server, struct nfs_fh *dir,
		    const char *name, struct nfs_sattr *sattr,
		    struct nfs_fh *fhandle, struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  create %s\n", name);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_CREATE);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	p = xdr_encode_sattr(p, sattr);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fhandle(p, fhandle);
		p = xdr_decode_fattr(p, fattr);
		PRINTK("NFS reply create\n");
	}
	else
		PRINTK("NFS reply create failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_remove(struct nfs_server *server, struct nfs_fh *dir, const char *name)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  remove %s\n", name);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_REMOVE);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		PRINTK("NFS reply remove\n");
	}
	else
		PRINTK("NFS reply remove failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_rename(struct nfs_server *server,
		    struct nfs_fh *old_dir, const char *old_name,
		    struct nfs_fh *new_dir, const char *new_name)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  rename %s -> %s\n", old_name, new_name);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_RENAME);
	p = xdr_encode_fhandle(p, old_dir);
	p = xdr_encode_string(p, old_name);
	p = xdr_encode_fhandle(p, new_dir);
	p = xdr_encode_string(p, new_name);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		PRINTK("NFS reply rename\n");
	}
	else
		PRINTK("NFS reply rename failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_link(struct nfs_server *server, struct nfs_fh *fhandle,
		  struct nfs_fh *dir, const char *name)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  link %s\n", name);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_LINK);
	p = xdr_encode_fhandle(p, fhandle);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		PRINTK("NFS reply link\n");
	}
	else
		PRINTK("NFS reply link failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_symlink(struct nfs_server *server, struct nfs_fh *dir,
		     const char *name, const char *path, struct nfs_sattr *sattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  symlink %s -> %s\n", name, path);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_SYMLINK);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	p = xdr_encode_string(p, path);
	p = xdr_encode_sattr(p, sattr);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		PRINTK("NFS reply symlink\n");
	}
	else
		PRINTK("NFS reply symlink failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_mkdir(struct nfs_server *server, struct nfs_fh *dir,
		   const char *name, struct nfs_sattr *sattr,
		   struct nfs_fh *fhandle, struct nfs_fattr *fattr)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  mkdir %s\n", name);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_MKDIR);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	p = xdr_encode_sattr(p, sattr);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fhandle(p, fhandle);
		p = xdr_decode_fattr(p, fattr);
		PRINTK("NFS reply mkdir\n");
	}
	else
		PRINTK("NFS reply mkdir failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_rmdir(struct nfs_server *server, struct nfs_fh *dir, const char *name)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  rmdir %s\n", name);
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_RMDIR);
	p = xdr_encode_fhandle(p, dir);
	p = xdr_encode_string(p, name);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		PRINTK("NFS reply rmdir\n");
	}
	else
		PRINTK("NFS reply rmdir failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

int nfs_proc_readdir(struct nfs_server *server, struct nfs_fh *fhandle,
		     int cookie, int count, struct nfs_entry *entry)
{
	int *p, *p0;
	int status;
	int i = 0; /* = 0 is for gcc */
	int size;
	int eof;

	PRINTK("NFS call  readdir %d @ %d\n", count, cookie);
	size = server->rsize;
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_READDIR);
	p = xdr_encode_fhandle(p, fhandle);
	*p++ = htonl(cookie);
	*p++ = htonl(size);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		for (i = 0; i < count && *p++; i++) {
			if (!(p = xdr_decode_entry(p, entry++)))
				break;
		}
		if (!p) {
			printk("nfs_proc_readdir: giant filename\n");
			status = NFSERR_IO;
		}
		else {
			eof = (i == count && !*p++ && *p++)
			      || (i < count && *p++);
			if (eof && i)
				entry[-1].eof = 1;
			PRINTK("NFS reply readdir %d %s\n", i,
			       eof ? "eof" : "");
		}
	}
	else
		PRINTK("NFS reply readdir failed = %d\n", status);
	free_page((long) p0);
	return (status == NFS_OK) ? i : -nfs_stat_to_errno(status);
}

int nfs_proc_statfs(struct nfs_server *server, struct nfs_fh *fhandle,
		    struct nfs_fsinfo *res)
{
	int *p, *p0;
	int status;

	PRINTK("NFS call  statfs\n");
	p = p0 = (int *) get_free_page(GFP_KERNEL);
	p = nfs_rpc_header(p, NFSPROC_STATFS);
	p = xdr_encode_fhandle(p, fhandle);
	if ((status = nfs_rpc_call(server, p0, p)) < 0) {
		free_page((long) p0);
		return status;
	}
	if (!(p = nfs_rpc_verify(p0)))
		status = NFSERR_IO;
	else if ((status = ntohl(*p++)) == NFS_OK) {
		p = xdr_decode_fsinfo(p, res);
		PRINTK("NFS reply statfs\n");
	}
	else
		PRINTK("NFS reply statfs failed = %d\n", status);
	free_page((long) p0);
	return -nfs_stat_to_errno(status);
}

/*
 * Here are a few RPC-assist functions.
 */

static int *nfs_rpc_header(int *p, int procedure)
{
	int *p1, *p2;
	int i;
	static int xid = 0;
	unsigned char *sys = system_utsname.nodename;

	if (xid == 0) {
		xid = CURRENT_TIME;
		xid ^= (sys[3]<<24) | (sys[2]<<16) | (sys[1]<<8) | sys[0];
	}
	*p++ = htonl(++xid);
	*p++ = htonl(RPC_CALL);
	*p++ = htonl(RPC_VERSION);
	*p++ = htonl(NFS_PROGRAM);
	*p++ = htonl(NFS_VERSION);
	*p++ = htonl(procedure);
	*p++ = htonl(RPC_AUTH_UNIX);
	p1 = p++;
	*p++ = htonl(CURRENT_TIME); /* traditional, could be anything */
	p = xdr_encode_string(p, sys);
	*p++ = htonl(current->euid);
	*p++ = htonl(current->egid);
	p2 = p++;
	for (i = 0; i < 16 && i < NGROUPS && current->groups[i] != NOGROUP; i++)
		*p++ = htonl(current->groups[i]);
	*p2 = htonl(i);
	*p1 = htonl((p - (p1 + 1)) << 2);
	*p++ = htonl(RPC_AUTH_NULL);
	*p++ = htonl(0);
	return p;
}

static int *nfs_rpc_verify(int *p)
{
	unsigned int n;

	p++;
	if ((n = ntohl(*p++)) != RPC_REPLY) {
		printk("nfs_rpc_verify: not an RPC reply: %d\n", n);
		return NULL;
	}
	if ((n = ntohl(*p++)) != RPC_MSG_ACCEPTED) {
		printk("nfs_rpc_verify: RPC call rejected: %d\n", n);
		return NULL;
	}
	switch (n = ntohl(*p++)) {
	case RPC_AUTH_NULL: case RPC_AUTH_UNIX: case RPC_AUTH_SHORT:
		break;
	default:
		printk("nfs_rpc_verify: bad RPC authentication type: %d\n", n);
		return NULL;
	}
	if ((n = ntohl(*p++)) > 400) {
		printk("nfs_rpc_verify: giant auth size\n");
		return NULL;
	}
	p += (n + 3) >> 2;
	if ((n = ntohl(*p++)) != RPC_SUCCESS) {
		printk("nfs_rpc_verify: RPC call failed: %d\n", n);
		return NULL;
	}
	return p;
}
	
/*
 * We need to translate between nfs status return values and
 * the local errno values which may not be the same.
 */

#ifndef EDQUOT
#define EDQUOT	ENOSPC
#endif

static struct {
	enum nfs_stat stat;
	int errno;
} nfs_errtbl[] = {
	{ NFS_OK,		0		},
	{ NFSERR_PERM,		EPERM		},
	{ NFSERR_NOENT,		ENOENT		},
	{ NFSERR_IO,		EIO		},
	{ NFSERR_NXIO,		ENXIO		},
	{ NFSERR_ACCES,		EACCES		},
	{ NFSERR_EXIST,		EEXIST		},
	{ NFSERR_NODEV,		ENODEV		},
	{ NFSERR_NOTDIR,	ENOTDIR		},
	{ NFSERR_ISDIR,		EISDIR		},
	{ NFSERR_FBIG,		EFBIG		},
	{ NFSERR_NOSPC,		ENOSPC		},
	{ NFSERR_ROFS,		EROFS		},
	{ NFSERR_NAMETOOLONG,	ENAMETOOLONG	},
	{ NFSERR_NOTEMPTY,	ENOTEMPTY	},
	{ NFSERR_DQUOT,		EDQUOT		},
	{ NFSERR_STALE,		ESTALE		},
#ifdef EWFLUSH
	{ NFSERR_WFLUSH,	EWFLUSH		},
#endif
	{ -1,			EIO		}
};

static int nfs_stat_to_errno(int stat)
{
	int i;

	for (i = 0; nfs_errtbl[i].stat != -1; i++) {
		if (nfs_errtbl[i].stat == stat)
			return nfs_errtbl[i].errno;
	}
	printk("nfs_stat_to_errno: bad nfs status return value: %d\n", stat);
	return nfs_errtbl[i].errno;
}

