/* nfsmount.c - simple nfs mount - rick sladkey */

#include <stdio.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <string.h>
#include <netdb.h>
#include <errno.h>

#include "mount.h"

struct nfs_fh {
	fhandle data;
};

#include <linux/nfs_mount.h>

int nfsmount(char *spec, char *node, int *flags, char **extra_opts)
{
	char hostdir[1024];
	CLIENT *mclient;
	char *hostname;
	char *dirname;
	char *mountpoint;
	fhandle root_fhandle;
	struct timeval total_timeout;
	enum clnt_stat clnt_stat;
	static struct nfs_mount_data data;
	char *opts, *opt, *opteq;
	int val;
	struct hostent *hp;
	struct sockaddr_in server_addr;
	int msock, fsock;
	struct timeval pertry_timeout;
	struct fhstatus status;
	char *s;
	int port;
	int bg;
	int soft;
	int intr;
	int posix;
	int nocto;
	int noac;

	msock = fsock = -1;
	strcpy(hostdir, spec);
	if ((s = (strchr(hostdir, ':')))) {
		hostname = hostdir;
		dirname = s + 1;
		*s = '\0';
	}
	else {
		fprintf(stderr, "directory to mount not in host:dir format\n");
		goto fail;
	}
	mountpoint = node;

	if ((hp = gethostbyname(hostname)) == NULL) {
		fprintf(stderr, "can't get addr for %s\n", hostname);
		goto fail;
	}

	/* set default options */

	data.rsize	= 0; /* let kernel decide */
	data.wsize	= 0; /* let kernel decide */
	data.timeo	= 7;
	data.retrans	= 3;
	data.acregmin	= 3;
	data.acregmax	= 60;
	data.acdirmin	= 30;
	data.acdirmax	= 60;

	port = 2049;
	bg = 0;
	soft = 0;
	intr = 0;
	posix = 0;
	nocto = 0;
	noac = 0;

	/* parse options */

	if ((opts = *extra_opts)) {
		for (opt = strtok(opts, ","); opt; opt = strtok(NULL, ",")) {
			if ((opteq = strchr(opt, '='))) {
				val = atoi(opteq + 1);	
				*opteq = '\0';
				if (!strcmp(opt, "rsize"))
					data.rsize = val;
				else if (!strcmp(opt, "wsize"))
					data.wsize = val;
				else if (!strcmp(opt, "timeo"))
					data.timeo = val;
				else if (!strcmp(opt, "retrans"))
					data.retrans = val;
				else if (!strcmp(opt, "acregmin"))
					data.acregmin = val;
				else if (!strcmp(opt, "acregmax"))
					data.acregmax = val;
				else if (!strcmp(opt, "acdirmin"))
					data.acdirmin = val;
				else if (!strcmp(opt, "acdirmax"))
					data.acdirmax = val;
				else if (!strcmp(opt, "actimeo")) {
					data.acregmin = val;
					data.acregmax = val;
					data.acdirmin = val;
					data.acdirmax = val;
				}
				else if (!strcmp(opt, "port"))
					port = val;
				else {
					printf("unknown nfs mount parameter: "
						"%s=%d\n", opt, val);
					goto fail;
				}
			}
			else {
				val = 1;
				if (!strncmp(opt, "no", 2)) {
					val = 0;
					opt += 2;
				}
				if (!strcmp(opt, "bg")) 
					bg = val;
				else if (!strcmp(opt, "fg")) 
					bg = !val;
				else if (!strcmp(opt, "soft"))
					soft = val;
				else if (!strcmp(opt, "hard"))
					soft = !val;
				else if (!strcmp(opt, "intr"))
					intr = val;
				else if (!strcmp(opt, "posix"))
					posix = val;
				else if (!strcmp(opt, "cto"))
					nocto = !val;
				else if (!strcmp(opt, "ac"))
					noac = !val;
				else {
					printf("unknown nfs mount option: "
						"%s%s\n", val ? "" : "no", opt);
					goto fail;
				}
			}
		}
	}
	data.flags = (soft ? NFS_MOUNT_SOFT : 0)
		| (intr ? NFS_MOUNT_INTR : 0)
		| (posix ? NFS_MOUNT_POSIX : 0)
		| (nocto ? NFS_MOUNT_NOCTO : 0)
		| (noac ? NFS_MOUNT_NOAC : 0);

#if 0
	printf("rsize = %d, wsize = %d, timeo = %d, retrans = %d\n",
		data.rsize, data.wsize, data.timeo, data.retrans);
	printf("acreg (min, max) = (%d, %d), acdir (min, max) = (%d, %d)\n",
		data.acregmin, data.acregmax, data.acdirmin, data.acdirmax);
	printf("port = %d, bg = %d, flags = %.8x\n",
		port, bg, data.flags);
	printf("soft = %d, intr = %d, posix = %d, nocto = %d, noac = %d\n",
		(data.flags & NFS_MOUNT_SOFT) != 0,
		(data.flags & NFS_MOUNT_INTR) != 0,
		(data.flags & NFS_MOUNT_POSIX) != 0,
		(data.flags & NFS_MOUNT_NOCTO) != 0,
		(data.flags & NFS_MOUNT_NOAC) != 0);
	goto fail;
#endif

	/* create mount deamon client */

	pertry_timeout.tv_sec = 3;
	pertry_timeout.tv_usec = 0;
	total_timeout.tv_sec = 20;
	total_timeout.tv_usec = 0;
	memcpy((caddr_t) &server_addr.sin_addr, hp->h_addr, hp->h_length);
	server_addr.sin_family = AF_INET;
	server_addr.sin_port = 0;
	msock = RPC_ANYSOCK;
	if ((mclient = clntudp_create(&server_addr, MOUNTPROG, MOUNTVERS,
		pertry_timeout, &msock)) == NULL) {
		clnt_pcreateerror("mount clntudp_create");
		goto fail;
	}
	mclient->cl_auth = authunix_create_default();

	/* try to mount hostname:dirname */

	clnt_stat = clnt_call(mclient, MOUNTPROC_MNT,
		xdr_dirpath, &dirname,
		xdr_fhstatus, &status,
		total_timeout);
	if (clnt_stat != RPC_SUCCESS) {
		clnt_perror(mclient, "rpc mount");
		goto fail;
	}
	if (status.fhs_status != 0) {
		fprintf(stderr, "mount %s:%s denied by server, error = %d\n",
			hostname, dirname, status.fhs_status);
		goto fail;
	}
	memcpy((char *) &root_fhandle, (char *) status.fhstatus_u.fhs_fhandle,
		sizeof (root_fhandle));

	/* create nfs socket for kernel */

	fsock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if (fsock < 0) {
		perror("nfs socket");
		goto fail;
	}
	if (bindresvport(fsock, 0) < 0) {
		perror("nfs bindresvport");
		goto fail;
	}
	memcpy((caddr_t) &server_addr.sin_addr, hp->h_addr, hp->h_length);
	server_addr.sin_family = AF_INET;
	server_addr.sin_port = htons(port);
	if (connect(fsock, &server_addr, sizeof (server_addr)) < 0) {
		perror("nfs connect");
		goto fail;
	}

	/* prepare data structure for kernel */

	data.version = NFS_MOUNT_VERSION;
	data.fd = fsock;
	memcpy((char *) &data.root, (char *) &root_fhandle,
		sizeof (root_fhandle));
	memcpy((char *) &data.addr, (char *) &server_addr, sizeof(data.addr));
	strncpy(data.hostname, hostname, sizeof(data.hostname));

	*extra_opts = &data;

	/* clean up */

	auth_destroy(mclient->cl_auth);
	clnt_destroy(mclient);
	close(msock);
	return 0;

	/* abort */

fail:
	if (msock != -1) {
		auth_destroy(mclient->cl_auth);
		clnt_destroy(mclient);
		close(msock);
	}
	if (fsock != -1)
		close(fsock);
	return 1;
}

