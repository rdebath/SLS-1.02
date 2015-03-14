#include <sys/param.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <dirent.h>
#include <netdb.h>
#include <string.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include <net/if.h>
#include <arpa/inet.h>
#include <rpc/pmap_clnt.h>
#include <rpc/pmap_prot.h>
#include <rpc/pmap_rmt.h>
#include "yp_prot.h"
#include "ypclnt.h"
#include "theo_yp.h"

#undef DAEMON

struct _dom_binding {
	struct _dom_binding *dom_pnext;
	char dom_domain[YPMAXDOMAIN + 1];
	struct sockaddr_in dom_server_addr;
	unsigned short int dom_server_port;
	int dom_socket;
	CLIENT *dom_client;
	long int dom_vers;
	time_t dom_check_t;
	int dom_lockfd;
};

char *domainname;

extern bool_t xdr_domainname(), xdr_ypbind_resp();
extern bool_t xdr_ypreq_key(), xdr_ypresp_val();
extern bool_t xdr_ypbind_setdom();

struct _dom_binding *ypbindlist;
int check;

int rpcsock;
struct rmtcallargs rmtca;
struct rmtcallres rmtcr;
char rmtcr_outval;
u_long rmtcr_port;

void *
ypbindproc_null_2(transp, argp, clnt)
SVCXPRT *transp;
void *argp;
CLIENT *clnt;
{
	static char res;

	bzero((char *)&res, sizeof(res));
	return ((void *)&res);
}

/*static struct _dom_binding *ypdb;*/

struct ypbind_resp *
ypbindproc_domain_2(transp, argp, clnt)
SVCXPRT *transp;
char *argp;
CLIENT *clnt;
{
	static struct ypbind_resp res;
	struct _dom_binding *ypdb;
	char path[MAXPATHLEN];

	bzero(&res, sizeof res);
	res.ypbind_status = YPBIND_FAIL_VAL;

	for(ypdb=ypbindlist; ypdb; ypdb=ypdb->dom_pnext)
		if( strcmp(ypdb->dom_domain, argp) == 0)
			break;

	if(ypdb && (ypdb->dom_lockfd!=-1) ) {
		res.ypbind_status = YPBIND_SUCC_VAL;
		res.ypbind_respbody.ypbind_bindinfo.ypbind_binding_addr.s_addr =
			ypdb->dom_server_addr.sin_addr.s_addr;
		res.ypbind_respbody.ypbind_bindinfo.ypbind_binding_port =
			ypdb->dom_server_port;
		/*printf("domain %s at %s/%d\n", ypdb->dom_domain,
			inet_ntoa(ypdb->dom_server_addr.sin_addr),
			ntohs(ypdb->dom_server_addr.sin_port));*/
		return &res;
	}

	if(ypdb == NULL) {
		ypdb = (struct _dom_binding *)malloc(sizeof *ypdb);
		bzero(ypdb, sizeof *ypdb);
		strncpy(ypdb->dom_domain, argp, sizeof ypdb->dom_domain);
		ypdb->dom_vers = YPVERS;
		ypdb->dom_lockfd = -1;
		sprintf(path, "%s/%s.%d", BINDINGDIR, ypdb->dom_domain, ypdb->dom_vers);
		unlink(path);

		ypdb->dom_pnext = ypbindlist;
		ypbindlist = ypdb;
		check++;
	}
	return NULL;
}

/* IGNORE THIS DANGEROUS CRITTER */
void *
ypbindproc_setdom_2(transp, argp, clnt)
SVCXPRT *transp;
struct ypbind_setdom *argp;
CLIENT *clnt;
{
	static char res;

	/* svc_getcaller()... */

	bzero((char *)&res, sizeof(res));
	return ((void *)&res);
}

static void
ypbindprog_2(rqstp, transp)
struct svc_req *rqstp;
register SVCXPRT *transp;
{
	union {
		char ypbindproc_domain_2_arg[MAXHOSTNAMELEN];
		struct ypbind_setdom ypbindproc_setdom_2_arg;
	} argument;
	char *result;
	bool_t (*xdr_argument)(), (*xdr_result)();
	char *(*local)();

	switch (rqstp->rq_proc) {
	case YPBINDPROC_NULL:
		xdr_argument = xdr_void;
		xdr_result = xdr_void;
		local = (char *(*)()) ypbindproc_null_2;
		break;

	case YPBINDPROC_DOMAIN:
		xdr_argument = xdr_domainname;
		xdr_result = xdr_ypbind_resp;
		local = (char *(*)()) ypbindproc_domain_2;
		break;

	case YPBINDPROC_SETDOM:
		xdr_argument = xdr_ypbind_setdom;
		xdr_result = xdr_void;
		local = (char *(*)()) ypbindproc_setdom_2;
		break;

	default:
		svcerr_noproc(transp);
		return;
	}
	bzero((char *)&argument, sizeof(argument));
	if (!svc_getargs(transp, xdr_argument, &argument)) {
		svcerr_decode(transp);
		return;
	}
	result = (*local)(transp, &argument, rqstp);
	if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_argument, &argument)) {
		fprintf(stderr, "unable to free arguments");
		exit(1);
	}
	return;
}

main()
{
	char path[MAXPATHLEN];
	struct timeval tv;
	SVCXPRT *transp;
	fd_set fdsr;
	int width;
	int i = 1;

	yp_get_default_domain(&domainname);
	if( domainname[0] == '\0') {
		fprintf(stderr, "domainname not set. Aborting.\n");
		exit(1);
	}

	/* blow away everything in BINDINGDIR */



#ifdef DAEMON
	switch(fork()) {
	case 0:
		break;
	case -1:
		perror("fork");
		exit(1);
	default:
		exit(0);
	}
	setsid();
#endif

	pmap_unset(YPBINDPROG, YPBINDVERS);

	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
		fprintf(stderr, "cannot create udp service.");
		exit(1);
	}
	if (!svc_register(transp, YPBINDPROG, YPBINDVERS, ypbindprog_2, IPPROTO_UDP)) {
		fprintf(stderr, "unable to register (YPBINDPROG, YPBINDVERS, udp).");
		exit(1);
	}

	transp = svctcp_create(RPC_ANYSOCK, 0, 0);
	if (transp == NULL) {
		fprintf(stderr, "cannot create tcp service.");
		exit(1);
	}

	if (!svc_register(transp, YPBINDPROG, YPBINDVERS, ypbindprog_2, IPPROTO_TCP)) {
		fprintf(stderr, "unable to register (YPBINDPROG, YPBINDVERS, tcp).");
		exit(1);
	}

	if( (rpcsock=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
		perror("socket");
		return -1;
	}
	fcntl(rpcsock, F_SETFL, fcntl(rpcsock, F_GETFL, 0) | FNDELAY);
	setsockopt(rpcsock, SOL_SOCKET, SO_BROADCAST, &i, sizeof(i));
	rmtca.prog = YPPROG;
	rmtca.vers = YPVERS;
	rmtca.proc = YPPROC_DOMAIN_NONACK;
	rmtca.xdr_args = NULL;		/* set at call time */
	rmtca.args_ptr = NULL;		/* set at call time */
	rmtcr.port_ptr = &rmtcr_port;
	rmtcr.xdr_results = xdr_bool;
	rmtcr.results_ptr = (caddr_t)&rmtcr_outval;

	/* build initial domain binding, make it "unsuccessful" */
	ypbindlist = (struct _dom_binding *)malloc(sizeof *ypbindlist);
	bzero(ypbindlist, sizeof *ypbindlist);
	strncpy(ypbindlist->dom_domain, domainname, sizeof ypbindlist->dom_domain);
	ypbindlist->dom_vers = YPVERS;
	ypbindlist->dom_lockfd = -1;
	sprintf(path, "%s/%s.%d", BINDINGDIR, ypbindlist->dom_domain,
		ypbindlist->dom_vers);
	(void)unlink(path);

	width = getdtablesize();
	while(1) {
		fdsr = svc_fdset;
		FD_SET(rpcsock, &fdsr);
		tv.tv_sec = 1;
		tv.tv_usec = 0;

		switch(select(width, &fdsr, NULL, NULL, &tv)) {
		case 0:
			checkwork();
			break;
		case -1:
			perror("select\n");
			break;
		default:
			if(FD_ISSET(rpcsock, &fdsr)) {
				FD_CLR(rpcsock, &fdsr);
				handle_replies();
			}
			svc_getreqset(&fdsr);
			if(check)
				checkwork();
			break;
		}
	}
}

/*
 * change to do something like this:
 *
 * STATE	TIME		ACTION		NEWTIME	NEWSTATE
 * no binding	t==*		broadcast 	t=2	no binding
 * binding	t==60		check server	t=10	binding
 * binding	t=10		broadcast	t=2	no binding
 */
checkwork()
{
	struct _dom_binding *ypdb;
	time_t t;

	check = 0;

	time(&t);
	for(ypdb=ypbindlist; ypdb; ypdb=ypdb->dom_pnext) {
		if(ypdb->dom_lockfd==-1 || ypdb->dom_check_t > t + 60) {
			if( ypdb->dom_check_t > t)
				continue;
			broadcast(ypdb->dom_domain);
			ypdb->dom_check_t = t + 2;
		}
	}
}

broadcast(dom)
char *dom;
{
	struct rpc_msg rpcmsg;
	char buf[1400], inbuf[8192];
	enum clnt_stat st;
	struct timeval tv;
	int outlen, i, sock, len;
	struct sockaddr_in bsin;
	struct ifconf ifc;
	struct ifreq ifreq, *ifr;
	struct in_addr in;
	AUTH *rpcua;
	XDR rpcxdr;

	rmtca.xdr_args = xdr_domainname;
	rmtca.args_ptr = dom;

	bzero(&bsin, sizeof bsin);
	bsin.sin_family = AF_INET;
	bsin.sin_port = htons(PMAPPORT);

	bzero(&rpcxdr, sizeof rpcxdr);
	bzero(&rpcmsg, sizeof rpcmsg);

	rpcua = authunix_create_default();
	if( rpcua == (AUTH *)NULL) {
		/*printf("cannot get unix auth\n");*/
		return RPC_SYSTEMERROR;
	}
	rpcmsg.rm_direction = CALL;
	rpcmsg.rm_call.cb_rpcvers = RPC_MSG_VERSION;
	rpcmsg.rm_call.cb_prog = PMAPPROG;
	rpcmsg.rm_call.cb_vers = PMAPVERS;
	rpcmsg.rm_call.cb_proc = PMAPPROC_CALLIT;
	rpcmsg.rm_call.cb_cred = rpcua->ah_cred;
	rpcmsg.rm_call.cb_verf = rpcua->ah_verf;

	gettimeofday(&tv, (struct timezone *)0);
	rpcmsg.rm_xid = (int)dom;
	tv.tv_usec = 0;
	xdrmem_create(&rpcxdr, buf, sizeof buf, XDR_ENCODE);
	if( (!xdr_callmsg(&rpcxdr, &rpcmsg)) ) {
		st = RPC_CANTENCODEARGS;
		AUTH_DESTROY(rpcua);
		return st;
	}
	if( (!xdr_rmtcall_args(&rpcxdr, &rmtca)) ) {
		st = RPC_CANTENCODEARGS;
		AUTH_DESTROY(rpcua);
		return st;
	}
	outlen = (int)xdr_getpos(&rpcxdr);
	xdr_destroy(&rpcxdr);
	if(outlen<1) {
		AUTH_DESTROY(rpcua);
		return st;
	}
	AUTH_DESTROY(rpcua);

	/* find all networks and send the RPC packet out them all */
	if( (sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) < 0) {
		perror("socket");
		return -1;
	}

	ifc.ifc_len = sizeof inbuf;
	ifc.ifc_buf = inbuf;
	if( ioctl(sock, SIOCGIFCONF, &ifc) < 0) {
		close(sock);
		perror("ioctl(SIOCGIFCONF)");
		return -1;
	}
	ifr = ifc.ifc_req;
	ifreq.ifr_name[0] = '\0';
	for(i=0; i<ifc.ifc_len; i+=len, ifr=(struct ifreq *)((caddr_t)ifr+len) ) {
		len = sizeof ifr->ifr_name + ifr->ifr_addr.sa_len;
		ifreq = *ifr;
		if( ifreq.ifr_addr.sa_family != AF_INET)
			continue;
		if( ioctl(sock, SIOCGIFFLAGS, &ifreq) < 0) {
			perror("ioctl(SIOCGIFFLAGS)");
			continue;
		}
		if( (ifreq.ifr_flags & IFF_UP) == 0)
			continue;

		ifreq.ifr_flags &= (IFF_LOOPBACK | IFF_BROADCAST);
		if( ifreq.ifr_flags==IFF_BROADCAST ) {
			if( ioctl(sock, SIOCGIFBRDADDR, &ifreq) < 0 ) {
				perror("ioctl(SIOCGIFBRDADDR)");
				continue;
			}
		} else if( ifreq.ifr_flags==IFF_LOOPBACK ) {
			if( ioctl(sock, SIOCGIFADDR, &ifreq) < 0 ) {
				perror("ioctl(SIOCGIFADDR)");
				continue;
			}
		} else
			continue;

		in = ((struct sockaddr_in *)&ifreq.ifr_addr)->sin_addr;
		bsin.sin_addr = in;
		if( sendto(rpcsock, buf, outlen, 0, (struct sockaddr *)&bsin,
		    sizeof bsin) < 0 )
			perror("sendto");
	}
	close(sock);
	return 0;
}

/*enum clnt_stat*/
handle_replies()
{
	char buf[1400];
	int fromlen, inlen;
	struct sockaddr_in raddr;
	struct rpc_msg msg;
	XDR xdr;

	/*printf("reply..\n");*/

recv_again:
	bzero(&xdr, sizeof(xdr));
	bzero(&msg, sizeof(msg));
	msg.acpted_rply.ar_verf = _null_auth;
	msg.acpted_rply.ar_results.where = (caddr_t)&rmtcr;
	msg.acpted_rply.ar_results.proc = xdr_rmtcallres;

try_again:
	fromlen = sizeof (struct sockaddr);
	inlen = recvfrom(rpcsock, buf, sizeof buf, 0,
		(struct sockaddr *)&raddr, &fromlen);
	if(inlen<0) {
		if(errno==EINTR)
			goto try_again;
		return RPC_CANTRECV;
	}
	if(inlen<sizeof(u_long))
		goto recv_again;

	/*
	 * see if reply transaction id matches sent id.
	 * If so, decode the results.
	 */
	xdrmem_create(&xdr, buf, (u_int)inlen, XDR_DECODE);
	if( xdr_replymsg(&xdr, &msg)) {
		if( (msg.rm_reply.rp_stat == MSG_ACCEPTED) &&
		    (msg.acpted_rply.ar_stat == SUCCESS)) {
			raddr.sin_port = htons((u_short)rmtcr_port);
			rpc_received(msg.rm_xid, &raddr);
		}
	}
	xdr.x_op = XDR_FREE;
	msg.acpted_rply.ar_results.proc = xdr_void;
	xdr_destroy(&xdr);

	return RPC_SUCCESS;
}

/*
 * LOOPBACK IS MORE IMPORTANT: PUT IN HACK
 */
rpc_received(dom, raddrp)
char *dom;
struct sockaddr_in *raddrp;
{
	struct _dom_binding *ypdb;
	char path[MAXPATHLEN];
	int fd;

	/*printf("returned from %s about %s\n", inet_ntoa(raddrp->sin_addr), dom);*/

	if(dom==NULL)
		return;

	for(ypdb=ypbindlist; ypdb; ypdb=ypdb->dom_pnext)
		if( strcmp(ypdb->dom_domain, dom) == 0)
			break;
	if(ypdb==NULL)
		return;

	bcopy(raddrp, &ypdb->dom_server_addr, sizeof ypdb->dom_server_addr);
	ypdb->dom_check_t = time(NULL) + 60;	/* recheck binding in 60 seconds */
	ypdb->dom_vers = YPVERS;

	if(ypdb->dom_lockfd != -1)
		close(ypdb->dom_lockfd);

	sprintf(path, "%s/%s.%d", BINDINGDIR,
		ypdb->dom_domain, ypdb->dom_vers);
	if( (fd=open(path, O_CREAT|O_SHLOCK|O_RDWR|O_TRUNC, 0644)) == -1) {
		(void)mkdir(BINDINGDIR, 755);
		if( (fd=open(path, O_CREAT|O_SHLOCK|O_RDWR|O_TRUNC, 0644)) == -1)
			return;
	}

	/*
	 * ok, if BINDINGDIR exists, and we can create the binding file,
	 * then write to it..
	 */
	ypdb->dom_lockfd = fd;
	if( write(ypdb->dom_lockfd, raddrp, sizeof *raddrp) != sizeof *raddrp) {
		perror("write");
		close(ypdb->dom_lockfd);
		ypdb->dom_lockfd = -1;
		return;
	}
}
