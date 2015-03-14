#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include "yp_prot.h"
#include "ypclnt.h"
#include "theo_yp.h"

struct dom_binding *ypbindlist;

extern bool_t xdr_domainname(), xdr_ypbind_resp();
extern bool_t xdr_ypreq_key(), xdr_ypresp_val();
extern bool_t xdr_ypreq_nokey(), xdr_ypresp_key_val();
extern bool_t xdr_ypresp_all(), xdr_ypresp_all_seq();
extern bool_t xdr_ypresp_master();

int (*ypresp_allfn)();
void *ypresp_data;

int
_yp_dobind(dom)
char *dom;
{
	char path[MAXPATHLEN];
	struct dom_binding *ysd;
	struct ypbind_resp ypbr;
	struct timeval tv;
	CLIENT *client;
	int new=0, r, fd;

	for(ysd = ypbindlist; ysd; ysd = ysd->dom_pnext)
		if( strcmp(dom, ysd->dom_domain) == 0)
			break;
	if(ysd==NULL) {
		ysd = (struct dom_binding *)malloc(sizeof *ysd);
		bzero(ysd, sizeof *ysd);
		new = 1;
	}
	if(ysd->dom_vers==0) {
again:
#ifdef BINDINGDIR
		sprintf(path, "%s/%s.%d", BINDINGDIR, dom, 2);
		if( (fd=open(path, O_RDONLY)) == -1) {
			/* no binding file, YP is dead. */
			if(new)
				free(ysd);
			return YPERR_YPBIND;
		}
		if( flock(fd, LOCK_EX|LOCK_NB) == -1 && errno==EWOULDBLOCK) {
			/* yp is running */
			r = read(fd, &ysd->dom_server_addr, sizeof ysd->dom_server_addr);
			if(r != sizeof ysd->dom_server_addr) {
				close(fd);
				goto again;
			}
			ysd->dom_server_port = ysd->dom_server_addr.sin_port;
			close(fd);
			goto gotit;
		} else {
			/* no lock on binding file, YP is dead. */
			close(fd);
			if(new)
				free(ysd);
			return YPERR_YPBIND;
		}
#endif
		client = clnt_create("localhost", YPBINDPROG, YPBINDVERS, "tcp");
		if(client==NULL) {
			clnt_pcreateerror("clnt_create");
			goto again;
		}

		tv.tv_sec = 2;
		tv.tv_usec = 0;
		r = clnt_call(client, YPBINDPROC_DOMAIN,
			xdr_domainname, dom, xdr_ypbind_resp, &ypbr, tv);
		if(r) {
			clnt_pcreateerror("yp_bind: clnt_call");
			clnt_destroy(client);
			goto again;
		}
		clnt_destroy(client);

		bzero(&ysd->dom_server_addr, sizeof ysd->dom_server_addr);
		ysd->dom_server_addr.sin_family = AF_INET;
		ysd->dom_server_addr.sin_port =
			ypbr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_port;
		ysd->dom_server_addr.sin_addr.s_addr =
			ypbr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_addr.s_addr;
		ysd->dom_server_port =
			ypbr.ypbind_respbody.ypbind_bindinfo.ypbind_binding_port;
gotit:
		ysd->dom_vers = YPVERS;
		strcpy(ysd->dom_domain, dom);

		tv.tv_sec = 5;
		tv.tv_usec = 0;
		ysd->dom_socket = RPC_ANYSOCK;
		ysd->dom_client = clntudp_create(&ysd->dom_server_addr,
			YPPROG, YPVERS, tv, &ysd->dom_socket);
		if(ysd->dom_client==NULL) {
			clnt_pcreateerror("clntudp_create");
			goto again;
		}
		if(new) {
			ysd->dom_pnext = ypbindlist;
			ypbindlist = ysd;
		}
	}
	return 0;
}

int
yp_bind(dom)
char *dom;
{
	return _yp_dobind(dom);
}

void
yp_unbind(dom)
char *dom;
{
	struct dom_binding *ypb, *ypbp;

	ypbp = NULL;
	for(ypb=ypbindlist; ypb; ypb=ypb->dom_pnext) {
		if( strcmp(dom, ypb->dom_domain) == 0) {
			clnt_destroy(ypb->dom_client);
			if(ypbp)
				ypbp->dom_pnext = ypb->dom_pnext;
			else
				ypbindlist = ypb->dom_pnext;
			free(ypb);
			return;
		}
		ypbp = ypb;
	}
	return;
}

int
yp_match(indomain, inmap, inkey, inkeylen, outval, outvallen)
char *indomain;
char *inmap;
char *inkey;
int inkeylen;
char **outval;
int *outvallen;
{
	struct dom_binding *ysd;
	struct ypresp_val yprv;
	struct timeval tv;
	struct ypreq_key yprk;
	int r;

again:
	if( _yp_dobind(indomain) != 0)
		return YPERR_YPBIND;
	for(ysd=ypbindlist; ysd; ysd=ysd->dom_pnext)
		if( strcmp(indomain, ysd->dom_domain) == 0)
			break;
	if(ysd==NULL)
		return YPERR_YPBIND;

	tv.tv_sec = 10;
	tv.tv_usec = 0;

	yprk.domain = indomain;
	yprk.map = inmap;
	yprk.keydat.dptr = inkey;
	yprk.keydat.dsize = inkeylen;

	bzero(&yprv, sizeof yprv);

	r = clnt_call(ysd->dom_client, YPPROC_MATCH,
		xdr_ypreq_key, &yprk, xdr_ypresp_val, &yprv, tv);
	if(r != 0) {
		printf("foo, %d\n", r);
		clnt_perror(ysd->dom_client, "clnt_call");
		*outval = NULL;
		*outvallen = 0;
		ysd->dom_vers = 0;
		goto again;
	}
	*outval = yprv.valdat.dptr;
	*outvallen = yprv.valdat.dsize;
	return yprv.status;
}

int
yp_get_default_domain(domp)
char **domp;
{
	static char dom[MAXHOSTNAMELEN];

	getdomainname(dom, sizeof dom);
	*domp = dom;
	return 0;
}

int
yp_first(indomain, inmap, outkey, outkeylen, outval, outvallen)
char *indomain;
char *inmap;
char **outkey;
int *outkeylen;
char **outval;
int *outvallen;
{
	struct ypresp_key_val yprkv;
	struct ypreq_nokey yprnk;
	struct dom_binding *ysd;
	struct timeval tv;
	int r;

again:
	if( _yp_dobind(indomain) != 0)
		return YPERR_YPBIND;
	for(ysd=ypbindlist; ysd; ysd=ysd->dom_pnext)
		if( strcmp(indomain, ysd->dom_domain) == 0)
			break;
	if(ysd==NULL)
		return YPERR_YPBIND;

	tv.tv_sec = 10;
	tv.tv_usec = 0;

	yprnk.domain = indomain;
	yprnk.map = inmap;
	bzero(&yprkv, sizeof yprkv);

	r = clnt_call(ysd->dom_client, YPPROC_FIRST,
		xdr_ypreq_nokey, &yprnk, xdr_ypresp_key_val, &yprkv, tv);
	if(r != 0) {
		printf("foo, %d\n", r);
		clnt_perror(ysd->dom_client, "clnt_call");
		*outkey = NULL;
		*outkeylen = 0;
		*outval = NULL;
		*outvallen = 0;
		ysd->dom_vers = 0;
		goto again;
	}

	*outkey = yprkv.keydat.dptr;
	*outkeylen = yprkv.keydat.dsize;
	*outval = yprkv.valdat.dptr;
	*outvallen = yprkv.valdat.dsize;
	return yprkv.status;
}

int
yp_next(indomain, inmap, inkey, inkeylen, outkey, outkeylen, outval, outvallen)
char *indomain;
char *inmap;
char *inkey;
int inkeylen;
char **outkey;
int *outkeylen;
char **outval;
int *outvallen;
{
	struct ypresp_key_val yprkv;
	struct ypreq_key yprk;
	struct dom_binding *ysd;
	struct timeval tv;
	int r;

again:
	if( _yp_dobind(indomain) != 0)
		return YPERR_YPBIND;
	for(ysd=ypbindlist; ysd; ysd=ysd->dom_pnext)
		if( strcmp(indomain, ysd->dom_domain) == 0)
			break;
	if(ysd==NULL)
		return YPERR_YPBIND;

	tv.tv_sec = 10;
	tv.tv_usec = 0;

	yprk.domain = indomain;
	yprk.map = inmap;
	yprk.keydat.dptr = inkey;
	yprk.keydat.dsize = inkeylen;
	bzero(&yprkv, sizeof yprkv);

	r = clnt_call(ysd->dom_client, YPPROC_FIRST,
		xdr_ypreq_key, &yprk, xdr_ypresp_key_val, &yprkv, tv);
	if(r != 0) {
		printf("foo, %d\n", r);
		clnt_perror(ysd->dom_client, "clnt_call");
		*outkey = NULL;
		*outkeylen = 0;
		*outval = NULL;
		*outvallen = 0;
		ysd->dom_vers = 0;
		goto again;
	}

	*outkey = yprkv.keydat.dptr;
	*outkeylen = yprkv.keydat.dsize;
	*outval = yprkv.valdat.dptr;
	*outvallen = yprkv.valdat.dsize;
	return yprkv.status;
}

int
yp_all(indomain, inmap, incallback)
char *indomain;
char *inmap;
struct ypall_callback *incallback;
{
	struct ypreq_nokey yprnk;
	struct dom_binding *ysd;
	struct timeval tv;
	struct sockaddr_in clnt_sin;
	CLIENT *clnt;
	u_long status;
	int clnt_sock, r;

	if( _yp_dobind(indomain) != 0)
		return YPERR_YPBIND;
	for(ysd=ypbindlist; ysd; ysd=ysd->dom_pnext)
		if( strcmp(indomain, ysd->dom_domain) == 0)
			break;
	if(ysd==NULL)
		return YPERR_YPBIND;

	tv.tv_sec = 10;
	tv.tv_usec = 0;
	clnt_sock = RPC_ANYSOCK;
	clnt_sin = ysd->dom_server_addr;
	clnt_sin.sin_port = 0;
	clnt = clnttcp_create(&clnt_sin, YPPROG, YPVERS, &clnt_sock, 0, 0);
	if(clnt==NULL) {
		printf("clnttcp_create failed\n");
		return YPERR_PMAP;
	}

	yprnk.domain = indomain;
	yprnk.map = inmap;
	ypresp_allfn = incallback->foreach;
	ypresp_data = incallback->data;

	r = clnt_call(clnt, YPPROC_ALL,
		xdr_ypreq_nokey, &yprnk, xdr_ypresp_all_seq, &status, tv);
	clnt_destroy(clnt);
	return status;
}

int
yp_order(indomain, inmap, outorder)
char *indomain;
char *inmap;
int *outorder;
{
	return YP_FALSE;
}

yp_master(indomain, inmap, outname)
char *indomain;
char *inmap;
char **outname;
{
	struct dom_binding *ysd;
	struct ypresp_master yprm;
	struct ypreq_nokey yprnk;
	struct timeval tv;
	int r;

again:
	if( _yp_dobind(indomain) != 0)
		return YPERR_YPBIND;
	for(ysd=ypbindlist; ysd; ysd=ysd->dom_pnext)
		if( strcmp(indomain, ysd->dom_domain) == 0)
			break;
	if(ysd==NULL)
		return YPERR_YPBIND;

	tv.tv_sec = 10;
	tv.tv_usec = 0;

	yprnk.domain = indomain;
	yprnk.map = inmap;

	bzero(&yprm, sizeof yprm);

	r = clnt_call(ysd->dom_client, YPPROC_MASTER,
		xdr_ypreq_nokey, &yprnk, xdr_ypresp_master, &yprm, tv);
	if(r != 0) {
		printf("foo, %d\n", r);
		clnt_perror(ysd->dom_client, "clnt_call");
		ysd->dom_vers = 0;
		goto again;
	}
	*outname = yprm.master;
	return yprm.status;
}

char *
yperr_string(incode)
int incode;
{
	switch(incode) {
	case YP_NOMAP:
		return "No such map in domain";
		break;
	case YP_NODOM:
		return "Domain not supported";
		break;
	case YP_NOKEY:
		return "No such key in map";
		break;
	case YP_BADOP:
		return "Invalid operation";
		break;
	case YP_BADDB:
		return "Server data base is bad";
		break;
	case YP_YPERR:
		return "NIS server error";
		break;
	case YP_BADARGS:
		return "Request arguments bad";
		break;
	case YP_VERS:
		return "NIS server version mismatch - server can't supply requested service.";
		break;
	}
	return NULL;
}

ypprot_err (incode)
unsigned int incode;
{
}
