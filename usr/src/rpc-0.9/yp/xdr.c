#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <ctype.h>

#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include "yp_prot.h"
#include "ypclnt.h"
#include "theo_yp.h"

extern int (*ypresp_allfn)();
extern void *ypresp_data;

bool_t
xdr_domainname(xdrs, objp)
XDR *xdrs;
char *objp;
{
	if (!xdr_string(xdrs, &objp, YPMAXDOMAIN)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_peername(xdrs, objp)
XDR *xdrs;
char *objp;
{
	if (!xdr_string(xdrs, objp, YPMAXPEER)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_keydat(xdrs, objp)
XDR *xdrs;
datum *objp;
{
	if (!xdr_bytes(xdrs, (char **)&objp->dptr, (u_int *)&objp->dsize, YPMAXRECORD)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_mapname(xdrs, objp)
XDR *xdrs;
char *objp;
{
	if (!xdr_string(xdrs, objp, YPMAXMAP)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypreq_key(xdrs, objp)
XDR *xdrs;
struct ypreq_key *objp;
{
	if (!xdr_domainname(xdrs, objp->domain)) {
		return (FALSE);
	}
	if (!xdr_mapname(xdrs, &objp->map)) {
		return (FALSE);
	}
	if (!xdr_keydat(xdrs, &objp->keydat)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypreq_nokey(xdrs, objp)
XDR *xdrs;
struct ypreq_nokey *objp;
{
	if (!xdr_domainname(xdrs, objp->domain)) {
		return (FALSE);
	}
	if (!xdr_mapname(xdrs, &objp->map)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypbind_binding(xdrs, objp)
XDR *xdrs;
struct ypbind_binding *objp;
{
	if (!xdr_opaque(xdrs, &objp->ypbind_binding_addr, 4)) {
		return (FALSE);
	}
	if (!xdr_opaque(xdrs, &objp->ypbind_binding_port, 2)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypbind_resptype(xdrs, objp)
XDR *xdrs;
enum ypbind_resptype *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypstat(xdrs, objp)
XDR *xdrs;
struct ypstat *objp;
{
	if (!xdr_enum(xdrs, (enum_t *)objp)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypbind_resp(xdrs, objp)
XDR *xdrs;
struct ypbind_resp *objp;
{
	if (!xdr_ypbind_resptype(xdrs, &objp->ypbind_status)) {
		return (FALSE);
	}
	switch (objp->ypbind_status) {
	case YPBIND_FAIL_VAL:
		if (!xdr_u_int(xdrs, &objp->ypbind_respbody.ypbind_error)) {
			return (FALSE);
		}
		break;
	case YPBIND_SUCC_VAL:
		if (!xdr_ypbind_binding(xdrs, &objp->ypbind_respbody.ypbind_bindinfo)) {
			return (FALSE);
		}
		break;
	default:
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypresp_val(xdrs, objp)
XDR *xdrs;
struct ypresp_val *objp;
{
	if (!xdr_ypstat(xdrs, &objp->status)) {
		return (FALSE);
	}
	if (!xdr_keydat(xdrs, &objp->valdat)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypbind_setdom(xdrs, objp)
XDR *xdrs;
struct ypbind_setdom *objp;
{
        if (!xdr_domainname(xdrs, objp->ypsetdom_domain)) {
                return (FALSE);
        }
        if (!xdr_ypbind_binding(xdrs, &objp->ypsetdom_binding)) {
                return (FALSE);
        }
        if (!xdr_u_int(xdrs, &objp->ypsetdom_vers)) {
                return (FALSE);
        }
        return (TRUE);
}

bool_t
xdr_ypresp_key_val(xdrs, objp)
XDR *xdrs;
struct ypresp_key_val *objp;
{
	if (!xdr_ypstat(xdrs, &objp->status)) {
		return (FALSE);
	}
	if (!xdr_keydat(xdrs, &objp->keydat)) {
		return (FALSE);
	}
	if (!xdr_keydat(xdrs, &objp->valdat)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ypresp_all(xdrs, objp)
XDR *xdrs;
struct ypresp_all *objp;
{
	if (!xdr_bool(xdrs, &objp->more)) {
		return (FALSE);
	}
	switch (objp->more) {
	case TRUE:
		if (!xdr_ypresp_key_val(xdrs, &objp->ypresp_all_u.val)) {
			return (FALSE);
		}
		break;
	case FALSE:
		break;
	default:
		return (FALSE);
	}
	return (TRUE);
}

/* XXXX BUGGY?? */
bool_t
xdr_ypresp_all_seq(xdrs, objp)
XDR *xdrs;
u_long *objp;
{
	struct ypresp_all out;
	u_long status;
	int r;

	bzero(&out, sizeof out);
	while(1) {
		if( !xdr_ypresp_all(xdrs, &out)) {
			*objp = YP_YPERR;
			return FALSE;
		}
		if(out.more == 0)
			return FALSE;
		status = out.ypresp_all_u.val.status;
		switch(status) {
		case YP_TRUE:
			r = (*ypresp_allfn)(status,
				out.ypresp_all_u.val.valdat.dptr,
				out.ypresp_all_u.val.valdat.dsize,
				out.ypresp_all_u.val.keydat.dptr,
				out.ypresp_all_u.val.keydat.dsize, 
				ypresp_data);
			*objp = status;
			xdr_free(xdr_ypresp_all, &out);
			break;
		case YP_NOMORE:
			xdr_free(xdr_ypresp_all, &out);
			return TRUE;
		default:
			*objp = status;
			xdr_free(xdr_ypresp_all, &out);
			return TRUE;
		}
	}
}

bool_t
xdr_ypresp_master(xdrs, objp)
XDR *xdrs;
struct ypresp_master *objp;
{
	if (!xdr_ypstat(xdrs, &objp->status)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->master, YPMAXPEER)) {
		return (FALSE);
	}
	return (TRUE);
}
