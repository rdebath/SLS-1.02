/*
 * SUN-DES-1 authentication mechanism
 *
 * $XConsortium: rpcauth.c,v 1.3 91/02/28 09:36:18 rws Exp $
 *
 * Copyright 1991 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Mayank Choudhary, Sun Microsystems
 */

#ifdef SECURE_RPC
#include <rpc/rpc.h>
#ifdef ultrix
#include <time.h>
#include <rpc/auth_des.h>
#endif
#include "Xauth.h"
#include "misc.h"
#include "X.h"
#include "os.h"

static char * 
authdes_ezdecode(inmsg, len)
char *inmsg;
int  len;
{
    enum auth_stat  why;
    struct rpc_msg  msg;
    char            cred_area[MAX_AUTH_BYTES];
    char            verf_area[MAX_AUTH_BYTES];
    char            *temp_inmsg;
    struct svc_req  r;
    bool_t          res0, res1;
    XDR             xdr;
    SVCXPRT         xprt;

    temp_inmsg = (char *) xalloc(len);
    bcopy(inmsg, temp_inmsg, len);

    memset(&msg, 0, sizeof(msg));
    memset(&r, 0, sizeof(r));
    memset(cred_area, 0, sizeof(cred_area));
    memset(verf_area, 0, sizeof(verf_area));

    msg.rm_call.cb_cred.oa_base = cred_area;
    msg.rm_call.cb_verf.oa_base = verf_area;
    why = AUTH_FAILED; 
    xdrmem_create(&xdr, temp_inmsg, len, XDR_DECODE);

    if ((r.rq_clntcred = (caddr_t) xalloc(MAX_AUTH_BYTES)) == NULL)
        goto bad1;
    r.rq_xprt = &xprt;

    /* decode into msg */
    res0 = xdr_opaque_auth(&xdr, &(msg.rm_call.cb_cred)); 
    res1 = xdr_opaque_auth(&xdr, &(msg.rm_call.cb_verf));
    if ( ! (res0 && res1) )
         goto bad2;

    /* do the authentication */

    r.rq_cred = msg.rm_call.cb_cred;        /* read by opaque stuff */
    if (r.rq_cred.oa_flavor != AUTH_DES) {
        why = AUTH_TOOWEAK;
        goto bad2;
    }
    if ((why = _authenticate(&r, &msg)) != AUTH_OK) {
            goto bad2;
    }
    return (((struct authdes_cred *) r.rq_clntcred)->adc_fullname.name); 

bad2:
    Xfree(r.rq_clntcred);
bad1:
    return ((char *)0); /* ((struct authdes_cred *) NULL); */
}

static XID  rpc_id = (XID) ~0L;

static Bool
CheckNetName (addr, len, closure)
    unsigned char    *addr;
    int		    len;
    pointer	    closure;
{
    return (len == strlen ((char *) closure) &&
	    strncmp ((char *) addr, (char *) closure, len) == 0);
}

XID
SecureRPCCheck (data_length, data)
register unsigned short	data_length;
char	*data;
{
    char *fullname;
    
    if (rpc_id != (XID) ~0L &&
	(fullname = authdes_ezdecode(data, data_length)) != (char *)0)
    {
	if (ForEachHostInFamily (FamilyNetname, CheckNetName, (pointer) fullname))
	    return rpc_id;
    }
    return (XID) ~0L;
}
    

SecureRPCInit ()
{
    if (rpc_id == ~0L)
	AddAuthorization (9, "SUN-DES-1", 0, (char *) 0);
}

int
SecureRPCAdd (data_length, data, id)
unsigned short	data_length;
char	*data;
XID	id;
{
    if (data_length)
	AddHost ((pointer) 0, FamilyNetname, data_length, data);
    rpc_id = id;
}

int
SecureRPCReset ()
{
    rpc_id = (XID) ~0L;
}

XID
SecureRPCToID (data_length, data)
    unsigned short	data_length;
    char		*data;
{
    return rpc_id;
}

SecureRPCFromID (id, data_lenp, datap)
     XID id;
     unsigned short	*data_lenp;
     char	**datap;
{
    return 0;
}

SecureRPCRemove (data_length, data)
     unsigned short	data_length;
     char	*data;
{
    return 0;
}
#endif SECURE_RPC

