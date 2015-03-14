%/*
% * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
% * unrestricted use provided that this legend is included on all tape
% * media and as a part of the software program in whole or part.  Users
% * may copy or modify Sun RPC without charge, but are not authorized
% * to license or distribute it to anyone else except as part of a product or
% * program developed by the user or with the express written consent of
% * Sun Microsystems, Inc.
% *
% * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
% * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
% * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
% *
% * Sun RPC is provided with no support and without any obligation on the
% * part of Sun Microsystems, Inc. to assist in its use, correction,
% * modification or enhancement.
% *
% * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
% * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
% * OR ANY PART THEREOF.
% *
% * In no event will Sun Microsystems, Inc. be liable for any lost revenue
% * or profits or other special, indirect and consequential damages, even if
% * Sun has been advised of the possibility of such damages.
% *
% * Sun Microsystems, Inc.
% * 2550 Garcia Avenue
% * Mountain View, California  94043
% */

%/*
% * Copyright (c) 1985, 1990 by Sun Microsystems, Inc.
% */
%
%/* from @(#)rpcb_prot.x	1.3 91/03/11 TIRPC 1.0 */

/*
 * rpcb_prot.x
 * RPCBIND protocol in rpc language
 */

#ifdef RPC_HDR
%
%#ifndef _rpcsvc_rpcb_prot_h
%#define _rpcsvc_rpcb_prot_h
%
#endif
/*
 * The following procedures are supported by the protocol:
 *
 * RPCBPROC_NULL() returns ()
 * 	takes nothing, returns nothing
 *
 * RPCBPROC_SET(RPCB) returns (bool_t)
 * 	TRUE is success, FALSE is failure.  Registers the tuple
 *	[prog, vers, address, netconfig].
 *
 * RPCBPROC_UNSET(RPCB) returns (bool_t)
 *	TRUE is success, FALSE is failure.  Un-registers tuple
 *	[prog, vers, netconfig->nc_netid].  addressss is ignored.
 *
 * RPCBPROC_GETADDR(RPCB) returns (Universal address).
 *	0 is failure.  Otherwise returns the universal address where the
 *	triple [prog, vers, nconf] is registered.
 *
 * RPCBPROC_DUMP() RETURNS (RPCBLIST *)
 *	used to dump the entire rpcbind maps
 * RPCBPROC_CALLIT(rpcb_rmtcallargs)
 * 	RETURNS (rpcb_rmtcallres);
 * 	Calls the procedure on the remote machine.  If it is not registered,
 *	this procedure is quiet; i.e. it does not return error information!!!
 *	This routine only passes null authentication parameters.
 *	It has no interface to xdr routines for RPCBPROC_CALLIT.
 *
 * RPCBPROC_GETTIME() returns (time).
 *	Gets the remote machines time
 *
 */

/*
 * A mapping of (program, version, network ID) to address
 */
struct rpcb {
	u_long r_prog;			/* program number */
	u_long r_vers;			/* version number */
	string r_netid<>;		/* network id */
	string r_addr<>;		/* universal address */
};

/*
 * A list of mappings
 */
struct rpcblist {
	rpcb rpcb_map;
	struct rpcblist *rpcb_next;
};

/*
 * Arguments of remote calls
 */
struct rpcb_rmtcallargs {
	u_long prog;			/* program number */
	u_long vers;			/* version number */
	u_long proc;			/* procedure number */
	opaque args_ptr<>;		/* argument */
};

/*
 * Results of the remote call
 */
struct rpcb_rmtcallres {
	string addr_ptr<>;		/* remote universal address */
	opaque results_ptr<>;		/* result */
};

/*
 * rpcbind procedures
 */
program RPCBPROG {
	version RPCBVERS {
		bool
		RPCBPROC_SET(rpcb) = 1;

		bool
		RPCBPROC_UNSET(rpcb) = 2;

		string
		RPCBPROC_GETADDR(rpcb) = 3;

		rpcblist
		RPCBPROC_DUMP(void) = 4;

		rpcb_rmtcallres
		RPCBPROC_CALLIT(rpcb_rmtcallargs) = 5;

		unsigned int
		RPCBPROC_GETTIME(void) = 6;

		struct netbuf
		RPCBPROC_UADDR2TADDR(string) = 7;

		string
		RPCBPROC_TADDR2UADDR(struct netbuf) = 8;
	} = 3;
} = 100000;

#ifdef RPC_HDR
%
%#endif /* ! _rpcsvc_rpcb_prot_h */
#endif
