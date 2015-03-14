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
% * Copyright (c) 1987, 1990 by Sun Microsystems, Inc.
% */
%
%/* from @(#)rnusers.x	1.4 91/03/11 TIRPC 1.0 */

/*
 * Find out about remote users
 */

#ifdef RPC_HDR
%#ifndef _rpcsvc_rnusers_h
%#define _rpcsvc_rnusers_h
%/*
% * This is a utmp entry that does not correspond to a genuine user
% */
%#ifndef nonuser
%#define nonuser(ut) ((ut).ut_host[0] == 0 && \
%      strncmp((ut).ut_line, "tty", 3) == 0 && ((ut).ut_line[3] == 'p' \
%                                            || (ut).ut_line[3] == 'q' \
%                                            || (ut).ut_line[3] == 'r' \
%                                            || (ut).ut_line[3] == 's'))
%#endif /* !def nonuser */
#endif

const MAXUSERS = 100;
const MAXUTLEN = 256;

/*
 * the structure (data object) passed between the rusers program and
 * rpc.rusersd. For historical reasons it is similar to the utmp structure
 * for the bsd sytems. It is the data object that is included in the stuctures
 * passed between the client and the rusers service.
 */

struct ru_utmp {
	string ut_line<MAXUTLEN>;       /* tty name */
	string ut_name<MAXUTLEN>;       /* user id */
	string ut_host<MAXUTLEN>;       /* host name, if remote */
	int ut_time;                    /* time on */
};


struct utmpidle {
	ru_utmp ui_utmp;
	unsigned int ui_idle;
};

typedef ru_utmp utmparr<MAXUSERS>;

typedef utmpidle utmpidlearr<MAXUSERS>;

program RUSERSPROG {
	/*
	 * Old version does not include idle information
	 */
	version RUSERSVERS_ORIG {
		int
		RUSERSPROC_NUM(void) = 1;

		utmparr
		RUSERSPROC_NAMES(void) = 2;

		utmparr
		RUSERSPROC_ALLNAMES(void) = 3;
	} = 1;

	/*
	 * Includes idle information
	 */
	version RUSERSVERS_IDLE {
		int
		RUSERSPROC_NUM(void) = 1;

		utmpidlearr
		RUSERSPROC_NAMES(void) = 2;

		utmpidlearr
		RUSERSPROC_ALLNAMES(void) = 3;
	} = 2;
} = 100002;
	
#ifdef RPC_HDR
%#endif /*!_rpcsvc_rnusers_h*/
#endif
