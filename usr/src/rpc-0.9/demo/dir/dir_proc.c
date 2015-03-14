/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user or with the express written consent of
 * Sun Microsystems, Inc.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */
/*
 * dir_proc.c: remote readdir implementation
 */
#include <rpc/rpc.h>
#include <sys/dir.h>
#include "dir.h"

extern int errno;
extern char *malloc();
extern char *strcpy();

readdir_res *
readdir_1_svc(dirname, svc_req)
	nametype *dirname;
	struct svc_req *svc_req;
{
	DIR *dirp;
	struct direct *d;
	namelist nl;
	namelist *nlp;
	static readdir_res res; /* must be static! */
	
	/*
	 * Open directory
	 */
	dirp = opendir(*dirname);
	if (dirp == NULL) {
		res.errno = errno;
		return (&res);
	}

	/*
	 * Free previous result
	 */
	xdr_free(xdr_readdir_res, &res);

	/*
	 * Collect directory entries
	 */
	nlp = &res.readdir_res_u.list;
	while (d = readdir(dirp)) {
		nl = *nlp = (namenode *) malloc(sizeof(namenode));
		nl->name = malloc(strlen(d->d_name)+1);
		strcpy(nl->name, d->d_name);
		nlp = &nl->next;
	}
	*nlp = NULL;

	/*
	 * Return the result
	 */
	res.errno = 0;
	closedir(dirp);
	return (&res);
}
