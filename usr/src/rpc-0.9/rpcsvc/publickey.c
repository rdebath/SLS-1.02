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
#if !defined(lint) && defined(SCCSIDS)
static char sccsid[] = "@(#)publickey.c 1.10 91/03/11 Copyr 1986 Sun Micro";
#endif

/*
 * publickey.c
 * Copyright (C) 1986, Sun Microsystems, Inc.
 */

/*
 * Public key lookup routines
 */
#include <stdio.h>
#include <pwd.h>
#include <rpc/rpc.h>
#include <rpc/key_prot.h>
#include <string.h>

extern char *strchr();
extern char *strcpy();

/*
 * Get somebody's public key
 */
getpublickey(netname, publickey)
	char *netname;
	char *publickey;
{
	char lookup[3 * HEXKEYBYTES];
	char *p;

	if (publickey == NULL)
		return (0);
	if (!getpublicandprivatekey(netname, lookup))
		return (0);
	p = strchr(lookup, ':');
	if (p == NULL) {
		return (0);
	}
	*p = 0;
	(void) strncpy(publickey, lookup, HEXKEYBYTES);
	return (1);
}

/*
 * reads the publickey NIS map
 */

int
getpublicandprivatekey(key, ret)
	char *key;
	char *ret;
{
#ifdef YP
	char *PKMAP = "publickey.byname";
	char *lookup;
	char *domain;
	int err;
	int len;

	err = yp_get_default_domain(&domain);
	if (err) {
		return (0);
	}
	lookup = NULL;
	err = yp_match(domain, PKMAP, key, strlen(key), &lookup, &len);
	if (err) {
#ifdef DEBUG
		fprintf(stderr, "match failed error %d\n", err);
#endif
		return (0);
	}
	lookup[len] = 0;
	strcpy(ret, lookup);
	free(lookup);
	return (2);
#else /* YP */
#ifdef DEBUG
#ifdef NOTDEF
	fprintf(stderr,
"Bad record in %s '+' -- NIS not supported in this library copy\n", PKFILE);
#endif /* NOTDEF */
	fprintf(stderr,
"NIS not supported in this library copy. Secure RPC is unavailable.\n");
#endif /* DEBUG */
	return (0);
#endif /* YP */
}

/*
 *  The above code is used for compatibility with SunOS 4.1 behavior.
 *  The code for SVR4 is shown below.
 */

#ifdef NOTDEF
/*
 * reads the file /etc/publickey looking for a + to optionally go to the
 * yellow pages
 */

int
getpublicandprivatekey(key, ret)
	char *key;
	char *ret;
{
	char buf[1024];	/* big enough */
	char *res;
	FILE *fd;
	char *mkey;
	char *mval;

	fd = fopen(PKFILE, "r");
	if (fd == (FILE *) 0)
		return (0);
	for (;;) {
		res = fgets(buf, 1024, fd);
		if (res == 0) {
			fclose(fd);
			return (0);
		}
		if (res[0] == '#')
			continue;
		else if (res[0] == '+') {
#ifdef YP
			char *PKMAP = "publickey.byname";
			char *lookup;
			char *domain;
			int err;
			int len;

			err = yp_get_default_domain(&domain);
			if (err) {
				continue;
			}
			lookup = NULL;
			err = yp_match(domain, PKMAP, key, strlen(key), &lookup, &len);
			if (err) {
#ifdef DEBUG
				fprintf(stderr, "match failed error %d\n", err);
#endif
				continue;
			}
			lookup[len] = 0;
			strcpy(ret, lookup);
			fclose(fd);
			free(lookup);
			return (2);
#else /* YP */
#ifdef DEBUG
			fprintf(stderr,
"Bad record in %s '+' -- NIS not supported in this library copy\n", PKFILE);
#endif /* DEBUG */
			continue;
#endif /* YP */
		} else {
			mkey = strtok(buf, "\t ");
			if (mkey == NULL) {
				fprintf(stderr,
				"Bad record in %s -- %s", PKFILE, buf);
				continue;
			}
			mval = strtok((char *)NULL, " \t#\n");
			if (mval == NULL) {
				fprintf(stderr,
			"Bad record in %s val problem - %s", PKFILE, buf);
				continue;
			}
			if (strcmp(mkey, key) == 0) {
				strcpy(ret, mval);
				fclose(fd);
				return (1);
			}
		}
	}
}
#endif /* NOTDEF */
