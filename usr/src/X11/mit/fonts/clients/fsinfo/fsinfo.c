/* $XConsortium: fsinfo.c,v 1.3 91/07/15 18:05:23 keith Exp $ */
/*
 * fsinfo -- report info about a font server
 */
/*
 * Copyright 1990 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include	<stdio.h>
#include	<X11/Xos.h>
#include	"FSlib.h"

static void print_server_info();
static void print_catalogue_info();
static void print_extension_info();
static void print_alternate_info();

char       *progname;

static void
usage()
{
    fprintf(stderr, "usage:  %s [-server server_name]\n", progname);
    exit(-1);
}

main(argc, argv)
    int         argc;
    char      **argv;
{
    FSServer   *svr;
    char       *servername = NULL;
    int         i;

    progname = argv[0];

    for (i = 1; i < argc; i++) {
	if (strncmp(argv[i], "-s", 2) == 0) {
	    if (++i > argc)
		usage();
	    servername = argv[i];
	} else {
	    usage();
	}
    }

    svr = FSOpenServer(servername);

    if (!svr) {
	fprintf(stderr, "%s:  unable to open server \"%s\"\n",
		progname, FSServerName(servername));
	exit(1);
    }
    print_server_info(svr);
    FSCloseServer(svr);
    exit(0);
}

static void
print_server_info(svr)
    FSServer   *svr;
{
    printf("name of server:	%s\n", FSServerString(svr));
    printf("version number:	%d\n", FSProtocolVersion(svr));
    printf("vendor string:	%s\n", FSServerVendor(svr));
    printf("vendor release number:	%d\n", FSVendorRelease(svr));
    printf("maximum request size:	%ld longwords (%ld bytes)\n",
	   FSMaxRequestSize(svr), FSMaxRequestSize(svr) * sizeof(long));
    print_catalogue_info(svr);
    print_alternate_info(svr);
    print_extension_info(svr);
}

static void
print_catalogue_info(svr)
    FSServer   *svr;
{
    int         n = 0;
    char      **cats = FSListCatalogues(svr, "*", 1000, &n);

    printf("number of catalogues:	%d\n", n);
    if (cats) {
	int         i;

	for (i = 0; i < n; i++) {
	    printf("	%s\n", cats[i]);
	}
    }
}

static void
print_extension_info(svr)
    FSServer   *svr;
{
    int         n = 0;
    char      **extlist = FSListExtensions(svr, &n);

    printf("number of extensions:	%d\n", n);
    if (extlist) {
	int         i;

	for (i = 0; i < n; i++) {
	    printf("	%s\n", extlist[i]);
	}
    }
}

static void
print_alternate_info(svr)
    FSServer   *svr;
{
    AlternateServer *alts;
    int         i,
                num;

    num = FSNumAlternateServers(svr);
    printf("Number of alternate servers: %d\n", num);
    if (num) {
	alts = FSAlternateServers(svr);
	for (i = 0; i < num; i++) {
	    printf("    #%1d\t%s%s\n", i, alts[i].name,
		   (alts[i].subset) ? "(subset)" : "");
	}
    }
}
