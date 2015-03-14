/*
 * showmount.c -- show mount information for an NFS server
 * Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include <stdio.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <string.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include <getopt.h>

#include "mount.h"

int headers = 1;
int hflag = 0;
int aflag = 0;
int dflag = 0;
int eflag = 0;
char *progname;

int dump_cmp(p, q)
char **p;
char **q;
{
	return strcmp(*p, *q);
}

int main(argc, argv)
int argc;
char **argv;
{
	char *hostname;
	enum clnt_stat clnt_stat;
	struct hostent *hp;
	struct sockaddr_in server_addr;
	int msock;
	struct timeval total_timeout;
	struct timeval pertry_timeout;
	int c;
	CLIENT *mclient;
	groups grouplist;
	exports exportlist;
	mountlist dumplist;
	mountlist list;
	int i;
	int n;
	char **dumpv;

	progname = argv[0];
	while ((c = getopt(argc, argv, "adeh")) != EOF) {
		switch (c) {
		case 'a':
			aflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'e':
			eflag = 1;
			break;
		case 'h':
			headers = 0;
			break;
		default:
			fprintf(stderr, "usage: %s [-ade] [host]\n", progname);
			exit(1);
			break;
		}
	}
	argc -= optind;
	argv += optind;

	switch (aflag + dflag + eflag) {
	case 0:
		hflag = 1;
		break;
	case 1:
		break;
	default:
		fprintf(stderr, "%s: only one of -a, -d or -e is allowed\n",
			progname);
		exit(1);
		break;
	}

	switch (argc) {
	case 0:
		hostname = "localhost";
		break;
	case 1:
		hostname = argv[0];
		break;
	default:
		fprintf(stderr, "%s: only one hostname is allowed\n",
			progname);
		exit(1);
		break;
	}

	if (hostname[0] >= '0' && hostname[0] <= '9') {
		server_addr.sin_family = AF_INET;
		server_addr.sin_addr.s_addr = inet_addr(hostname);
	}
	else {
		if ((hp = gethostbyname(hostname)) == NULL) {
			fprintf(stderr, "%s: can't get address for %s\n",
				progname, hostname);
			exit(1);
		}
		server_addr.sin_family = AF_INET;
		memcpy(&server_addr.sin_addr, hp->h_addr, hp->h_length);
	}

	/* create mount deamon client */

	server_addr.sin_port = 0;
	msock = RPC_ANYSOCK;
	if ((mclient = clnttcp_create(&server_addr,
	    MOUNTPROG, MOUNTVERS, &msock, 0, 0)) == NULL) {
		server_addr.sin_port = 0;
		msock = RPC_ANYSOCK;
		pertry_timeout.tv_sec = 3;
		pertry_timeout.tv_usec = 0;
		if ((mclient = clntudp_create(&server_addr,
		    MOUNTPROG, MOUNTVERS, pertry_timeout, &msock)) == NULL) {
			clnt_pcreateerror("mount clntudp_create");
			exit(1);
		}
	}
	mclient->cl_auth = authunix_create_default();
	total_timeout.tv_sec = 20;
	total_timeout.tv_usec = 0;

	if (eflag) {
		clnt_stat = clnt_call(mclient, MOUNTPROC_EXPORT,
			xdr_void, NULL, xdr_exports, &exportlist,
			total_timeout);
		if (clnt_stat != RPC_SUCCESS) {
			clnt_perror(mclient, "rpc mount export");
			exit(1);
		}
		printf("Exports list on %s:\n", hostname);
		while (exportlist) {
			printf("%-21s ", exportlist->ex_dir);
			grouplist = exportlist->ex_groups;
			while (grouplist) {
				printf("%s%s", grouplist->gr_name,
					grouplist->gr_next ? "," : "\n");
 				grouplist = grouplist->gr_next;
			}
			exportlist = exportlist->ex_next;
		}
		exit(0);
	}

	clnt_stat = clnt_call(mclient, MOUNTPROC_DUMP,
		xdr_void, NULL, xdr_mountlist, &dumplist,
		total_timeout);
	if (clnt_stat != RPC_SUCCESS) {
		clnt_perror(mclient, "rpc mount dump");
		exit(1);
	}

	n = 0;
	for (list = dumplist; list; list = list->ml_next)
		n++;
	dumpv = (char **) calloc(n, sizeof (char *));
	if (n && !dumpv) {
		fprintf(stderr, "%s: out of memory\n", progname);
		exit(1);
	}
	i = 0;

	if (hflag) {
		if (headers)
			printf("Hosts on %s:\n", hostname);
		while (dumplist) {
			dumpv[i++] = dumplist->ml_hostname;
			dumplist = dumplist->ml_next;
		}
	}
	else if (aflag) {
		if (headers)
			printf("All mount points on %s:\n", hostname);
		while (dumplist) {
			char s[1024];
			char *t;

			sprintf(s, "%s:%s", dumplist->ml_hostname,
				dumplist->ml_directory);
			t = malloc(strlen(s) + 1);
			if (t)
				strcpy(t, s);
			else {
				printf("%s: out of memory\n", progname);
				exit(1);
			}
			dumpv[i++] = t;
			dumplist = dumplist->ml_next;
		}
	}
	else if (dflag) {
		if (headers)
			printf("Directories on %s:\n", hostname);
		while (dumplist) {
			dumpv[i++] = dumplist->ml_directory;
			dumplist = dumplist->ml_next;
		}
	}

	qsort(dumpv, n, sizeof (char *), dump_cmp);
	
	for (i = 0; i < n; i++) {
		if (i == 0 || strcmp(dumpv[i], dumpv[i - 1]) != 0)
			printf("%s\n", dumpv[i]);
	}
	exit(0);
}

