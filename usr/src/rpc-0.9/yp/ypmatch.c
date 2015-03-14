#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <ctype.h>

#include <rpc/rpc.h>
#include <rpc/xdr.h>
#ifdef __386BSD__
#include "yp_prot.h"
#include "ypclnt.h"
#else
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#endif

struct ypalias {
	char *alias, *name;
} ypaliases[] = {
	{ "passwd", "passwd.byname" },
	{ "group", "group.byname" },
	{ "networks", "networks.byaddr" },
	{ "hosts", "hosts.byaddr" },
	{ "protocols", "protocols.bynumber" },
	{ "services", "services.byname" },
	{ "aliases", "mail.aliases" },
	{ "ethers", "ethers.byname" },
};

usage()
{
	fprintf(stderr, "Usage:\n");
	fprintf(stderr, "\typmatch [-d domain] [-t] [-k] key [key ...] mname\n");
	fprintf(stderr, "\typmatch -x\n");
	fprintf(stderr, "where\n");
	fprintf(stderr, "\tmname may be either a mapname or a nickname for a map\n");
	fprintf(stderr, "\t-t inhibits map nickname translation\n");
	fprintf(stderr, "\t-k prints keys as well as values.\n");
	fprintf(stderr, "\t-x dumps the map nickname translation table.\n");
	exit(1);
}

int
main(argc, argv)
char **argv;
{
	char domainname[MAXHOSTNAMELEN];
	char *inkey, *inmap, *outbuf;
	extern char *optarg;
	extern int optind;
	int outbuflen, key;
	int c, r, i;

	key = 0;
	getdomainname(domainname, sizeof domainname);

	while( (c=getopt(argc, argv, "xd:k")) != -1)
		switch(c) {
		case 'x':
			for(i=0; i<sizeof ypaliases/sizeof ypaliases[0]; i++)
				printf("Use \"%s\" for \"%s\"\n",
					ypaliases[i].alias,
					ypaliases[i].name);
			exit(0);
		case 'd':
			strncpy(domainname, optarg, sizeof domainname);
			break;
		case 'k':
			key++;
			break;
		}

	if(argc<2)
		usage();

	inmap = argv[argc-1];
	for(i=0; i<sizeof ypaliases/sizeof ypaliases[0]; i++)
		if( strcmp(inmap, ypaliases[i].alias) == 0)
			inmap = ypaliases[i].name;
	for(; optind < argc-1; optind++) {
		inkey = argv[optind];

		r = yp_match(domainname, inmap, inkey,
			strlen(inkey), &outbuf, &outbuflen);
		switch(r) {
		case 0:
			fprintf(stderr, "yp_match: not running ypbind\n");
			exit(1);
		case 1:
			if(key)
				printf("%s ", inkey);
			printf("%s\n", outbuf);
			break;
		default:
			fprintf(stderr, "Can't match key %s in map %s. Reason: %s\n",
				inkey, inmap, yperr_string(r));
			break;
		}
	}
}
