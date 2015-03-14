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

int key;

usage()
{
	fprintf(stderr, "Usage:\n");
	fprintf(stderr, "\typcat [-k] [-d domainname] [-t] mapname\n");
	fprintf(stderr, "\typcat -x\n");
	exit(1);
}

printit(instatus, inkey, inkeylen, inval, invallen, indata)
int instatus;
char *inkey;
int inkeylen;
char *inval;
int invallen;
char *indata;
{
	if(instatus != YP_TRUE)
		return 1;
	if(key)
		printf("%*.*s ", inkeylen, inkeylen, inkey);
	printf("%*.*s\n", invallen, invallen, inval);
	return 0;
}

int
main(argc, argv)
char **argv;
{
	char domainname[MAXHOSTNAMELEN];
	struct ypall_callback ypcb;
	char *inkey, *inmap, *outbuf;
	extern char *optarg;
	extern int optind;
	int outbuflen;
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

	if(optind + 1 != argc )
		usage();

	inmap = argv[optind];
	for(i=0; i<sizeof ypaliases/sizeof ypaliases[0]; i++)
		if( strcmp(inmap, ypaliases[i].alias) == 0)
			inmap = ypaliases[i].name;
	ypcb.foreach = printit;
	ypcb.data = NULL;

	r = yp_all(domainname, inmap, &ypcb);
	switch(r) {
	case YP_FALSE:
		fprintf(stderr, "yp_cat: not running ypbind\n");
		exit(1);
	case YP_TRUE:
		break;
	default:
		fprintf(stderr, "No such map %s. Reason: %s\n",
			inmap, yperr_string(r));
		break;
	}
}
