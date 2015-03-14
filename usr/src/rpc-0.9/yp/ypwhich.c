#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <ctype.h>
#include <netdb.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include "yp_prot.h"
#include "ypclnt.h"
#include "theo_yp.h"

extern struct dom_binding *ypbindlist;

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
	fprintf(stderr, "\typwhich [-d domain] [[-t] -m [mname] | host]\n");
	fprintf(stderr, "\typwhich -x\n");
	exit(1);
}

int
main(argc, argv)
char **argv;
{
	char domainname[MAXHOSTNAMELEN];
	struct dom_binding *ysd;
	extern char *optarg;
	extern int optind;
	struct hostent *hent;
	int outbuflen, key;
	int c, r, i;
	char *arg, mode;

	key = 0;
	getdomainname(domainname, sizeof domainname);

	mode = ' ';
	while( (c=getopt(argc, argv, "xd:k")) != -1)
		switch(c) {
		case 'x':
			for(i=0; i<sizeof ypaliases/sizeof ypaliases[0]; i++)
				printf("Use \"%s\" for \"%s\"\n",
					ypaliases[i].alias,
					ypaliases[i].name);
			exit(0);
		case 't':
			if(mode != ' ')
				usage();
			mode = 't';
			break;
		case 'm':
			if(mode != ' ')
				usage();
			mode = 'm';
			break;
		case 'd':
			strncpy(domainname, optarg, sizeof domainname);
			break;
		case 'k':
			key++;
			break;
		}
	if(mode==' ')
		mode = 'h';

	if(optind + 1 < argc )
		usage();

	arg = NULL;
	if(optind + 1 == argc)
		arg = argv[optind];

	switch(mode) {
	case 'h':
		r = yp_bind(domainname);
		switch(r) {
		case YP_TRUE:
			for(ysd = ypbindlist; ysd; ysd = ysd->dom_pnext)
				if( strcmp(domainname, ysd->dom_domain) == 0)
					break;
			if(!ysd) {
				fprintf(stderr, "yp_match: not running ypbind\n");
				exit(1);
			}
			hent = gethostbyaddr( (char *)&ysd->dom_server_addr.sin_addr,
				sizeof ysd->dom_server_addr.sin_addr, AF_INET);
			if(hent) {
				printf("%s\n", hent->h_name);
				exit(0);
			}
			printf("%s\n", inet_ntoa(ysd->dom_server_addr.sin_addr));
			exit(0);
		case YP_FALSE:
			fprintf(stderr, "yp_match: not running ypbind\n");
			exit(1);
		default:
			fprintf(stderr, "can't yp_bind:. Reason: %s\n",
				yperr_string(r));
			exit(1);
		}
		break;
	case 'm':
		if(arg) {
			for(i=0; i<sizeof ypaliases/sizeof ypaliases[0]; i++)
				if( strcmp(arg, ypaliases[i].alias) == 0)
					arg = ypaliases[i].name;
		}
	case 't':
		if(arg)
			printf("check one map\n");
		else
			printf("check all maps\n");
		exit(0);
	}

#if 0
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
#endif
}
