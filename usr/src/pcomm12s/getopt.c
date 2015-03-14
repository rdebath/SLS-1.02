/*
 * Parse the command line and return option flags and arguments
 */

#include <stdio.h>
#include "config.h"

int optind = 1;
char *optarg;

int
getopt(argc, argv, opts)
int argc;
char *argv[];
char *opts;
{
	static int sp = 1;
	int c, strcmp();
	char *cp, *strchr();

	if (sp == 1) {
		if (optind >= argc || argv[optind][0] != '-' || argv[optind][1] == '\0')
			return(EOF);
		else if (strcmp(argv[optind], "--") == 0) {
			optind++;
			return(EOF);
		}
	}
	c = argv[optind][sp];
	if (c == ':' || (cp=strchr(opts, c)) == NULL) {
		fprintf(stderr, "%s: illegal option \"%c\"\n", argv[0], c);
		if (argv[optind][++sp] == '\0') {
			optind++;
			sp = 1;
		}
		return('?');
	}
	if (*++cp == ':') {
		if (argv[optind][sp+1] != '\0')
			optarg = &argv[optind++][sp+1];
		else if (++optind >= argc) {
			fprintf(stderr, "%s: option \"%c\" requires an argument\n", argv[0], c);
			sp = 1;
			return('?');
		} else
			optarg = argv[optind++];
		sp = 1;
	} else {
		if (argv[optind][++sp] == '\0') {
			sp = 1;
			optind++;
		}
		optarg = NULL;
	}
	return(c);
}
