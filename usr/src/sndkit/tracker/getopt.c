/* getopt.c */

/* $Id: getopt.c,v 1.2 1992/11/27 10:29:00 espie Exp espie $
 * $Log: getopt.c,v $
 * Revision 1.2  1992/11/27  10:29:00  espie
 * General cleanup
 *
 */

#include <stdio.h>
#include <ctype.h>

#include "defs.h"
#include "getopt.h"

int optind = 1;
char *optarg = 0;
static not_an_option = 0;

LOCAL int parse_option(argv, option)
char *argv[];
struct long_option *option;
	{
	optind++;
	if (option->argn)
		optarg = argv[optind++];
	return option->abbrev;
	}

int getopt(argc, argv, options)
int argc;
char *argv[];
struct long_option *options;
	{
	if (not_an_option == optind)
		return -1;
	if (argv[optind][0] == '-')
		{
		char *match = argv[optind]+1;
		if (strlen(match) == 1)
			{
			if (match[0] == '-')
				{
				not_an_option = ++optind;
				return -1;
				}
			while(options->fulltext)
				{
				if (options->abbrev == match[0])
					return parse_option(argv, options);
				else
					options++;
				}
			return -1;
			}
		else
			{
			int max_match = 0;
			struct long_option *best = 0;

			while (options->fulltext)
				{
				int i;
				for (i = 0; ; i++)
					{
					if (options->fulltext[i] == 0 && match[i] == 0)
						return parse_option(argv, options);
					if (match[i] == 0)
						{
						if (i > max_match)
							{
							max_match = i;
							best = options;
							}
						break;
						}
					if (tolower(options->fulltext[i]) != tolower(match[i]))
						break;
					}
				options++;
				}
			if (max_match < 3)
				{
				fprintf(stderr, "Unrecognized option: %s\n", match);
				return -1;
				}
			return parse_option(argv, best);
			}
		}
	else
		return -1;
	}
