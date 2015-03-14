/* getopt.h */
/* $Id: getopt.h,v 1.2 1992/11/27 10:29:00 espie Exp espie $
 * $Log: getopt.h,v $
 * Revision 1.2  1992/11/27  10:29:00  espie
 * General cleanup
 * 
 */

struct long_option
	{
	char *fulltext;
	int argn;
	char abbrev;
	};

extern int optind;
extern char *optarg;
