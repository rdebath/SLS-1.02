/*
 * See if two strings match.  Returns a 0 on success, and a 1 on failure.
 * This is an external program to be used in shell scripts.
 */

#ifndef linux
#define STRSTR
#endif /* linux */

#include <stdio.h>

main(argc, argv)
int argc;
char *argv[];
{
	char *strstr();
	void exit();

	if (argc != 3) {
		fprintf(stderr, "Usage: matches string1 string2\n");
		exit(-1);
	}

	if (strstr(argv[1], argv[2]))
		exit(0);
	exit(1);
}

#ifdef STRSTR
/*
 * Return a pointer to the first occurance of string str2 in str1.
 * Returns a NULL if str2 is not in str1.
 */

char *
strstr(str1, str2)
char *str1, *str2;
{
	int len;

	len = strlen(str2);
	while (*str1) {
		if (*str2 == *str1) {
			if (!strncmp(str2, str1, len))
				return(str1);
		}
		str1++;
	}
	return(NULL);
}
#endif /* STRSTR */
