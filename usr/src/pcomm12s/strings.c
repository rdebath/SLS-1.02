/*
 * Miscellaneous string routines.
 */

#ifndef linux
#define STRSTR
#endif

#include <stdio.h>
#include "config.h"

/*
 * Do a fancy string copy.  If NULL, return null.  If pointer to NULL, then
 * return the special "null_ptr" variable.  If a normal copy, allocate
 * memory first.
 */

char *
str_dup(str)
char *str;
{
	extern char *null_ptr;
	char *ret, *malloc(), *strcpy();
	void error_win();

	if (str == NULL)
		return(NULL);
					/* if pointer to null */
	if (*str == '\0')
		return(null_ptr);

	if (!(ret = malloc((unsigned int) strlen(str)+1)))
		error_win(1, "Out of memory", "");

	strcpy(ret, str);
	return(ret);
}

/*
 * Perform the free(2) function, but check for NULL and the special
 * "null_ptr" variable first.
 */

void
free_ptr(str)
char *str;
{
	extern char *null_ptr;
	void free();

	if (str != NULL && str != null_ptr)
		free(str);
	return;
}

/*
 * Replace a string.  Follows the same convention as str_dup(), except
 * that realloc() is used instead of malloc().  Returns a pointer to
 * the new string (which may have moved).
 */

char *
str_rep(s1, s2)
char *s1, *s2;
{
	extern char *null_ptr;
	void free_ptr(), error_win();
	char *s, *malloc(), *realloc(), *strcpy();

					/* copy null pointer ? */
	if (s2 == NULL) {
		free_ptr(s1);
		return(NULL);
	}
	if (s2 == '\0') {
		free_ptr(s1);
		return(null_ptr);
	}
					/* use realloc()? */
	if (s1 == NULL || s1 == null_ptr) {
		if (!(s = malloc((unsigned int) strlen(s2)+1)))
			error_win(1, "Out of memory", "");
	}
	else {
		if (!(s = realloc(s1, (unsigned int) strlen(s2)+1)))
			error_win(1, "Out of memory", "");
	}

	strcpy(s, s2);
	return(s);
}

/*
 * This routine is similar to strtok(3).  But this version handles missing
 * tokens by returning a pointer to null.  Also it takes a single separator
 * character as an argument.  Returns a NULL on end of string or error.
 */

char *
str_tok(str, c)
char *str, c;
{
	char *strchr();
	static char *ptr, *sep;
					/* start at beginning */
	if (str != NULL)
		ptr = str;
	else
		ptr = sep;
					/* at the end? */
	if (*ptr == '\0')
		return(NULL);
					/* no separator? */
	if (!(sep = strchr(ptr, c)))
		return(NULL);
					/* zap the sep, move past it */
	*sep = '\0';
	sep++;

	return(ptr);
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

#ifdef BSD
/*
 * Returns the length of the initial segment of string which consists
 * entirely of characters from charset.
 */

int
strspn(string, charset)
char	*string;
register char	*charset;
{
	register char *p, *q;

	for(q=string; *q != '\0'; ++q) {
		for(p=charset; *p != '\0' && *p != *q; ++p)
			;
		if(*p == '\0')
			break;
	}
	return(q-string);
}

/*
 * Strtok considers string to consist of a sequence of zero or more
 * text tokens separated by spans of one or more characters from sepset.
 */

char *
strtok(string, sepset)
char	*string, *sepset;
{
	register char	*p, *q, *r;
	static char	*savept;
	char *strpbrk();

	/*first or subsequent call*/
	p = (string == NULL)? savept: string;

	if(p == 0)		/* return if no tokens remaining */
		return(NULL);

	q = p + strspn(p, sepset);	/* skip leading separators */

	if(*q == '\0')		/* return if no tokens remaining */
		return(NULL);

	if((r = strpbrk(q, sepset)) == NULL)	/* move past token */
		savept = 0;	/* indicate this is last token */
	else {
		*r = '\0';
		savept = ++r;
	}
	return(q);
}

/*
 * Return ptr to first occurrence of any character from `brkset'
 * in the character string `string'; NULL if none exists.
 */

char *
strpbrk(string, brkset)
register char *string, *brkset;
{
	register char *p;

	if(!string || !brkset)
		return(0);
	do {
		for(p=brkset; *p != '\0' && *p != *string; ++p)
			;
		if(*p != '\0')
			return(string);
	}
	while(*string++);
	return(0);
}

/*
 * Copies the character c, n times to string s
 */

char *
memset(s, c, n)
char *s, c;
int n;
{
	char *s1 = s;

	while (n > 0) {
		--n;
		*s++ = c;
	}
	return(s1);
}
#endif /* BSD */
