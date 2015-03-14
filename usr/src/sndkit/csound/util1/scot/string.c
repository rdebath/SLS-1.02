/* general string processing routines */

#include "data.h"

/* compare two strings:  return TRUE or FALSE */

int compare(str1, str2)
 register char *str1, *str2;
{
	while (*str1++ == *str2)
		if (*str2++ == '\0')
			return (TRUE);
	return (FALSE);
}

/* return TRUE if the first string is a prefix of the second */

int prefix(str1, str2)
 register char *str1, *str2;
{
	while (*str2++ == *str1)
		if (*(++str1) == '\0')
			return (TRUE);
	return (FALSE);
}

/* return string length up to '\0' */
/*** NOT USED
int length(str)
 register char *str;
{
	register int lth = 0;
	while (*str++ != '\0') lth++;
	return (lth);
}
***/

/* return the index of a character into a character array,
   else -1 if not found
*/

int index(ch,str)
 register char ch;
 register char *str;
{
	int match = FALSE;
	register int x = 0;
	while (! ((*str == '\0') || (match = (ch == *str++)))) x++;
	if (match)	return (x);
	else		return (-1);
}

/* copy string */

copy(source, dest, endchar)
 register char *source;
 register char *dest;
 register char endchar;
{
	do {	*dest++ = *source;
	} while (*source++ != endchar);
}
