#ifdef BCOPY_MISSING
/* Berkeley type bcopy, allows for overlapped strings (copies backward). */
bcopy (s,d,n)
char *s, *d;
int n;
{
   register char *from, *to, *base;

   base = s;
   from = base + n;
   to = d + n;
   while (from > base)
      *--to = *--from;
}
#endif

#ifdef BZERO_MISSING
bzero (s,n)
char *s;
int n;
{
   register char *start;
   register int i=n;;

   start = s;

   while (i--)
      *start++ = 0;
}
#endif

char *basename(string)
char *string;
{
	register char *ptr = string;

	while (*ptr)
		if (*ptr++=='/')
			string=ptr;
	
	return string;
}
