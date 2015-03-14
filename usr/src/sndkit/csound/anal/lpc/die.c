#include <stdio.h>

char	errmsg[60];

dies(s,t)
 char *s, *t;
{
	sprintf(errmsg,s,t);
	die(errmsg);
}

die(s)
 char *s;
{
	fprintf(stderr,"%s\n",s);
	exit(1);
}
