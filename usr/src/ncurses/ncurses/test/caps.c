#include <term.h>

main()
{
int i;
char *s;

	setupterm(NULL, 1, (int *)0);
	fprintf(stderr, "BOOLCOUNT = %d\n", BOOLCOUNT);
	for (i = 0; i < BOOLCOUNT; i++) {
		s = boolnames[i];
		fprintf(stderr, "%-10s 'boolnames[%d]' is %d\n", s, i, tigetflag(s));
	}
	fprintf(stderr, "NUMCOUNT = %d\n", NUMCOUNT);
	for (i = 0; i < NUMCOUNT; i++) {
		s = numnames[i];
		fprintf(stderr, "%-10s 'numnames[%d]' is %d\n", s, i, tigetnum(s));
	}
	fprintf(stderr, "STRCOUNT = %d\n", STRCOUNT);
	for (i = 0; i < STRCOUNT; i++) {
		s = strnames[i];
		fprintf(stderr, "%-10s 'strnames[%d]' is %s\n", s, i, tigetstr(s));
	}
}
