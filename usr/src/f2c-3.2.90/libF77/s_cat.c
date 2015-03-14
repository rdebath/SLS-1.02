#include "f2c.h"

VOID s_cat(lp, rpp, rnp, np, ll)
char *lp, *rpp[];
#ifdef f2c_i2
short int rnp[], *np, ll;
#else
long int rnp[], *np, ll;
#endif
{
int i, n, nc;
char *rp;

n = *np;
for(i = 0 ; i < n ; ++i)
	{
	nc = ll;
	if(rnp[i] < nc)
		nc = rnp[i];
	ll -= nc;
	rp = rpp[i];
	while(--nc >= 0)
		*lp++ = *rp++;
	}
while(--ll >= 0)
	*lp++ = ' ';
}
