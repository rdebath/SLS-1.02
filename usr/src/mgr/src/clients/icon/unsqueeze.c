# include "stdio.h"
main()
{/* undoes the runlength encoding */
/* remember to copy 8 bytes */
int oc, c, k;
for(k=8; k--; )
	if ((c=getchar())!=EOF)
		{
		if (k==6 && oc=='y')
			{
			if ( c=='x') c='z';
			else if (c=='z') c='x';
			}
		putchar(oc=c);
		}
	else break;
if (c==EOF) exit(0);
while ( (k=getchar())!=EOF)
	{
	k&=0377;
	if (k>=128)
		for (k -= 127; k>0; k--)
			putchar(getchar());
	else
		{
		c=getchar();
		while(k--)
			putchar(c);
		}
	}
}
