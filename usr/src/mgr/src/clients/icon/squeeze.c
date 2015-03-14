# include "stdio.h"
main()
{/* reads a file and does byte run-length encoding. */
/* copies the first eight bytes regardless */
/* if first two bytes are yz changes to yx */
char bb[128], *bp, *b;
int oc= EOF, c, nrep;
bp=bb;
for (nrep=8; ( nrep-- > 0 && (c=getchar())!=EOF) ; )
	{
	if (nrep==6 && oc=='y')
		{
		if ( c=='z') c='x';
		else if ( c=='x') c='z';
		}
	putchar(c);
	oc=c;
	}
if (c==EOF) exit(0);
nrep=0;
while (1)
	{
	c =getchar();
	if (c==oc)
		{
		if (nrep>126)
			{
			if (bp>bb)
				{
				putchar(127+bp-bb);
				for(b=bb; b<bp; b++)
					putchar(*b);
				bp=bb;
				}
			putchar(nrep);
			putchar(oc);
			nrep=0;
			}
		nrep++;
		}
	else
		{
		if (nrep>2)
			{
			if (bp>bb)
				{
				putchar(127+bp-bb);
				for(b=bb; b<bp; b++)
					putchar(*b);
				bp=bb;
				}
			putchar(nrep);
			putchar(oc);
			nrep=0;
			}
		while (nrep--)
			*bp++ = oc;
		if (bp-bb>125)
			{
			putchar(127+bp-bb);
			for(b=bb; b<bp; b++)
				putchar(*b);
			bp=bb;
			}
		oc=c; nrep=1;
		}
	if (c==EOF) break;
	}
if (bp>bb)
	{
	putchar(127+bp-bb);
	for(b=bb; b<bp; b++)
		putchar(*b);
	}
}
