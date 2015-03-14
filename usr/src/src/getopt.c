#include <string.h>

main(int argc, char *argv[])
{
	int i, k; char *cptr;
	if (argc < 2)
		return(1);
	for (i=2; i<argc; i++)
	{
		k = 0;
		if (argv[i][k] != '-')
		{
			printf("--");
			while (i<argc)
				printf(" %s", argv[i++] );
			puts("");
			return(0);
		}
		else
		{
			k++;
			while (argv[i][k] && (cptr = strchr(argv[1], argv[i][k])))
			{
				printf("-%c ", *cptr );
				if (':' != cptr[1])
					k++;
				else
				{
					if (argv[i][k+1])
						printf("%s ", argv[i]+1+k );
					else
					{	
						i++;
						if (i>=argc)
							return(2);
						printf("%s ", argv[i] );
					}
					break;
				}
			}
		}
	}
	puts("--");
	return(0);
}
