#include <stdio.h>
#include <sys/stat.h>

struct stat sbuf;
main(int argc, char *argv[])
{
	if (argc==2) 
	{	lstat(argv[1], &sbuf);
		printf("%d", sbuf.st_size );
	}
}
