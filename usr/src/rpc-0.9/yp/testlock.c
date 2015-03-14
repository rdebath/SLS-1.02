#include <sys/file.h>
#include <errno.h>

main(argc, argv)
char **argv;
{
	int fd;
	int i;

	if( (fd=open(argv[1], O_RDONLY)) == -1) {
		perror("open");
		exit(1);
	}
	
	i = flock(fd, LOCK_EX | LOCK_NB);
	printf("%d %d\n", i, errno);
}

