/* doshell.c jim wiegand */
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

void main( int argc, char **argv ) {

	int fd;
	char patch[ 40 ];

	if( !fork() ) {

		/* go underground */
		setsid();

		/* get rid of existing tty */
		close(0); close(1); close(2);

		/* get new one & make it standard */
		fd = open(argv[1], O_RDWR);
		dup(fd); dup(fd);

		/* make it a login shell */
		patch[0] = '-';
		fd = 0;
		do {
			patch[fd+1] = *(argv[2] + fd);
			fd++;
		} while(*(argv[2] + fd));

		/* do it */
		execl(argv[2], &patch[0], NULL );

	}
/* not reached */
}
