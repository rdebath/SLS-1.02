#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#define LOAD_FILE "/proc/loadavg"

void loadavg(double *av1, double *av5, double *av15)
{
    int n;
    char buff[80];
    static int fd = -1;

    if (fd < 0) {
	if ((fd = open(LOAD_FILE, 0)) < 0) {
	    perror(LOAD_FILE);
	    exit(1);
	}
    }

    lseek(fd, 0L, 0);
    n = read(fd, buff, sizeof buff - 1);
    if (n < 0) {
	perror(LOAD_FILE);
	exit(1);
    }
    buff[n] = '\0';

    if (sscanf(buff, "%lf %lf %lf", av1, av5, av15) < 3) {
	fprintf(stderr, "bad data in " LOAD_FILE "\n");
	exit(1);
    }

    return;
}

#define MEM_FILE "/proc/meminfo"

void meminfo(unsigned *total, unsigned *used, unsigned *free,
	     unsigned *shared, unsigned *buffers)
{
    int n;
    char *cp, buff[1024];
    static int fd = -1;

    if (fd < 0) {
	if ((fd = open(MEM_FILE, 0)) < 0) {
	    perror(MEM_FILE);
	    exit(1);
	}
    }

    lseek(fd, 0L, 0);
    n = read(fd, buff, sizeof buff - 1);
    if (n < 0) {
	perror(MEM_FILE);
	exit(1);
    }
    buff[n] = '\0';

    /* skip over the first line */
    cp = strchr(buff, '\n');
    if (cp)
	cp = strchr(cp, ' ');

    if (!cp || sscanf(cp, "%lu %lu %lu %lu %lu", total, used,
		      free, shared, buffers) < 5) {
	fprintf(stderr, "bad data in " MEM_FILE "\n");
	exit(1);
    }

    return;
}

