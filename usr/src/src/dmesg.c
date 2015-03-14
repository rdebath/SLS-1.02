/*
 * dmesg.c --- prints out the contents of the Kernel ring buffer 
 */

#include <linux/unistd.h>
#include <stdio.h>

#define __NR_klog	__NR_syslog

static inline _syscall3(int,klog,int,type,char *,b,int,len)

main(argc, argv)
	int	argc;
	char	**argv;
{
	char buf[4096];
	int	i;
	int	cmd = 3;

	if (argc > 2) {
		fprintf(stderr, "Usage: %s [-c]\n", argv[0]);
		exit(1);
	}
	if (argc > 1 && !strcmp(argv[1], "-c")) {
		cmd = 4;
	}
	i = klog(cmd, buf, sizeof(buf));
	if (i < 0) {
		perror("klog");
		exit(1);
	}
	write(1, buf, i);
	exit(0);
}

	
