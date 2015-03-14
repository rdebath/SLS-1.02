/*
 * Wait for a string on the stdin.  Returns a 0 on success, 1 on failure
 * and -1 on error.  This is an external program designed to be used in
 * shell scripts.
 */

#define TIMEOUT	10
#define BUF_SIZ	1024
#define HZ	60

#ifndef linux
#define STRSTR
#endif /* linux */

int wf_flag;

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>

#ifdef BSD
#include <setjmp.h>
jmp_buf wf_buf;
#endif /* BSD */

main(argc, argv)
int argc;
char *argv[];
{
	int i, j, timeout;
	char c, buf[BUF_SIZ], *string, *strstr();
	struct tms t;
	long t1;
	void exit();

	if (argc < 2 || argc > 3) {
		fprintf(stderr, "Usage: waitfor -n string\n");
		exit(-1);
	}

	if (argv[1][0] == '-') {
		timeout = atoi(&argv[1][1]);
		if (argc != 3) {
			fprintf(stderr, "Usage: waitfor -n string\n");
			exit(-1);
		}
		string = argv[2];
	}
	else {
		timeout = TIMEOUT;
		string = argv[1];
	}
					/* here we go.. */
	i = 0;
	t1 = times(&t);
	while ((times(&t) - t1) < (HZ * timeout)) {
		if ((j = getc_line()) != -1) {
			c = j & 0x7f;
					/* no NULLs please */
			if (c != '\0') {
				buf[i++] = c;
				buf[i] = '\0';
			}

			if (i >= BUF_SIZ) {
				fprintf(stderr, "waitfor: buffer overflow\n");
				exit(-1);
			}
					/* yea.. we found it! */
			if (strstr(buf, string))
				exit(0);
		}
	}
	exit(1);
}

int
getc_line()
{
	int wf_force();
	char c;
	unsigned int alarm();

	signal(SIGALRM, wf_force);
	wf_flag = 0;

	alarm(1);

#ifdef BSD
	if (setjmp(wf_buf))
		return(-1);
#endif /* BSD */

	if (read(0, &c, 1) <= 0) {
		alarm(0);
		return(-1);
	}
	if (wf_flag)
		return(-1);
	alarm(0);
	return(c & 0xff);
}

/* ARGSUSED */
static int
wf_force(dummy)
int dummy;
{
#ifdef BSD
	longjmp(wf_buf, 1);
#else /* BSD */
	signal(SIGALRM, wf_force);
	wf_flag = 1;
#endif /* BSD */
}

#ifdef STRSTR
/*
 * Return a pointer to the first occurance of string str2 in str1.
 * Returns a NULL if str2 is not in str1.
 */

char *
strstr(str1, str2)
char *str1, *str2;
{
	int len;
	len = strlen(str2);
	while (*str1) {
		if (*str2 == *str1) {
			if (!strncmp(str2, str1, len))
				return(str1);
		}
		str1++;
	}
	return(NULL);
}
#endif /* STRSTR */
