/*
 * krishna balasubramanian 1993
 * check shm multiple attaches are actually sharing memory!
 */

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/shm.h>
static char *shmaddr;

static int shmid = 0;
static unsigned long counter = 0;
static int pid;

static void cleanup (int x)
{
	shmctl (shmid, IPC_RMID, NULL);
	fprintf (stderr, "shmswap pid=%d did %d loops\n", pid, counter);
	exit (0);
}


int main(int argc, char **argv)
{
	int i, *addr, skip;
	int pages, nattch;
	char id;
	unsigned int sleeptime = 0;
	key_t key;

	if (argc != 4) {
		printf ("usage: %s key_id pages sleep\n", argv[0]);
		exit (0);
	}
	id = argv[1][0];
	pages = atoi (argv[2]);
	sleeptime = atoi (argv[3]);
	key = ftok ("/etc/rc", id);
	pid = getpid();
	
	if ((shmid = shmget (key, pages << 12, IPC_EXCL | 01600)) < 0) {
		perror ("shmget ");
		exit (1);
	}
	if ((shmaddr = shmat (shmid, 0, 0)) == (char *) -1) {
		perror ("shmat ");
		if(shmctl (shmid, IPC_RMID, NULL)) 
			perror ("shmctl IPC_RMID ");
		exit (1);
	}
	skip = 1;
	counter = 0;
	signal (SIGKILL, cleanup);
	signal (SIGINT, cleanup);
	signal (SIGTERM, cleanup);
	signal (SIGALRM, cleanup);

loop:
	if (skip > 64)
		skip = 2;
	for (i=0; i < pages; i+= skip) {
		addr = (int *)(shmaddr + 4096*i); 
		*(addr) = i;
	}

	sleep (sleeptime);
	skip <<= 1;
	counter ++;
	goto loop;

	return 0;
}
			
