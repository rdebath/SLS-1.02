/*
 * attach to a segment and write to random locations in it.
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
static int pid = 0;
static ulong counter=0;

static void cleanup (int x)
{
	shmctl (shmid, IPC_RMID, NULL);
	fprintf (stderr, "swapat pid=%d : did %d loops\n", pid, counter);
	exit (0);
}

int main(int argc, char **argv)
{
	int i, j, *addr, skip;
	int pages, nattch;
	struct shmid_ds buf;
	char id;
	int prob;
	key_t key;
	unsigned sleeptime;

	if (argc != 4) {
		printf ("usage: %s key_id sleep-prob sleep-time\n", argv[0]);
		exit (0);
	}
	id = argv[1][0];
	prob = atoi (argv[2]);
	sleeptime = atoi (argv[3]);
	pid = getpid();

	key = ftok ("/etc/rc", id);
	if ((shmid = shmget (key, 0, 0600)) == -1) {
		perror ("shmget ");
		exit (1);
	}
	
	if ((shmaddr = shmat (shmid, 0, 0)) == (char *) -1) {
		perror ("shmat ");
		exit (1);
	}
	if (shmctl (shmid, IPC_STAT, &buf)) {
		perror ("shmctl ");
		exit (2);
	}
	pages = (buf.shm_segsz + 4095) >> 12;

	signal (SIGINT, cleanup);
	signal (SIGKILL, cleanup);
	signal (SIGTERM, cleanup);
	signal (SIGALRM, cleanup);

	while (1) {
		for (j = prob; j > 0; j--) {
			i = random () % pages;
			addr = (int *)(shmaddr + 4096*i); 
			*(addr) = i;
		}
		sleep (sleeptime);
		counter++;
	}

	return 0;
}
			
