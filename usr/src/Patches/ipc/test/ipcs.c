#include <stdio.h>
#include <errno.h>
#include <sys/sem.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <linux/ipc.h>

static struct seminfo seminfo; 
static struct msginfo msginfo;
static struct shminfo shminfo;
/*
initialize seminfo, msginfo, shminfo and call ipc_init.
*/

main(int argc, char **argv)
{
   
    if (argc != 2) {
	fprintf (stderr, "usage : %s  [sem | msg | shm]\n", argv[0]);
        return 1;
    } 

/* Change these values to suit your needs */
    seminfo.semmni = 10;
    msginfo.msgmni = 10;
    shminfo.shmmni = 10;
    seminfo.semopm = 10;  
    msginfo.msgmnb = 8192; 
/* The rest are 0 so they get default values from <linux/ipc.h> */

    if (!strcmp (argv[1], "sem")) {
        if (ipc_init (SEM_INIT, &seminfo)) {
	    perror ("sem_init ");
	    return 1;
        }
    }
    else if (!strcmp (argv[1], "msg")) {
        if (ipc_init (MSG_INIT, &msginfo)) {
	    perror ("msg_init ");
	    return 1;
        }
    }
    else if (!strcmp (argv[1], "shm")) {
        if (ipc_init (SHM_INIT, &shminfo)) {
	    perror ("shm_init ");
	    return 1;
        }
    }
    else { 
	fprintf (stderr, "usage : %s  [sem | msg | shm]\n", argv[0]);
        return 1;
    }
	
    printf ("ipc_init succeeded\n");
    return 0;
}
    
