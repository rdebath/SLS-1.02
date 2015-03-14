#include <stdio.h>
#include <errno.h>
#include <sys/sem.h>
#include <sys/msg.h>
#include <sys/shm.h>
#include <linux/ipc.h>


main(int argc, char **argv)
{
    if (argc != 2) {
	fprintf (stderr, "usage : %s  [sem | msg | shm]\n", argv[0]);
        return 1;
    } 

    if (!strcmp (argv[1], "sem")) {
        if (ipc_rm (SEM_RM)) {
	    perror ("sem_rm ");
	    return 1;
        }
    }
    else if (!strcmp (argv[1], "msg")) {
        if (ipc_rm (MSG_RM)) {
	    perror ("msg_rm ");
	    return 1;
        }
    }
    else if (!strcmp (argv[1], "shm")) {
        if (ipc_rm (SHM_RM)) {
	    perror ("shm_rm ");
	    return 1;
        }
    }
    else { 
	fprintf (stderr, "usage : %s  [sem | msg | shm]\n", argv[0]);
        return 1;
    }
	
    printf ("ipc_rm succeeded\n");
    return 0;
}
    
