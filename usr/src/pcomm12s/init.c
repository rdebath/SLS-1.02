/*
 * Display the welcome screen and find the Pcomm support files.  Returns a
 * pointer to a static area (or shared memory) containing the STATUS
 * structure.  All errors are fatal.
 */

#define TMP_FILE	"/tmp/pcommXXXXXX"

#include <stdio.h>
#include <curses.h>
#include "config.h"
#include "misc.h"
#include "status.h"

#ifdef SHAREDMEM
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif /* SHAREDMEM */

struct STATUS *
init(short_cut)
char *short_cut;
{
	char *strcpy();
	struct STATUS *s_ptr;
	void info();
#ifdef SHAREDMEM
	int mode;
	extern int shm_id;
	char *shmat(), *memset();
	void perror(), exit();

	/*
	 * Since the "pcomm_input" program does not run set-user/group-id
	 * the mode must be set so the effective ID can read/write to the
	 * shared memory segment.  Kinda strange... real ID's aren't used.
	 */
#ifdef SETUGID
	mode = 0666;
#else /* SETUGID */
	mode = 0600;
#endif /* SETUGID */
					/* create a shared memory segment */
	shm_id = shmget(IPC_PRIVATE, sizeof (struct STATUS),  mode|IPC_CREAT|IPC_EXCL|IPC_NOWAIT);
	if (shm_id < 0) {
		endwin();
		perror("shmget");
		exit(1);
	}
	s_ptr = (struct STATUS *) shmat(shm_id, (char *) 0, 0);
	if ((int) s_ptr == -1) {
		endwin();
		perror("shmat");
		exit(1);
	}
#else /* SHAREDMEM */
	char *mktemp(), tempfile[sizeof(TMP_FILE)];
	static struct STATUS s;
	s_ptr = &s;
#endif /* SHAREDMEM */
					/* some defaults */
	s_ptr->fd = -1;
	s_ptr->dup_fd = -1;
	s_ptr->add_lf = 0;
	s_ptr->log = 0;
	s_ptr->print = 0;
	strcpy(s_ptr->log_path, "NOT_DEFINED");

#ifdef SHAREDMEM
	s_ptr->clr = 0;
	s_ptr->row = 0;
	s_ptr->col = 0;
	memset(s_ptr->vs, '\0', MAX_ROW * MAX_COL);
#else /* SHAREDMEM */
	strcpy(tempfile, TMP_FILE);
	strcpy(s_ptr->vs_path, mktemp(tempfile));
#endif /* SHAREDMEM */
					/* display herald if no short-cut */
	if (short_cut == NULL)
		info(AUTO_CLEAR);

	erase();
	refresh();
	return(s_ptr);
}

/*
 * Search the extra directory (supplied on the command line), then the
 * directory in the PCOMM environmental variable, then the current working
 * directory, and lastly, the default directory.
 */

char *
findfile(extra, name)
char *extra, *name;
{
	int i;
	char *pcomm, *getenv(), *path, pbuf[200], *getcwd(), *str_dup();
	char temp[200];

					/* see if PCOMM variable is set */
	pcomm = getenv("PCOMM");
	if (pcomm == NULL || *pcomm == '\0')
		pcomm = NULL;
	else {
					/* zap the trailing separator */
		if (pcomm[strlen(pcomm)-1] == '/')
			pcomm[strlen(pcomm)-1] = '\0';
	}

	for (i=0; i<4; i++) {
					/* directory search order */
		switch (i) {
			case 0:		/* extra directory from command line */
				path = extra;
				break;
			case 1:		/* PCOMM environmental variable */
				path = pcomm;
				break;
			case 2:		/* current working directory */
				path = getcwd(pbuf, 200);
				break;
			case 3:		/* Pcomm's default directory */
				path = DEFAULT_DIR;
				break;
		}
		if (path == NULL)
			continue;

		sprintf(temp, "%s/%s", path, name);
					/* read permission checked */
		if (!access(temp, 4))
			return(str_dup(temp));
	}
	return(NULL);
}
