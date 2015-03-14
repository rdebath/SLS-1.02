/*
 #  getutmp  -- routines to get utmp file entries
 #  Roger Maurice Levasseur
 #  July 1985 - 4.2BSD
 #  October 1985 - 2.9BSD
 #  New Mexico Tech
 #
 */

#include <sys/types.h>
#ifndef COHERENT
#include <sys/file.h>
#else
#include <sys/fcntl.h>
#endif
#include <utmp.h>
#include <pwd.h>

#ifndef NULL
#define NULL 0
#endif

#define   UTMP  "/etc/utmp"		/* location of the utmp file */

/*
 #  when utmp is closed, u_utmp contains a -1,
 #  otherwise it will contain the descriptor
 */
static int u_utmp = -1;

/*
 #   setutent -- open or rewind the utmp file to the start of the file
 #
 #     A successful open returns a 1 (true) otherwise a NULL.
 */

setutent()
{
	if (u_utmp >= 0) {

		/*
		 #  it is open already; rewind it
		 #  if the long seek fails, return it as an error
		 */

		if (lseek(u_utmp,0L,0) == -1) return(NULL);
		return(1);
	}

	/*
	 #  it is not open, so lets open it
	 #  0 == READ ONLY
	 */

	if ((u_utmp = open(UTMP,O_RDONLY)) < 0) return(NULL);
	else return(1);
}


/*
 #  endutent -- close the utmp file if it is open.
 #
 #     It is not an error to call this routine if the file is already closed.
 #     A 1 (true) is returned on successful closure, a NULL on error
 */

endutent()
{
	if (u_utmp >= 0) {
		if (close(u_utmp) == -1) {

			/*
			 #  might be wise just to check anyhow in case funky
			 #  things were done when getutmp wasn't looking.
			 */

			u_utmp = -1;
			return(NULL);
		}
		u_utmp = -1;
	}
	return(1);
}



/*
 #  getutent  -- get the next entry in the utmp file.
 #
 #     Returns a pointer to the next valid structure, or returns a
 #     NULL on error, or an EOF, meaning the utmp file is exhausted.
 */

struct utmp *getutent()
{
	static struct utmp next;

	if (u_utmp < 0) {

		/*
		 #  if the file hasn't been open yet, get it opened
		 #  and handle errors accordingly
		 */

		if ( setutent() == NULL) return(NULL);
	}

	for (;;) {	/* loop until a valid entry, or file is exhausted */

		if (read(u_utmp,&next,sizeof(struct utmp)) == 0) return(NULL);

		if (next.ut_name[0] == '\0') continue; /* an 'empty' entry */
		return(&next);
	}
}


/*
 #  getutuid  -- if a user by the given user id is on the system, this
 #     returns a pointer to a struct utmp; otherwise a NULL pointer
 */

struct utmp *getutuid(uid)
int uid;
{
	struct utmp *up;
	struct passwd *getpwnam(), *ptr;
	char nambuf[9];

	if (setutent() == NULL) return(NULL); /* start from beginning */

	while (up = getutent()) {

		/*
		 #   assuming here too that 8 is unwise, but it is not
		 #   #def'd in the include file like it should be, so....
		 */

		strncpy(nambuf,up->ut_name,8);
		nambuf[8] = '\0';

		/*
		 #  fetch an uid for that name (if possible), and see if it matches
		 */

		setpwent();	/* see getpwent(3) */

		if ((ptr = getpwnam(up->ut_name)) == NULL) continue;

		if (ptr->pw_uid == uid) return(up);
	}
	endpwent();		/* done for now */
	return(NULL);	/* no matching entry */
}

/*
 #   getutnam  -- if the given user's name is logged into the system,
 #      a pointer is returned to a struct utmp; otherwise a NULL pointer is
 #      returned.
 */

struct utmp *getutnam(name)
char *name;
{
	struct utmp *up;

	if (setutent() == NULL) return(NULL);

	while (up = getutent()) 
		if (!strncmp(name,up->ut_name,8)) return(up);

	return(NULL);
}

/* pututline by Harry C. Pulley, IV.  It doesn't work yet, but doesn't harm 
   anything.  It just returns since the open() fails.
*/
int pututline(new)
struct utmp *new;
{
	struct utmp *curr;
	int new_utmp=-1,written=0;

	new_utmp=open("/tmp/utmp",O_RDWR|O_CREAT,0666);

	if (new_utmp<0)
		return -1;

	setutent();

	while (curr=getutent())
	{
		if (strcmp(curr->ut_line,new->ut_line)==0)
		{
			written++;

			if (strlen(new->ut_name))
				write(new_utmp,new,sizeof(struct utmp));
		}
		else
			write(new_utmp,curr,sizeof(struct utmp));
	}

	endutent();

	if (!written)
		write(new_utmp,new,sizeof(struct utmp));

	close(new_utmp);
	
	system("cp /tmp/utmp /etc/utmp");

	unlink("/tmp/utmp");
}
