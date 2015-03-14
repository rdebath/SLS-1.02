/*
 * Routines to get or release a TTY port.
 */

#define MAX_PID	30000
#define TRUE	1
#define FALSE	0

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include "config.h"
#include "dial_dir.h"
#include "modem.h"

#ifdef BSD
#include <sys/file.h>
#endif /* BSD */

#ifdef UNIXPC
#include <sys/phone.h>
#endif /* UNIXPC */

#ifdef XENIX_LOCKS
#include <ctype.h>
#endif /* XENIX_LOCKS */

static int getty_status = 0;
static char *lock_path = NULL;
/*
 * Finds a free (or requested) serial port.  Creates a lock file to hold
 * for our use.  Loads the modem database.  A non-zero return code means
 * all ports (or the requested port) are busy.
 */

int
get_port()
{
	extern int fd;
	register int i;
	int j, k, lfd, list[NUM_TTY], cmask, tbaud, is_dev, progpid;
	char file[80], buf[80], message[80], *str_rep(), *last_c;
	unsigned int sleep();
	void error_win(), line_set(), release_port(), send_str();

	is_dev = chk_script(dir->script[dir->d_cur]);
	/*
	 * If you already have a port, see if it is good enough for the
	 * current request.
	 */
#ifdef KEEP_PORT
	if (fd != -1) {
		if (!strcmp(dir->script[dir->d_cur], modem->tty[modem->t_cur])
		 || (!is_dev && ck_speed(modem->t_cur, dir->baud[dir->d_cur]))){
			/*
			 * Reset the line because the baud rate (or other
			 * parameters) may have changed.
			 */
			line_set();
			return(0);
		}
	}
#endif /* KEEP_PORT */

	release_port(VERBOSE);

	list[0] = -1;
	/*
	 * See if you want a specific TTY port.  If the script field in the
	 * dialing directory is a valid device name, then use that TTY.
	 */
	if (is_dev) {
		for (i=0; i<modem->t_entries; i++) {
					/* and it exists in modem database */
			if (!strcmp(dir->script[dir->d_cur], modem->tty[i])) {
				list[0] = i;
				list[1] = -1;
				break;
			}
		}
					/* oops... we don't know that port */
		if (list[0] == -1) {
			sprintf(message, "Device \"%s\" in the script field doesn't exist in", dir->script[dir->d_cur]);
			sprintf(buf, "modem/TTY database \"%s\"", modem->m_path);
			error_win(0, message, buf);
		}
	}

	/*
	 * Create a list of acceptable TTYs.  It searches the modem database
	 * for the requested baud rate.
	 */
	k = 0;
	if (list[0] == -1) {
		for (i=0; i<modem->t_entries; i++) {
					/* skip ports with no modems */
			if (!strcmp(modem->tname[i], "DIRECT"))
				continue;

					/* can handle requested baud rate? */
			if (ck_speed(i, dir->baud[dir->d_cur]))
				list[k++] = i;
		}
					/* the end of list marker */
		list[k] = -1;
	}
					/* empty list? */
	if (list[0] == -1) {
		sprintf(message, "No modem at a %d baud rating exists in the", dir->baud[dir->d_cur]);
		sprintf(buf, "modem database \"%s\"", modem->m_path);
		error_win(0, message, buf);
		return(1);
	}
					/* check the list for a free port */
	i = 0;
	while (list[i] != -1) {
					/* create a lock file name */
		sprintf(file, "%s/LCK..%s", LOCK_DIR, modem->tty[list[i]]);

#ifdef XENIX_LOCKS
		last_c = file + strlen(file)-1;
		if (isuuper(*last_c))
			*last_c = tolower(*last_c);
#endif /* XENIX_LOCKS */

#ifdef DEBUG
		fprintf(stderr, "get_port: checking '/dev/%s'\n", modem->tty[list[i]]);
#endif /* DEBUG */

					/* no lock exists or it is dead */
		if (checklock(file)) {
			getty_status = set_getty(modem->tty[list[i]], FALSE);

			cmask = umask(0);
			if ((lfd = open(file, O_CREAT|O_EXCL|O_WRONLY, 0666)) < 0) {
				if (getty_status)
					set_getty(modem->tty[list[i]], TRUE);
				sprintf(buf, "\"%s\"", file);
				error_win(1, "Can't create the lockfile", buf);
			}
			umask(cmask);
#ifdef ASCII_PID
			sprintf(buf, "%10d\n", getpid());
			write(lfd, buf, 11);
#else /* ASCII_PID */
			progpid = getpid();
			write(lfd, (char *) &progpid, sizeof(int));
#endif /* ASCII_PID */
			close(lfd);
					/* store the new values */
			lock_path = str_rep(lock_path, file);
			modem->t_cur = list[i];

					/* open the device (ignore DCD) */
			sprintf(buf, "/dev/%s", modem->tty[modem->t_cur]);
			if ((fd = open(buf, O_RDWR|O_NDELAY)) < 0) {
				if (getty_status)
					set_getty(modem->tty[modem->t_cur], TRUE);
				sprintf(file, "Can't open port \"%s\" for read and write", buf);
				error_win(1, file, "");
			}
					/* change line settings */
			line_set();
					/* ...just to be sure */
			close(open(buf, O_RDWR));

					/* turn off the O_NDELAY setting */
			tty_noblock(fd, FALSE);

					/* load the modem data base */
			modem->m_cur = -1;
			for (j=0; j<modem->m_entries; j++) {
				if (!strcmp(modem->tname[modem->t_cur], modem->mname[j])) {
					modem->m_cur = j;
					break;
				}
			}
			if (modem->m_cur == -1) {
				sprintf(buf, "Modem name \"%s\" in TTY database",
				 modem->tname[modem->t_cur]);
				error_win(1, buf, "does not exist in modem database");
			}
					/* initialize the modem */
			if (modem->init_sp[modem->t_cur]) {
				tbaud = dir->baud[dir->d_cur];
				dir->baud[dir->d_cur] = modem->init_sp[modem->t_cur];
				line_set();
				send_str(modem->init[modem->m_cur], SLOW);
				dir->baud[dir->d_cur] = tbaud;
			}
			else
				send_str(modem->init[modem->m_cur], SLOW);
			sleep(1);
			return(0);
		}
		i++;
	}
	error_win(0, "All ports are busy now, try again later", "");
	return(1);
}

/*
 * Release the port.  Closes the file descriptor and removes the
 * lock file
 */

void
release_port(verbose)
int verbose;
{
	extern int fd;
	extern char *null_ptr;
	char buf[80];
	void free_ptr(), hang_up();

	/*
	 * The modem structure can't be guaranteed to exist yet.  For example,
	 * an error in reading one of the other support files would cause
	 * this routine to be used before the MODEM structure gets allocated.
	 */
	if (modem == NULL)
		return;
					/* close the port */
	if (fd != -1) {
		tty_flush(fd, 2);
		/*
		 * Since HUPCL is set, the close() should drop the DTR and
		 * hang up the modem (provided you've got the modem to
		 * respond to DTR).  Since this is not guaranteed, we send
		 * the hang_up string first.
		 */
		hang_up(verbose);
		close(fd);
	}
					/* remove the lock */
	if (lock_path != NULL && *lock_path != '\0') {
		if (unlink(lock_path)) {
			sprintf(buf, "\"%s\"", lock_path);
			error_win(0, "Can't remove the lock file", buf);
		}
		free_ptr(lock_path);
		lock_path = null_ptr;
	}
					/* turn the getty back on? */
	if (getty_status && modem->t_cur != -1)
		set_getty(modem->tty[modem->t_cur], TRUE);
					/* clean up the structure */
	fd = -1;
	modem->m_cur = -1;
	modem->t_cur = -1;
	return;
}

/*
 * Turn the /etc/getty on or off for the specified port.  A non-zero return
 * code means that the getty was on.  Systems with uugetty() or dedicated
 * dialout ports won't need this routine.
 */

/* ARGSUSED */
static int
set_getty(tty, on)
char *tty;
int on;
{
#ifdef UNIXPC
	int i, ret_code;
	char buf[40];
	unsigned int sleep();
					/* the last three characters */
	i = strlen(tty) -3;

	ret_code = 0;
	if (on) {
		sprintf(buf, "setgetty %s 1", tty+i);
		system(buf);
	}
	else {
		sprintf(buf, "setgetty %s 0", tty+i);
		if (system(buf) == 512)
			ret_code++;
		sleep(1);
	}
	return(ret_code);
#else /* UNIXPC */
	/*
	 * If you don't have one of these cute little routines, you
	 * might wanna write one.  It should check for an existing lock
	 * file, edit the /etc/inittab file, and issue an init -q.
	 * The return code should tell if there was a getty or not.
	 * Obviously the program would be suid to root.
	 */
	return(0);
#endif /* UNIXPC */
}

/*
 * Check the lock file for a valid pid value.  Error conditions such
 * as not being able to open the lock file or not being able to interpret
 * the contents of the lock file cause the code to assume that the lock
 * file is valid.  Let the user worry about weird special cases.  A 
 * non-zero return code means the lock is dead or doesn't exist.
 */

static int
checklock(lockfile)
char *lockfile;
{
	extern int errno;
	int lfd, lckpid, n;
	char buf[40];
					/* doesn't exist */
	if (access(lockfile, 0))
		return(1);
					/* can't open the lock file */
	if ((lfd = open(lockfile, 0)) < 0)
		return(0);

#ifdef ASCII_PID
	if ((n = read(lfd, buf, 40)) <= 0) {
		close(lfd);
		return(0);
	}
	close(lfd);
	buf[n--] = '\0';
	lckpid = atoi(buf);
#else /* ASCII_PID */
	if (read(lfd, (char *) &lckpid, sizeof(int)) != sizeof(int)) {
		close(lfd);
		return(0);
	}
	close(lfd);
#endif /* ASCII_PID */
					/* invalid pid? */
	if (lckpid <= 0 || lckpid > MAX_PID)
		return(0);

	if ((kill(lckpid, 0) == -1) && (errno == ESRCH)) {
		/*
		 * If the kill was unsuccessful due to an ESRCH error,
		 * that means the process is no longer active and the
		 * lock file can be safely removed.
		 */
		unlink(lockfile);
		sleep(1);
		return(1);
	}
	/*
	 * If the kill() was successful, that means the process must
	 * still be active.
	 */
	return(0);
}

/*
 * Check to see if the desired baud rate can be handled by the modem.
 * Uses the connect strings to make this determination.  The first
 * argument is the index into the TTY database.  A non-zero return code
 * means "yes it can".
 */

static int
ck_speed(tty, baud)
int tty, baud;
{
	int i, mod;
	char buf[60];
					/* find the modem database */
	mod = -1;
	for (i=0; i<modem->m_entries; i++) {
		if (!strcmp(modem->mname[i], modem->tname[tty])) {
			mod = i;
			break;
		}
	}
	if (mod == -1) {
		sprintf(buf, "Modem name \"%s\" in TTY database", modem->tname[tty]);
		error_win(1, buf, "does not exist in modem database");
	}

#ifdef DEBUG
	fprintf(stderr, "ck_speed: checking modem \"%s\" for %d baud\n", modem->mname[mod], baud);
#endif /* DEBUG */

	switch (baud) {
		case 300:
			if (*modem->con_3[mod] != '\0')
				return(1);
			break;
		case 1200:
			if (*modem->con_12[mod] != '\0')
				return(1);
			break;
		case 2400:
			if (*modem->con_24[mod] != '\0')
				return(1);
			break;
		case 4800:
			if (*modem->con_48[mod] != '\0')
				return(1);
			break;
		case 9600:
			if (*modem->con_96[mod] != '\0')
				return(1);
			break;
		case 19200:
			if (*modem->con_192[mod] != '\0')
				return(1);
			break;
	}
	return(0);
}

/*
 * Check to see if the script field is a valid device name.
 */

chk_script(script)
char *script;
{
	char buf[80], *strcpy(), *strcat();

	if (*script == '\0')
		return(0);

	strcpy(buf, "/dev/");
	strcat(buf, script);

	if (!access(buf, 0))
		return(1);
	return(0);
}
