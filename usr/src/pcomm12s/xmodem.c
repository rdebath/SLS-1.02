/*
 * Miscellaneous routines to support the xmodem file transfer protocols.
 */

#define TMP_FILE	"trunXXXXXX"

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "config.h"
#include "misc.h"
#include "param.h"
#include "xmodem.h"

#ifdef BSD
#include <setjmp.h>
jmp_buf gl_buf, rl_buf;
#endif /* BSD */

/*
 * Calculate the CRC for the given buffer
 */

unsigned short
calc_crc(buf, len)
unsigned char *buf;
int len;
{
	register int i;
	unsigned short crc;
	static unsigned short crctab[256] = {
	0x0000, 0x1021, 0x2042, 0x3063, 0x4084, 0x50a5, 0x60c6, 0x70e7,
	0x8108, 0x9129, 0xa14a, 0xb16b, 0xc18c, 0xd1ad, 0xe1ce, 0xf1ef,
	0x1231, 0x0210, 0x3273, 0x2252, 0x52b5, 0x4294, 0x72f7, 0x62d6,
	0x9339, 0x8318, 0xb37b, 0xa35a, 0xd3bd, 0xc39c, 0xf3ff, 0xe3de,
	0x2462, 0x3443, 0x0420, 0x1401, 0x64e6, 0x74c7, 0x44a4, 0x5485,
	0xa56a, 0xb54b, 0x8528, 0x9509, 0xe5ee, 0xf5cf, 0xc5ac, 0xd58d,
	0x3653, 0x2672, 0x1611, 0x0630, 0x76d7, 0x66f6, 0x5695, 0x46b4,
	0xb75b, 0xa77a, 0x9719, 0x8738, 0xf7df, 0xe7fe, 0xd79d, 0xc7bc,
	0x48c4, 0x58e5, 0x6886, 0x78a7, 0x0840, 0x1861, 0x2802, 0x3823,
	0xc9cc, 0xd9ed, 0xe98e, 0xf9af, 0x8948, 0x9969, 0xa90a, 0xb92b,
	0x5af5, 0x4ad4, 0x7ab7, 0x6a96, 0x1a71, 0x0a50, 0x3a33, 0x2a12,
	0xdbfd, 0xcbdc, 0xfbbf, 0xeb9e, 0x9b79, 0x8b58, 0xbb3b, 0xab1a,
	0x6ca6, 0x7c87, 0x4ce4, 0x5cc5, 0x2c22, 0x3c03, 0x0c60, 0x1c41,
	0xedae, 0xfd8f, 0xcdec, 0xddcd, 0xad2a, 0xbd0b, 0x8d68, 0x9d49,
	0x7e97, 0x6eb6, 0x5ed5, 0x4ef4, 0x3e13, 0x2e32, 0x1e51, 0x0e70,
	0xff9f, 0xefbe, 0xdfdd, 0xcffc, 0xbf1b, 0xaf3a, 0x9f59, 0x8f78,
	0x9188, 0x81a9, 0xb1ca, 0xa1eb, 0xd10c, 0xc12d, 0xf14e, 0xe16f,
	0x1080, 0x00a1, 0x30c2, 0x20e3, 0x5004, 0x4025, 0x7046, 0x6067,
	0x83b9, 0x9398, 0xa3fb, 0xb3da, 0xc33d, 0xd31c, 0xe37f, 0xf35e,
	0x02b1, 0x1290, 0x22f3, 0x32d2, 0x4235, 0x5214, 0x6277, 0x7256,
	0xb5ea, 0xa5cb, 0x95a8, 0x8589, 0xf56e, 0xe54f, 0xd52c, 0xc50d,
	0x34e2, 0x24c3, 0x14a0, 0x0481, 0x7466, 0x6447, 0x5424, 0x4405,
	0xa7db, 0xb7fa, 0x8799, 0x97b8, 0xe75f, 0xf77e, 0xc71d, 0xd73c,
	0x26d3, 0x36f2, 0x0691, 0x16b0, 0x6657, 0x7676, 0x4615, 0x5634,
	0xd94c, 0xc96d, 0xf90e, 0xe92f, 0x99c8, 0x89e9, 0xb98a, 0xa9ab,
	0x5844, 0x4865, 0x7806, 0x6827, 0x18c0, 0x08e1, 0x3882, 0x28a3,
	0xcb7d, 0xdb5c, 0xeb3f, 0xfb1e, 0x8bf9, 0x9bd8, 0xabbb, 0xbb9a,
	0x4a75, 0x5a54, 0x6a37, 0x7a16, 0x0af1, 0x1ad0, 0x2ab3, 0x3a92,
	0xfd2e, 0xed0f, 0xdd6c, 0xcd4d, 0xbdaa, 0xad8b, 0x9de8, 0x8dc9,
	0x7c26, 0x6c07, 0x5c64, 0x4c45, 0x3ca2, 0x2c83, 0x1ce0, 0x0cc1,
	0xef1f, 0xff3e, 0xcf5d, 0xdf7c, 0xaf9b, 0xbfba, 0x8fd9, 0x9ff8,
	0x6e17, 0x7e36, 0x4e55, 0x5e74, 0x2e93, 0x3eb2, 0x0ed1, 0x1ef0};

	crc = 0;
	for (i=0; i<len; i++)
		crc = (crc<<8) ^ crctab[(crc>>8) ^ *buf++];

	return(crc);
}

/*
 * Calculate the checksum for the given buffer.
 */

unsigned char
calc_sum(buf, len)
unsigned char *buf;
int len;
{
	unsigned char sum;

	sum = 0;
	while (--len >= 0)
		sum += *buf++;

	return(sum);
}

/*
 * Get a single character from the line with a specified time-out period
 * in seconds.  If the function times-out, it returns a -1.
 */

static int gl_flag;

int
getc_line(sec)
unsigned int sec;
{
	extern int fd;
	int gl_force();
	char c;
	unsigned int alarm();

	signal(SIGALRM, gl_force);
	gl_flag = 0;

	alarm(sec);

#ifdef BSD
	if (setjmp(gl_buf))
		return(-1);
#endif /* BSD */

	if (read(fd, &c, 1) <= 0) {
		alarm(0);
		return(-1);
	}
	if (gl_flag)
		return(-1);
	alarm(0);
	return(c & 0xff);
}

/* ARGSUSED */
static int
gl_force(dummy)
int dummy;
{
#ifdef BSD
	longjmp(gl_buf, 1);
#else /* BSD */
	signal(SIGALRM, gl_force);
	gl_flag = 1;
#endif /* BSD */
}

/*
 * Same as above, but reads a bunch of characters.  The return code is
 * now just a success/fail indicator.
 */

static int rl_flag;

int
fread_line(buf, len, sec)
unsigned char *buf;
unsigned int len, sec;
{
	extern int fd;
	int n, rl_force();
	unsigned int try, alarm();

	signal(SIGALRM, rl_force);
	rl_flag = 0;

	alarm(sec);
	while (len) {
					/* read at most CLIST_SIZ chars */
		try = (len > CLIST_SIZ) ? CLIST_SIZ : len;
#ifdef BSD
		if (setjmp(rl_buf))
			return(-1);
#endif /* BSD */
		if ((n = read(fd, (char *) buf, try)) <= 0) {
			alarm(0);
			return(-1);
		}
		if (rl_flag)
			return(-1);
		len -= n;
		buf = buf + n;
	}
	alarm(0);
	return(0);
}

/* ARGSUSED */
static int
rl_force(dummy)
int dummy;
{
#ifdef BSD
	longjmp(rl_buf, 1);
#else /* BSD */
	signal(SIGALRM, rl_force);
	rl_flag = 1;
#endif /* BSD */
}

/*
 * Put a character on the TTY line.  This serves no useful purpose other
 * than making the code look pretty.
 */

int
putc_line(c)
unsigned char c;
{
	extern int fd;

	return(write(fd, (char *) &c, 1));
}

/*
 * Cancel the file transfer.  Send several ^X's to the remote, followed
 * by an equal number of backspaces (in case they have already aborted and
 * we're really at the command line).
 */

void
cancel_xfer(up)
int up;
{
	extern char file_name[15];

	if (!up && !strcmp(param->abort, "DELETE"))
		unlink(file_name);

	putc_line(CAN);
	putc_line(CAN);
	putc_line(CAN);
	putc_line(BS);
	putc_line(BS);
	putc_line(BS);
	return;
}

/*
 * Shorten a file to a predetermined length.  Used to remove the ^Z
 * padding from the end of files.  (Heaven help us, if one day a binary
 * file actually has ^Z's as part of the end of the file).
 */

int
fix_length(file, len)
char *file;
long len;
{
	FILE *fp, *tempfp, *my_fopen();
	register int num;
	char *mktemp(), tempfile[128], buf[BUFSIZ], *strcpy();
	char *s, *strrchr(), *strcat();
	struct stat stbuf;

	if (stat(file, &stbuf) < 0)
		return(1);
					/* see if we have any work to do */
	if (len >= stbuf.st_size)
		return(0);

	if (!(fp = my_fopen(file, "r")))
		return(1);

	/*
	 * The temporary file should be in the same directory as the
	 * file being received because otherwise we'd have no way of
	 * guaranteeing they would be in the same file system.  (Hard
	 * links across different file systems aren't allowed).
	 */
	strcpy(tempfile, file);
	if (s = strrchr(tempfile, '/'))
		*++s = '\0';
	else
		strcpy(tempfile, "./");

	strcat(tempfile, TMP_FILE);
	mktemp(tempfile);

	if (!(tempfp = my_fopen(tempfile, "w"))) {
		fclose(fp);
		return(1);
	}

	while (len != 0L) {
		num = (len > BUFSIZ) ? BUFSIZ : len;
		fread(buf, sizeof(char), num, fp);
		if (fwrite(buf, sizeof(char), num, tempfp) != num) {
			fclose(fp);
			fclose(tempfp);
			return(1);
		}
		len = len - (unsigned int) num;
	}

	fclose(fp);
	fclose(tempfp);

	if (unlink(file) < 0)
		return(1);

	if (link(tempfile, file) < 0)
		return(1);

	if (unlink(tempfile) < 0)
		return(1);

	return(0);
}
