#include <termios.h>
#include <errno.h>
#include <sys/ioctl.h>


int
tcsetattr(int fildes, int optional_actions, struct termios *termios_p)
{
	switch(optional_actions) {
		case TCSANOW:
			return __ioctl(fildes, TCSETS, termios_p);
		case TCSADRAIN:
			return __ioctl(fildes, TCSETSW, termios_p);
		case TCSAFLUSH:
			return __ioctl(fildes, TCSETSF, termios_p);
		default:
			errno = EINVAL;
			return -1;
	}
}
