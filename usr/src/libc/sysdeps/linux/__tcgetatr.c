#include <termios.h>
#include <sys/ioctl.h>

int
__tcgetattr(int fildes, struct termios *termios_p)
{
	return __ioctl(fildes, TCGETS, termios_p);
}
