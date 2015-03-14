#include <unistd.h>
#include <termios.h>
#include <sys/ioctl.h>

int tcflow(int fd, int action)
{
	return(__ioctl(fd,TCXONC,action));
}
