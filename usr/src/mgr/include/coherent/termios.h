#include <termio.h>

#define termios termio

#define TCSANOW TCSETA
#define TCIOFLUSH TIOCFLUSH
#define TIOCFLUSH ('t'<<8|16)

#define tcgetattr(x,y) ioctl(x,TCGETA,y)
#define tcsetattr(x,y,z) ioctl(x,y,z)
