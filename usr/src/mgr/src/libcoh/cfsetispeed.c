/* cfsetispeed.c by Harry Pulley, IV; 18DEC92.  Written for Coherent 4.0 to 
   emulate functions, etc. so that it may compile Linux MGR code. */

/* Try changing VMIN to 1 and VTIME to 0 in set_mouseio in set_mode.c - if this */
/* works then we can maybe change this permanently? */

#include <termio.h>

/* cfsetispeed() adapted from Udo Munk's mouse init code for Coherent 
   udo@umunk.GUN.de */

cfsetispeed(buf,speed)
struct termio *buf;
unsigned short speed;
{
        buf->c_cflag &= ~(CBAUD);
        buf->c_cflag |= speed;

/* this line is needed for the mouse code to work - it shouldn't be in a 'real'
   cfsetispeed() implementation, I think - HCPIV */
	buf->c_iflag = IGNBRK;
}
