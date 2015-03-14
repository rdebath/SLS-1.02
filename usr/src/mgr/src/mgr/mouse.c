/* simple driver for serial mouse */
/* Andrew Haylett, 14th December 1992 */

#include <unistd.h>
#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#ifdef sun
#define u_char unsigned char
#define u_short unsigned short
#include <sys/time.h>
#include <sundev/vuid_event.h>
#endif

#include "mouse.h"

/* thse settings may be altered by the user */
static const int mtype = MOUSE; 
static const int mbaud = 1200;		/* 1200 baud default */
static const int msample = 100;		/* sample rate for Logitech mice */
static const int mdelta = 16;		/* x+y movements more than 25 pixels..*/
static const int maccel = 2;		/* ..are multiplied by 2. */

#define limit(n,l,u)	n = ((n) < (l) ? (l) : ((n) > (u) ? (u) : (n)))
#define abs(x)		(((x) < 0) ? -(x) : (x))

static int mx = 800;  
static int my = 600;  
static int x = 32, y = 32;
int mfd = -1;

static const unsigned short cflag[6] =
{
      (CS7                   | CREAD | CLOCAL | HUPCL ),   /* MicroSoft */
      (CS8 | CSTOPB          | CREAD | CLOCAL | HUPCL ),   /* MouseSystems 3 */
      (CS8 | CSTOPB          | CREAD | CLOCAL | HUPCL ),   /* MouseSystems 5 */
      (CS8 | PARENB | PARODD | CREAD | CLOCAL | HUPCL ),   /* MMSeries */
      (CS8 | CSTOPB          | CREAD | CLOCAL | HUPCL ),   /* Logitech */
      0                                                    /* BusMouse */
};

static const unsigned char proto[5][5] =
{
    /*  hd_mask hd_id   dp_mask dp_id   nobytes */
    { 	0x40,	0x40,	0x40,	0x00,	3 	},  /* MicroSoft */
    {	0xf8,	0x80,	0x00,	0x00,	3	},  /* MouseSystems 3 */
    {	0xf8,	0x80,	0x00,	0x00,	5	},  /* MouseSystems 5 */
    {	0xe0,	0x80,	0x80,	0x00,	3	},  /* MMSeries */
    {	0xe0,	0x80,	0x80,	0x00,	3	},  /* Logitech */
    {	0xf8,	0x80,	0x00,	0x00,	5	}   /* BusMouse */
};

static void
ms_setspeed(const int old, const int new,
            const unsigned short c_cflag)
{
    struct termios tty;
    char *c;

    tcgetattr(mfd, &tty);
    
    tty.c_iflag = IGNBRK | IGNPAR;
    tty.c_oflag = 0;
    tty.c_lflag = 0;
    tty.c_line = 0;
    tty.c_cc[VTIME] = 0;
    tty.c_cc[VMIN] = 1;

    switch (old)
    {
    	case 9600:	tty.c_cflag = c_cflag | B9600; break;
    	case 4800:	tty.c_cflag = c_cflag | B4800; break;
    	case 2400:	tty.c_cflag = c_cflag | B2400; break;
    	case 1200:
	default:	tty.c_cflag = c_cflag | B1200; break;
    }

    tcsetattr(mfd, TCSAFLUSH, &tty);

    switch (new)
    {
    	case 9600:	c = "*q";  tty.c_cflag = c_cflag | B9600; break;
    	case 4800:	c = "*p";  tty.c_cflag = c_cflag | B4800; break;
    	case 2400:	c = "*o";  tty.c_cflag = c_cflag | B2400; break;
    	case 1200:
	default:	c = "*n";  tty.c_cflag = c_cflag | B1200; break;
    }

    write(mfd, c, 2);
    usleep(100000);
    tcsetattr(mfd, TCSAFLUSH, &tty);
}

static int init = 0;

void
ms_init()
{

    init = 1;
    if (mtype != P_BM )
    {
	ms_setspeed(9600, mbaud, cflag[mtype]);
	ms_setspeed(4800, mbaud, cflag[mtype]);
	ms_setspeed(2400, mbaud, cflag[mtype]);
	ms_setspeed(1200, mbaud, cflag[mtype]);

	if (mtype == P_LOGI)
	{
	    write(mfd, "S", 1);
	    ms_setspeed(mbaud, mbaud, cflag[P_MM]);
	}

	if	(msample <= 0)		write(mfd, "O", 1);
	else if	(msample <= 15)		write(mfd, "J", 1);
	else if	(msample <= 27)		write(mfd, "K", 1);
	else if	(msample <= 42)		write(mfd, "L", 1);
	else if	(msample <= 60)		write(mfd, "R", 1);
	else if	(msample <= 85)		write(mfd, "M", 1);
	else if	(msample <= 125)	write(mfd, "Q", 1);
	else				write(mfd, "N", 1);
    }

}

int
get_ms_event(struct ms_event *ev)
{
    unsigned char buf[5];
    char dx, dy;
    int i, acc;

    if (init == 0) ms_init();
    if (mfd == -1)
	return -1;
    if (read(mfd, &buf[0], 1) != 1)
    	return -1;
restart:
/* find a header packet */
    while ((buf[0] & proto[mtype][0]) != proto[mtype][1])
    {
	if (read(mfd, &buf[0], 1) != 1)
	    return -1;
    }

/* read in the rest of the packet */
    for (i = 1; i < proto[mtype][4]; ++i)
    {
	if (read(mfd, &buf[i], 1) != 1)
	    return -1;
/* check whether it's a data packet */
	if ((buf[i] & proto[mtype][2]) != proto[mtype][3] || buf[i] == 0x80)
	    goto restart;
    }

/* construct the event */
    switch (mtype)
    {
	case P_MS:		/* Microsoft */
	default:
	    ev->ev_butstate = ((buf[0] & 0x20) >> 3) | ((buf[0] & 0x10) >> 4);
	    dx = (char)(((buf[0] & 0x03) << 6) | (buf[1] & 0x3F));
	    dy = (char)(((buf[0] & 0x0C) << 4) | (buf[2] & 0x3F));
	    break;
        case P_SUN:		/* Mouse Systems 3 byte as used in Sun workstations */
	    ev->ev_butstate = (~buf[0]) & 0x07;
	    dx =    (char)(buf[1]);
	    dy = - (char)(buf[2]);
	    break;
	case P_MSC:             /* Mouse Systems Corp 5 bytes as used for PCs */
	    ev->ev_butstate = (~buf[0]) & 0x07;
	    dx =    (char)(buf[1]) + (char)(buf[3]);
	    dy = - ((char)(buf[2]) + (char)(buf[4]));
	    break;
	case P_MM:              /* MM Series */
	case P_LOGI:            /* Logitech */
	    ev->ev_butstate = buf[0] & 0x07;
	    dx = (buf[0] & 0x10) ?   buf[1] : - buf[1];
	    dy = (buf[0] & 0x08) ? - buf[2] :   buf[2];
	    break;
	case P_BM:              /* BusMouse */
	    ev->ev_butstate = (~buf[0]) & 0x07;
	    dx =   (char)buf[1];
	    dy = - (char)buf[2];
	    break;
    }

    acc = (abs(ev->ev_dx) + abs(ev->ev_dy) > mdelta) ? maccel : 1;
    ev->ev_dx = dx * acc;
    ev->ev_dy = dy * acc;
    x += ev->ev_dx;
    y += ev->ev_dy;
    limit(x, 0, mx);
    limit(y, 0, my);
    ev->ev_x = x;
    ev->ev_y = y;
    if (dx || dy)
    {
	if (ev->ev_butstate)
	    ev->ev_code = MS_DRAG;
	else
	    ev->ev_code = MS_MOVE;
    }
    else
    {
	if (ev->ev_butstate)
	    ev->ev_code = MS_BUTDOWN;
	else
	    ev->ev_code = MS_BUTUP;
    }
    return 0;
}
