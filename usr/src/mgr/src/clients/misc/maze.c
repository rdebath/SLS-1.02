/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/* mgr version */
/* *************************************************************\

	maze.c		

	Author: JGosling
	Information Technology Center
	Carnegie-Mellon University

	(c) Copyright IBM Corporation, 1985
	Written: 28.July.1984


\* ************************************************************ */

static char rcsid[] = "$Header: maze.c,v 4.2 88/06/22 14:37:47 bianchi Exp $";
static char IBMid[] = "(c) Copyright IBM Corporation, 1985";

/* ************************************************************ */
/*								*/
/*	$Log:	maze.c,v $
 * Revision 4.2  88/06/22  14:37:47  bianchi
 * remove version.h
 * 
 * Revision 4.1  88/06/21  13:42:38  bianchi
 * convert copyright notice to public distribution form
 * 
 * Revision 2.1  88/01/19  18:00:58  bianchi
 * add ckmgrterm()
 * 
 * Revision 1.2  87/12/18  09:28:28  bianchi
 * add copyright
 * 
 * Revision 1.1  87/07/27  16:29:27  bianchi
 * initial version
 * 
 * Revision 1.2  85/01/29  02:42:30  peterson
 * . Add standard header and copyright
 * 							*/
/*								*/
/* ************************************************************ */

/* A simple maze wars game to test out user level graphics */

#include <stdio.h>
#include "term.h"
#ifdef sun
#undef _POSIX_SOURCE
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

int errno;

int Redraw = 1;
int debug = 0;
int _func = -1;

#define dprintf		if(debug)fprintf
#define SERVER	"mazewar"

#define M_func(n)	(_func!=n ? (m_func(n),_func=n) : 0)

#define MazeWidth 15
#define MazeHeight (sizeof MazeWalls/MazeWidth)
char MazeWalls[] = {
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
1,1,0,1,0,1,0,1,1,1,0,1,1,0,1,
1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,
1,0,1,0,0,0,1,1,1,0,1,1,0,0,1,
1,0,1,0,1,1,0,0,0,0,1,0,1,0,1,
1,0,0,0,0,1,1,0,1,1,1,0,0,0,1,
1,1,0,1,0,1,1,0,1,1,0,0,1,0,1,
1,1,0,1,0,1,0,0,0,0,0,1,0,0,1,
1,1,0,1,1,1,1,1,0,1,1,1,0,1,1,
1,0,0,0,1,0,0,0,0,0,1,1,0,1,1,
1,0,1,1,1,1,0,1,1,0,0,0,0,1,1,
1,0,0,0,1,0,0,0,1,1,1,1,0,1,1,
1,1,1,0,0,0,1,1,1,1,1,0,0,0,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
};
/*
char MazeWalls[] = {
1,1,1,1,1,1,1,1,1,1,
1,0,0,0,0,0,0,0,0,1,
1,0,1,1,1,0,1,1,0,1,
1,0,0,1,0,0,0,0,1,1,
1,1,1,1,0,1,1,0,0,1,
1,0,0,0,0,1,0,1,0,1,
1,1,0,1,1,1,0,0,0,1,
1,1,0,1,1,0,0,1,0,1,
1,0,0,0,0,0,1,1,0,1,
1,1,1,1,1,1,1,1,1,1,
};
*/

struct DirectionOffsets {
	char left, right, forward, backward;
} DirectionOffsets[4] = {
	{ -1, 1, -MazeWidth, MazeWidth }, /* facing up */
	{ -MazeWidth, MazeWidth, 1, -1 }, /* facing right */
	{ 1, -1, MazeWidth, -MazeWidth }, /* facing down */
	{ MazeWidth, -MazeWidth, -1, 1 }  /* facing left */
};

struct state {
    int id;
    short position, direction;
};

struct state me, him;
#define HashSize 47
struct state others[HashSize];

struct state *SlotsUsed[HashSize];
int NOthers;

int direction = 1;
int position = MazeWidth+1;
int BogyDistance = 999;
struct state *BogyId;
int BogyRDir;

int swidth, sheight;
int fwidth, fheight;

int displaystate[MazeWidth+1];
int MaxDepth;

cx, cy;

#define ForwardFrom(p) ((p)+DirectionOffsets[direction].forward)
#define BackwardFrom(p) ((p)+DirectionOffsets[direction].backward)
#define LeftFrom(p) ((p)+DirectionOffsets[direction].left)
#define RightFrom(p) ((p)+DirectionOffsets[direction].right)

#define At(p) MazeWalls[p]
#define AtForward(p) MazeWalls[ForwardFrom(p)]
#define AtBackward(p) MazeWalls[BackwardFrom(p)]
#define AtLeft(p) MazeWalls[LeftFrom(p)]
#define AtRight(p) MazeWalls[RightFrom(p)]

DrawFrom (p, w, h)
register    p; {
    int     depth = 0;
    int     nmax = 0;
    if (BogyId) {
	DrawEye (BogyDistance, BogyRDir);
	BogyDistance = 9999;
	BogyId = 0;
    }
    M_func(BIT_INVERT);
    while (!At (p) || depth <= MaxDepth) {
	register    iw = w * 4 / 5;
	register    ih = h * 4 / 5;
	register    ThisMask = 0;
	register    DoMask;
	if (depth && BogyId == 0) {
	    register struct state **f;
	    for (f = SlotsUsed; *f; f++)
		if ((*f) -> position == p) {
		    DrawEye (depth, BogyRDir = (*f) -> direction - direction);
		    BogyDistance = depth;
		    BogyId = *f;
		}
	}
	if (!At (p)) {
	    nmax = depth;
	    if (AtLeft (p))
		ThisMask |= 1 << 0;
	    if (AtRight (p))
		ThisMask |= 1 << 1;
	    if (AtForward (p))
		ThisMask |= 1 << 2;
	    if (!AtLeft (p) && AtForward (LeftFrom (p)))
		ThisMask |= 1 << 3;
	    if (!AtRight (p) && AtForward (RightFrom (p)))
		ThisMask |= 1 << 4;
	    if (((AtRight (p) + AtForward (p) + AtRight (ForwardFrom (p))) & 1)
		    || (AtRight (p) && AtForward (p)))
		ThisMask |= 1 << 5;
	    if (((AtLeft (p) + AtForward (p) + AtLeft (ForwardFrom (p))) & 1)
		    || (AtLeft (p) && AtForward (p)))
		ThisMask |= 1 << 6;
	    p = ForwardFrom (p);
	}
	DoMask = depth > MaxDepth ? ThisMask : displaystate[depth] ^ ThisMask;
	displaystate[depth] = ThisMask;

/****************************************************************/

	if (DoMask & (1 << 0)) {
	    m_go (cx - w + 1, cy - h + 1);
	    m_draw (cx - iw, cy - ih);
	    m_go (cx - w + 1, cy + h - 1);
	    m_draw (cx - iw, cy + ih);
	}
	if (DoMask & (1 << 1)) {
	    m_go (cx + w - 1, cy - h + 1);
	    m_draw (cx + iw, cy - ih);
	    m_go (cx + w - 1, cy + h - 1);
	    m_draw (cx + iw, cy + ih);
	}
	if (DoMask & (1 << 2)) {
	    m_go (cx - iw, cy - ih);
	    m_draw (cx + iw, cy - ih);
	    m_go (cx - iw, cy + ih);
	    m_draw (cx + iw, cy + ih);
	}
	if (DoMask & (1 << 3)) {
	    m_go (cx - w + 1, cy - ih);
	    m_draw (cx - iw, cy - ih);
	    m_go (cx - w + 1, cy + ih);
	    m_draw (cx - iw, cy + ih);
	}
	if (DoMask & (1 << 4)) {
	    m_go (cx + w - 1, cy - ih);
	    m_draw (cx + iw, cy - ih);
	    m_go (cx + w - 1, cy + ih);
	    m_draw (cx + iw, cy + ih);
	}
	if (DoMask & (1 << 5)) {
	    m_go (cx + iw, cy - ih);
	    m_draw (cx + iw, cy + ih);
	}
	if (DoMask & (1 << 6)) {
	    m_go (cx - iw, cy - ih);
	    m_draw (cx - iw, cy + ih);
	}
	w = iw;
	h = ih;
	depth++;
    }
    MaxDepth = nmax;
}

FlagRedraw () {
    Redraw++;
}

CanSee (him) {
    register    p;
    register    depth = 1;
    for (p = position; ((p = ForwardFrom (p)), p>0 && p<MazeWidth*MazeWidth && !At (p)); depth++)
	if (p == him)
	    return depth;
    return 0;
}

DrawEye (depth, rdir) {
    register    r = cy < cx ? cy : cx;
    register    sr;
    while (--depth >= 0)
	r = r * 4 / 5;
    sr = r / 3;
    m_go (cx - sr, cy - r);
    m_draw (cx + sr, cy - r);
    m_draw (cx + r, cy - sr);
    m_draw (cx + r, cy + sr);
    m_draw (cx + sr, cy + r);
    m_draw (cx - sr, cy + r);
    m_draw (cx - r, cy + sr);
    m_draw (cx - r, cy - sr);
    m_draw (cx - sr, cy - r);
    while (rdir < 0)
	rdir += 4;
    switch (rdir) {
	case 2: 
	    m_go (cx - r, cy);
	    m_draw (cx, cy - sr);
	    m_draw (cx + r, cy);
	    m_draw (cx, cy + sr);
	    m_draw (cx - r, cy);
	    break;
	case 3: 
	    m_go (cx - r, cy - sr);
	    m_draw (cx, cy);
	    m_draw (cx - r, cy + sr);
	    break;
	case 1: 
	    m_go (cx + r, cy - sr);
	    m_draw (cx, cy);
	    m_draw (cx + r, cy + sr);
	    break;
    }
}

DrawMaze (xo, yo, width, height) {
    register    x,
                y;
    M_func(BIT_OR);
    for (x = 0; x < MazeWidth; x++)
	for (y = 0; y < MazeHeight; y++)
	    if (At (y * MazeWidth + x))
                m_bitwrite(xo + x * width / MazeWidth,
			yo + y * height / MazeHeight,
			(x + 1) * width / MazeWidth - x * width / MazeWidth ,
			(y + 1) * height / MazeHeight - y * height / MazeHeight );
}

DrawAllArrows () {
    register struct state **f;
    DrawArrow (position,direction);
    for (f = SlotsUsed; *f; f++)
    DrawArrow ((*f)->position,(*f)->direction);
}

DrawArrow (position, direction) {
    static char *arrow[4] = {
	"^", ">", "v", "<"
    };
    register    x = position % MazeWidth;
    register    y = position / MazeWidth;
    M_func(BIT_XOR);
    m_moveprint ((((x << 1) + 1) * swidth / MazeWidth) >> 1,
	    ((((y << 1) + 1) * cy / MazeHeight) >> 1) + cy * 2 + (fheight>>1),
	    arrow[direction]);
    m_movecursor(0,0);
}

UpdateOther () {
    register struct state  *s = &others[him.id % HashSize];
    register dist;
    while (s -> id && s -> id != him.id) {
	s--;
	if (s < others)
	    s = others + HashSize - 1;
    }
    M_func(BIT_XOR);
    if (s -> id == 0)
	SlotsUsed[NOthers++] = s;
    else {
	DrawArrow (s -> position, s -> direction);
	if (s == BogyId) {
	    DrawEye (BogyDistance, BogyRDir);
	    BogyDistance = 9999;
	    BogyId = 0;
	}
    }
    if ((dist = CanSee (him.position)) && dist<BogyDistance) {
	BogyDistance = dist;
	BogyId = s;
	DrawEye (BogyDistance, BogyRDir = him.direction - direction);
    }
    *s = him;
    DrawArrow (s -> position, s -> direction);
}

struct sockaddr_in  sock_in, sout;

int     RecvSocket,
        SendSocket,
        rmask;

main( argc, argv )
int	argc;
char	*argv[];
{
    char *getenv();
    int net;
    struct hostent *hp;
    struct servent *sp;
    struct netent  *np;
    char host[32];
    int id;

    ckmgrterm( *argv );

    if (getenv("DEBUG")) {
       debug++;
       }

    m_setup(0);
    m_push(P_FLAGS|P_EVENT);
    m_setmode(M_ABS);
    m_setmode(M_OVERSTRIKE);
    M_func(BIT_SRC);
    m_setevent(REDRAW,"R");
    m_setevent(RESHAPE,"R");
    m_ttyset();
    m_setraw();
    m_setnoecho();

    if ((RecvSocket = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
       perror("recv socket");
    if ((SendSocket = socket (AF_INET, SOCK_DGRAM, 0)) < 0)
       perror("send socket");

    rmask = (1 << fileno (m_termin)) | (1 << RecvSocket);
    gethostname(host,sizeof(host)-1);
    if ((hp=gethostbyname(host)) == NULL)
       perror("gethostbyname");
    if ((sp=getservbyname(SERVER,"udp")) ==0)
       perror("Unknown service");
    net = inet_netof(*((struct in_addr *)hp->h_addr));
    id = inet_lnaof(*((struct in_addr *)hp->h_addr));

    sock_in.sin_family = hp->h_addrtype;
    if (sp>0) {
        sock_in.sin_port = sp->s_port;
        sock_in.sin_addr = inet_makeaddr(net,INADDR_ANY);
        setsockopt (RecvSocket, SOL_SOCKET, SO_REUSEADDR, 0, 0);
        sout = sock_in;
        }

    dprintf(stderr,"host: %s port: %ld, network %d service: %s\n",
           hp->h_name, sp->s_port, net, sp->s_name);
    if (bind (RecvSocket, &sock_in, sizeof sock_in) < 0)
	perror ("bind");

    me.id = (getpid () << 16) + id;

    FlagRedraw ();
    while (1) {
	    me.position = position;
	    me.direction = direction;
	    if (sp>0 && sendto (SendSocket, (char *) &me, sizeof me, 0, &sout, sizeof sout)
                        != sizeof me)
		perror ("sendto");
            else
                dprintf(stderr,"Sent %d %d %d\n",
                        me.id,me.direction,me.position);
	if (Redraw) {
            get_size(0,0,&swidth,&sheight);
            get_font(&fwidth,&fheight);
	    M_func(BIT_SET);
	    cx = swidth / 2;
	    cy = sheight / 3;
	    m_clear();
	    DrawMaze (0, 2 * cy, swidth, cy);
	    Redraw = 0;
	    MaxDepth = -1;
	}
	DrawFrom (position, cx, cy);
	M_func(BIT_OR);
	DrawAllArrows ();
	{
	    register    op = position;
	    register    c;
	    while (1) {
		int     raction = rmask;
		m_flush();
                dprintf(stderr,"Select %x...",raction); fflush(stderr);
		select (32, &raction, 0, 0, 0);
                dprintf(stderr," got %x\n",raction);
		if ((raction & (1 << RecvSocket))
			&& read (RecvSocket, (char *)&him, sizeof him) == sizeof him) {
                    dprintf(stderr,"Got %d (%d) %d %d\n",
                            him.id,me.id,him.direction,him.position);
		    if (him.id != me.id && him.direction < 4)
			UpdateOther ();
		}
		else
		    perror ("read");
		if (Redraw || (raction & (1 << fileno (m_termin))))
		    break;
	    };
	    if (!Redraw) {
		c = getc (m_termin);
	        M_func(BIT_OR);
		DrawAllArrows ();
	    }
	    else
		if (errno != EINTR)
		    break;
		else {
		    errno = 0;
		    continue;
		}
	    switch (c & 0177) {
		case ' ': 
		case 'f': 
		case '8': 
		    position = ForwardFrom (position);
		    break;
		case 'l': 
		case '4': 
		    if (--direction < 0)
			direction = 3;
		    break;
		case 'r': 
		case '6': 
		    if (++direction > 3)
			direction = 0;
		    break;
		case 'b': 
		case '2': 
		    position = BackwardFrom (position);
		    break;
                case 'R':
                    FlagRedraw();
                    break;
		case 'q': 
		case 3: 	/* ^C */
		    m_ttyreset();
                    m_clear();
                    m_pop();
		    exit (1);
		    break;
	    }
	    if (At (position))
		position = op;
	}
    }
}
