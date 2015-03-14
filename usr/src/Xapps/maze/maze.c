/* $XConsortium: maze.c,v 1.11 91/04/24 08:30:59 gildea Exp $ */

/******************************************************************************
 * [ maze ] ...
 *
 * modified:  [ 10-4-88 ]  Richard Hess    ...!uunet!cimshop!rhess  
 *              [ Revised primary execution loop within main()...
 *              [ Extended X event handler, check_events()...
 * modified:  [ 1-29-88 ]  Dave Lemke      lemke@sun.com  
 *              [ Hacked for X11...
 *              [  Note the word "hacked" -- this is extremely ugly, but at 
 *              [   least it does the job.  NOT a good programming example 
 *              [   for X.
 * original:  [ 6/21/85 ]  Martin Weiss    Sun Microsystems  [ SunView ]
 *
 ******************************************************************************
 Copyright 1988 by Sun Microsystems, Inc. Mountain View, CA.
  
 All Rights Reserved
  
 Permission to use, copy, modify, and distribute this software and its
 documentation for any purpose and without fee is hereby granted, 
 provided that the above copyright notice appear in all copies and that
 both that copyright notice and this permission notice appear in 
 supporting documentation, and that the names of Sun or MIT not be
 used in advertising or publicity pertaining to distribution of the
 software without specific prior written permission. Sun and M.I.T. 
 make no representations about the suitability of this software for 
 any purpose. It is provided "as is" without any express or implied warranty.
 
 SUN DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE. IN NO EVENT SHALL SUN BE LIABLE FOR ANY SPECIAL, INDIRECT
 OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
 OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
 OR PERFORMANCE OF THIS SOFTWARE.
 *****************************************************************************/

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <stdio.h>

#include <X11/bitmaps/gray1>
#include <X11/bitmaps/xlogo64>
#define logo_width xlogo64_width
#define logo_height xlogo64_height
#define logo_bits xlogo64_bits

#define LOGOSIZE	7
#define MIN_MAZE_SIZE	3
#define MAX_MAZE_SIZE_X	205
#define MAX_MAZE_SIZE_Y	205

#define MOVE_LIST_SIZE  (MAX_MAZE_SIZE_X * MAX_MAZE_SIZE_Y)
  
#define WALL_TOP	0x8000
#define WALL_RIGHT	0x4000
#define WALL_BOTTOM	0x2000
#define WALL_LEFT	0x1000
  
#define DOOR_IN_TOP	0x800
#define DOOR_IN_RIGHT	0x400
#define DOOR_IN_BOTTOM	0x200
#define DOOR_IN_LEFT	0x100
#define DOOR_IN_ANY	0xF00
  
#define DOOR_OUT_TOP	0x80
#define DOOR_OUT_RIGHT	0x40
#define DOOR_OUT_BOTTOM	0x20
#define DOOR_OUT_LEFT	0x10
  
#define START_SQUARE	0x2
#define END_SQUARE	0x1
  
#define SQ_SIZE_X	10
#define SQ_SIZE_Y       10
  
#define NUM_RANDOM      100
  
#define	BORDERWIDTH     2
#define	border_x        (0)
#define	border_y        (0)
#define	MIN_W	        200
#define	MIN_H	        200

#define	DEF_W	        636
#define	DEF_H	        456
char *defgeo = "636x456+10+10";

#define	get_random(x)	(rand() % (x))
  
static int logo_x, logo_y;

static unsigned short maze[MAX_MAZE_SIZE_X][MAX_MAZE_SIZE_Y];

static struct {
  unsigned char x;
  unsigned char y;
  unsigned char dir;
} move_list[MOVE_LIST_SIZE], save_path[MOVE_LIST_SIZE], path[MOVE_LIST_SIZE];

static int maze_size_x, maze_size_y;
static int sqnum, cur_sq_x, cur_sq_y, path_length;
static int start_x, start_y, start_dir, end_x, end_y, end_dir;

Display	*dpy;
Window	win;
Window	iwin;
GC	gc, cgc;
XWindowAttributes win_attr;
int	screen;
long	background;
Pixmap	logo_map;
int	reverse = 0;

int	width = DEF_W, height = DEF_H ;
int	x = 0, y = 0, restart = 0, stop = 1, state = 1;


main(argc,argv)                                               /* main module */
     int argc;
     char **argv;
{
  extern int	optind;
  extern char	*optarg;
  char	*display = NULL;
  char	*geo = NULL;
  char	*cmd;
  int	c;
  int	screen_saver = 0;
  XSizeHints size_hints;
  Pixmap gray;
  int bw = 2;
  int flags;

  cmd = argv[0];
  while ((c = getopt(argc, argv, "rSd:g:")) != EOF)
    switch(c)	{
      
    case 'S':
      screen_saver = 1;
      break;
    case 'd':
      display = optarg;
      break;
    case 'g':
      geo = optarg;
      break;
    case 'r':
      reverse = 1;
      break;
    case '?':
      usage(cmd);
      exit(0);
    }
  
  if ((dpy = XOpenDisplay(display)) == NULL)	{
    fprintf(stderr, "%s: Can\'t open display: %s\n",
	    cmd, XDisplayName(display));
    exit(1);
  }
  screen = DefaultScreen(dpy);
  
  if (screen_saver)	{
    width = DisplayWidth(dpy, screen) - 2 * BORDERWIDTH;
    height = DisplayHeight(dpy, screen) - 2 * BORDERWIDTH;
    x = 0; y = 0;
    flags = (USPosition | USSize);
  } else {
    flags = XGeometry (dpy, DefaultScreen(dpy), geo, defgeo, bw, 1, 1, 0, 0,
		       &x, &y, &width, &height);
  }


  if (reverse)
    background = BlackPixel(dpy, screen) ;
  else
    background = WhitePixel(dpy, screen) ;

  win = XCreateSimpleWindow(dpy, RootWindow(dpy, screen), x, y, width,
			    height, BORDERWIDTH, 1, background );
  
  set_maze_sizes(width, height);
  XSelectInput(dpy, win,
	       ExposureMask | ButtonPressMask | StructureNotifyMask );
  
  gc = XCreateGC(dpy, win, 0, 0);
  cgc = XCreateGC(dpy, win, 0, 0);
  
  gray = XCreateBitmapFromData (dpy, win, gray1_bits, 
				gray1_width, gray1_height);

  if (reverse)	{
      XSetForeground(dpy, gc, WhitePixel(dpy, screen));
      XSetBackground(dpy, gc, BlackPixel(dpy, screen));
      XSetForeground(dpy, cgc, BlackPixel(dpy, screen)); 
      XSetBackground(dpy, cgc, WhitePixel(dpy, screen)); 
      XSetWindowBackground(dpy, win, BlackPixel(dpy, screen));
  }
  else {
      XSetForeground(dpy, gc, BlackPixel(dpy, screen));
      XSetBackground(dpy, gc, WhitePixel(dpy, screen));
      XSetForeground(dpy, cgc, WhitePixel(dpy, screen)); 
      XSetBackground(dpy, cgc, BlackPixel(dpy, screen)); 
      XSetWindowBackground(dpy, win, WhitePixel(dpy, screen)); 
  }
  XSetStipple (dpy, cgc, gray);
  XSetFillStyle (dpy, cgc, FillOpaqueStippled);
  
  if  (!(logo_map = XCreateBitmapFromData(dpy, win, (char *)logo_bits,
					  logo_width, logo_height))) {
    fprintf(stderr, "Can't create logo pixmap\n");
    exit (1);
  }
  size_hints.flags =  flags | PMinSize ;
  size_hints.x = x;
  size_hints.y = y;
  size_hints.width = width;
  size_hints.height = height;
  size_hints.min_width = MIN_W;
  size_hints.min_height = MIN_H;
  
  XSetStandardProperties(dpy, win, "Xmaze", "Xmaze", logo_map,
			 argv, argc, &size_hints);
  XMapWindow(dpy, win);
  srand(getpid());

  while (1) {                            /* primary execution loop [ rhess ] */
    if (check_events()) continue ;
    if (restart || stop) goto pop;
    switch (state) {
    case 1:
      initialize_maze();
      break;
    case 2:
      XClearWindow(dpy, win);
      draw_maze_border();
      break;
    case 3:
      create_maze();
      break;
    case 4:
      XFlush(dpy);
      sleep(2);
      break;
    case 5:
      solve_maze();
      break;
    default:
      XFlush(dpy) ;
      sleep(4) ;
      state = 0 ;
      break;
    }
    ++state;
  pop:
    if (restart) {
      restart = 0 ;
      stop = 0 ;
      state = 1 ;
      XGetWindowAttributes(dpy, win, &win_attr);
      width = win_attr.width ;
      height = win_attr.height ;
      set_maze_sizes(width, height);
      XClearWindow(dpy, win);
      XFlush(dpy) ;
    }
  }
}


check_events()                                  /* X event handler [ rhess ] */
{
  XEvent	e;

  if (XPending(dpy)) {
    XNextEvent(dpy, &e);
    switch (e.type) {
    case ButtonPress:
      switch (e.xbutton.button) {
      case 3:
	XFreeGC(dpy, gc);
	XFreeGC(dpy, cgc);
	XDestroyWindow(dpy, win);
	XCloseDisplay(dpy);
	exit(0);
	break;
      case 2:
	stop = !stop ;
	if (state == 5) state = 4 ;
	else {
	  restart = 1;
	  stop = 0;
	}
	break;
      default:
	restart = 1 ;
	stop = 0 ;
	break;
      }
      break;
    case ConfigureNotify:
      restart = 1;
      break;
    case UnmapNotify:
      stop = 1;
      XClearWindow(dpy, win);
      XFlush(dpy);
      break;
    case Expose:
      restart = 1;
      break;
    }
    return(1);
  }
  return(0);
}	  


usage(cmd)
     char	*cmd;
{
  fprintf(stderr, "usage: %s -S -r [-g geometry] [-d display]\n", cmd);
}


set_maze_sizes(width, height)
{
  maze_size_x = width / SQ_SIZE_X;
  maze_size_y = height / SQ_SIZE_Y;
  
}


initialize_maze()         /* draw the surrounding wall and start/end squares */
{
  register int i, j, wall;
  
  /* initialize all squares */
  for ( i=0; i<maze_size_x; i++) {
    for ( j=0; j<maze_size_y; j++) {
      maze[i][j] = 0;
    }
  }
  
  /* top wall */
  for ( i=0; i<maze_size_x; i++ ) {
    maze[i][0] |= WALL_TOP;
  }
  
  /* right wall */
  for ( j=0; j<maze_size_y; j++ ) {
    maze[maze_size_x-1][j] |= WALL_RIGHT;
  }
  
  /* bottom wall */
  for ( i=0; i<maze_size_x; i++ ) {
    maze[i][maze_size_y-1] |= WALL_BOTTOM;
  }
  
  /* left wall */
  for ( j=0; j<maze_size_y; j++ ) {
    maze[0][j] |= WALL_LEFT;
  }
  
  /* set start square */
  wall = get_random(4);
  switch (wall) {
  case 0:	
    i = get_random(maze_size_x);
    j = 0;
    break;
  case 1:	
    i = maze_size_x - 1;
    j = get_random(maze_size_y);
    break;
  case 2:	
    i = get_random(maze_size_x);
    j = maze_size_y - 1;
    break;
  case 3:	
    i = 0;
    j = get_random(maze_size_y);
    break;
  }
  maze[i][j] |= START_SQUARE;
  maze[i][j] |= ( DOOR_IN_TOP >> wall );
  maze[i][j] &= ~( WALL_TOP >> wall );
  cur_sq_x = i;
  cur_sq_y = j;
  start_x = i;
  start_y = j;
  start_dir = wall;
  sqnum = 0;
  
  /* set end square */
  wall = (wall + 2)%4;
  switch (wall) {
  case 0:
    i = get_random(maze_size_x);
    j = 0;
    break;
  case 1:
    i = maze_size_x - 1;
    j = get_random(maze_size_y);
    break;
  case 2:
    i = get_random(maze_size_x);
    j = maze_size_y - 1;
    break;
  case 3:
    i = 0;
    j = get_random(maze_size_y);
    break;
  }
  maze[i][j] |= END_SQUARE;
  maze[i][j] |= ( DOOR_OUT_TOP >> wall );
  maze[i][j] &= ~( WALL_TOP >> wall );
  end_x = i;
  end_y = j;
  end_dir = wall;
  
  /* set logo */
  if ( (maze_size_x > 15) && (maze_size_y > 15) ) {
    logo_x = get_random(maze_size_x - LOGOSIZE - 6) + 3;
    logo_y = get_random(maze_size_y - LOGOSIZE - 6) + 3;
    
    for (i=0; i<LOGOSIZE; i++) {
      for (j=0; j<LOGOSIZE; j++) {
	maze[logo_x + i][logo_y + j] |= DOOR_IN_TOP;
      }
    }
  }
  else
    logo_y = logo_x = -1;
}


create_maze()             /* create a maze layout given the intiialized maze */
{
  register int i, newdoor;
  
  do {
    move_list[sqnum].x = cur_sq_x;
    move_list[sqnum].y = cur_sq_y;
    move_list[sqnum].dir = newdoor;
    while ( ( newdoor = choose_door() ) == -1 ) { /* pick a door */
      if ( backup() == -1 ) { /* no more doors ... backup */
	return; /* done ... return */
      }
    }
    
    /* mark the out door */
    maze[cur_sq_x][cur_sq_y] |= ( DOOR_OUT_TOP >> newdoor );
    
    switch (newdoor) {
    case 0: cur_sq_y--;
      break;
    case 1: cur_sq_x++;
      break;
    case 2: cur_sq_y++;
      break;
    case 3: cur_sq_x--;
      break;
    }
    sqnum++;
    
    /* mark the in door */
    maze[cur_sq_x][cur_sq_y] |= ( DOOR_IN_TOP >> ((newdoor+2)%4) );
    
    /* if end square set path length and save path */
    if ( maze[cur_sq_x][cur_sq_y] & END_SQUARE ) {
      path_length = sqnum;
      for ( i=0; i<path_length; i++) {
	save_path[i].x = move_list[i].x;
	save_path[i].y = move_list[i].y;
	save_path[i].dir = move_list[i].dir;
      }
    }
    
  } while (1);
  
}


choose_door()                                            /* pick a new path */
{
  int candidates[3];
  register int num_candidates;
  
  num_candidates = 0;
  
 topwall:
  /* top wall */
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_IN_TOP )
    goto rightwall;
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_OUT_TOP )
    goto rightwall;
  if ( maze[cur_sq_x][cur_sq_y] & WALL_TOP )
    goto rightwall;
  if ( maze[cur_sq_x][cur_sq_y - 1] & DOOR_IN_ANY ) {
    maze[cur_sq_x][cur_sq_y] |= WALL_TOP;
    maze[cur_sq_x][cur_sq_y - 1] |= WALL_BOTTOM;
    draw_wall(cur_sq_x, cur_sq_y, 0);
    goto rightwall;
  }
  candidates[num_candidates++] = 0;
  
 rightwall:
  /* right wall */
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_IN_RIGHT )
    goto bottomwall;
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_OUT_RIGHT )
    goto bottomwall;
  if ( maze[cur_sq_x][cur_sq_y] & WALL_RIGHT )
    goto bottomwall;
  if ( maze[cur_sq_x + 1][cur_sq_y] & DOOR_IN_ANY ) {
    maze[cur_sq_x][cur_sq_y] |= WALL_RIGHT;
    maze[cur_sq_x + 1][cur_sq_y] |= WALL_LEFT;
    draw_wall(cur_sq_x, cur_sq_y, 1);
    goto bottomwall;
  }
  candidates[num_candidates++] = 1;
  
 bottomwall:
  /* bottom wall */
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_IN_BOTTOM )
    goto leftwall;
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_OUT_BOTTOM )
    goto leftwall;
  if ( maze[cur_sq_x][cur_sq_y] & WALL_BOTTOM )
    goto leftwall;
  if ( maze[cur_sq_x][cur_sq_y + 1] & DOOR_IN_ANY ) {
    maze[cur_sq_x][cur_sq_y] |= WALL_BOTTOM;
    maze[cur_sq_x][cur_sq_y + 1] |= WALL_TOP;
    draw_wall(cur_sq_x, cur_sq_y, 2);
    goto leftwall;
  }
  candidates[num_candidates++] = 2;
  
 leftwall:
  /* left wall */
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_IN_LEFT )
    goto donewall;
  if ( maze[cur_sq_x][cur_sq_y] & DOOR_OUT_LEFT )
    goto donewall;
  if ( maze[cur_sq_x][cur_sq_y] & WALL_LEFT )
    goto donewall;
  if ( maze[cur_sq_x - 1][cur_sq_y] & DOOR_IN_ANY ) {
    maze[cur_sq_x][cur_sq_y] |= WALL_LEFT;
    maze[cur_sq_x - 1][cur_sq_y] |= WALL_RIGHT;
    draw_wall(cur_sq_x, cur_sq_y, 3);
    goto donewall;
  }
  candidates[num_candidates++] = 3;
  
 donewall:
  if (num_candidates == 0)
    return ( -1 );
  if (num_candidates == 1)
    return ( candidates[0] );
  return ( candidates[ get_random(num_candidates) ] );
  
}


backup()                                                  /* back up a move */
{
  sqnum--;
  cur_sq_x = move_list[sqnum].x;
  cur_sq_y = move_list[sqnum].y;
  return ( sqnum );
}


draw_maze_border()                                  /* draw the maze outline */
{
  register int i, j;
  
  
  for ( i=0; i<maze_size_x; i++) {
    if ( maze[i][0] & WALL_TOP ) {
      XDrawLine(dpy, win, gc,
		border_x + SQ_SIZE_X * i,
		border_y,
		border_x + SQ_SIZE_X * (i+1),
		border_y);
    }
    if ((maze[i][maze_size_y - 1] & WALL_BOTTOM)) {
      XDrawLine(dpy, win, gc,
		border_x + SQ_SIZE_X * i,
		border_y + SQ_SIZE_Y * (maze_size_y),
		border_x + SQ_SIZE_X * (i+1),
		border_y + SQ_SIZE_Y * (maze_size_y));
    }
  }
  for ( j=0; j<maze_size_y; j++) {
    if ( maze[maze_size_x - 1][j] & WALL_RIGHT ) {
      XDrawLine(dpy, win, gc,
		border_x + SQ_SIZE_X * maze_size_x,
		border_y + SQ_SIZE_Y * j,
		border_x + SQ_SIZE_X * maze_size_x,
		border_y + SQ_SIZE_Y * (j+1));
    }
    if ( maze[0][j] & WALL_LEFT ) {
      XDrawLine(dpy, win, gc,
		border_x,
		border_y + SQ_SIZE_Y * j,
		border_x,
		border_y + SQ_SIZE_Y * (j+1));
    }
  }
  
  if (logo_x != -1) {
    XCopyPlane(dpy, logo_map, win, gc,
	       0, 0, logo_width, logo_height,
	       border_x + 3 + SQ_SIZE_X * logo_x,
	       border_y + 3 + SQ_SIZE_Y * logo_y, 1);
  }
  
  draw_solid_square( start_x, start_y, start_dir, gc);
  draw_solid_square( end_x, end_y, end_dir, gc);
}


draw_wall(i, j, dir)                                   /* draw a single wall */
     int i, j, dir;
{
  switch (dir) {
  case 0:
    XDrawLine(dpy, win, gc,
	      border_x + SQ_SIZE_X * i, 
	      border_y + SQ_SIZE_Y * j,
	      border_x + SQ_SIZE_X * (i+1), 
	      border_y + SQ_SIZE_Y * j);
    break;
  case 1:
    XDrawLine(dpy, win, gc,
	      border_x + SQ_SIZE_X * (i+1), 
	      border_y + SQ_SIZE_Y * j,
	      border_x + SQ_SIZE_X * (i+1), 
	      border_y + SQ_SIZE_Y * (j+1));
    break;
  case 2:
    XDrawLine(dpy, win, gc,
	      border_x + SQ_SIZE_X * i, 
	      border_y + SQ_SIZE_Y * (j+1),
	      border_x + SQ_SIZE_X * (i+1), 
	      border_y + SQ_SIZE_Y * (j+1));
    break;
  case 3:
    XDrawLine(dpy, win, gc,
	      border_x + SQ_SIZE_X * i, 
	      border_y + SQ_SIZE_Y * j,
	      border_x + SQ_SIZE_X * i, 
	      border_y + SQ_SIZE_Y * (j+1));
    break;
  }
}


draw_solid_square(i, j, dir, gc)          /* draw a solid square in a square */
     register int i, j, dir;
     GC	gc;
{
  switch (dir) {
  case 0: XFillRectangle(dpy, win, gc,
			 border_x + 3 + SQ_SIZE_X * i, 
			 border_y - 3 + SQ_SIZE_Y * j, 
			 SQ_SIZE_X - 6, SQ_SIZE_Y);
    break;
  case 1: XFillRectangle(dpy, win, gc,
			 border_x + 3 + SQ_SIZE_X * i, 
			 border_y + 3 + SQ_SIZE_Y * j, 
			 SQ_SIZE_X, SQ_SIZE_Y - 6);
    break;
  case 2: XFillRectangle(dpy, win, gc,
			 border_x + 3 + SQ_SIZE_X * i, 
			 border_y + 3 + SQ_SIZE_Y * j, 
			 SQ_SIZE_X - 6, SQ_SIZE_Y);
    break;
  case 3: XFillRectangle(dpy, win, gc,
			 border_x - 3 + SQ_SIZE_X * i, 
			 border_y + 3 + SQ_SIZE_Y * j, 
			 SQ_SIZE_X, SQ_SIZE_Y - 6);
    break;
  }
  XFlush(dpy);
#ifdef	notdef
  (void) check_events();
#endif
}


solve_maze()                             /* solve it with graphical feedback */
{
  int i;
  
  
  /* plug up the surrounding wall */
  maze[start_x][start_y] |= (WALL_TOP >> start_dir);
  maze[end_x][end_y] |= (WALL_TOP >> end_dir);
  
  /* initialize search path */
  i = 0;
  path[i].x = end_x;
  path[i].y = end_y;
  path[i].dir = -1;
  
  /* do it */
  while (1) {
    if ( ++path[i].dir >= 4 ) {
      i--;
      draw_solid_square( (int)(path[i].x), (int)(path[i].y), 
			(int)(path[i].dir), cgc);
    }
    else if ( ! (maze[path[i].x][path[i].y] & 
		 (WALL_TOP >> path[i].dir))  && 
	     ( (i == 0) || ( (path[i].dir != 
			      (int)(path[i-1].dir+2)%4) ) ) ) {
      enter_square(i);
      i++;
      if ( maze[path[i].x][path[i].y] & START_SQUARE ) {
	return;
      }
    } 
    if (check_events()) return;
    /* Abort solve on expose - cheapo repaint strategy */
  }
} 


enter_square(n)                            /* move into a neighboring square */
     int n;
{
  draw_solid_square( (int)path[n].x, (int)path[n].y, 
		    (int)path[n].dir, gc);
  
  path[n+1].dir = -1;
  switch (path[n].dir) {
  case 0: path[n+1].x = path[n].x;
    path[n+1].y = path[n].y - 1;
    break;
  case 1: path[n+1].x = path[n].x + 1;
    path[n+1].y = path[n].y;
    break;
  case 2: path[n+1].x = path[n].x;
    path[n+1].y = path[n].y + 1;
    break;
  case 3: path[n+1].x = path[n].x - 1;
    path[n+1].y = path[n].y;
    break;
  }
}

/* ----<eof> */
