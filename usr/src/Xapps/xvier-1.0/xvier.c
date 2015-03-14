#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>

#include "xvier.h"

char *malloc();

char *displayname = NULL, *geostring = NULL, *fontpattern = NULL;
int iconic = 0, level = 0;
int rows = 6, columns = 7;
Display *mydisplay;
int myscreen;
Window topwindow, boardwindow, quitwindow,
       newwindow, undowindow, startwindow, changewindow;
#define TOPBORDERWIDTH 5
#define DEFAULTWIDTH	(((10 + 50 * columns) * 5 + 3) / 4)
#define DEFAULTHEIGHT	(10 + 50 * rows)
#define MINWIDTH	((DEFAULTWIDTH * 2) / 5)
#define MINHEIGHT	((DEFAULTHEIGHT * 2) / 5)
#ifndef XVIER_WM_ASPECT_BUG
#define MINASPECTWIDTH	DEFAULTWIDTH
#define MINASPECTHEIGHT	(2 * DEFAULTHEIGHT)
#define MAXASPECTWIDTH	(2 * DEFAULTWIDTH)
#define MAXASPECTHEIGHT	DEFAULTHEIGHT
#endif
GC stonegc[2], buttongc, textgc;
XEvent myevent;
KeySym mykey;
XSizeHints myhint;
XClassHint myclass;
XWMHints *mywmhints;
unsigned long PixelArray[5];
#define WHITE  PixelArray[0]
#define BLACK  PixelArray[1]
#define BLUE   PixelArray[2]
#define YELLOW PixelArray[3]
#define RED    PixelArray[4]
Colormap cmap;
int private_cmap = 0;
Cursor cursor_normal, cursor_q[4];
#include "qup.xbm"
#include "qupm.xbm"
#include "qright.xbm"
#include "qrightm.xbm"
#include "qdown.xbm"
#include "qdownm.xbm"
#include "qleft.xbm"
#include "qleftm.xbm"
Pixmap qup, qupmask, qright, qrightmask, qdown, qdownmask, qleft, qleftmask;
XColor cursorforeground, cursorbackground;
int cursor_num;
#ifndef NO_SELECT
struct timeval selectval;
#endif
unsigned long valuemask;
XSetWindowAttributes attributes;
Pixmap bgpixmap, iconpixmap;
int icon_w_x[6] = {0, 0, 1, 1, 2, 3},
    icon_w_y[6] = {1, 3, 1, 2, 1, 3},
    icon_b_x[6] = {0, 1, 1, 2, 2, 3},
    icon_b_y[6] = {2, 0, 3, 2, 3, 2};
#define DEFAULTICONSIZE 64
#include "patchlevel.h"
char Title[] = XVIER_VERSION;
int c_index = 1, message_index = 0;
char *playercolor[2], levelnumstring[2] = "0",
     *messagestring[5], yellowmovestring[20],
     redmovestring[20];
#define DEFAULTFONTPATTERN "*-Helvetica-Medium-R-Normal-*"
#define MAXFONTS 100
XFontStruct *fontstructarray[MAXFONTS];
int fontnum, quitposx, quitposy, newposx, newposy,
    undoposx, undoposy, startposx, startposy, changeposx, changeposy;
int text1x, text2x, levely, humany, compy, movey;
int topwidth = 0, topheight = 0, boardwidth,
    buttonwidth, buttonheight, buttonspace,
    piece_dx, piece_dy, piece_width, piece_height,
    *stone_x[2], *stone_y[2], stone_n[2] = {0, 0};
XArc *stone[2], *holes;
int pipei[2], pipeo[2], pid, processing;
char *progname = PROGNAME;
#ifdef NO_SELECT
#include <poll.h>
struct pollfd pfd[2];
unsigned long npfd;
#else
#ifdef NO_FD_SET
int readfds, fullfds;
#else
fd_set readfds, fullfds;
#endif
#endif
int *columnfill;

struct _rgb_vals {
  unsigned short red, green, blue;
} color_values[5] = {
    { 65535, 65535, 65535 },
    {     0,     0,     0 },
    {     0,     0, 65535 },
    { 65535, 65535,     0 },
    { 65535,     0,     0 }},
  gray_values[5] = {
    { 65535, 65535, 65535 },
    {     0,     0,     0 },
    { 32767, 32767, 32767 },
    { 65535, 65535, 65535 },
    {     0,     0,     0 }},
  *rgb_values;

int font_cmp(p1, p2)
XFontStruct **p1, **p2;
{
  int ret = ((*p1)->max_bounds.rbearing + (*p1)->max_bounds.lbearing) -
	    ((*p2)->max_bounds.rbearing + (*p2)->max_bounds.lbearing);

  if (ret == 0)
    ret = ((*p1)->max_bounds.ascent + (*p1)->max_bounds.descent) -
	  ((*p2)->max_bounds.ascent + (*p2)->max_bounds.descent);
  return ret;
}

void change_cursor()
{
  if (cursor_num == 3)
    cursor_num = 0;
  else
    cursor_num++;
  XDefineCursor(mydisplay, topwindow, cursor_q[cursor_num]);
}

void write_prog(ch)
char ch;
{
  if (processing)
    XBell(mydisplay, 0);
  else {
    if (write(pipeo[1], &ch, 1) < 1) {
      perror("write to xvier_prog failed");
      exit(1);
    }
    processing = 1;
    cursor_num = 0;
    XDefineCursor(mydisplay, topwindow, cursor_q[0]);
  }
}

void message(newindex)
int newindex;
{
  message_index = newindex;
  XClearArea(mydisplay, topwindow, boardwidth,
	     topheight - movey + compy, 0, 0, False);
  XDrawImageString(mydisplay, topwindow, textgc, text1x, movey,
		   messagestring[newindex], strlen(messagestring[newindex]));
}

void domove(ind, col)
int ind, col;
{
  int n = stone_n[ind]++;

  stone_x[ind][n] = col;
  stone_y[ind][n] = columnfill[col]++;
  stone[ind][n].x = piece_dx + stone_x[ind][n] * (piece_dx + piece_width);
  stone[ind][n].y = piece_dy +
    (rows - 1 - stone_y[ind][n]) * (piece_dy + piece_height);
  stone[ind][n].width = piece_width;
  stone[ind][n].height = piece_height;
  XFillArc(mydisplay, boardwindow, stonegc[ind], stone[ind][n].x,
	   stone[ind][n].y, piece_width, piece_height, 0, 360 * 64);
}

void undomove(ind)
int ind;
{
  int n = --stone_n[ind], col = stone_x[ind][n];

  columnfill[col]--;
  XClearArea(mydisplay, boardwindow, piece_dx / 2 +
	     col * (piece_dx + piece_width), piece_dy / 2 +
	     (rows - 1 - stone_y[ind][n]) * (piece_dy + piece_height),
	     piece_dx + piece_width, piece_dy + piece_height, False);
  XDrawArc(mydisplay, boardwindow, textgc, 1 + piece_dx +
	   col * (piece_dx + piece_width), 1 + piece_dy +
	   (rows - 1 - stone_y[ind][n]) * (piece_dy + piece_height),
	   piece_width - 2, piece_height - 2, 0, 360 * 64);
}

void recalculate(width, height)
int width, height;
{
  int i, j, fontid, d1, d2, d3, maxtextwidth, maxtextheight;
  XCharStruct tmpsize;

  if (width == topwidth && height == topheight)
    return;
  topwidth = width;
  topheight = height;
  boardwidth = (topwidth * 4) / 5;
  buttonspace = (topwidth - boardwidth) / 10;
  buttonwidth = (topwidth - boardwidth) - 2 * buttonspace;
  buttonheight = (topheight / 2 - 4 * buttonspace) / 4;
  if (buttonheight < 2 * buttonspace) {
    buttonspace = topheight / 24;
    buttonwidth = (topwidth - boardwidth) - 2 * buttonspace;
    buttonheight = (topheight / 2 - 4 * buttonspace) / 4;
  }
  for (i = 0; i < fontnum; i++) {
    int cpx, cpy, qpx, qpy, npx, npy, upx, upy;

    XTextExtents(fontstructarray[i], "Change", 6, &d1, &d2, &d3, &tmpsize);
    if (i > 0 && (tmpsize.rbearing + tmpsize.lbearing > buttonwidth - 2 ||
		  tmpsize.ascent + tmpsize.descent > buttonheight - 2))
      continue;
    cpx = (buttonwidth - tmpsize.lbearing - tmpsize.rbearing) / 2 +
      tmpsize.lbearing;
    cpy = (buttonheight - tmpsize.ascent - tmpsize.descent) / 2 +
      tmpsize.ascent;
    XTextExtents(fontstructarray[i], "Quit", 4, &d1, &d2, &d3, &tmpsize);
    if (i > 0 && (tmpsize.rbearing + tmpsize.lbearing > buttonwidth - 2 ||
		  tmpsize.ascent + tmpsize.descent > buttonheight - 2))
      continue;
    qpx = (buttonwidth - tmpsize.lbearing - tmpsize.rbearing) / 2 +
      tmpsize.lbearing;
    qpy = (buttonheight - tmpsize.ascent - tmpsize.descent) / 2 +
      tmpsize.ascent;
    XTextExtents(fontstructarray[i], "New", 3, &d1, &d2, &d3, &tmpsize);
    if (i > 0 && (tmpsize.rbearing + tmpsize.lbearing > buttonwidth - 2 ||
		  tmpsize.ascent + tmpsize.descent > buttonheight - 2))
      continue;
    npx = (buttonwidth - tmpsize.lbearing - tmpsize.rbearing) / 2 +
      tmpsize.lbearing;
    npy = (buttonheight - tmpsize.ascent - tmpsize.descent) / 2 +
      tmpsize.ascent;
    XTextExtents(fontstructarray[i], "Undo", 4, &d1, &d2, &d3, &tmpsize);
    if (i > 0 && (tmpsize.rbearing + tmpsize.lbearing > buttonwidth - 2 ||
		  tmpsize.ascent + tmpsize.descent > buttonheight - 2))
      continue;
    upx = (buttonwidth - tmpsize.lbearing - tmpsize.rbearing) / 2 +
      tmpsize.lbearing;
    upy = (buttonheight - tmpsize.ascent - tmpsize.descent) / 2 +
      tmpsize.ascent;
    XTextExtents(fontstructarray[i], "Start", 5, &d1, &d2, &d3, &tmpsize);
    if (i > 0 && (tmpsize.rbearing + tmpsize.lbearing > buttonwidth - 2 ||
		  tmpsize.ascent + tmpsize.descent > buttonheight - 2))
      continue;
    changeposx = cpx; changeposy = cpy;
    quitposx = qpx; quitposy = qpy;
    newposx = npx; newposy = npy;
    undoposx = upx; undoposy = upy;
    startposx = (buttonwidth - tmpsize.lbearing - tmpsize.rbearing) / 2 +
      tmpsize.lbearing;
    startposy = (buttonheight - tmpsize.ascent - tmpsize.descent) / 2 +
      tmpsize.ascent;
    fontid = fontstructarray[i]->fid;
  }
  XSetFont(mydisplay, buttongc, fontid);
  maxtextheight = (topheight  - 5 * (buttonspace + buttonheight)) / 5;
  maxtextwidth = topwidth - boardwidth - maxtextheight / 4;
  for (i = 0; i < fontnum; i++) {
    int max1x = 0, max2x = 0, j;

    if (i > 0) {
      if (fontstructarray[i]->max_bounds.ascent +
	  fontstructarray[i]->max_bounds.descent > maxtextheight)
	goto next_font;
      for (j = 0; j < 5; j++) {
	XTextExtents(fontstructarray[i], messagestring[j],
		     strlen(messagestring[j]), &d1, &d2, &d3, &tmpsize);
	if (tmpsize.rbearing +
	    fontstructarray[i]->max_bounds.lbearing > maxtextwidth)
	  goto next_font;
      }
    }
    XTextExtents(fontstructarray[i], "Level: ", 7,
		 &d1, &d2, &d3, &tmpsize);
    if (tmpsize.rbearing > max1x)
      max1x = tmpsize.rbearing;
    XTextExtents(fontstructarray[i], "You: ", 5,
		 &d1, &d2, &d3, &tmpsize);
    if (tmpsize.rbearing > max1x)
      max1x = tmpsize.rbearing;
    XTextExtents(fontstructarray[i], "Me: ", 4,
		 &d1, &d2, &d3, &tmpsize);
    if (tmpsize.rbearing > max1x)
      max1x = tmpsize.rbearing;
    XTextExtents(fontstructarray[i], playercolor[0], strlen(playercolor[0]),
		 &d1, &d2, &d3, &tmpsize);
    if (tmpsize.rbearing > max2x)
      max2x = tmpsize.rbearing;
    XTextExtents(fontstructarray[i], playercolor[1], strlen(playercolor[1]),
		 &d1, &d2, &d3, &tmpsize);
    if (tmpsize.rbearing > max2x)
      max2x = tmpsize.rbearing;
    if (i > 0 && max1x + max2x +
	2 * fontstructarray[i]->max_bounds.lbearing > maxtextwidth)
      goto next_font;
    fontid = fontstructarray[i]->fid;
    text1x  = boardwidth + fontstructarray[i]->max_bounds.lbearing +
      (topwidth - boardwidth - maxtextwidth) / 2;
    text2x = text1x + max1x  + fontstructarray[i]->max_bounds.lbearing;
    movey = topheight - maxtextheight / 8 -
      fontstructarray[i]->max_bounds.descent -
	(maxtextheight - fontstructarray[i]->max_bounds.ascent -
	 fontstructarray[i]->max_bounds.descent) / 2;
    compy = movey - maxtextheight - maxtextheight / 4;
    humany = compy - maxtextheight - maxtextheight / 4;
    levely = humany - maxtextheight - maxtextheight / 4;
  next_font:
    ;
  }
  XSetFont(mydisplay, textgc, fontid);
  piece_dx = boardwidth / (columns * 5 + 1);
  piece_width = (boardwidth - (columns + 1) * piece_dx) / columns;
  piece_dy = topheight / (rows * 5 + 1);
  piece_height = (topheight - (rows + 1) * piece_dy) / rows;
  for (i = 0; i < stone_n[0]; i++) {
    stone[0][i].x = piece_dx + stone_x[0][i] * (piece_dx + piece_width);
    stone[0][i].y = piece_dy +
      (rows - 1 - stone_y[0][i]) * (piece_dy + piece_height);
    stone[0][i].width = piece_width;
    stone[0][i].height = piece_height;
  }
  for (i = 0; i < stone_n[1]; i++) {
    stone[1][i].x = piece_dx + stone_x[1][i] * (piece_dx + piece_width);
    stone[1][i].y = piece_dy +
      (rows - 1 - stone_y[1][i]) * (piece_dy + piece_height);
    stone[1][i].width = piece_width;
    stone[1][i].height = piece_height;
  }
  for (i = 0; i < rows; i++)
    for (j = 0; j < columns; j++) {
      holes[i*columns+j].x = 1 + piece_dx + j * (piece_dx + piece_width);
      holes[i*columns+j].y = 1 + piece_dy + i * (piece_dy + piece_height);
      holes[i*columns+j].width = piece_width - 2;
      holes[i*columns+j].height = piece_height - 2;
    }
  XResizeWindow(mydisplay, boardwindow, boardwidth, topheight);
  XMoveResizeWindow(mydisplay, quitwindow, boardwidth + buttonspace,
		    buttonspace, buttonwidth, buttonheight);
  XMoveResizeWindow(mydisplay, newwindow, boardwidth + buttonspace,
		    2 * buttonspace + buttonheight,
		    buttonwidth, buttonheight);
  XMoveResizeWindow(mydisplay, undowindow, boardwidth + buttonspace,
		    buttonspace + 2 * (buttonheight + buttonspace),
		    buttonwidth, buttonheight);
  XMoveResizeWindow(mydisplay, startwindow, boardwidth + buttonspace,
		    buttonspace + 3 * (buttonheight + buttonspace),
		    buttonwidth, buttonheight);
  XMoveResizeWindow(mydisplay, changewindow, boardwidth + buttonspace,
		    buttonspace + 4 * (buttonheight + buttonspace),
		    buttonwidth, buttonheight);
  XSetWindowBorderWidth(mydisplay, quitwindow,
			((buttonspace / 8 < 1) ? 1 : buttonspace / 8));
  XSetWindowBorderWidth(mydisplay, newwindow,
			((buttonspace / 8 < 1) ? 1 : buttonspace / 8));
  XSetWindowBorderWidth(mydisplay, undowindow,
			((buttonspace / 8 < 1) ? 1 : buttonspace / 8));
  XSetWindowBorderWidth(mydisplay, startwindow,
			((buttonspace / 8 < 1) ? 1 : buttonspace / 8));
  XSetWindowBorderWidth(mydisplay, changewindow,
			((buttonspace / 8 < 1) ? 1 : buttonspace / 8));
}

#define Repaint_topwindow() XClearArea(mydisplay, topwindow, boardwidth,\
			      5 * (buttonspace + buttonheight), 0, 0, True)

int main(argc, argv)
int    argc;
char **argv;
{
  char text[10], **fontnames, *av[4], row_string[3], column_string[3];
  int i, j, geo_ret, userx, usery, userwidth, userheight;
  int iconsize, Ilistsize;
  XIconSize *Ilist;

  for (i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-display") == 0)
      if (++i == argc)
	goto usage;
      else
	displayname = argv[i];
    else if (strcmp(argv[i], "-geometry") == 0)
      if (++i == argc)
	goto usage;
      else
	geostring = argv[i];
    else if (strcmp(argv[i], "-fn") == 0)
      if (++i == argc)
	goto usage;
      else
	fontpattern = argv[i];
    else if (strcmp(argv[i], "-prog") == 0)
      if (++i == argc)
	goto usage;
      else
	progname = argv[i];
    else if (strcmp(argv[i], "-rows") == 0)
      if (++i == argc)
	goto usage;
      else {
	if ((rows = atoi(argv[i])) < 4 || rows > MAXRC)
	  goto usage;
      }
    else if (strcmp(argv[i], "-columns") == 0)
      if (++i == argc)
	goto usage;
      else {
	if ((columns = atoi(argv[i])) < 4 || columns > MAXRC)
	  goto usage;
      }
    else if (strcmp(argv[i], "-iconic") == 0)
      iconic++;
    else if (strcmp(argv[i], "-level") == 0)
      if (++i == argc)
	goto usage;
      else {
	if (argv[i][0] == '+' ||
	    ('0' <= argv[i][0] && argv[i][0] <= '9'))
	  level = atoi(argv[i]);
	else
	  level = -1;
	if (level < 0 || level > 9) {
	  fprintf(stderr, "%s: level should be in the range 0..9\n", *argv);
	  exit(1);
	}
      }
    else {
    usage:
      fprintf(stderr, "usage: %s\t[-display <display>]\
\n\t\t[-geometry <geometry>]\n\t\t[-fn <fontpattern>]\n\t\t[-iconic]\
\n\t\t[-rows <rows>]\n\t\t[-columns <columns>]\
\n\t\t[-prog <path>]\n\t\t[-level <num>]\n", *argv);
      exit(1);
    }
  }
  if ((mydisplay = XOpenDisplay(displayname)) == NULL) {
    if (displayname != NULL)
      fprintf(stderr, "%s: Couldn't open display \"%s\".\n",
	      *argv, displayname);
    else
      fprintf(stderr, "%s: Couldn't open display.\n", *argv);
    exit(1);
  }
  if (pipe(pipeo) < 0 || pipe(pipei) < 0) {
    perror("xvier pipe failed");
    exit(1);
  }
  switch (pid = fork()) {
  case -1:
    perror("xvier fork failed");
    exit(1);
  case 0:
    if (dup2(pipeo[0], 0) < 0 || dup2(pipei[1], 1) < 0) {
      perror("xvier dup2 failed");
      exit(1);
    }
#ifdef NO_GETDTABSIZE
#ifdef hpux
    for (i = _NFILE; i > 2; i--)
#else
    for (i = sysconf(_SC_OPEN_MAX); i > 2; i--)
#endif
#else
    for (i = getdtablesize(); i > 2; i--)
#endif
      close(i);
    for (i = 0; i < NSIG; i++)
      signal(i, SIG_DFL);
    sprintf(row_string, "%d", rows);
    sprintf(column_string, "%d", columns);
    av[0] = "xvier_prog";
    av[1] = row_string;
    av[2] = column_string;
    av[3] = (char *) NULL;
    execvp(progname, av);
    perror("xvier child exec");
    exit(1);
  default:
    close(pipeo[0]);
    close(pipei[1]);
  }
#ifdef NO_SELECT
  pfd[0].fd = ConnectionNumber(mydisplay);
  pfd[1].fd = pipei[0];
  pfd[0].events = pfd[1].events = POLLIN;
  npfd = 2L;
#else
#ifdef NO_FD_SET
  fullfds = (1 << ConnectionNumber(mydisplay)) | (1 << pipei[0]);
#else
  FD_ZERO(&fullfds);
  FD_SET(ConnectionNumber(mydisplay), &fullfds);
  FD_SET(pipei[0], &fullfds);
#endif
#endif
  for (i = 0; i < 6; i++) {
    if (read(pipei[0], &text[i], 1) < 1) {
      fprintf(stderr, "%s: read from xvier_prog failed\n", *argv);
      exit(1);
    }
    if (text[i] == 'C') {
      text[i+1] = '\0';
      break;
    }
  }
  text[6] = '\0';
  if (sscanf(text, "%dR%dC", &rows, &columns) != 2 ||
      rows < 4 || rows > MAXRC || columns < 4 || columns > MAXRC) {
    fprintf(stderr, "%s: wrong format from xvier_prog\n", *argv);
    exit(1);
  }
  columnfill = (int *)malloc(columns * sizeof(int));
  for (i = 0; i < columns; i++)
    columnfill[i] = 0;
  stone_x[0] = (int *)malloc(((columns * rows + 1) >> 1) * sizeof(int));
  stone_x[1] = (int *)malloc(((columns * rows + 1) >> 1) * sizeof(int));
  stone_y[0] = (int *)malloc(((columns * rows + 1) >> 1) * sizeof(int));
  stone_y[1] = (int *)malloc(((columns * rows + 1) >> 1) * sizeof(int));
  stone[0] = (XArc *)malloc(((columns * rows + 1) >> 1) * sizeof(XArc));
  stone[1] = (XArc *)malloc(((columns * rows + 1) >> 1) * sizeof(XArc));
  holes = (XArc *)malloc((columns * rows) * sizeof(XArc));
  myscreen = DefaultScreen(mydisplay);
  if (geostring != NULL) {
    char defaultstring[10];

    sprintf(defaultstring, "%dx%d", DEFAULTWIDTH, DEFAULTHEIGHT);
    geo_ret = XGeometry(mydisplay, myscreen, geostring, defaultstring,
			TOPBORDERWIDTH, 1, 1, 0, 0,
			&userx, &usery, &userwidth, &userheight);
  } else
    geo_ret = 0;
  for (i = 0; i < ((columns * rows + 1) >> 1); i++) {
    stone[0][i].angle1 = stone[1][i].angle1 = 0;
    stone[0][i].angle2 = stone[1][i].angle2 = 360 * 64;
  }
  for (i = 0; i < columns * rows; i++) {
    holes[i].angle1 = 0;
    holes[i].angle2 = 360 * 64;
  }
  valuemask = 0;
  playercolor[0] = "WHITE";
  playercolor[1] = "BLACK";
  if (DefaultDepth(mydisplay, myscreen) == 1) {
    WHITE = YELLOW = WhitePixel(mydisplay, myscreen);
    BLACK = BLUE = RED = BlackPixel(mydisplay, myscreen);
  } else {
    Visual *vis = DefaultVisual(mydisplay, myscreen);

    if (vis->class == GrayScale || vis->class == StaticGray ||
	vis->map_entries < 5)
      rgb_values = gray_values;
    else {
      rgb_values = color_values;
      playercolor[0] = "YELLOW";
      playercolor[1] = "RED";
    }
    cmap = DefaultColormap(mydisplay, myscreen);
    for (i = 0; i < 5; i++) {
      XColor color;

      color.red   = rgb_values[i].red;
      color.green = rgb_values[i].green;
      color.blue  = rgb_values[i].blue;
      color.flags = DoRed | DoGreen | DoBlue;
    retry:
      if (XAllocColor(mydisplay, cmap, &color) == 0)
	if (!private_cmap) {
	  private_cmap = 1;
	  cmap = XCopyColormapAndFree(mydisplay, cmap);
	  goto retry;
	} else {
	  fprintf(stderr, "%s: Couldn't allocate color %d\n", *argv, i);
	  exit(2);
	}
      PixelArray[i] = color.pixel;
    }
    if (private_cmap) {
      attributes.colormap = cmap;
      valuemask |= CWColormap;
    }
  }
  sprintf(yellowmovestring, "%s to move", playercolor[0]);
  sprintf(redmovestring, "%s to move", playercolor[1]);
  messagestring[0] = yellowmovestring;
  messagestring[1] = redmovestring;
  messagestring[2] = "I win";
  messagestring[3] = "You win!";
  messagestring[4] = "Draw";
  attributes.background_pixel = WHITE;
  valuemask |= CWBackPixel;
  attributes.border_pixel = BLACK;
  valuemask |= CWBorderPixel;
  attributes.cursor = cursor_normal = XCreateFontCursor(mydisplay, XC_hand2);
  valuemask |= CWCursor;
  attributes.event_mask = KeyPressMask | ExposureMask | StructureNotifyMask;
  valuemask |= CWEventMask;
  attributes.do_not_propagate_mask = ButtonPressMask;
  valuemask |= CWDontPropagate;
  switch (geo_ret & (WidthValue | HeightValue)) {
  case 0:
    myhint.width = DEFAULTWIDTH;
    myhint.height = DEFAULTHEIGHT;
    myhint.flags = PSize;
    break;
  case WidthValue | HeightValue:
    if (userwidth < MINWIDTH) {
      if (geo_ret & XNegative)
	userx += userwidth - MINWIDTH;
      userwidth = MINWIDTH;
    }
    if (userheight < MINHEIGHT) {
      if (geo_ret & YNegative)
	usery += userheight - MINHEIGHT;
      userheight = MINHEIGHT;
    }
#ifndef XVIER_WM_ASPECT_BUG
    if (userwidth * MINASPECTHEIGHT < userheight * MINASPECTWIDTH) {
      if (geo_ret & XNegative)
	userx += userwidth -
	  (userheight * MINASPECTWIDTH) / MINASPECTHEIGHT;
      userwidth = (userheight * MINASPECTWIDTH) / MINASPECTHEIGHT;
    }
    if (userwidth * MAXASPECTHEIGHT > userheight * MAXASPECTWIDTH) {
      if (geo_ret & YNegative)
	usery += userheight -
	  (userwidth * MAXASPECTHEIGHT) / MAXASPECTWIDTH;
      userheight = (userwidth * MAXASPECTHEIGHT) / MAXASPECTWIDTH;
    }
#endif
    myhint.width = userwidth;
    myhint.height = userheight;
    myhint.flags = USSize;
    break;
  case WidthValue:
    if (userwidth < MINWIDTH) {
      if (geo_ret & XNegative)
	userx += userwidth - MINWIDTH;
      userwidth = MINWIDTH;
    }
    if (geo_ret & YNegative)
      usery += DEFAULTHEIGHT - (userwidth * DEFAULTHEIGHT) / DEFAULTWIDTH;
    myhint.width = userwidth;
    myhint.height = (userwidth * DEFAULTHEIGHT) / DEFAULTWIDTH;
    myhint.flags = USSize;
    break;
  case HeightValue:
    if (userheight < MINHEIGHT) {
      if (geo_ret & YNegative)
	usery += userheight - MINHEIGHT;
      userheight = MINHEIGHT;
    }
    if (geo_ret & XNegative)
      userx += DEFAULTWIDTH - (userheight * DEFAULTWIDTH) / DEFAULTHEIGHT;
    myhint.width = (userheight * DEFAULTWIDTH) / DEFAULTHEIGHT;
    myhint.height = userheight;
    myhint.flags = USSize;
    break;
  }
  if (geo_ret & XValue)
    myhint.x = userx;
  else
    myhint.x = (DisplayWidth(mydisplay, myscreen) - myhint.width) / 2;
  if (geo_ret & YValue)
    myhint.y = usery;
  else
    myhint.y = (DisplayHeight(mydisplay, myscreen) - myhint.height) / 2;
  if (geo_ret & (XValue | YValue))
    myhint.flags |= USPosition;
  else
    myhint.flags |= PPosition;
  topwindow = XCreateWindow(mydisplay, DefaultRootWindow(mydisplay),
			    myhint.x, myhint.y, myhint.width, myhint.height,
			    TOPBORDERWIDTH, CopyFromParent, InputOutput,
			    CopyFromParent, valuemask, &attributes);
  qup = XCreateBitmapFromData(mydisplay, topwindow, qup_bits,
			      qup_width, qup_height);
  qupmask = XCreateBitmapFromData(mydisplay, topwindow, qupmask_bits,
				  qupmask_width, qupmask_height);
  qright = XCreateBitmapFromData(mydisplay, topwindow, qright_bits,
				 qright_width, qright_height);
  qrightmask = XCreateBitmapFromData(mydisplay, topwindow, qrightmask_bits,
				     qrightmask_width, qrightmask_height);
  qdown = XCreateBitmapFromData(mydisplay, topwindow, qdown_bits,
				qdown_width, qdown_height);
  qdownmask = XCreateBitmapFromData(mydisplay, topwindow, qdownmask_bits,
				    qdownmask_width, qdownmask_height);
  qleft = XCreateBitmapFromData(mydisplay, topwindow, qleft_bits,
				qleft_width, qleft_height);
  qleftmask = XCreateBitmapFromData(mydisplay, topwindow, qleftmask_bits,
				    qleftmask_width, qleftmask_height);
  cursorforeground.red = cursorforeground.green = cursorforeground.blue = 0;
  cursorforeground.flags = DoRed | DoGreen | DoBlue;
  cursorbackground.red = cursorbackground.green = cursorbackground.blue = 65535;
  cursorbackground.flags = DoRed | DoGreen | DoBlue;
  if ((cursor_q[0] = XCreatePixmapCursor(mydisplay, qup, qupmask,
					 &cursorforeground, &cursorbackground,
					 qup_x_hot, qup_y_hot)) == None ||
      (cursor_q[1] = XCreatePixmapCursor(mydisplay, qright, qrightmask,
					 &cursorforeground, &cursorbackground,
					 qright_x_hot, qright_y_hot)) == None ||
      (cursor_q[2] = XCreatePixmapCursor(mydisplay, qdown, qdownmask,
					 &cursorforeground, &cursorbackground,
					 qdown_x_hot, qdown_y_hot)) == None ||
      (cursor_q[3] = XCreatePixmapCursor(mydisplay, qleft, qleftmask,
					 &cursorforeground, &cursorbackground,
					 qleft_x_hot, qleft_y_hot)) == None) {
    fprintf(stderr, "%s: couldn't create cursors\n", *argv);
    exit(1);
  }
#ifndef NO_SELECT
  selectval.tv_sec = 0;
  selectval.tv_usec = 500000;
#endif
  iconsize = 0;
  if (XGetIconSizes(mydisplay, topwindow, &Ilist, &Ilistsize) != 0)
    for (i = 0; i < Ilistsize; i++) {
      if (Ilist[i].max_width < Ilist[i].max_height) {
	if (Ilist[i].max_width > iconsize &&
	    (Ilist[i].max_width == Ilist[i].max_height ||
	     (Ilist[i].max_width < Ilist[i].max_height &&
	      Ilist[i].max_width >= Ilist[i].min_height &&
	      (Ilist[i].max_width - Ilist[i].min_height) %
	      Ilist[i].height_inc == 0)))
	  iconsize = Ilist[i].max_width;
      } else
	if (Ilist[i].max_height > iconsize &&
	    (Ilist[i].max_height == Ilist[i].max_width ||
	     (Ilist[i].max_height < Ilist[i].max_width &&
	      Ilist[i].max_height >= Ilist[i].min_width &&
	      (Ilist[i].max_height - Ilist[i].min_width) %
	      Ilist[i].width_inc == 0)))
	  iconsize = Ilist[i].max_height;
    }
  if (iconsize == 0)
    iconsize = DEFAULTICONSIZE;
  {
    GC iconpgc;
    int spacing, width;

    spacing = iconsize / 16;
    width = (iconsize - 4 * spacing) / 4;
    iconpixmap = XCreatePixmap(mydisplay, DefaultRootWindow(mydisplay),
			       iconsize, iconsize, 1);
    iconpgc = XCreateGC(mydisplay, iconpixmap, 0, 0);
    XSetForeground(mydisplay, iconpgc, 0L);
    XFillRectangle(mydisplay, iconpixmap, iconpgc, 0, 0, iconsize, iconsize);
    XSetForeground(mydisplay, iconpgc, ~0L);
    for (i = 0; i < 6; i++)
      XDrawArc(mydisplay, iconpixmap, iconpgc,
	       spacing / 2 + icon_w_x[i] * (spacing + width),
	       spacing / 2 + icon_w_y[i] * (spacing + width),
	       width, width, 0, 360 * 64);
    for (i = 0; i < 6; i++)
      XFillArc(mydisplay, iconpixmap, iconpgc,
	       spacing / 2 + icon_b_x[i] * (spacing + width),
	       spacing / 2 + icon_b_y[i] * (spacing + width),
	       width, width, 0, 360 * 64);
    XFreeGC(mydisplay, iconpgc);
  }
  myhint.min_width = MINWIDTH;
  myhint.min_height = MINHEIGHT;
  myhint.flags |= PMinSize;
#ifndef XVIER_WM_ASPECT_BUG
  myhint.min_aspect.x = MINASPECTWIDTH;
  myhint.min_aspect.y = MINASPECTHEIGHT;
  myhint.max_aspect.x = MAXASPECTWIDTH;
  myhint.max_aspect.y = MAXASPECTHEIGHT;
  myhint.flags |= PAspect;
#endif
  XSetStandardProperties(mydisplay, topwindow, Title, Title, iconpixmap,
			 argv, argc, &myhint);
  myclass.res_name = "xvier";
  myclass.res_class = "Xvier";
  XSetClassHint(mydisplay, topwindow, &myclass);
  if (iconic) {
    mywmhints = XGetWMHints(mydisplay, topwindow);
    mywmhints->initial_state = IconicState;
    mywmhints->flags |= StateHint;
    XSetWMHints(mydisplay, topwindow, mywmhints);
  }
  valuemask = 0;
  if (DefaultDepth(mydisplay, myscreen) == 1) {
    unsigned int w, h, w_8;
    char *data;

    XQueryBestStipple(mydisplay, topwindow, 8, 8, &w, &h);
    if (w & 1)
      w *= 2;
    if (h & 1)
      h *= 2;
    w_8 = (w + 7) / 8;
    data = malloc(w_8 * h);
    for (j = 0; j < h; j++)
      for (i = 0; i < w_8; i++)
	data[j * w_8 + i] = ((j & 1) ? 0xaa : 0x55);
    bgpixmap = XCreateBitmapFromData(mydisplay, topwindow, data, w, h);
    attributes.background_pixmap = bgpixmap;
    valuemask |= CWBackPixmap;
    free(data);
  } else {
    attributes.background_pixel = BLUE;
    valuemask |= CWBackPixel;
  }
  attributes.event_mask = ButtonPressMask | ExposureMask;
  valuemask |= CWEventMask;
  attributes.override_redirect = True;
  valuemask |= CWOverrideRedirect;
  attributes.win_gravity = UnmapGravity;
  valuemask |= CWWinGravity;
  boardwindow = XCreateWindow(mydisplay, topwindow, 0, 0, 10, 10, 0,
			      CopyFromParent, InputOutput, CopyFromParent,
			      valuemask, &attributes);
  valuemask = 0;
  attributes.background_pixel = WHITE;
  valuemask |= CWBackPixel;
  attributes.border_pixel = BLACK;
  valuemask |= CWBorderPixel;
  attributes.event_mask = ButtonPressMask | ExposureMask;
  valuemask |= CWEventMask;
  attributes.override_redirect = True;
  valuemask |= CWOverrideRedirect;
  attributes.win_gravity = UnmapGravity;
  valuemask |= CWWinGravity;
  quitwindow = XCreateWindow(mydisplay, topwindow, 0, 0, 10, 10, 0,
			     CopyFromParent, InputOutput, CopyFromParent,
			     valuemask, &attributes);
  newwindow = XCreateWindow(mydisplay, topwindow, 0, 0, 10, 10, 0,
			    CopyFromParent, InputOutput, CopyFromParent,
			    valuemask, &attributes);
  undowindow = XCreateWindow(mydisplay, topwindow, 0, 0, 10, 10, 0,
			     CopyFromParent, InputOutput, CopyFromParent,
			     valuemask, &attributes);
  startwindow = XCreateWindow(mydisplay, topwindow, 0, 0, 10, 10, 0,
			       CopyFromParent, InputOutput, CopyFromParent,
			       valuemask, &attributes);
  changewindow = XCreateWindow(mydisplay, topwindow, 0, 0, 10, 10, 0,
			       CopyFromParent, InputOutput, CopyFromParent,
			       valuemask, &attributes);
  if (fontpattern != NULL) {
    fontnames = XListFonts(mydisplay, fontpattern, MAXFONTS, &fontnum);
    if (fontnames != NULL && fontnum > 0) {
      for (i = 0; i < fontnum; i++)
	fontstructarray[i] = XLoadQueryFont(mydisplay, fontnames[i]);
      qsort((char *) fontstructarray, fontnum, sizeof(XFontStruct *), font_cmp);
      XFreeFontNames(fontnames);
      goto got_fonts;
    } else
      fprintf(stderr, "%s: no fonts found with pattern %s.\n",
	      *argv, fontpattern);
  }
  fontnames = XListFonts(mydisplay, DEFAULTFONTPATTERN, MAXFONTS, &fontnum);
  if (fontnames == NULL || fontnum == 0) {
    fontstructarray[0] = XLoadQueryFont(mydisplay, "fixed");
    fontnum = 1;
  } else {
    for (i = 0; i < fontnum; i++)
      fontstructarray[i] = XLoadQueryFont(mydisplay, fontnames[i]);
    qsort((char *) fontstructarray, fontnum, sizeof(XFontStruct *), font_cmp);
    XFreeFontNames(fontnames);
  }
 got_fonts:
  stonegc[0] = XCreateGC(mydisplay, topwindow, 0, 0);
  XSetForeground(mydisplay, stonegc[0], YELLOW);
  stonegc[1] = XCreateGC(mydisplay, topwindow, 0, 0);
  XSetForeground(mydisplay, stonegc[1], RED);
  buttongc = XCreateGC(mydisplay, topwindow, 0, 0);
  XSetBackground(mydisplay, buttongc, WHITE);
  XSetForeground(mydisplay, buttongc, BLACK);
  textgc = XCreateGC(mydisplay, topwindow, 0, 0);
  XSetBackground(mydisplay, textgc, WHITE);
  XSetForeground(mydisplay, textgc, BLACK);
  recalculate(myhint.width, myhint.height);
  levelnumstring[0] = '0' + level;
  write_prog(levelnumstring[0]);
  XMapSubwindows(mydisplay, topwindow);
  XMapRaised(mydisplay, topwindow);
  while (1) {
    while (XEventsQueued(mydisplay, QueuedAfterReading) > 0) {
      XNextEvent(mydisplay, &myevent);
      switch (myevent.type) {
      case Expose:
	if (myevent.xexpose.count == 0) {
	  if (myevent.xexpose.window == boardwindow) {
	    XDrawArcs(mydisplay, boardwindow, textgc, holes, columns * rows);
	    if (stone_n[0] > 0)
	      XFillArcs(mydisplay, boardwindow,
			stonegc[0], stone[0], stone_n[0]);
	    if (stone_n[1] > 0)
	      XFillArcs(mydisplay, boardwindow,
			stonegc[1], stone[1], stone_n[1]);
	  } else if (myevent.xexpose.window == quitwindow) {
	    XDrawImageString(mydisplay, quitwindow, buttongc,
			     quitposx, quitposy, "Quit", 4);
	  } else if (myevent.xexpose.window == newwindow) {
	    XDrawImageString(mydisplay, newwindow, buttongc,
			     newposx, newposy, "New", 3);
	  } else if (myevent.xexpose.window == undowindow) {
	    XDrawImageString(mydisplay, undowindow, buttongc,
			     undoposx, undoposy, "Undo", 4);
	  } else if (myevent.xexpose.window == startwindow) {
	    XDrawImageString(mydisplay, startwindow, buttongc,
			     startposx, startposy, "Start", 5);
	  } else if (myevent.xexpose.window == changewindow) {
	    XDrawImageString(mydisplay, changewindow, buttongc,
			     changeposx, changeposy, "Change", 6);
	  } else if (myevent.xexpose.window == topwindow) {
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text1x, levely, "Level: ", 7);
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text2x, levely, levelnumstring, 1);
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text1x, humany, "You: ", 5);
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text2x, humany, playercolor[1 - c_index],
			     strlen(playercolor[1 - c_index]));
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text1x, compy, "Me: ", 4);
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text2x, compy, playercolor[c_index],
			     strlen(playercolor[c_index]));
	    XDrawImageString(mydisplay, topwindow, textgc,
			     text1x, movey, messagestring[message_index],
			     strlen(messagestring[message_index]));
	  }
	}
	break;
      case MappingNotify:
	XRefreshKeyboardMapping((XMappingEvent *) &myevent);
	break;
      case ConfigureNotify:
	recalculate(myevent.xconfigure.width, myevent.xconfigure.height);
	XMapSubwindows(mydisplay, topwindow);
	break;
      case ButtonPress:
	if (myevent.xbutton.window == quitwindow)
	  goto xvier_end;
	if (myevent.xbutton.window == newwindow)
	  write_prog('n');
	else if (myevent.xbutton.window == undowindow)
	  write_prog('u');
	else if (myevent.xbutton.window == changewindow) {
	  GC tmpgc;
	  char *stmp;

	  tmpgc = stonegc[0];
	  stonegc[0] = stonegc[1];
	  stonegc[1] = tmpgc;
	  stmp = playercolor[0];
	  playercolor[0] = playercolor[1];
	  playercolor[1] = stmp;
	  stmp = messagestring[0];
	  messagestring[0] = messagestring[1];
	  messagestring[1] = stmp;
	  if (stone_n[0] > 0)
	    XFillArcs(mydisplay, boardwindow, stonegc[0], stone[0], stone_n[0]);
	  if (stone_n[1] > 0)
	    XFillArcs(mydisplay, boardwindow, stonegc[1], stone[1], stone_n[1]);
	  Repaint_topwindow();
	} else if (myevent.xbutton.window == startwindow) {
	  if (processing || message_index > 1 ||
	      stone_n[0] > 0 || stone_n[1] > 0)
	    XBell(mydisplay, 0);
	  else {
	    GC tmpgc;
	    char *stmp;

	    write_prog('s');
	    tmpgc = stonegc[0];
	    stonegc[0] = stonegc[1];
	    stonegc[1] = tmpgc;
	    stmp = playercolor[0];
	    playercolor[0] = playercolor[1];
	    playercolor[1] = stmp;
	    stmp = messagestring[0];
	    messagestring[0] = messagestring[1];
	    messagestring[1] = stmp;
	    c_index = 1 - c_index;
	    Repaint_topwindow();
	  }
	} else if (myevent.xbutton.window == boardwindow) {
	  if (processing || message_index > 1)
	    XBell(mydisplay, 0);
	  else {
	    if ((myevent.xbutton.x % (piece_width + piece_dx)) > piece_dx) {
	      int col = myevent.xbutton.x / (piece_width + piece_dx);

	      if (col < columns)
		if (columnfill[col] < rows) {
		  write_prog('a' + col);
		  domove(1 - c_index, col);
		  message(1 - message_index);
		} else
		  XBell(mydisplay, 0);
	    }
	  }
	}
	break;
      case KeyPress:
	if (XLookupString((XKeyEvent *) &myevent, text, 10, &mykey, 0) == 1) {
	  switch (text[0]) {
	  case 'q': case 'Q':
	    goto xvier_end;
	  case 'a': case 'b': case 'c': case 'd': case 'e':
	  case 'f': case 'g': case 'h': case 'i': case 'j':
	  case 'k': case 'l': case 'm':
	    if (processing || message_index > 1 ||
		text[0] - 'a' >= columns || columnfill[text[0] - 'a'] >= rows)
	      XBell(mydisplay, 0);
	    else {
	      write_prog(text[0]);
	      domove(1 - c_index, text[0] - 'a');
	      message(1 - message_index);
	    }
	    break;
	  case 's': case 'S':
	    if (processing || message_index > 1 ||
		stone_n[0] > 0 || stone_n[1] > 0)
	      XBell(mydisplay, 0);
	    else {
	      GC tmpgc;
	      char *stmp;

	      write_prog('s');
	      tmpgc = stonegc[0];
	      stonegc[0] = stonegc[1];
	      stonegc[1] = tmpgc;
	      stmp = playercolor[0];
	      playercolor[0] = playercolor[1];
	      playercolor[1] = stmp;
	      stmp = messagestring[0];
	      messagestring[0] = messagestring[1];
	      messagestring[1] = stmp;
	      c_index = 1 - c_index;
	      Repaint_topwindow();
	    }
	    break;
	  case 'N': case 'U':
	    text[0] += 'a' - 'A';
	  case 'n': case 'u':
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    write_prog(text[0]);
	    break;
	  case 'C':
	    {
	      GC tmpgc;
	      char *stmp;

	      tmpgc = stonegc[0];
	      stonegc[0] = stonegc[1];
	      stonegc[1] = tmpgc;
	      stmp = playercolor[0];
	      playercolor[0] = playercolor[1];
	      playercolor[1] = stmp;
	      stmp = messagestring[0];
	      messagestring[0] = messagestring[1];
	      messagestring[1] = stmp;
	    }
	    if (stone_n[0] > 0)
	      XFillArcs(mydisplay, boardwindow,
			stonegc[0], stone[0], stone_n[0]);
	    if (stone_n[1] > 0)
	      XFillArcs(mydisplay, boardwindow,
			stonegc[1], stone[1], stone_n[1]);
	    Repaint_topwindow();
	    break;
	  default:
	    XBell(mydisplay, 0);
	    break;
	  }
	}
	break;
      }
    }
    XFlush(mydisplay);
#ifdef NO_SELECT
    poll(pfd, npfd, 500);
    if (pfd[1].revents == POLLIN) {
#else
    readfds = fullfds;
#ifdef NO_FD_SET
    select(32, &readfds, 0, 0, &selectval);
    if (readfds & (1 << pipei[0])) {
#else
    select(FD_SETSIZE, &readfds, (fd_set *) 0, (fd_set *) 0, &selectval);
    if (FD_ISSET(pipei[0], &readfds)) {
#endif
#endif
      char answer;

      if (read(pipei[0], &answer, 1) < 1) {
	fprintf(stderr, "%s: read from xvier_prog failed\n", *argv);
	exit(1);
      }
      if (processing == 0) {
	fprintf(stderr, "%s: unexpected read from xvier_prog\n", *argv);
	goto xvier_end;
      }
      processing = 0;
      XDefineCursor(mydisplay, topwindow, cursor_normal);
      switch (answer) {
      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
      case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
	if (answer - 'a' >= columns ||
	    columnfill[answer - 'a'] >= rows) {
	  fprintf(stderr, "%s: wrong move %c from xvier_prog\n",
		  *argv, answer);
	  goto xvier_end;
	}
	domove(c_index, answer - 'a');
	message(1 - message_index);
	break;
      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
      case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
	if (answer - 'A' >= columns ||
	    columnfill[answer - 'A'] >= rows) {
	  fprintf(stderr, "%s: wrong move %c from xvier_prog\n",
		  *argv, answer);
	  goto xvier_end;
	}
	domove(c_index, answer - 'A');
	message(2);
	break;
      case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
      case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	if (answer - 'N' >= columns ||
	    columnfill[answer - 'N'] >= rows) {
	  fprintf(stderr, "%s: wrong move %c from xvier_prog\n",
		  *argv, answer);
	  goto xvier_end;
	}
	domove(c_index, answer - 'N');
      case 'z':
	message(4);
	break;
      case 'w':
	message(3);
	break;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
	levelnumstring[0] = answer;
	Repaint_topwindow();
	break;
      case 'n':
	XClearArea(mydisplay, boardwindow, 0, 0, 0, 0, False);
	XDrawArcs(mydisplay, boardwindow, textgc, holes, columns * rows);
	stone_n[0] = stone_n[1] = 0;
	for (i = 0; i < columns; i++)
	  columnfill[i] = 0;
	message_index = 1 - c_index;
	Repaint_topwindow();
	break;
      case 'u':
	undomove(c_index);
      case 'v':
	undomove(1 - c_index);
	message_index = 1 - c_index;
	Repaint_topwindow();
	break;
      default:
	XBell(mydisplay, 0);
	break;
      }
    }
    if (processing)
      change_cursor();
  }
 xvier_end:
  XCloseDisplay(mydisplay);
  kill(pid, SIGKILL);
  exit(0);
}
