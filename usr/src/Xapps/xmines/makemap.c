/*
 *     xmineomatic - win xmines damn fast
 */

#include <stdio.h>
#include <X11/Xlib.h>


#define WIDTH     30
#define HEIGHT    16
#define NUM_MINES 99

#define MINE 9
#define MISSMARKED 10
#define BOOM 11
#define MARKED  0x20
#define COVERED 0x10

#define SWIDTH  20
#define SHEIGHT 20
#define BWIDTH  14
#define XOFFSET 14
#define YOFFSET 80
#define BHEIGHT 14

#define IsMine(x,y) (map[x][y] & MINE)


/* Function prototypes: */
int main(int argc, char *argv[]);
static void RunXmines(void);
static void InitMap(void);
static void WaitForGameStart(void);
static void GetWindowID(void);
static void ClickOnSquare(int x, int y, int button);
static void WinGame(void);
static void InitXStuff(void);

/* Global variables: */
int map[WIDTH][HEIGHT];
int pid;
Window xwid, root;
Display *dpy;
XButtonEvent event;

int
main(int argc, char *argv[])
{
  RunXmines();
  InitMap();
  WaitForGameStart();
  GetWindowID();
  InitXStuff();
  WinGame();

  return 0;
}


static void
WinGame()
{
  int x, y;

  for (y=0; y < HEIGHT; y++)
    for (x=0; x < WIDTH; x++)
      ClickOnSquare(x, y, IsMine(x,y) ? 2 : 0);

  XSync(dpy, 0);
}

static void
RunXmines()
{
  if ((pid=fork()) == 0)  /* child */
    execl("/zbt/games/xmines", "xmines", 0);  /* never return */
}


static void
InitMap()
{
  int i, x, y;
  
  srandom(pid);

  bzero(map, sizeof(map));
  for (i=0; i < NUM_MINES; i++)
    {
      do
	{
	  x = random() % WIDTH;
	  y = random() % HEIGHT;
	}
      while (IsMine(x,y));

      map[x][y] = MINE + COVERED;
    }
}


static void
WaitForGameStart()
{
  printf("Press return when xmines is up and running.\n");
  (void) getchar();
}

static void
InitXStuff()
{ 
  unsigned int junk;

  dpy = XOpenDisplay("");
  XGetGeometry(dpy, xwid, &root, (int *)&junk, (int *)&junk, &junk, &junk,
               &junk, &junk);
  XSync(dpy, 0);

/* Be friendly and fill in all of the XSendEvent fields, but just do it once */  event.type        = ButtonPress;
  event.serial      = 0;
  event.send_event  = 0;
  event.display     = dpy;
  event.window      = xwid;
  event.root        = root;
  event.subwindow   = 0;
  event.time        = CurrentTime;
  event.x           = 0;
  event.y           = 0;
  event.x_root      = 0;
  event.y_root      = 0;
  event.state       = 0;
  event.button      = 0;
  event.same_screen = 1;
}

static void
GetWindowID()
{
  FILE *xwidpipe;

  xwidpipe = popen("/usr/bin/X/xlswins | /bin/awk '/xmines/ {print $1}'", "r");

  if (xwidpipe == NULL)
    fprintf(stderr, "Problem with xlswins.\n"), exit(-1);

  if (getc(xwidpipe) != EOF)    /* There is currently a window! Joy! */
    {
      getc(xwidpipe);    /* Strip off "0x" with these last two getc()'s */
      fscanf(xwidpipe, "%lx", &xwid);
      pclose(xwidpipe);
    }

  else
    fprintf(stderr, "Sorry, xmines window not found.\n"), exit(-1);
}

static void
ClickOnSquare(int x, int y, int button)
{
  event.x      = XOFFSET + x * SWIDTH;
  event.y      = YOFFSET + y * SHEIGHT;
  event.button = button;

  while (!XSendEvent(dpy, xwid, True, 0, (XEvent *) &event));
}
