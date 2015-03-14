#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#define UWIDTH          10      /* canvas size in units */
#define UHEIGHT         30

#define STANDARD_SPEED	10		/* minimum speed you must start 
								   at in order to get into the high score table */

#ifndef HIGH_SCORE_TABLE
#define HIGH_SCORE_TABLE	"tetris.scores"
#endif

static Arg args[20];

Boolean running;
Boolean paused;

Widget	 
	toplevel, frame,
	score_frame, score_text,
	about_frame, about_text,
	canvas, shadow, nextobject, start_bt, pause_bt, newgame_bt,
	score_item, level_item, rows_item, game_over;

GC       gc, erasegc, cleargc;
Pixmap	 tetris_icon;

int	end_of_game, score_position;
int     shape_no, xpos, ypos, rot, score, rows;
int	next_no, next_rot;
char   *name;	/* Name of player */
char *programname;

struct resource_struct
{
  Pixel foreground;
  Pixel background;
  Boolean usescorefile;
  Pixmap erasestipple;
  Pixmap boxstipple;
  Dimension boxsize;
  Dimension speed;
  String scorefile;
  String customization;
}
resources;

typedef struct
{
  unsigned long unitson;  /* an array of 4x4 = 16 bits, indicating the on 
			     units in this order:

    X11 coordinates	     <0,0> <1,0> <2,0> <3,0>   <0,1> <1,1> <2,1> <3,1>  <0,2> <1,2> <2,2> <3,2> ... <3,3>*/


  int points;             /* Points for acceptance in this position. */
  XRectangle urect[2];    /* Rectangles to draw in unit form */
  XRectangle rect[2];     /* Rectangles to draw in pixel form */
  short nrect;

  short shadowx;
  unsigned short shadowwidth;
  
  short highesty[4];      /* highest non-zero y in unitson, for each x */
  short highestx[4];      /* highest non-zero x in unitson, for each y */
  short lowestx[4];       /* lowest non-zero y in unitson, for each y */
}
rotshape_type, *rotshape_ptr;

typedef struct shape_table {
  rotshape_type forms[4];
  Pixel   foreground;
  Pixel   background;
  GC      gc;
} shape_type, *shape_ptr;

shape_ptr grid[UWIDTH][UHEIGHT];

extern shape_type shape[7];

void    print_high_scores(), done_proc(), quit_proc(), start_proc(), pause_proc(), newgame_proc();
void	resume_proc(), about_proc();
void    drop_block(), restore_widget(), show_score(), end_game(), left_proc(), right_proc();
void    clock_proc(), anti_proc(), fast_proc();

void store_shape();
void create_shape();

  XtAppContext context;
