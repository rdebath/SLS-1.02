#include "defs.h"
#include "icon"
#include <X11/Xos.h>
#include <pwd.h>

initialise()
{
  struct passwd *who;
  char   *getenv();
  int i;
  Arg args[20];
  
  srandom((int) time((time_t *) 0));
  define_shapes();
  if ((name = getenv("XTETRIS")) == NULL) {
    who = getpwuid(getuid());
    name = who->pw_name;
  }
  
  init_all();
  if (resources.speed < STANDARD_SPEED && resources.usescorefile)
  {
    resources.usescorefile = False;
    fprintf( stderr, "%s: Speed too low for high-score table.  Use '-noscore' to avoid this message.\n",
	    programname );
  }
  read_high_scores();
  
  /* Make the icon. */
  
  tetris_icon = XCreateBitmapFromData(XtDisplay(frame),
				      XtWindow(frame),
				      icon_bits, icon_width, icon_height);
  i=0;
  XtSetArg(args[i], XtNiconPixmap, tetris_icon); i++; 
  XtSetValues(toplevel, args, i);
}

init_all()
{
  int     i, j;
  Arg args[20];
  
  paused = False;	/* "running" is set False elsewhere */
  score_position = -1;
  end_of_game = 0;
  rows = score = shape_no = rot = xpos = ypos = 0;
  
  for (i = 0; i < UWIDTH; i++)
    for (j = 0; j < UHEIGHT; j++)
      grid[i][j] = NULL;
  
  create_shape();         /* Set up 1st shape */
  create_shape();         /* Set up next shape */
  /*
   *	Don't show anything until it starts. 
   */
  XClearWindow(XtDisplay(canvas), XtWindow(canvas) );
  XClearWindow(XtDisplay(shadow), XtWindow(shadow) );
  XClearWindow(XtDisplay(nextobject), XtWindow(nextobject) );

  XtVaSetValues(game_over, XtNlabel, "         ", NULL );
  show_score();
}
/*
  emacs mode: indented-text
  
  emacs Local Variables: 
  emacs mode: c 
  emacs c-indent-level: 2
  emacs c-continued-statement-offset: 2
  emacs c-continued-brace-offset: -2
  emacs c-tab-always-indent: nil
  emacs c-brace-offset: 0 
  emacs tab-width: 8
  emacs tab-stop-list: (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84)
  emacs End:
  */
