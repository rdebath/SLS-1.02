#include "defs.h"

static XtIntervalId timer;

start_timer()
{
  unsigned long interval;
  int     droprate;
  
  droprate = 2500 - rows * 12;
  if (droprate < 0) 
    interval = 0;
  else
    interval = droprate / (int)resources.speed;
  
  XFlush(XtDisplay(canvas));
  timer = XtAppAddTimeOut( context, interval, drop_block, NULL);
}

stop_timer()
{
  if( timer )
  {
    XtRemoveTimeOut(timer);
    timer = NULL;
  }
}

set_events()
{
  running = True;
  paused = False;
  XtUnmapWidget( start_bt );
  XtMapWidget( pause_bt );
}

clear_events()
{
  running = False;
  XtUnmapWidget( pause_bt );
  XtMapWidget( start_bt );
}

void restore_widget(w, event, pars, npars )
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  int x, y;

  if (!running && !end_of_game)
  {
    XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, False );
    return;
  }
  if (w == canvas) 
  {
    for(x=0; x<UWIDTH; x++)
      for(y=0; y<UHEIGHT; y++)
	if (grid[x][y] != NULL) {
	  XFillRectangle(XtDisplay(w), XtWindow(w), grid[x][y]->gc,
			 x * resources.boxsize, y * resources.boxsize, 
			 resources.boxsize, resources.boxsize);
	}
    print_shape( canvas, shape_no, xpos, ypos, rot, False );
  }
  else if (w == shadow)
    draw_shadow(shape_no, xpos, rot );
  else if (w == nextobject)
    show_next();
  else
    fprintf( stderr, "Hmm. I got a Refresh() for an unrecognized window!\n" );
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
