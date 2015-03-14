#include "defs.h"

void show_score(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  char    buf[BUFSIZ];
  
  sprintf( buf, "Score: %d", score);
  XtVaSetValues( score_item, XtNlabel, buf, NULL );
  
  sprintf( buf, "Level: %d", rows / 10);
  XtVaSetValues( level_item, XtNlabel, buf, NULL );

  sprintf( buf, "Rows:  %d", rows);
  XtVaSetValues( rows_item, XtNlabel, buf, NULL );
}

void about_proc( w, event, pars, npars )
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  XtPopupSpringLoaded(about_frame);
}

void quit_proc( w, event, pars, npars )
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  clear_events();
  stop_timer();
  XtDestroyWidget(toplevel);
  exit(0);
}

void end_game(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  end_of_game = 1;
  clear_events();
  stop_timer();
  XtUnmapWidget( start_bt );
  XtUnmapWidget( pause_bt );

  XtVaSetValues(game_over,XtNlabel, "Game Over", NULL); 
  update_highscore_table();
  print_high_scores();
}

void newgame_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  clear_events();
  stop_timer();
  init_all();
}

void start_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (running || end_of_game) return;
  paused = False;
  running = True;
  restore_widget(canvas,event,pars,npars);
  restore_widget(shadow,event,pars,npars);
  restore_widget(nextobject,event,pars,npars);
  set_events();
  start_timer();
}

void resume_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (!paused) return;
  start_proc(w,event,pars,npars);
}

void pause_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (!running) return;
  paused = True;
  running = False;
  restore_widget(canvas,event,pars,npars);
  restore_widget(shadow,event,pars,npars);
  restore_widget(nextobject,event,pars,npars);
  clear_events();
  stop_timer();
}

void drop_block(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (block_can_drop(shape_no, xpos, ypos, rot))
    print_shape( canvas, shape_no, xpos, ypos++, rot, True );
  else {
    if (ypos < 0)
    {
      end_game();
      return;
    }
    else {
      score += shape[shape_no].forms[rot].points;
      store_shape(shape_no, xpos, ypos, rot);
      remove_full_lines(ypos);
      create_shape();
      show_score();
      show_next();
      draw_shadow( shape_no, xpos, rot );
    }
  }
  print_shape( canvas, shape_no, xpos, ypos, rot, False );
  start_timer();
}

void left_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (!running) return;
  
  if (block_can_left(shape_no, xpos, ypos, rot)) {
    print_shape( canvas, shape_no, xpos, ypos, rot, True );
    xpos--;
    print_shape( canvas, shape_no, xpos, ypos, rot, False );
    draw_shadow(shape_no, xpos, rot );
  }
}

void right_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (!running) return;
  
  if (block_can_right(shape_no, xpos, ypos, rot)) {
    print_shape( canvas, shape_no, xpos, ypos, rot, True );
    xpos++;
    print_shape( canvas, shape_no, xpos, ypos, rot, False );
    draw_shadow(shape_no, xpos, rot );
  }
}

void anti_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  int     newrot;
  
  if (!running) return;
  
  newrot = (rot + 3) % 4;
  if (check_rot(shape_no, xpos, ypos, newrot)) {
    print_shape( canvas, shape_no, xpos, ypos, rot, True );
    rot = newrot;
    print_shape( canvas, shape_no, xpos, ypos, rot, False );
    draw_shadow(shape_no, xpos, rot );
  }
}

void clock_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  int     newrot;
  
  if (!running) return;
  
  newrot = (rot + 1) % 4;
  if (check_rot(shape_no, xpos, ypos, newrot)) {
    print_shape( canvas, shape_no, xpos, ypos, rot, True );
    rot = newrot;
    print_shape( canvas, shape_no, xpos, ypos, rot, False );
    draw_shadow(shape_no, xpos, rot );
  }
}

void fast_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  if (!running) return;
  
  while (block_can_drop(shape_no, xpos, ypos, rot)) {
    print_shape( canvas, shape_no, xpos, ypos, rot, True );
    ypos++;
    print_shape( canvas, shape_no, xpos, ypos, rot, False );
  }
}

void done_proc(w, event, pars, npars)
  Widget w;
  XEvent *event;
  String *pars;
  Cardinal *npars;
{
  XtPopdown(score_frame);
  XtPopdown(about_frame);
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
