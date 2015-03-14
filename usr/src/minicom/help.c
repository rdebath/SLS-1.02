/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 */
#include <stdio.h>
#include <sys/types.h>
#include <setjmp.h>
#include "window.h"
#include "minicom.h"
#include "configsym.h"

/* Draw a help screen and return the keypress code. */
int help()
{
  WIN *w;
  int c;
  
  w = wopen(6, 3, 72, 19, BDOUBLE, stdattr, MFG, MBG, 0);
  
  wlocate(w, 21, 0);
  wputs(w, "Minicom Command Summary");
  wlocate(w, 10, 2);

  wprintf(w, "Commands can be called by %s<key>", esc_key());

  wlocate(w, 15, 4);
  wputs(w, "Main Functions");
  wlocate(w, 47, 4);
  wputs(w, "Other Functions");
  wlocate(w, 0, 6);
  wputs(w, " Dialing directory..D  run script (Go)....G | Clear Screen.......C\n");
  wputs(w, " Send files.........S  Receive files......R | cOnfigure Minicom..O\n");
  wputs(w, " comm Parameters....P  Add linefeed.......A | Jump to a shell....J\n");
  wputs(w, " Capture on/off.....L  Hangup.............H | eXit and reset.....X\n");
  wputs(w, " send Break.........B  initialize Modem...M | Quit with no reset.Q\n");
  wputs(w, " Terminal emulation.T  run Kermit.........K\n");
  wputs(w, " lineWrap on/off....W  Help screen........Z");
  wlocate(w, 13, 16);
  wputs(w, "Written by Miquel van Smoorenburg 1992");
  wlocate(w, 6, 14);
  wputs(w, "Select function or press Enter for none.");
  
  wredraw(w, 1);

  c = getch();
  wclose(w, 1);
  return(c);
}
