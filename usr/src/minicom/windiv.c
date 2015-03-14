/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 */
#include <sys/types.h>
#include <setjmp.h>
#ifdef _POSIX_SOURCE
#  include <stdlib.h>
#  undef NULL
#endif
#include <stdio.h>
#include "window.h"
#include "minicom.h"

/*
 * Popup a window and put a text in it.
 */  
/*VARARGS1*/
WIN *tell(s, a1, a2, a3, a4)
char *s, *a1, *a2, *a3, *a4;
{
  WIN *w;
  char buf[128];

  sprintf(buf, s, a1, a2, a3, a4);

  w = wopen(38 - strlen(buf) / 2, 8, 42 + strlen(buf) / 2, 10,
  	BDOUBLE, stdattr, MFG, MBG, 0);
  wcursor(w, CNONE);	
  wlocate(w, 2, 1);
  wputs(w, buf);
  wredraw(w, 1);
  return(w);
}

/*
 * Show an error message.
 */
/*VARARGS1*/
void werror(s, a1, a2, a3, a4)
char *s, *a1, *a2, *a3, *a4;
{
  WIN *tellwin;
  
  tellwin = tell(s, a1, a2, a3, a4);
  sleep(2);
  wclose(tellwin, 1);
}

/*
 * Vertical "wselect" function.
 */
int ask(what, s)
char *what;
char *s[];
{
  int num = 0;
  int cur = 0, ocur = 0;
  int f, c;
  WIN *w;

  for(f = 0; s[f]; f++) num++;

  w = wopen(40 - 5*num , 8, 41 + 5*num, 9, BSINGLE, stdattr, MFG, MBG, 0);
	
  dirflush = 0;

  wcursor(w, CNONE);
  wlocate(w, 1 + 5*num - (strlen(what) / 2), 0);
  wputs(w, what);

  for(f = 1; f < num; f++) {
  	wlocate(w, 2 + 10*f, 1);
  	wputs(w, s[f]);
  }
  wredraw(w, 1);

  while(1) {
  	wlocate(w, 2 + 10*cur, 1);
	if (!useattr)
		wprintf(w, ">%s", s[cur] + 1);
	else {
	  	wsetattr(w, A_REVERSE | stdattr);
  		wputs(w, s[cur]);
	}
  	ocur = cur;
  	wflush();
  	switch(c = getch()) {
  		case ' ':
  		case 27:
  		case 3:
  			dirflush = 1;
  			wclose(w, 1);
  			return(-1);
  		case '\r':
  		case '\n':
  			dirflush = 1;
  			wclose(w, 1);
  			return(cur);
  		case K_LT:
  		case 'h':
  			cur--;
  			if (cur < 0) cur = num - 1;
  			break;
  		default:
  			cur = (cur + 1) % num;
  			break;
  	}
  	wlocate(w, 2 + 10*ocur, 1);
  	wsetattr(w, stdattr);
	if (!useattr)
		wputs(w, " ");
	else
  		wputs(w, s[ocur]);
  }
}

/*
 * Popup a window and ask for input.
 */
char *input(s, buf)
char *s;
char *buf;
{
  WIN *w;

  w = wopen(20, 11, 60, 12, BDOUBLE, stdattr, MFG, MBG, 1);
  wputs(w, s);
  wlocate(w, 0, 1);
  wprintf(w, "> %s", buf);
  wlocate(w, 2, 1);
  if (wgets(w, buf, 40) < 0) buf = CNULL;
  wclose(w, 1);
  return(buf);
}

