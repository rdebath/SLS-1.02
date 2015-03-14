/*{{{}}}*/
/*{{{  #includes*/
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>

#include "mouse.h"
/*}}}  */

/*{{{  variables*/
static int button_map[8] = {0,1,2,3,4,5,6,7};
/*}}}  */

/*{{{  mouse_get*/
/* primary mouse interface
*/

int mouse_get(int mouse, int *x_delta, int *y_delta)
{
  struct ms_event ev;

	mfd = mouse;
	get_ms_event(&ev);

	*x_delta = ev.ev_dx;
	*y_delta = -ev.ev_dy;

	/* emulate three button mice. middle button is emulated */
	/* by pressing left & right buttons at the same time */

	if (ev.ev_butstate == (MS_BUTLEFT + MS_BUTRIGHT)) ev.ev_butstate = MS_BUTMIDDLE;

#ifdef MOVIE
	log_time();
#endif
	return(button_map[(int)ev.ev_butstate]);
}
/*}}}  */
/*{{{  map_mouse buttons (for non-left handers)*/
int *map_mouse(button,map) int button, map;
{
	if (button>0 && button<8) button_map[button]=map;
	return(button_map);
}
/*}}}  */
/*{{{  mouse_count -- are there still characters in the mouse buffer?*/
int mouse_count()
{
fd_set r;
struct timeval timeout;

	FD_ZERO(&r); FD_SET(mfd, &r);
	timeout.tv_sec = 0;
	timeout.tv_usec = 0;

	return(select(mfd+1, &r, NULL, NULL, &timeout));
}
/*}}}  */
