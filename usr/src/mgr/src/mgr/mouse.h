/* interface file for mouse driver */
/* Andrew Haylett, 14th December 1992 */

#ifndef MOUSE_H
#define MOUSE_H

#define MS_BUTLEFT	4
#define MS_BUTRIGHT	1
#define MS_BUTMIDDLE	2

#define P_MS		0
#define P_SUN		1
#define P_MSC		2
#define P_MM		3
#define P_LOGI		4
#define P_BM		5

struct ms_event {
    enum { MS_NONE, MS_BUTUP, MS_BUTDOWN, MS_MOVE, MS_DRAG } ev_code;
    char ev_butstate;
    int ev_x, ev_y;
    int ev_dx, ev_dy;
};

extern int mfd;

int get_ms_event(struct ms_event *ev);

#endif /* MOUSE_H */
