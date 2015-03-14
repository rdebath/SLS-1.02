/* interface file for mouse driver */
/* Andrew Haylett, 14th December 1992 */

#ifndef MOUSE_H
#define MOUSE_H

#ifndef REVERSE_BUTTONS
#define MS_BUTLEFT	4
#define MS_BUTRIGHT	1
#else
#define MS_BUTLEFT	1
#define MS_BUTRIGHT	4
#endif	/* REVERSE_BUTTONS */
#define MS_BUTMIDDLE	2

#define P_MS		0
#define P_MSC		1
#define P_MM		2
#define P_LOGI		3
#define P_BM		4

struct ms_event {
    enum { MS_NONE, MS_BUTUP, MS_BUTDOWN, MS_MOVE, MS_DRAG } ev_code;
    char ev_butstate;
    int ev_x, ev_y;
    int ev_dx, ev_dy;
};

int ms_init(const int maxx, const int maxy);
int get_ms_event(struct ms_event *ev);

#endif /* MOUSE_H */
