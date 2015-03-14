/* exp_event.h - event definitions */

int exp_get_next_event();
void exp_usleep();
void exp_init_event();

void (*exp_event_exit)();

extern void exp_event_disarm();
