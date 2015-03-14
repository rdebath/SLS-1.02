#ifdef __STDC__
void initbell(void);
void bell_on(void);
void kbd_reset(void);
int set_kbd(int how);
void initkbd(void);
#else
extern void initbell();
extern void bell_on();
extern void kbd_reset();
extern int set_kbd();
extern void initkbd();
#endif
/*{{{}}}*/
