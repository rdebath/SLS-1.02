#ifdef __STDC__
int intersect(WINDOW *win1, WINDOW *win2);
int alone(WINDOW *check);
int mousein(int x, int y, WINDOW *win, int how);
int in_text(int x, int y, WINDOW *win);
#else
extern int intersect();
extern int alone();
extern int mousein();
extern int in_text();
#endif
