#ifdef __STDC__
int mouse_get(int mouse, int *x_delta, int *y_delta);
int *map_mouse(int button, int map);
int mouse_count(void);
#else
extern int mouse_get();
extern int *map_mouse();
extern int mouse_count();
#endif
/*{{{}}}*/
