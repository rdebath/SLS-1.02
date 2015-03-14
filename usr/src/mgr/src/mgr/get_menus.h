#ifdef __STDC__
void put_str(BITMAP *map, int x, int y, struct font *font, int op, char *str);
struct menu_state *menu_define(struct font *font, char *list[], char *values[], int max, int color);
struct menu_state *menu_setup(struct menu_state *state, BITMAP *screen, int x, int y, int start);
int menu_get(struct menu_state *state, int mouse, int button, int exit);
struct menu_state *menu_remove(struct menu_state *state);
int menu_destroy(struct menu_state *state);
struct menu_state *menu_copy(register struct menu_state *menu);
#else
extern void put_str();
extern struct menu_state *menu_define();
extern struct menu_state *menu_setup();
extern int menu_get();
extern struct menu_state *menu_remove();
extern int menu_destroy();
extern struct menu_state *menu_copy();
#endif
/*{{{}}}*/
