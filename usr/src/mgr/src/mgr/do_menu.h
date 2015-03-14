#ifdef __STDC__
int set_slide(int x, int y);
int set_page(int x, int y);
struct menu_state *do_menu(char *line, struct font *font, int color);
struct menu_result *do_menus(BITMAP *screen, int mouse, int x, int y, struct font *font,struct menu_state *menu_list[], int menu,int exit_code);
void go_menu(int n);
#else
extern int set_slide();
extern int set_page();
extern struct menu_state *do_menu();
extern struct menu_result *do_menus();
extern void go_menu();
#endif
/*{{{}}}*/
