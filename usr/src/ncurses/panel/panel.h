
/*+-------------------------------------------------------------------------
	panel.h - part of PANLES implementation for ncurses/Linux
	zmbenhal@netcom.com
	author: 
	wht@n4hgf.Mt-Park.GA.US
--------------------------------------------------------------------------*/

typedef struct panelobs
{
	struct panelobs *above;
	struct panel *pan;
} PANELOBS;

typedef struct panel
{
	WINDOW *win;
	int wstarty;
	int wendy;
	int wstartx;
	int wendx;
	struct panel *below;
	struct panel *above;
	char *user;
	struct panelobs *obscure;
} PANEL;

extern  WINDOW *panel_window(PANEL *pan);
extern  void update_panels(void );
extern  int hide_panel(PANEL *pan);
extern  int show_panel(PANEL *pan);
extern  int del_panel(PANEL *pan);
extern  int top_panel(PANEL *pan);
extern  int bottom_panel(PANEL *pan);
extern  PANEL *new_panel(WINDOW *win);
extern  PANEL *panel_above(PANEL *pan);
extern  PANEL *panel_below(PANEL *pan);
extern  int set_panel_userptr(PANEL *pan,char *uptr);
extern  char *panel_userptr(PANEL *pan);
extern  int move_panel(PANEL *pan,int starty,int startx);
extern  int replace_panel(PANEL *pan,WINDOW *win);

/* end of function declarations */

/* end of libpanel.h */
