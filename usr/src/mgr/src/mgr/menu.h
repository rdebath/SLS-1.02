/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* header file for menu management routines */

/* low level menu routine parameters */

#define MENU_BORDER		3	/* size of border around menu */

/* high level menu routine params */

#define EXIT_CHOICE	0	/* item was chosen */
#define EXIT_BOTTOM	1	/* menu exited from bottom */
#define EXIT_TOP	2	/* menu exited from top */
#define EXIT_LEFT	4	/* menu exited from left */
#define EXIT_RIGHT	8	/* menu exited from right */

#define MENU_FLAGS	1	/* change menu flags */
#define MENU_AUTO	2	/* auto exit from menu to next */
#define MENU_PAGE	4	/* auto page from menu to next */
#define MENU_SNIP	8	/* only return leaf entries */

/* menu struct member access macros */

#define menu_exit(state)	(state)->exit
#define menu_choice(state)	(state)->current
#define menu_ischoice(state)	((state)->current>=0 && \
				 (state)->current<(state)->count)
#define menu_ypos(state)	((state)->menu_start.y + \
                                 (state)->current * \
                                 (state)->bar_size.y)
#define menu_value(state)	(menu_ischoice(state)&&state->action? \
                                 state->action[state->current].value: \
                                 (char *) 0)
#define menu_next(state)	( menu_exit(state)==EXIT_BOTTOM && \
				    (state)->next>=0 ? (state)->next : \
				  (menu_ischoice(state)? \
                                 (state)->action[(state)->current].next_menu:\
                                  -1))
#define menu_setnext(state,nxt)	((state)->action[nxt].next_menu)

/* menu structures */

struct menu_action {
   char *value;			/* string associated with item */
   short next_menu;		/* next menu number off right */
   };

struct menu_result {
   char *value;			/* value string for this menu chioce */
   struct menu_result *next;	/* next string */
   };

struct menu_state {
   BITMAP *menu;	/* where the menu image goes */
   BITMAP *screen;	/* pointer to the display */
   BITMAP *save;	/* stuff covered by menu */
   short menu_startx, 	/* menu starting coords on screen */
         menu_starty;
   short bar_sizex, 	/* size of one menu item */
         bar_sizey,
         x_pos;		/* x position relative to inside */
   struct menu_action *action;	/* value associated with each item */
   short current;			/* currently selected bar */
   short exit;			/* mouse position in menu */
   short count;			/* number of entries */
   short next;			/* index of next menu page */
   short flags;			/* menu flags */
   };

struct menu_state *menu_setup();
struct menu_state *menu_define();
struct menu_state *menu_remove();
struct menu_state *menu_copy();
